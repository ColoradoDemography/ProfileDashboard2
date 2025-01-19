#' Unemployment Creates a plot comparing unemployment and percent of population in labor force
#' Pulls data from the BLS API using blascrapeR
#'
#' @param listID the list containing place id and Place names
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphics and associated data sets
#' @export

unemployment <- function(DBPool,listID, curyr, base=10){
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }

# Unemployment data  Reading API credentials for BLS
 bls_key <-  "83f30bc393ec476ea29aa55d5bb06cf7"
 begyr1 <- "2000"
 endyr1 <- "2019"
 
 begyr2 <- "2020"
 endyr2 <- as.character(curyr)

 unempn <- paste0("LAUCN08",ctyfips,"0000000003")
 unemplf <- paste0("LAUCN08",ctyfips,"0000000006")
 blsseries <- c(unempn, unemplf)

 f.unemptmp1 <- bls_api(blsseries,
               startyear = begyr1, endyear = endyr1,
               registrationKey = bls_key,
               calculations = FALSE, annualaverage = TRUE, catalog = FALSE) %>% filter(period == "M13") %>% arrange(year)
 
 f.unemptmp2 <- bls_api(blsseries,
                        startyear = begyr2, endyear = endyr2,
                        registrationKey = bls_key,
                        calculations = FALSE, annualaverage = TRUE, catalog = FALSE) %>% filter(period == "M13") %>% arrange(year)

 f.unemptmp <- bind_rows( f.unemptmp1,  f.unemptmp2) 
 
f.unempN <-  f.unemptmp %>% filter(seriesID == unempn) %>% select(year, value)
              
f.unempLF <- f.unemptmp %>% filter(seriesID == unemplf) %>%
             mutate(lfpart = value) %>%
             select(year, lfpart)



  #single year of age (to get pop16+)
  sya <- paste0("SELECt * FROM estimates.county_sya where age >= 16 AND countyfips = ", as.numeric(ctyfips)," AND year >= 2000 AND year <= ",curyr,";")
  f.sya <- dbGetQuery(DBPool, sya)
  
  f.syasum <- f.sya %>% group_by(year) %>% summarize(pop16 = sum(totalpopulation))
  
  f.unemploym <- inner_join(f.unempLF,f.syasum,by ="year") %>% mutate(lfpart16 = (lfpart/pop16) * 100) %>% select(year, lfpart16)
  # Creating final data set for output
  f.out <- inner_join(f.unempN, f.unemploym, by="year") %>% 
          mutate(place = ctyname,
                 unemprate = value) %>%
          select(place, year, lfpart16, unemprate)
          

  f.unempN$type <- "Unemployment Rate"
  names(f.unempN) <- c("year","value","type")
 
  f.unemploym$type <- "Labor Force Participation Rate"
  names(f.unemploym) <- c("year","value","type")

  f.plot <- bind_rows(f.unempN,f.unemploym)
  
  p.jobs <- f.plot %>%
    ggplot(aes(x=year, y=value))+
    geom_rect(aes(xmin=2008, xmax=2010, ymin=-Inf, ymax=+Inf), fill=rgb(208, 210, 211, max = 255), alpha=.03)+
    geom_rect(aes(xmin=2001, xmax=2002, ymin=-Inf, ymax=+Inf), fill=rgb(208, 210, 211, max = 255), alpha=.03)+
    geom_rect(aes(xmin=2020, xmax=2021, ymin=-Inf, ymax=+Inf), fill=rgb(208, 210, 211, max = 255), alpha=.03)+
    geom_line(color=rgb(0, 168, 58, max = 255), size=1.5)+
    scale_x_continuous(breaks=c(2000:curyr))+
    scale_y_continuous(labels=percent)+
    theme_codemog(base_size=base)+
    facet_grid(rows =vars(type), scales = "free_y") +
    labs(x="Year",
         y = "Rate",
         title= paste0("Labor Force Participation and Unemployment Rate\n2000 to ",as.character(curyr)),
         subtitle = ctyname,
         caption= paste0(captionSrc("SDOBEA","") ,"\nNote: Grey shading represents beginning to bottom of U.S. recessions")) +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          axis.text.x = element_text(angle=45,size=10),
          axis.text.y = element_text(size=11))
 
   # Final Data

   f.out$unemprate <- sprintf("%1.1f%%",round(f.out$unemprate,digits=1))
   f.out$lfpart16 <- sprintf("%1.1f%%",round(f.out$lfpart16,digits=1))
   names(f.out) <- c("Place","Year","Labor Force Participation Rate","Unemployment Rate")
   
   outText <- "The labor force participation and employment plot compares the percentage of persons age 16 and older in the labor force to the unemployment rate."
   outText <- paste0(outText," The pattern of labor force partipation and unemployment in ",ctyname," are closely related.")
   outText <- paste0(outText," The downward trend in labor force partipation is related to the aging patterns in the county,")
   outText <- paste0(outText," along with the availability and character of employment.")
   outText <- paste0(outText," Additionally, as unemployment falls, the incentive for people to enter the labor force increases.") 
   
  outList <- list("plot"= p.jobs, "data" = f.out,"text" = outText)

  return(outList)
}
