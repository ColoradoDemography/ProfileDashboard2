#' Unemployment Creates a plot comapairing unemployment and percent of populaton in labor force
#'
#'
#' @param listID the list containing place id and Place names
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
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
 
# Unemployment data
  Unemp <-  paste0("SELECT * FROM estimates.bls_unemployment WHERE fips = ",as.numeric(ctyfips), ";")
  # Read data files
  f.unemploy <- dbGetQuery(DBPool, Unemp)
  f.unemploy <- f.unemploy[which(f.unemploy$year >= 2000 & f.unemploy$year <= curyr),]
 

  #single year of age (to get pop16+)
  sya <- paste0("SELECt * FROM estimates.county_sya where age >= 16 AND countyfips = ", as.numeric(ctyfips)," AND year >= 2000 AND year <= ",curyr,";")
  f.sya <- dbGetQuery(DBPool, sya)
  
  f.syasum <- f.sya %>% group_by(year) %>% summarize(pop16 = sum(totalpopulation))
  
  f.unemploym <- inner_join(f.unemploy,f.syasum,by ="year") %>% mutate(lfpart16 = (lfpart/pop16) * 100)
  
  f.plotlf <-  f.unemploym[,c(2,8)]
  f.plotlf$type <- "Labor Force Participation Rate"
  names(f.plotlf) <- c("year","value","type")
  
  f.plotun <-  f.unemploym[,c(2,6)]
  f.plotun$type <- "Unemployment Rate"
  names(f.plotun) <- c("year","value","type")

  f.plot <- bind_rows(f.plotlf,f.plotun)
  
  p.jobs <- f.plot %>%
    ggplot(aes(x=year, y=value))+
    geom_rect(aes(xmin=2008, xmax=2010, ymin=-Inf, ymax=+Inf), fill=rgb(208, 210, 211, max = 255), alpha=.03)+
    geom_rect(aes(xmin=2001, xmax=2002, ymin=-Inf, ymax=+Inf), fill=rgb(208, 210, 211, max = 255), alpha=.03)+
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
   
   f.unemploym$geoname <- ctyname
   f.out <- f.unemploym[,c(9,2,6,8)]
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
