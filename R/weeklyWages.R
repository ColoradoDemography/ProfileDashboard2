#' weeklyWages Produces a plot and dataset showing  average weekly wages
#'  for the period from 2000 to the present
#' Pulls data from the BLS API using blascrapeR
#'
#' @param listID the list containing place id and Place names
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic, a html or lates table and a dataset
#' @export
#'

weeklyWages <- function(DBPool,listID, curyr,base=10){

  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }


  # Read data files

  bls_key <-  "83f30bc393ec476ea29aa55d5bb06cf7"
  begyr1 <- "2001"
  endyr1 <- "2019"
  
  begyr2 <- "2020"
  endyr2 <- as.character(curyr)
  # State level  ENU0800140010
  wwagest <- "ENU0800040010"
  wwagecty <- paste0("ENU08",ctyfips,"40010")
  
  blsseries <- c(wwagest,wwagecty)
  
  f.wwage1 <- bls_api(blsseries,
                         startyear = begyr1, endyear = endyr1,
                         registrationKey = bls_key,
                         calculations = FALSE, annualaverage = TRUE, catalog = TRUE) %>% filter(period == "Q05") %>% arrange(year)
  
  f.wwage2 <- bls_api(blsseries,
                         startyear = begyr2, endyear = endyr2,
                         registrationKey = bls_key,
                         calculations = FALSE, annualaverage = TRUE, catalog = FALSE) %>% filter(period == "Q05") %>% arrange(year)
 

  f.wagePL <- bind_rows(f.wwage1, f.wwage2)  %>%
    mutate(geoname = ifelse(str_sub(seriesID,4,8) == "08000","Colorado",ctyname),
           fmt_wages = paste0("$", formatC(as.numeric(value), format="f", digits=0, big.mark=",")))  %>%
      filter(value != 0) %>%
      select(geoname, year, fmt_wages, value) %>%
       arrange(geoname, year)

  #Preparing the Plot

  f.plot <- f.wagePL

  axs <- setAxis(f.plot$value)
  axs$maxBrk <- axs$maxBrk + 50

  if(max(f.plot$year) %% 2 ==0) {  #even year
       yrRng = seq(2002,max(f.plot$year),2)
  } else {
    yrRng = seq(2001,max(f.plot$year),2) 
       }

  f.plot <- f.plot %>% filter(year %in% yrRng)
  
  f.plot$geoname <- factor(f.plot$geoname,levels=c(ctyname,"Colorado"))
 

  pltTitle <- "Average Weekly Wage,\nin Nominal Dollars"

  Plot <- f.plot %>%
    ggplot(aes(x=year, y=value, colour=geoname, group=geoname))+
    geom_line(linewidth=1.5) + geom_point(size=2.5) +
    scale_colour_manual("Geography", values=c("#6EC4E8", "#00953A")) +
    geom_text(mapping=aes(x=year, y=value, label=fmt_wages),
              vjust = -0.75, size = 4,  colour="black",
              position = position_dodge(width = 1),
              inherit.aes = TRUE) +
    scale_y_continuous(limits=c(axs$minBrk,axs$maxBrk), label=dollar)+
  #  scale_x_discrete() +
    scale_fill_manual(values=c("#6EC4E8","#00953A"),
                      name="Geography")+
    theme_codemog(base_size=base)+
    labs(title = pltTitle,
         subtitle = ctyname,
         caption = captionSrc("QCEW",""),
         x = "Year",
         y= "Average Weekly Wage") +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text = element_text(size=12),
          legend.position= "bottom")
 
  f.wages <- f.wagePL %>% select(-value) %>% pivot_wider(names_from=geoname, values_from=fmt_wages) 

  
  # Text
  OutText <- paste0("The unajdusted (nominal) average weekly wages for ",ctyname," and Colorado are shown here.")
  OutText <- paste0(OutText," The gain or loss of a major employer such as a mine or a hospital can have a significant impact on a county’s average weekly wage.")
  OutText <- paste0(OutText," These wages are shown only for jobs located within that county and do not include most proprietors.")
  OutText <- paste0(OutText," Household income can be influenced by the average weekly wage, but in areas that have")
  OutText <- paste0(OutText," considerable amounts commuting or unearned income this relationship is not particularly strong.")
  
  
  outList <- list("plot" = Plot, "data" = f.wages, "text" = OutText)


  return(outList)
}
