#' weeklyWages Produces a plot and dataset showing  average weekly wages
#'  for the period from 2000 to the present
#'
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

  wagePLSQL <- paste0("SELECT * FROM estimates.weekly_wages WHERE fips = '",as.numeric(ctyfips), "';")
  wageSTSQL <- paste0("SELECT * FROM estimates.weekly_wages WHERE fips = '0';")

  
  # Read data files

  f.wagePL <- dbGetQuery(DBPool, wagePLSQL)
  f.wageST <- dbGetQuery(DBPool, wageSTSQL)


  # Place data
  f.wagePL$wages <- as.numeric(f.wagePL$weekly_wage)
  
  f.wagePL$fmt_wages <- paste0("$", formatC(as.numeric(f.wagePL$wages), format="f", digits=0, big.mark=","))
  f.wagePL <- f.wagePL[which(f.wagePL$year >= 2001),]
  f.wagePL <- f.wagePL[which(f.wagePL$wages != 0),]
  f.wagePL$geoname <- ctyname

  # State data
  f.wageST$wages <- as.numeric(f.wageST$weekly_wage)
  f.wageST$fmt_wages <- paste0("$", formatC(as.numeric(f.wageST$wages), format="f", digits=0, big.mark=","))
  f.wageST <- f.wageST[which(f.wageST$year >= 2001),]
  f.wageST$geoname <- "Colorado"

  #Preparing the Plot

  f.plot <- rbind(f.wagePL, f.wageST)

  maxYr <- as.numeric(max(f.plot$year))
  f.plot <- f.plot[which(f.plot$year %in% seq(2001,maxYr,2)),]

  axs <- setAxis(f.plot$wages)
  axs$maxBrk <- axs$maxBrk + 50

  f.plot$geoname <- factor(f.plot$geoname,levels=c(ctyname,"Colorado"))
 

  pltTitle <- paste0("Average Weekly Wage,\nin Real (",max(f.plot$year),") Dollars")
  f.plot$year <- factor(f.plot$year,labels=c("2001","2003", "2005",
                                              "2007","2009",
                                              "2011","2013","2015",
                                              "2017"))
  
  Plot <- f.plot %>%
    ggplot(aes(x=year, y=wages, colour=geoname, group=geoname))+
    geom_line(size=1.5) + geom_point(size=2.5) +
    scale_colour_manual("Geography", values=c("#6EC4E8", "#00953A")) +
    geom_text(mapping=aes(x=year, y=wages, label=fmt_wages),
              vjust = -0.75, size = 4,  colour="black",
              position = position_dodge(width = 1),
              inherit.aes = TRUE) +
    scale_y_continuous(limits=c(axs$minBrk,axs$maxBrk), label=dollar)+
    scale_x_discrete() +
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
  
  f.wages <- left_join(f.wagePL,f.wageST,by="year")
  f.wages <- f.wages[,c(3,6,11)]
  names(f.wages) <- c("Year",paste0(" Average Weekly Wage: ",ctyname), "Average Weekly Wage: Colorado")

  
  # Text
  OutText <- paste0("The inflation adjusted (real) average weekly wages for ",ctyname," and Colorado are shown here.")
  OutText <- paste0(OutText," In 2016 dollars, wages in Colorado have been essentially unchanged since 2010.")
  OutText <- paste0(OutText," The gain or loss of a major employer such as a mine or a hospital can have a significant impact on a county’s average weekly wage.")
  OutText <- paste0(OutText," These wages are shown only for jobs located within that county and do not include most proprietors.")
  OutText <- paste0(OutText," Household income can be influenced by the average weekly wage, but in areas that have")
  OutText <- paste0(OutText," considerable amounts commuting or unearned income this relationship is not particularly strong.")
  
  
  outList <- list("plot" = Plot, "data" = f.wages, "text" = OutText)


  return(outList)
}
