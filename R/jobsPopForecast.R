#' jobsPopForecast Produces a plot and table showing changes in forecasted jobs and population
#'  for the period from 2000 to 2035
#'
#'   This includes code to output data for the Denver-Boulder MSA when Adams, Arapahoe, Boulder,
#'     Broomfield, Denver, Douglas, or Jefferson County are selected.
#'
#' @param listID the list containing place id and Place names
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic, a html or latex table and a dataset
#' @export

jobsPopForecast <- function(DBPool,listID, curyr, base=10){
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }
  
  #fips is the 3-digit character string

  # creating alternative fips code for Denver MSA
  if(ctyfips %in% c("001", "005", "013", "014", "031", "035", "059")) {
    ctyfips = "500"
    MSAList <- c(1,5,13,14,31,35,59)
    ctyname = "Denver-Boulder MSA"
  } else {
    MSAList <- as.numeric(ctyfips)
  }

  jobsSQL <- paste0("SELECT * FROM estimates.jobs_forecast WHERE countyfips = '",as.numeric(ctyfips), "';")


  
  f.totalJobs <- dbGetQuery(DBPool, jobsSQL)


  f.totalJobs$type <- "Jobs"

  # Gathering population data
  f.Pop =county_sya(MSAList, 1990:2040,"totalpopulation")
  f.Pop$totalpopulation <- as.numeric(f.Pop$totalpopulation)
  f.totalPop <- f.Pop %>%
    group_by(year, datatype) %>%
    summarize(TotalPop = sum(totalpopulation))
  f.totalPop$type <- "Population"


  x <- as.data.frame(f.totalJobs[,c(3,4)])
  names(x) <- c("year","Jobs")

  y <- as.data.frame(f.totalPop[,c(1,3)])
  names(y) <- c("year","Population")
  f.plotdata <- left_join(x,y,by="year")
 
  f.plotdata$Series <-  ifelse(f.plotdata$year > curyr,"Forecast","Estimate")
  
  
 
  f.plotdata <- f.plotdata[which(f.plotdata$year >= 2010 & f.plotdata$year <= 2040),]
  

  pltTitle <- paste0("Forecast Change in Jobs and Population\n",as.character(min(f.plotdata$year))," to ",as.character(max(f.plotdata$year)))

  axsP <- setAxis(f.plotdata$Population)
  axsJ <- setAxis(f.plotdata$Jobs)
  #Selecting the Axis boundaries
  if(axsP$maxBrk > axsJ$maxBrk) {
    minAxis <- axsJ$minBrk
    maxAxis <- axsP$maxBrk
  } else {
    minAxis <- axsP$minBrk
    maxAxis <- axsJ$maxBrk
  }

  Plot <-  ggplot(data=f.plotdata)+
    geom_line(aes(x=year, y=Jobs, colour= "Jobs", linetype=Series),  size=1.50) +
    geom_line(aes(x=year, y=Population,color="Population", linetype=Series), size=1.50) +
    scale_colour_manual(" ", values=c("Jobs" = "#6EC4E8", "Population" = "#00953A")) +
    scale_y_continuous(limits=c(minAxis,maxAxis), label=comma)+
    scale_x_continuous(breaks=seq(2010,2040, 5)) +
    theme_codemog(base_size=base)+
    labs(title = pltTitle,
         subtitle = ctyname,
         caption = captionSrc("SDO",""),
         x = "Year",
         y= "Change") +
    theme(plot.title = element_text(hjust = 0.5, size=14),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          axis.text = element_text(size=12),
          legend.position= "bottom")


  # producing the table...
 
  f.plotdata$geoname <- ctyname
  f.plotdata <- f.plotdata[,c(5,1,4,2,3)]
  names(f.plotdata) <- c("Year","Place","Type", "Jobs","Population")
  f.plotdata$Jobs <- format(round(f.plotdata$Jobs,digits=0),big.mark=",")
  f.plotdata$Population <- format(round(f.plotdata$Population,digits=0),big.mark=",")

#Text
  OutText <- paste0("The total jobs forecast and population forecast are for ",ctyname," shown here.")
  OutText <- paste0(OutText,"  The two lines diverge over time due to the aging of our population and continued growth in our under 18 population – two segments of the population that are less likely to be employed.")
  OutText <- paste0(OutText," Growth in the 65 plus population in the labor force through 2040 compared to the universe population of those over the age of 16 since labor force participation declines with age,")
  OutText <- paste0(OutText," especially among those eligible for pensions or social security.")
  if(ctyname == "Denver-Boulder MSA"){
    OutText <- paste0(OutText," Note: Statistics for the counties in the Denver Metropolitan Statistical Area (Adams, Arapahoe, Boulder, Broomfield, Denver, Douglas and Jefferson) are combined in this section.") 
  }
  
  outList <- list("plot" = Plot, "data" = f.plotdata,"text" = OutText)


  return(outList)
}
