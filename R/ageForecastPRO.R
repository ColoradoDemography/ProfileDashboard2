#' ageForecastPRO Produces a Age Forecast data set and chart
#'
#' Copied from "ms_popage" in codemgprofile, Modified by AB 12/2017
#' Creates a Chart comparing Forecast Population Growth by Age in Colorado.
#'
#' Uses the data from the State Demography Office package codemog to
#' create a graph showing projected population  changes by Age for each Colorado county from
#' 2000 to 2025.
#' The chart is modified from the original.  Now, we show three bars, one for each series.
#'
#' @param listID the list containing place id and Place names
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic and data file
#' @export

ageForecastPRO=function(listID, sYr, mYr, eYr, base=12, agegroup="ten"){

  
  # Collecting place ids from  idList, setting default values
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }
  
  fips=as.numeric(ctyfips)

  yrs=c(sYr, mYr, eYr)

  d=county_sya(fips, yrs)%>%
    mutate(agecat=age_cat(., "age", groups=agegroup))%>%
    group_by(countyfips,county, year, agecat)%>%
    summarise(totalpopulation=sum(as.numeric(totalpopulation)))  %>%
    ungroup()%>%
    group_by(agecat)%>%
    arrange(countyfips, year)


  barCol <- c("#82BC00", "#009ADD", "#5C666F")
  pltTitle <- paste0("Age Forecast: ",sYr," to ",eYr)
  subTitle <- paste0(as.character(d[1,2]), " County")
  names(d)[3] <- "Year"
  d$Year <- as.factor(d$Year)
  d$Year <- factor(d$Year, levels=yrs)

 #Setting MaxValue

 axs <- setAxis(d$totalpopulation)

  p <- d %>%
    ggplot(aes(x=agecat, y=totalpopulation, fill=Year))+
    geom_bar(stat="identity",color="black", position = position_dodge()) +
    scale_y_continuous(limits=c(axs$minBrk,axs$maxBrk), breaks= axs$yBrk, label=comma, expand = c(0, 0))+
    scale_fill_manual(values=barCol) +
    theme_codemog(base_size=base)+
    theme(axis.text.x=element_text(angle=45, hjust=1))+
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = captionSrc("SDO",""),
         x = "Age Group",
         y= "Total Population") +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          axis.text = element_text(size=12),
          legend.position= "bottom")

  #Regrouping Data

  dWide <- spread(d, Year, totalpopulation)
 
  dWide[,4:6] <- sapply(dWide[,4:6], function(x)  format(round(x, digits = 0),big.mark=",",scientific=FALSE))
  

  dWide$county <- paste0(dWide$county," County")
  
  dWide <- dWide[,2:6]
  names(dWide)[1] <- "Geography"
  names(dWide)[2] <- "Age Category"
  names(dWide)[3] <- paste0("Total Population ",sYr)
  names(dWide)[4] <- paste0("Total Population ",mYr)
  names(dWide)[5] <- paste0("Total Population ",eYr)
  

  # Generating text
    OutText <- paste0("  The changing age distribution of the population of ",ctyname," for the period from 2010 through 2025 is shown here.")

  OutText <- paste0(OutText,"  The changes in proporion of different groups can highligh the need for future planning and service provision.")
  OutText <- paste0(OutText," Many areas have a larger share of older adults, indicating the need to evaluate housing, transportation and other needs of the senior population.")

  #binding List
  outList <- list("plot"= p, "data" = dWide, "text" = OutText)

  return(outList)
}
