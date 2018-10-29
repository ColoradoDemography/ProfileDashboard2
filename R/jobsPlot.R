#' jobsPlot Creates a Chart showing the Total Estimates Jobs series for each County in Colorado
#'
#' Modified from ms_jobs January, 2018 AB
#'
#' Uses State Demography Office data to create a chart showing the timeseries of Total Estimated Jobs
#' (which means it includes Proprietors and Agricultural Workers) for a selected Colorado County
#'
#' @param listID the list containing place id and Place names
#' @param maxyr The maximum year value, from CurYr
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
#' @return ggplot graphic and data file
#' @export
#'
jobsPlot=function(listID, maxyr,base=10){
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }
  
  jobs_data <- county_jobs(as.numeric(ctyfips), 2001:maxyr) %>%
    mutate(jobs=car::recode(totalJobs, "'S'=NA"),
           jobs=round(as.numeric(jobs),0),
           year=as.numeric(as.character(year)))

  jobs_data <- jobs_data[which(!is.na(jobs_data$jobs)),]
  
  
  axs <- setAxis(as.numeric(jobs_data$jobs))

  jobs_plot <- jobs_data %>%
    ggplot(aes(x=year, y=as.numeric(jobs)))+
    geom_rect(aes(xmin=2008, xmax=2010, ymin=-Inf, ymax=+Inf), fill=rgb(208, 210, 211, max = 255), alpha=.03)+
    geom_rect(aes(xmin=2001, xmax=2002, ymin=-Inf, ymax=+Inf), fill=rgb(208, 210, 211, max = 255), alpha=.03)+
    geom_line(color=rgb(0, 168, 58, max = 255), size=1.5)+
    scale_x_continuous(breaks=c(2001:maxyr))+
    scale_y_continuous(limits=c(axs$minBrk,axs$maxBrk), breaks=axs$yBrk, labels=comma)+
    theme_codemog(base_size=base)+
    labs(x="Year",
         y="Jobs",
         title= paste0("Total Estimated Jobs, 2001 to ",as.character(maxyr)),
         subtitle = ctyname,
         caption= paste0(captionSrc("SDO","") ,"\nNote: Grey shading represents beginning to bottom of U.S. recessions")) +
    theme(plot.title = element_text(hjust = 0.5, size=18),
          axis.text.x = element_text(angle=45,size=12),
          axis.text.y = element_text(size=12))
  
  # Creating text
  OutText <- paste0("The Estimated Jobs is a series created by the SDO to give a comprehensive look at the number of jobs located within ",ctyname,".")  
  OutText <- paste0(OutText," It is broad in scope, capturing both wage and salary workers as well as most proprietors and agricultural workers.")
  OutText <- paste0(OutText,"  A more diverse economy is typically more resilient too; when looking at the employment trends recently ")
  OutText <- paste0(OutText," and after a recession (shaded in gray) it is also important to look at the current share of employment by industry.")
  OutText <- paste0(OutText," Areas dependent on a single industry such as agriculture, mining or tourism can suffer from prolonged downturns due to drought,")
  OutText <- paste0(OutText," shifting demand for commodities, and the health of the national economy. ")
 
  
  outList <- list("plot" = jobs_plot, "data" = jobs_data, "text" = OutText)
  return(outList)
}
