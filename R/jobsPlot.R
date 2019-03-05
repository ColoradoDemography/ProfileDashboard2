#' jobsPlot Creates a Chart showing the Total Estimated Numer of Firms and Jobs series for each County in Colorado
#'
#' Modified from ms_jobs March, 2019 AB
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
jobsPlot=function(DBPool,listID, maxyr,base=10){
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }
  
  # Jobs Data
  jobsStr <- paste0("SELECT * FROM estimates.jobs_by_sector WHERE area_code = ", as.numeric(ctyfips), " AND sector_id = '0';")
  
  jobs_data <-   dbGetQuery(DBPool,jobsStr) %>%
    filter(population_year >= 2001 & population_year <= maxyr) %>%
    mutate(jobs=round(total_jobs,0),
           year=population_year)
  
  jobs_data <- jobs_data[which(!is.na(jobs_data$jobs)),]
  
  # Firm Data
  
  sqlStrFirms <- paste0("SELECT * FROM estimates.firm_count WHERE fips = ", ctyfips,";")
  
  f.firms <-  dbGetQuery(DBPool, sqlStrFirms)
  
  f.firms <- subset(f.firms, (!is.na(f.firms[,4])))
  
  #Preparing Plor Data set
  f.jobspl <- jobs_data[,c(9,8)]
  f.jobspl$Series <- "Jobs"
  names(f.jobspl)[2] <- "count"
  
  f.firmpl <- f.firms[,c(3,4)]
  f.firmpl$Series <- "Firms"
  names(f.firmpl)[2] <- "count"
  
  f.plot <- rbind(f.jobspl, f.firmpl)
  
  
  
  jobs_plot <- f.plot %>%
    ggplot(aes(x=year, y=count))+
    geom_rect(aes(xmin=2008, xmax=2010, ymin=-Inf, ymax=+Inf), fill=rgb(208, 210, 211, max = 255), alpha=.03)+
    geom_rect(aes(xmin=2001, xmax=2002, ymin=-Inf, ymax=+Inf), fill=rgb(208, 210, 211, max = 255), alpha=.03)+
    geom_line(color=rgb(0, 168, 58, max = 255), size=1.5)+
    scale_x_continuous(breaks=c(2001:maxyr))+
    scale_y_continuous(labels=comma)+
    theme_codemog(base_size=base)+
    facet_grid(rows =vars(Series), scales = "free_y") +
    labs(x="Year",
         y = "Number",
         title= paste0("Total Estimated Firms and Jobs, 2001 to ",as.character(maxyr)),
         subtitle = ctyname,
         caption= paste0(captionSrc("SDO","") ,"\nNote: Grey shading represents beginning to bottom of U.S. recessions")) +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          axis.text.x = element_text(angle=45,size=10),
          axis.text.y = element_text(size=11))
  
  # Creating text
  OutText <- paste0("The Estimated Firms and Jobs series created by the SDO gives a comprehensive look at the number of firms and jobs located within ",ctyname,".")  
  OutText <- paste0(OutText," It is broad in scope, capturing both wage and salary workers as well as most proprietors and agricultural workers.")
  OutText <- paste0(OutText,"  A more diverse economy is typically more resilient too; when looking at the employment trends recently ")
  OutText <- paste0(OutText," and after a recession (shaded in gray) it is also important to look at the current share of employment by industry.")
  OutText <- paste0(OutText," Areas dependent on a single industry such as agriculture, mining or tourism can suffer from prolonged downturns due to drought,")
  OutText <- paste0(OutText," shifting demand for commodities, and the health of the national economy. ")
  
  # Final Data set
  f.jobsd <- jobs_data[,c(9,8)]
  
  f.firmd <- f.firms[,c(3,4)]
  
  f.dataFin <- inner_join(f.jobsd,f.firmd, by="year")
  
  f.dataFin$Geography <- ctyname 
  f.dataFin <- f.dataFin[,c(4,1:3)]
  f.dataFin[3] <- format(f.dataFin[3],big.mark=",",scientific=FALSE)
  f.dataFin[4] <- format(f.dataFin[4],big.mark=",",scientific=FALSE)
  names(f.dataFin) <- c("Geography","Year","Jobs Estimate", "Firms Estimate")
  
  outList <- list("plot" = jobs_plot, "data" = f.dataFin, "text" = OutText)
  return(outList)
}