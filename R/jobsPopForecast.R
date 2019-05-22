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


  f.totalJobs <- f.totalJobs[,c(3,4)]
  names(f.totalJobs) <- c("year","Jobs")

  f.totalPop <- f.totalPop[,c(1,3)]
  names(f.totalPop) <- c("year","Population")
  f.plotdata <- left_join(f.totalJobs,f.totalPop,by="year")
 
  f.plotdata$Series <-  ifelse(f.plotdata$year > curyr,"Forecast","Estimate")

  f.plotdata <- f.plotdata[which(f.plotdata$year >= 2010 & f.plotdata$year <= 2040),]
  f.plotdata$JobGrowth <- percent(signif((((f.plotdata$Jobs/lag(f.plotdata$Jobs))^(1/(f.plotdata$year-lag(f.plotdata$year)))) -1)*100),digits=1)
  f.plotdata$PopGrowth <- percent(signif((((f.plotdata$Population/lag(f.plotdata$Population))^(1/(f.plotdata$year-lag(f.plotdata$year)))) -1)*100),digits=1)
  
  f.plotdata$JobGrowth <- gsub("NA%","",f.plotdata$JobGrowth)
  f.plotdata$PopGrowth <- gsub("NA%","",f.plotdata$PopGrowth)
  
  f.plotdata$Jobs <- format(round(f.plotdata$Jobs,digits=0),big.mark=",",scientific=FALSE)
  f.plotdata$Population <- format(round(f.plotdata$Population,digits=0),big.mark=",",scientific=FALSE)

  
  f.plotdata <-  f.plotdata[,c(1,4,2,5,3,6)]
  
  #Producing the tables
  names(f.plotdata) <- c("Year","Type", "Jobs", "Annual Growth Rate: Jobs","Population","Annual Growth Rate: Population")
  
  
  
  
  
  m.forecast <- as.matrix(f.plotdata[seq(1, nrow(f.plotdata), 5), ])
 
  f.flex <- as.data.frame(m.forecast)
  
  # set vector names
  tblHead <- c(ctyname = 6)
  names(tblHead) <- ctyname
  
  names_spaced <- c("Year","Type", "Jobs", "Annual Growth Rate: Jobs","Population","Annual Growth Rate: Population")
  
  tabHTML <- m.forecast %>%
    kable(format='html', table.attr='class="cleanTable"',
          digits=1,
          row.names=FALSE,
          align="llrrrr",
          caption="Jobs and Population Forecast",
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 12) %>%
    column_spec(1, width="0.5in") %>%
    column_spec(2, width="0.5in") %>%
    column_spec(3, width="0.5in") %>%
    column_spec(4, width="0.5in") %>%
    column_spec(5, width="0.5in") %>%
    column_spec(6, width="0.5in") %>%
    add_header_above(header=tblHead) %>%
    kableExtra::footnote(captionSrc("SDO",""))
  
  
  #LATEX Table
  # set vector names
  tabLATEX <- kable(m.forecast, col.names = names_spaced,
                    caption="Jobs and Population Forecast", 
                    row.names=FALSE, align="llrrrrr",
                    format="latex", booktabs=TRUE)  %>%
    kable_styling(latex_options="HOLD_position") %>%
    column_spec(1, width="0.5in") %>%
    column_spec(2, width="0.5in") %>%
    column_spec(3, width="0.5in") %>%
    column_spec(4, width="0.5in") %>%
    column_spec(5, width="0.5in") %>%
    column_spec(6, width="0.5in") %>%
    add_header_above(header=tblHead) %>%
    kableExtra::footnote(captionSrc("SDO",""),threeparttable = T) 
  
  #Flextable
  
  FlexOut <- regulartable(f.flex) %>% 
    add_header(Year=paste0("Jobs and Population Forecast: ",ctyname),top=TRUE) %>%
    add_footer(Year=captionSrc("SDO","")) %>%
    merge_at(i=1,j=1:6,part="header") %>%
    merge_at(i=1,j=1:6,part="footer") %>%
    align(i=1, j=1, align="left",part="header") %>%
    width(j=1:6, width=1.0) 
  
    

  # producing the Dataset

  f.flex$geoname <- ctyname
  f.flex <- f.flex[,c(7,1:6)]
  names(f.flex) <- c("Place", "Year","Type", "Jobs", "Annual Growth Rate: Jobs","Population","Annual Growth Rate: Population")
  

#Text
  OutText <- paste0("The total jobs forecast and population forecast are for ",ctyname," shown here.")
  OutText <- paste0(OutText,"  The two lines diverge over time due to the aging of our population and continued growth in our under 18 population – two segments of the population that are less likely to be employed.")
  OutText <- paste0(OutText," Growth in the 65 plus population in the labor force through 2040 compared to the universe population of those over the age of 16 since labor force participation declines with age,")
  OutText <- paste0(OutText," especially among those eligible for pensions or social security.")
  if(ctyname == "Denver-Boulder MSA"){
    OutText <- paste0(OutText," Note: Statistics for the counties in the Denver Metropolitan Statistical Area (Adams, Arapahoe, Boulder, Broomfield, Denver, Douglas and Jefferson) are combined in this section.") 
  }
  
  outList <- list("Htable"= tabHTML,"Ltable" = tabLATEX , "FlexTable" = FlexOut, "data" = f.flex,"text" = OutText)


  return(outList)
}
