#' popForecast Creates a Chart showing population and estmates
#'
#'
#' @param listID the list containing place id and Place names
#' @param byr is the first year of the series to be extracted by county_sya (min 2000)
#' @param eyr is the last  year of the series to be extracted by county_sya (max 2050)
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic and data file
#' @export

popForecast <- function(listID, byr=2000,eyr=2050, base=10) {

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
  yrs <- seq(byr,eyr, by=2)
  
  
    d <- county_sya(fips, yrs)  %>%
      group_by(county, datatype, year) %>%
      summarize(Tot_pop = sum(as.numeric(totalpopulation)))
  

  
  
  yaxs <- setAxis(d$Tot_pop)
  xaxs <- setAxis(d$year)
  xaxs$maxBrk <- ifelse(xaxs$maxBrk > eyr,eyr,xaxs$maxBrk)
  xaxs$yBrk <- seq(byr, eyr, by = 10)
  
  #Creating Plot  
  p=d%>%
    ggplot(aes(x=year, y=round(Tot_pop, digits=0), group=datatype))+
    geom_line(aes(linetype=datatype), color="#00953A", size=1.5) +
    labs(x="Year", y="Population", title=paste("Population Forecast,", byr, "to", eyr, sep=" "),
         subtitle = ctyname,
         caption = captionSrc("SDO",""))+
    scale_y_continuous(limits=c(yaxs$minBrk,yaxs$maxBrk), breaks=yaxs$yBrk, label=comma)+
    scale_x_continuous(limits=c(xaxs$minBrk,xaxs$maxBrk),breaks= xaxs$yBrk) +
    theme_codemog(base_size=base)+
    theme(plot.title = element_text(hjust = 0.5, size=16),
          axis.text.x=element_text(angle=90,size=12),
          axis.text.y = element_text(size=12),
          legend.title=element_blank())
  
  # Creating Output data file
    d[4] <- round(d[4],digits=0)
    d$county <- ctyname
  
  
  
  #Output text
 
  d10 <- d[which(d$year%% 10  == 0),]
  d10$grNum <- (((d10$Tot_pop/lag(d10$Tot_pop))^(1/(d10$year-lag(d10$year)))) -1)*100
  
  
  pop2020 <- as.numeric(d10[which(d10$year == 2020),4])
  pop2040 <- as.numeric(d10[which(d10$year == 2040),4])
  gr20102020 <- as.numeric(d10[which(d10$year == 2020),5])
  gr20202030 <- as.numeric(d10[which(d10$year == 2030),5])
  gr20302040 <- as.numeric(d10[which(d10$year == 2040),5])
  
  
  grDir <- ifelse(gr20102020 > gr20302040,"decrease",
                  ifelse(gr20102020 < gr20302040,"increase", "remain stable"))     
  
  gr20102020 <- gsub("%"," percent",percent(gr20102020,digits=1))
  gr20202030 <- gsub("%"," percent",percent(gr20202030,digits=1))
  gr20302040 <- gsub("%"," percent",percent(gr20302040,digits=1))
  
  grText1 <- paste0(" Overall, the growth rate for ",ctyname," is expected to ",grDir," between 2020 and 2040.")
  grText2  <-paste0("  Between 2010 and 2020 the forecast growth rate was ",gr20102020,", between 2020 and 2030 the forecast growth rate is ",gr20202030,", ")
  grText3  <-paste0(" while the forecast growth rate between 2030 and 2040 is ",gr20302040,".")
  
  OutText <- paste0("  The population of ",ctyname," is forecast to reach ",format(pop2020,big.mark=",")," by 2020 and ",format(pop2040,big.mark=",")," by 2040.")
  OutText <- paste0(OutText,grText1, grText2,grText3)
  OutText <- paste0(OutText,"  The change is due in part to population aging and changes in the proportion of the population in childbearing ages.")
  
  OutText <- paste0(OutText,"  Note: Population forecasts are only provided for Colorado counties.")
  
  names(d) <- c("Geography","Data Type","Year","Total Population")
  
  outList <- list("plot" = p,"data" = d,"text" = OutText)
  

  return(outList)
}