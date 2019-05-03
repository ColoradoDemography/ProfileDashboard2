#' pop_timeseries Creates a \code{ggplot2} chart of the population for a CO county
#'
#' This is a replacement for county_ts_chart  Copied from codemogProfile AB 3/2018
#'
#' Takes some basic input on the time period and county then creates a
#' plot of the data in \code{ggplot2}.  Similar to the county_ts_data()
#' function.  Can create timeseries from 2000 to 2040 (beyond 2013 are
#' forecasts).
#'
#'
#' @param listID the list containing place id and Place names
#' @param beginyear The first year in the timeseries Defaults to 1990.
#' @param endyear The last year in the timeseries Defaults to 2013.
#' @param base Base font size.
#' @return ggplot2 graphic and data file
#' @export



pop_timeseries=function(DBPool,lvl,listID, beginyear=2000,endyear, base=10){

  # Collecting place ids from  idList, setting default values

  ctyfips <- listID$ctyNum
  ctynum <- as.numeric(ctyfips)
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
 

  # Checking length of fips to idenify municipality data series
  if(lvl == "Municipalities") {
    sqlStrPop1 <- paste0("SELECT countyfips, placefips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE (placefips = ",as.numeric(placefips),"  and (year >= ",beginyear,") and (year <= ",endyear,"));")
                          
    d1 <-  dbGetQuery(DBPool, sqlStrPop1)
   

    d1 <- d1[which(d1$countyfips != 999), ]  # removing "Total" for multi-county cities
    d1$totalpopulation <- ifelse(is.na(d1$totalpopulation),0,d1$totalpopulation)  #Fixing NA values
    d1$municipalityname <-gsub(' \\([P,p]art\\)','',d1$municipalityname)
    d <- d1 %>% group_by(placefips, municipalityname, year) %>% summarize(totalpopulation = sum(totalpopulation))
    d$placename <- d$municipalityname
    
   } 
   if(lvl == "Counties") { #fips is a county code
        sqlCtyPop <-  paste0("SELECT countyfips, year, totalpopulation FROM estimates.county_profiles WHERE (countyfips = ",as.numeric(ctyfips),") 
                            and (year >= ",beginyear,") and (year <= ",endyear,");")
        d=dbGetQuery(DBPool, sqlCtyPop) 
        d$placename <- ctyname
   }
  
  

  d <- d[which(d$totalpopulation != 0),]
  
  yaxs <- setAxis(d$totalpopulation)
  xaxs <- setAxis(d$year)
  if(xaxs$maxBrk != endyear) {
    xaxs$maxBrk <- endyear
    xaxs$yBrk[length(xaxs$yBrk)] <- endyear
  }
  
    d2 <- d

  names(d2) <- c("Geography","Year","Total Population")
  
  p=d%>%
    ggplot(aes(x=year, y=totalpopulation))+
    geom_line(color="#00953A", size=1.75)+
    labs(x="Year", y="Population", title=paste("Population,", beginyear, "to", max(d$year), sep=" "),
         subtitle = d$placename,
         caption = captionSrc("SDO",""))+
    scale_y_continuous(limits=c(yaxs$minBrk,yaxs$maxBrk), breaks=yaxs$yBrk, label=comma)+
    scale_x_continuous(limits=c(xaxs$minBrk,xaxs$maxBrk), breaks=xaxs$yBrk) +
    theme_codemog(base_size=base)+
    theme(plot.title = element_text(hjust = 0.5, size=16),
          axis.text.x=element_text(angle=90,size=12),
          axis.text.y = element_text(size=12))

  # Bind List

  outList <- list("plot" = p, "data" = d2)

  return(outList)
}

