#' firmcount Creates a \code{ggplot2} chart of the private firmcount for a CO county
#'
#' This is a replacement for county_ts_chart  Copied from codemogProfile AB 3/2018
#'
#' Takes some basic input on the time period and county then creates a
#' plot of the data in \code{ggplot2}.  Similar to the county_ts_data()
#' function.  Can create timeseries from 2001 to the current year
#' forecasts).
#'
#'
#' @param listID the list containing place id and Place names
#' @param base Base font size.
#' @return ggplot2 graphic and data file
#' @export



firmcount=function(DBPool,listID, base=10){
 
  # Collecting place ids from  idList, setting default values

  ctyfips <- listID$ctyNum
  ctynum <- as.numeric(ctyfips)
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName

  # Checking length of fips to idenify municipality data series
    sqlStrFirms <- paste0("SELECT * FROM estimates.firm_count WHERE fips = ", ctynum,";")
                          
    d <-  dbGetQuery(DBPool, sqlStrFirms)

    d <- subset(d, (!is.na(d[,4])))  
 
  yaxs <- setAxis(d$firms)
  xaxs <- setAxis(d$year)
  
  
  
  d2 <- d[,c(2:4)]
  names(d2) <- c("Geography","Year","Number of Firms")

  p=d%>%
    ggplot(aes(x=year, y=firms))+
    geom_line(color="#00953A", size=1.75)+
    labs(x="Year", y="Number of Firms", title=paste0("Number of Firms (",min(d$year),"-",max(d$year),")"),
         subtitle = ctyname,
         caption = captionSrc("QCEW",""))+
    scale_y_continuous(limits=c(yaxs$minBrk,yaxs$maxBrk), breaks=yaxs$yBrk, label=comma)+
    scale_x_continuous(limits=c(xaxs$minBrk,xaxs$maxBrk), breaks=xaxs$yBrk) +
    theme_codemog(base_size=base)+
    theme(plot.title = element_text(hjust = 0.5, size=16),
          axis.text.x=element_text(angle=90,size=12),
          axis.text.y = element_text(size=12))
  
  t <- paste0("This plot shows the number of active firms in ",ctyname," between ", min(d$year)," and",max(d$year),".")
  t<-  paste0(t," This is an indication of the economic activity and employment opportuities in the county" )

  # Bind List
  

  outList <- list("plot" = p, "data" = d2, "text" = t)

  return(outList)
}

