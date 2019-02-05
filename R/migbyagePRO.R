#' migbyagePRO Creates a Chart showing the 2000-2010 net Migration rate by age
#'
#' @param listID the list containing place id and Place names
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic and data file
#' @export
#'
migbyagePRO <- function(DBPool,listID, base=10) {

  # Collecting place ids from  idList, setting default values
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }
  

  
  sqlPlace <- paste0("SELECT fips, county, agegroup, netmig0010 FROM data.netmigrbyage WHERE fips = ",as.numeric(ctyfips),";")
  f.migPlace <- dbGetQuery(DBPool, sqlPlace)

 # sqlState <- paste0("SELECT fips, county, agegroup, rate0010 FROM data.netmigrbyage WHERE fips = ",state,";")
 # f.migState <- dbGetQuery(con, sqlState)



  # Preparing for merge
  f.migPlace[2] <- ctyname
 # f.migState[2] <- "Colorado"


 # f.migplot <- rbind(f.migPlace, f.migState)
   f.migplot <- f.migPlace
  names(f.migplot)[2] <- "geoname"

  f.migplot <- f.migplot[which(f.migplot$agegroup < 75),]
  #f.migplot$geoname <- factor(f.migplot$geoname, levels=c(ctyname, "Colorado"))
  pltTitle <- "Net Migration  by Age: 2000-2010"
  subTitle <- ctyname
  xTitle = "Age Group"




  p <- f.migplot %>%ggplot(aes(x=agegroup, y=netmig0010))+
    geom_bar(stat="identity", position="dodge", fill="#6EC4E8", color="black") +
    theme_codemog(base_size=base)+
    scale_x_continuous(breaks=seq(0, 70, 5)) +
    scale_y_continuous(labels = comma) +
    geom_hline(yintercept=0, size=1.05) +
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = captionSrc("SDO",""),
         x = xTitle,
         y= "Net Migration") +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          axis.text = element_text(size=12),
          legend.position= "bottom")


  #Preparing data set
 
  f.migData <- f.migPlace[,2:4]
  f.migData[3] <- format(round(f.migData[3], digits = 0),big.mark=",",scientific=FALSE)
  names(f.migData) <- c("Geography","5-Year Age Group","Net Migration 2000-2010")

  #Geneerating Text
   OutText <-paste0("  This plot shows the net migration by age in ",ctyname,". ")  
   OutText <-paste0(OutText," Colorado typically draws many young adults as migrants. Areas with colleges and resorts draw a number of 18 to 24 year olds.")
   OutText <-paste0(OutText," Areas with a growing economy tend to account mostly 25 to 35 year olds and areas attractive to retirees")
   OutText <-paste0(OutText," tend to draw both workers and older adults.")
  
  outList <-list("plot"=p,"data"= f.migData,"text" = OutText)
  return(outList)
}
