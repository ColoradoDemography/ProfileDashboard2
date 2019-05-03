#' cocPlot: Components of Change Chart, this is a county-level chart, regardless of output level
#'
#' @param listID the list containing place id and Place names
#' @param  ctyname County Name string, from input$unit
#' @param  lyr the last year of the output date range
#' @return ggplot2 graphic and data file
#' @export

cocPlot <- function(DBPool,lvl,listID,fyr=2000,lyr,base=12) {
  # Collecting place ids from  idList, setting default values
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }  

  cocStr <- paste0("SELECT countyfips, year, totalpopulation, births, deaths, netmigration FROM estimates.county_profiles WHERE countyfips = ",as.numeric(ctyfips),";")
  
  f.coccty <-  dbGetQuery(DBPool,cocStr) %>%
    filter(year >= fyr & year <= lyr) %>%
    mutate( totalpopulation = as.numeric(totalpopulation),
            births=as.numeric(births),
            deaths=as.numeric(deaths),
            netmigration=as.numeric(netmigration),
            naturalincrease=births-deaths)

  f.cocLong <- gather(f.coccty, TypeChange, Pop, c(births,deaths,netmigration))
  f.cocLong$TypeChange <- ifelse(f.cocLong$TypeChange =="netmigration","Net Migration",
                                 ifelse(f.cocLong$TypeChange =="births", "Births","Deaths"))
  
  f.cocLong$TypeChange <- factor(f.cocLong$TypeChange,
                                 levels=c("Births","Deaths", "Net Migration"))
  
  pltTitle <- "Components of Change:\nBirths, Deaths, and Net Migration"
  subTitle <- ctyname
  axs <- setAxis(f.cocLong$Pop)
  
  cocPlt <-  ggplot(data=f.cocLong,aes(x=year, y=Pop, colour=TypeChange)) +
    geom_line() +
    geom_point(aes(x=year, y=Pop, colour=TypeChange, shape=TypeChange),size=2) +
    geom_hline(yintercept=0, size=1.05) +
    scale_colour_manual("Type of Change", values=c("#82BC00", "#009ADD", "#5C666F")) +
    scale_shape_manual("Type of Change", values=seq(15, 17, 1)) +
    scale_x_continuous(breaks=seq(fyr, lyr, 2)) +
    scale_y_continuous(limits=c(axs$minBrk,axs$maxBrk), breaks=axs$yBrk,label=comma)+
    theme_codemog(base_size=base)+
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = captionSrc("SDO",""),
         x = "Year",
         y= "Population Change") +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          axis.text = element_text(size=12),
          legend.position= "bottom")
  
  f.coccty$place <- ctyname
  
  f.coccty2 <- f.coccty[,c(8,2:7)]

  f.coccty5 <- tail(f.coccty2,6)
  names(f.coccty2)  <- c("Place","Year","Total Population","Births","Deaths", "Net Migration","Natural Increase")
  
  
  #Creating text 
  
  firstyr <- as.numeric(f.coccty5[2,2])
  lastyr <- as.numeric(f.coccty5[6,2])
  
  totChng <- as.numeric(f.coccty5[6,3] -  f.coccty5[1,3])
  
  f.sum <- f.coccty5 %>%
    summarise(sumNat = sum(naturalincrease),
              sumMig = sum(netmigration))
  
  chgDir <- ifelse(totChng > 0,"increased",
                   ifelse(totChng < 0,"decreased", "stayed the same"))
  
  
  
  OutText <- paste0("Over the past five years, between ", firstyr," and  ", lastyr,", the population of ", ctyname," has ",chgDir," by ",format(totChng,big.mark=",")," people.")
  OutText <- paste0(OutText," The total natural increase (births - deaths) over this period was ",format(f.sum$sumNat,big.mark=",")," and the total net migration (new residents who moved in minus those who moved out) was ",format(f.sum$sumMig,big.mark=","),".")                 
  OutText <- paste0(OutText,"  Note: Components of Change data are only available for Colorado counties.")
  
  outList <- list("plot" = cocPlt, "data" = f.coccty2,"text" = OutText)
  
  return(outList)
}
