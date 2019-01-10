#' residentialLF Creates a two plots showing residential labor force participation
#'
#' Plot 1: a line chart showing forcasted trend line for persons in the labor force and
#'     persons age 16 and older.
#' Plor2: a bar chart comparing forcasted labor force participation for a place and Colorado
#'
#' @param listID the list containing place id and Place names
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphics and associated data sets
#' @export

residentialLF <- function(listID, curyr, base=10){
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }
  #fips is the 3-digit character string



  LFSQLPL <- paste0("SELECT * FROM estimates.labor_force_participation WHERE area_code = ",as.numeric(ctyfips), ";")
  LFSQLST <- paste0("SELECT * FROM estimates.labor_force_participation WHERE area_code = 0;")


  pw <- {
    "demography"
  }

  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <- dbConnect(drv, dbname = "dola",
                   host = "104.197.26.248", port = 5433,
                   user = "codemog", password = pw)
  rm(pw) # removes the password

  # Read data files
  f.LFPlace <- dbGetQuery(con, LFSQLPL)
  f.LFState <- dbGetQuery(con, LFSQLST)

  #closing the connections
  dbDisconnect(con)
  dbUnloadDriver(drv)
  rm(con)
  rm(drv)

  # Summing by Participation Year

  f.LFPlaceSum <- f.LFPlace %>%
    group_by(population_year) %>%
    summarize(LForce = sum(laborforce),
              Pop16P = sum(cni_pop_16pl))
  
  f.LFPlaceSum$Series <-  ifelse(f.LFPlaceSum$population_year > curyr,"Forecast","Estimate")
 


  # Creating 10-year data file
  f.LFPlaceSum$year10 <- f.LFPlaceSum$population_year - (f.LFPlaceSum$population_year %%10)
  f.LFPlaceSum10 <- f.LFPlaceSum %>%
    group_by(year10) %>%
    summarize(LForce = sum(LForce),
              Pop16P = sum(Pop16P)) %>%
    mutate(PctPart = (LForce/Pop16P) * 100)


  f.LFStateSum <- f.LFState %>%
    group_by(population_year) %>%
    summarize(LForce = sum(laborforce),
              Pop16P = sum(cni_pop_16pl))

  # Creating 10-year data file
  f.LFStateSum$year10 <- f.LFStateSum$population_year - (f.LFStateSum$population_year %%10)
  f.LFStateSum10 <- f.LFStateSum %>%
    group_by(year10) %>%
    summarize(LForce = sum(LForce),
              Pop16P = sum(Pop16P)) %>%
    mutate(PctPart = (LForce/Pop16P) * 100)

  # Building Line Chart
  pltTitle <- "Forecast Resident Labor Force and\nPopulation, Age 16 +"

   minval <- min(f.LFPlaceSum$LForce)
   maxval <- max(f.LFPlaceSum$Pop16P)
   yBrk <- pretty(minval:maxval)

  LFLine <-  ggplot(data=f.LFPlaceSum) +
    geom_line(aes(x=population_year, y=Pop16P, colour= "Population 16 +",linetype=Series), size=1.50) +
    geom_line(aes(x=population_year, y=LForce,color="Labor Force",linetype=Series), size=1.50) +
    scale_colour_manual(" ", values=c("Labor Force" = "#6EC4E8", "Population 16 +" = "#00953A")) +
    scale_x_continuous(breaks=seq(2010,2040, 5)) +
    scale_y_continuous(limits=c(minval,maxval), breaks=yBrk, label=comma)+
    theme_codemog(base_size=base)+
    labs(title = pltTitle,
         subtitle = ctyname,
         caption = captionSrc("SDO",""),
         x = "Year",
         y= "Population") +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          axis.text = element_text(size=12),
          legend.position= "bottom")




  # preparing datasets

  #line data

  f.LFPlaceSum$geoname <- ctyname
  f.LFPlaceSum <- f.LFPlaceSum[,c(6,1:3)]

  f.LFPlaceSum[,c(3,4)] <- sapply(f.LFPlaceSum[,c(3,4)], function(x) format(round(x,digits=0),big.mark=",",scientific=FALSE))


  names(f.LFPlaceSum) <- c("Geography","Year", "Persons in Labor Force", "Persons Age 16 +")
#Text
  OutText <- paste0("This plot compares the forecast residential labor force to the forecast population of person age 16 and older for ",ctyname,".")

  outList <- list("plot" = LFLine, "data" = f.LFPlaceSum, "text" = OutText)

  return(outList)
}
