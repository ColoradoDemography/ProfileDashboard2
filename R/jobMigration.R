#' jobMigration Creates a Chart showing the number of jobs and net migration trends
#'  1985 to present
#'
#' Uses State Demography Office data to create a chart comparing jobs by major sector
#' for a selected place, compared to Colorado.
#'
#' Changes format to a basic bar chart with text annotations from the original formattable output.
#'
#' @param listID the list containing place id and Place names
#' @param maxyr The maximum year value, from CurYr
#' @param base is the abse text size for the ggplot2 object and codemog_theme(), defaults to base = 10
#' @return ggplot graphic and data file
#' @export
#'
jobMigration <- function(listID, maxyr, base=10){
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }

  jobsSQL <- paste0("SELECT * FROM estimates.bea_jobs WHERE fips = ",as.numeric(ctyfips), ";")
  jobslyr <- paste0("jobs_",maxyr)

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
  f.jobsBea <- dbGetQuery(con, jobsSQL)

  #closing the connections
  dbDisconnect(con)
  dbUnloadDriver(drv)
  rm(con)
  rm(drv)


  #Jobs
  # convert datasets to wide

  f.jobs1yr <- gather(f.jobsBea, year,jobs, jobs_1970:jobslyr)
  f.jobs1yr$year <- as.numeric(gsub("jobs_","",f.jobs1yr$year))

  # Calculating  5-year value
  f.jobs1yr$year5 <- f.jobs1yr$year - (f.jobs1yr$year %%5)


  #creating a lagged difference
  f.jobs1yr$lagjobs <- f.jobs1yr$jobs - lag(f.jobs1yr$jobs,1)

  f.jobs5yr <- f.jobs1yr %>%
    group_by(year5) %>%
    summarize(avgjobs = sum(lagjobs)/n())

  f.jobs5yr <- f.jobs5yr[which(f.jobs5yr$year5 >= 1985),]

  #Net Migration
  # convert datasets to wide
  f.migr1yr <- county_profile(as.numeric(ctyfips), 1985:maxyr, vars="netmigration")
  f.migr1yr$netmigration <- as.numeric(f.migr1yr$netmigration)
  f.migr1yr$year5 <- f.migr1yr$year - (f.migr1yr$year %% 5)



  f.migr5yr <- f.migr1yr %>%
    group_by(year5) %>%
    summarize(avgmigr = sum(netmigration)/n())

  
  f.pltdata <- merge(f.migr5yr,f.jobs5yr,by="year5")

  # Generating Plot
  maxYr <- as.numeric(max(f.pltdata$year5))
 #Setting y axis 
  f.mig <- as.data.frame(f.pltdata$avgmigr)
  names(f.mig)[1] <- "val"
  f.job <- as.data.frame(f.pltdata$avgjobs)
  names(f.job)[1] <- "val"
  f.ylim <- rbind(f.mig,f.job)
  ymin <- min(f.ylim$val)
  ymax <- max(f.ylim$val)

  migrPlot <- ggplot(f.pltdata) + 
    geom_bar(aes(x=year5, y=avgjobs,color="Jobs"), stat="identity", fill= "#d8c772") +
    geom_line( aes(x=year5, y=avgmigr, color="Net Migration"), size=1.75) +
    geom_hline(yintercept=0, size=1.05) +
    scale_x_continuous(breaks=seq(1985,maxYr, by=5)) +
    scale_y_continuous(labels=scales::comma) +
    scale_colour_manual(" ", values=c("Jobs" = "#d8c772", "Net Migration" = "#00953A")) +
    scale_fill_manual("",values="#00953A") +

    labs(title = "Job Growth and Net Migration",
         subtitle = ctyname,
         caption = captionSrc("SDOBEA",""),
         x = "Year",
         y= "Number") +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.key=element_blank(),
          legend.title=element_blank(),
          axis.text = element_text(size=12),
          legend.position = "bottom", legend.box="horizontal")


  #Final dataset

  f.pltdata$Geography <- ctyname
  f.pltdata <-f.pltdata[,c(4,1:3)]
  f.pltdata[,3:4] <- sapply(f.pltdata[,3:4], function(x) format(round(x,digits=0),big.mark=",",scientific=FALSE))
  names(f.pltdata) <- c("Geography"," 5-Year Period", "5-Year Average Jobs","5-Year Average Net Migration")

  
  #Building text
  OutText <- paste0("The Job Growth and Net Migration plot shows the relationship between job gowth and migration in ",ctyname,".")  
  OutText <- paste0(OutText," Generally, migration patterns follow changes in job growth demand.")
  
  
  outList <- list("plot" = migrPlot, "data" = f.pltdata, "text" = OutText)
  return(outList)

}
