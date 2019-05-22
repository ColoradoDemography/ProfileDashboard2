#' statsTable1 outputs the summary table in the stats section of the dashboard, draws data from the census API
#'
#' @param listID list containing id numbers and place names
#' @param sYr Start Year
#' @param eYr End year
#' @param ACS American Cummunity Survey Data series
#' @param oType Controls the rendering of the table, HTML or Latex
#' @return kable formatted table
#' @export

statsTable1 <- function(DBPool,lvl,listID,sYr,eYr,ACS){
  #outputs the top table in the dashboard

  # Collecting place ids from  idList, setting default values
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  
  state <- "08"
  jobsChk <- 0

  # Setting up calls etc. 
  
  strCtyProf <- "SELECT * FROM estimates.county_profiles;"
  strJobs <- "SELECT * FROM estimates.jobs_by_sector WHERE sector_id = '0';"
  
  
  if(lvl == "Counties") {  #Counties
    sqlStrPop1 <- paste0("SELECT countyfips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips)," and year = ", sYr," and placefips = 0;")
    sqlStrPop2 <- paste0("SELECT countyfips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips)," and year = ", eYr," and placefips = 0;")
    sqlStrJobs <- paste0("SELECT area_code, population_year, total_jobs FROM estimates.jobs_by_sector WHERE area_code = ",as.numeric(ctyfips)," and population_year = ",eYr,
                         " and sector_id = '0';")
    }
  
  if(lvl == "Municipalities") {  #Municialities
    #these pull out county
    c_sqlStrPop1 <- paste0("SELECT countyfips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips)," and year = ", sYr," and placefips = 0;")
    c_sqlStrPop2 <- paste0("SELECT countyfips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips)," and year = ", eYr," and placefips = 0;")
    c_sqlStrJobs <- paste0("SELECT area_code, population_year, total_jobs FROM estimates.jobs_by_sector WHERE area_code = ",as.numeric(ctyfips)," and population_year = ",eYr,
                         " and sector_id = '0';")
    
    #these pull out the municipalities
    sqlStrPop1 <- paste0("SELECT countyfips, placefips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE placefips = ",as.numeric(placefips)," and year = ", sYr,";")
    sqlStrPop2 <- paste0("SELECT countyfips, placefips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE placefips = ",as.numeric(placefips)," and year = ", eYr,";")
    sqlStrJobs <- paste0("SELECT ctyfips, placefips, geoname, year, jobs FROM estimates.muni_jobs_long WHERE placefips = ",as.numeric(placefips)," and year = ", eYr, ";")
  }
  

      f.tPopyr1p <-  dbGetQuery(DBPool, sqlStrPop1)
      f.tPopyr2p <-  dbGetQuery(DBPool, sqlStrPop2)
      f.Jobsp <- dbGetQuery(DBPool, sqlStrJobs)
 
  
  if(lvl == "Municipalities") { #Pulling the County level data for Municipalities
    f.tPopyr1c <-  dbGetQuery(DBPool, c_sqlStrPop1)
    f.tPopyr2c <-  dbGetQuery(DBPool, c_sqlStrPop2)
    f.Jobsc <- dbGetQuery(DBPool, c_sqlStrJobs)
  }
  
  cty_Profile <- dbGetQuery(DBPool, strCtyProf)
  cty_jobs <- dbGetQuery(DBPool, strJobs)
  
  

  #Counties
  if(lvl == "Counties") {
    #Fixing NA Values
    tpop1c <- ifelse(is.na(f.tPopyr1p$totalpopulation),0,f.tPopyr1p$totalpopulation)  # Pop 2010
    tpop2c <- ifelse(is.na(f.tPopyr2p$totalpopulation),0,f.tPopyr2p$totalpopulation)  # Pop Current Year
    tpopchngc <- tpop2c - tpop1c   # Pop Change value
    
    jobsVal <-  cty_jobs %>%  filter(area_code == as.numeric(ctyfips) & population_year == eYr)
    jobsValc <- jobsVal$total_jobs
    
    hhincc <- codemog_api(data="b19013",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    medhhincc <- hhincc$b19013001
    
    MedHHValuec <- codemog_api(data="b25077",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    medhhvalc <- MedHHValuec$b25077001
    
    Povertyc <- codemog_api(data="b17001",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    povertyc <- percent(as.numeric(Povertyc$b17001002)/as.numeric(Povertyc$b17001001)*100)
    
    Nativec <- codemog_api(data="b05002",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    nativec <- percent(as.numeric(Nativec$b05002003)/as.numeric(Nativec$b05002001)*100)
    
    countyData <- c(tpop2c,tpopchngc,jobsValc,medhhincc,medhhvalc,povertyc,nativec)
  }

  # Municipalities
  if(lvl == "Municipalities") {   
    
    # Building County data
    tpop1c <- ifelse(is.na(f.tPopyr1c$totalpopulation),0,f.tPopyr1c$totalpopulation)  # Pop 2010
    tpop2c <- ifelse(is.na(f.tPopyr2c$totalpopulation),0,f.tPopyr2c$totalpopulation)  # Pop Current Year
    tpopchngc <- tpop2c - tpop1c   # Pop Change value
    
    jobsVal <-  f.Jobsc %>% summarize(totalJobs = sum(total_jobs)) #County
    jobsValc <- jobsVal$totalJobs   
    
    
    hhincc <- codemog_api(data="b19013",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    medhhincc <- hhincc$b19013001
 
    MedHHValuec <- codemog_api(data="b25077",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    medhhvalc <- MedHHValuec$b25077001
    
    Povertyc <- codemog_api(data="b17001",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    povertyc <- percent(as.numeric(Povertyc$b17001002)/as.numeric(Povertyc$b17001001)*100)
    
    Nativec <- codemog_api(data="b05002",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    nativec <- percent(as.numeric(Nativec$b05002003)/as.numeric(Nativec$b05002001)*100)
    
    countyData <- c(tpop2c,tpopchngc,jobsValc,medhhincc,medhhvalc,povertyc,nativec)
    
    # Building Municipal Data
 
    if(listID$multiCty == "F") {
      #Fixing NA Values
      tpop1m <- ifelse(is.na(f.tPopyr1p$totalpopulation),0,f.tPopyr1p$totalpopulation)  # Pop 2010
      tpop2m <- ifelse(is.na(f.tPopyr2p$totalpopulation),0,f.tPopyr2p$totalpopulation)  # Pop Current Year
      tpopchngm <- tpop2m - tpop1m   # Pop Change value
      
      jobsValm <-  ifelse(f.Jobsp$jobs == -9,"",f.Jobsp$jobs)  #The  Total Jobs Value
      
      hhincm <- codemog_api(data="b19013",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
      medhhincm <- hhincm$b19013001
      
      MedHHValuem <- codemog_api(data="b25077",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
      medhhvalm <- MedHHValuem$b25077001
      
      Povertym <- codemog_api(data="b17001",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
      povertym <- percent(as.numeric(Povertym$b17001002)/as.numeric(Povertym$b17001001)*100)
   
      Nativem <- codemog_api(data="b05002",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
      nativem <- percent(as.numeric(Nativem$b05002003)/as.numeric(Nativem$b05002001)*100)
      
      muniData <- c(tpop2m,tpopchngm,jobsValm,medhhincm,medhhvalm,povertym,nativem)
    } else {  #This for multiple county cities
      
      #Selecting total
      f.tpop1t <- f.tPopyr1p[which(f.tPopyr1p$countyfips == 999),]
      f.tpop2t <- f.tPopyr2p[which(f.tPopyr2p$countyfips == 999),]
      
      tpop1m <- ifelse(is.na(f.tpop1t$totalpopulation),0,f.tpop1t$totalpopulation)  # Pop 2010
      tpop2m <- ifelse(is.na(f.tpop2t$totalpopulation),0,f.tpop2t$totalpopulation)  # Pop Current Year
      tpopchngm <- tpop2m - tpop1m   # Pop Change value
      
      jobsValm <-  ifelse(f.Jobsp$jobs == -9,"",f.Jobsp$jobs)  #The  Total Jobs Value
      
      #Prepping for multiple counties
      hhincm <- codemog_api(data="b19013",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
      medhhincm <- hhincm$b19013001
      
      MedHHValuem <- codemog_api(data="b25077",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
      medhhvalm <- MedHHValuem$b25077001
      
      Povertym <- codemog_api(data="b17001",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
      povertym <- percent(as.numeric(Povertym$b17001002)/as.numeric(Povertym$b17001001)*100)
      
      Nativem <- codemog_api(data="b05002",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
      nativem <- percent(as.numeric(Nativem$b05002003)/as.numeric(Nativem$b05002001)*100)
      
      muniData <- c(tpop2m,tpopchngm,jobsValm,medhhincm,medhhvalm,povertym,nativem)
    }  
     
  }
  
  #state Values
  #Population
 
  f.tPopyr1ST <- cty_Profile %>%  filter(countyfips == 0 & year == sYr) 
  tpop1ST <- f.tPopyr1ST$totalpopulation
  
  f.tPopyr2ST <- cty_Profile %>%  filter(countyfips == 0 & year == eYr) 
  tpop2ST <- f.tPopyr2ST$totalpopulation
  
  tpopchngST <- tpop2ST - tpop1ST
  
  #Jobs
  
  f.tJobsST <- cty_jobs %>%  filter(area_code == 0 & population_year == eYr) 
  jobsValST <- f.tJobsST$total_jobs
  
  hhincST <- codemog_api(data="b19013",db=ACS, geonum=paste("1", state,  sep=""), meta="no")
  medhhincST <- hhincST$b19013001
  
  MedHHValueST <- codemog_api(data="b25077",db=ACS, geonum=paste("1", state, sep=""), meta="no")
  medhhvalST <- MedHHValueST$b25077001
  
  PovertyST <- codemog_api(data="b17001",db=ACS, geonum=paste("1", state, sep=""), meta="no")
  povertyST <- percent(as.numeric(PovertyST$b17001002)/as.numeric(PovertyST$b17001001)*100)
  
  NativeST <- codemog_api(data="b05002",db=ACS, geonum=paste("1", state,  sep=""), meta="no")
  nativeST <- percent(as.numeric(NativeST$b05002003)/as.numeric(NativeST$b05002001)*100)
  
  
  stateData <- c(tpop2ST,tpopchngST,jobsValST,medhhincST,medhhvalST,povertyST,nativeST)
  
  
  
  #Preparing table
  
  if(lvl == "Municipalities"){  # if the table is for a municipality, there are 4 columns (List, Muni, County, State)
    mcol <- 4
  } else {
    mcol <- 3
  }
  
  outTab <- matrix(" ",nrow=7,ncol=mcol)
  nCol <- 1
  outTab[1,nCol] <- paste0("Population (",eYr,")",footnote_marker_symbol(1))
  outTab[2,nCol] <- paste0("Population Change (",sYr," to ",eYr, ")",footnote_marker_symbol(1))
  
  if(jobsChk == -9) {
    outTab[3,nCol] <- paste0("Total Employment (",jobsPl$year,")",footnote_marker_symbol(1))
  } else {
    outTab[3,nCol] <- paste0("Total Employment (",eYr,")",footnote_marker_symbol(1))
  }
  
  outTab[4,nCol] <- paste0("Median Household Income",footnote_marker_symbol(2))
  outTab[5,nCol] <- paste0("Median House Value",footnote_marker_symbol(2))
  outTab[6,nCol] <- paste0("Percentage of Population with Incomes lower than the Poverty Line",footnote_marker_symbol(2))
  outTab[7,nCol] <- paste0("Percentage of Population Born in Colorado",footnote_marker_symbol(2))
  nCol <- nCol + 1 
  
  if(lvl == "Municipalities"){
    
    # Municipality output
    
    outTab[1,nCol] <- format(as.numeric(muniData[1]),nsmall=0, big.mark=",")
    outTab[2,nCol] <- format(as.numeric(muniData[2]),nsmall=0, big.mark=",")
    outTab[3,nCol] <- format(round(as.numeric(muniData[3]),digits=0),nsmall=0, big.mark=",")
    outTab[4,nCol] <- paste0("$",format(as.numeric(muniData[4]),nsmall=0, big.mark=","))
    outTab[5,nCol] <- paste0("$",format(as.numeric(muniData[5]),nsmall=0, big.mark=","))
    outTab[6,nCol] <- muniData[6]
    outTab[7,nCol] <- muniData[7]
    outTab[3,nCol] <- ifelse(outTab[3,nCol] == "NA","",outTab[3,nCol])
    nCol <- nCol + 1
    
    #County
    outTab[1,nCol] <- format(as.numeric(countyData[1]),nsmall=0, big.mark=",")
    outTab[2,nCol] <- format(as.numeric(countyData[2]),nsmall=0, big.mark=",")
    outTab[3,nCol] <- format(round(as.numeric(countyData[3]),digits=0),nsmall=0, big.mark=",")
    outTab[4,nCol] <- paste0("$",format(as.numeric(countyData[4]),nsmall=0, big.mark=","))
    outTab[5,nCol] <- paste0("$",format(as.numeric(countyData[5]),nsmall=0, big.mark=","))
    outTab[6,nCol] <- countyData[6]
    outTab[7,nCol] <- countyData[7]
    nCol <- nCol + 1
    
    
    #State
    outTab[1,nCol] <- format(as.numeric(stateData[1]),nsmall=0, big.mark=",")
    outTab[2,nCol] <- format(as.numeric(stateData[2]),nsmall=0, big.mark=",")
    outTab[3,nCol] <- format(round(as.numeric(stateData[3]),digits=0),nsmall=0, big.mark=",")
    outTab[4,nCol] <- paste0("$",format(as.numeric(stateData[4]),nsmall=0, big.mark=","))
    outTab[5,nCol] <- paste0("$",format(as.numeric(stateData[5]),nsmall=0, big.mark=","))
    outTab[6,nCol] <- stateData[6]
    outTab[7,nCol] <- stateData[7]
  }   
  
  if(lvl == "Counties"){
    #County
    outTab[1,nCol] <- format(as.numeric(countyData[1]),nsmall=0, big.mark=",")
    outTab[2,nCol] <- format(as.numeric(countyData[2]),nsmall=0, big.mark=",")
    outTab[3,nCol] <- format(round(as.numeric(countyData[3]),digits=0),nsmall=0, big.mark=",")
    outTab[4,nCol] <- paste0("$",format(as.numeric(countyData[4]),nsmall=0, big.mark=","))
    outTab[5,nCol] <- paste0("$",format(as.numeric(countyData[5]),nsmall=0, big.mark=","))
    outTab[6,nCol] <- countyData[6]
    outTab[7,nCol] <- countyData[7]
    nCol <- nCol + 1
    
    #State
    outTab[1,nCol] <- format(as.numeric(stateData[1]),nsmall=0, big.mark=",")
    outTab[2,nCol] <- format(as.numeric(stateData[2]),nsmall=0, big.mark=",")
    outTab[3,nCol] <- format(round(as.numeric(stateData[3]),digits=0),nsmall=0, big.mark=",")
    outTab[4,nCol] <- paste0("$",format(as.numeric(stateData[4]),nsmall=0, big.mark=","))
    outTab[5,nCol] <- paste0("$",format(as.numeric(stateData[5]),nsmall=0, big.mark=","))
    outTab[6,nCol] <- stateData[6]
    outTab[7,nCol] <- stateData[7]
  }   
  
  
  
  
  # Create Column headings

  if(lvl == "Municipalities") {
    names_spaced <- c(" ", placename, ctyname, "Colorado")
  } else {
    names_spaced <- c(" ",ctyname,"Colorado")
  }
  
  #Generating HTMl File
  if(lvl == "Municipalities") {
    outHTML <-  kable(outTab, format='html', table.attr='class="cleanTab"',
                      digits=1,
                      row.names=FALSE,
                      align='lrrr',
                      col.names = names_spaced,
                      caption="Community Quick Facts",
                      escape = FALSE)   %>%
      kable_styling() %>%
      column_spec(1, width = "4in") %>%
      column_spec(2, width = "0.4in") %>%
      column_spec(3, width = "0.4in") %>%
      column_spec(4, width = "0.4in") %>%
      kableExtra::footnote(symbol=c("Source: State Demography Office",captionSrc("ACS",ACS)))
  } else {
    outHTML <-  kable(outTab, format='html', table.attr='class="cleanTab"',
                      digits=1,
                      row.names=FALSE,
                      align='lrr',
                      col.names = names_spaced,
                      caption="Community Quick Facts",
                      escape = FALSE)   %>%
      kable_styling() %>%
      column_spec(1, width = "4in") %>%
      column_spec(2, width = "0.4in") %>%
      column_spec(3, width = "0.4in") %>%
      kableExtra::footnote(symbol=c("Source: State Demography Office",captionSrc("ACS",ACS)))
  } 
  
  #Generate Flextable
  outTab <- gsub("<sup>"," ",outTab)
  outTab <- gsub("</sup>","",outTab)
  outTab <- gsub("&dagger;"," ^",outTab)
  f.Flex <- as.data.frame(outTab)
  FlexOut <- regulartable(f.Flex)
  if(ncol(f.Flex) == 3) {
    FlexOut <- set_header_labels(FlexOut, V1 = "", 
                                 V2 = ctyname, V3 = "Colorado")
    FlexOut <- add_header(FlexOut,V1 ="Basic Statistics Table", top=TRUE)
    FlexOut <- add_footer(FlexOut,V1=paste0("* State Demography Office ||"," ^",captionSrc("ACS",ACS)))
    FlexOut <- merge_at(FlexOut,i=1,j = 1:3,part="header")
    FlexOut <- merge_at(FlexOut,i=1, j = 1:3, part = "footer")
    FlexOut <- align(FlexOut,i=1,j = 1, align="left",part="header")
    FlexOut <- align(FlexOut,i=2,j = 1:3, align="center",part="header")     
    FlexOut <- align(FlexOut,i=1, align="left",part="footer")
    FlexOut <- align(FlexOut, j=1, align="left", part="body")
    FlexOut <- autofit(FlexOut)
    FlexOut <- width(FlexOut, j = ~ V1, width = 4)
  }
  if(ncol(f.Flex) == 4) {
    FlexOut <- set_header_labels(FlexOut, V1 = "", V2=placename,
                                 V3 = ctyname, V4 = "Colorado")
    FlexOut <- add_header(FlexOut,V1 ="Basic Statistics Table", top=TRUE)
    FlexOut <- add_footer(FlexOut,V1=paste0("* State Demography Office ||"," ^",captionSrc("ACS",ACS)))
    FlexOut <- merge_at(FlexOut,i=1,j = 1:3,part="header")
    FlexOut <- merge_at(FlexOut, j = 1:4, part = "footer")
    FlexOut <- align(FlexOut,i=1,j = 1, align="left",part="header")
    FlexOut <- align(FlexOut,i=2,j = 1:4, align="center",part="header")     
    FlexOut <- align(FlexOut,i=1, align="left",part="footer")
    FlexOut <- align(FlexOut, j=1, align="left", part="body")
    FlexOut <- autofit(FlexOut)
    FlexOut <- width(FlexOut, j = ~ V1, width = 4)
  }
  
  
  
  #redefining rows for latex footnotes
  outTab[1,1] <- paste0("Population (",eYr,")","+")
  outTab[2,1] <- paste0("Population Change (",sYr," to ",eYr, ")","+")
  outTab[3,1] <- paste0("Total Employment (",eYr,")","+")
  outTab[4,1] <- paste0("Median Household Income","^")
  outTab[5,1] <- paste0("Median House Value","^")
  outTab[6,1] <- paste0("Percentage of Population with Incomes lower than the Poverty Line","^")
  outTab[7,1] <- paste0("Percentage of Population Born in Colorado","^")
  
  add_mat <- matrix(" ",nrow=2,ncol=nCol)
  add_mat[1,1] <- "+Source: State Demography Office"
  add_mat[2,1] <- paste0("^",captionSrc("ACS",ACS))
  
  outTab <- rbind(outTab,add_mat)
  
  
  if(lvl == "Municipalities") {
    outLATEX <- outTab %>%
      kable(digits=1,
            row.names=FALSE,
            align="lrrr",
            col.names = names_spaced,
            caption="Community Quick Facts",
            format ="latex", booktabs=TRUE,escape=TRUE) %>%
      kable_styling(latex_options="HOLD_position",font_size=10) %>%
      row_spec(0, align="c") %>%
      column_spec(1, width = "4in") %>%
      column_spec(2, width = "0.4in") %>%
      column_spec(3, width = "0.4in") %>%
      column_spec(4, width = "0.4in") 
  }  else  {
    outLATEX <- outTab %>%
      kable(digits=1,
            row.names=FALSE,
            align="lrr",
            col.names = names_spaced,
            caption="Community Quick Facts",
            format ="latex", booktabs=TRUE, escape=TRUE) %>%
      kable_styling(latex_options="HOLD_position",font_size=10) %>%
      row_spec(0, align="c") %>%
      column_spec(1, width = "4in") %>%
      column_spec(2, width = "0.4in") %>%
      column_spec(3, width = "0.4in")
  }
  OutTxt <- " "
  if(listID$multiCty == "T") {
    OutTxt <- "Note: For municipalities in multiple counties, comparison data from the largest county is displayed."
  }
  
  outList <- list("Htable" = outHTML, "FlexTable" = FlexOut, "Ltable" = outLATEX, "text" = OutTxt, "data"=f.Flex)
  return(outList)
  
  
}

