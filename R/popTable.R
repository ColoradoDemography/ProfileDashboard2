#' popTable The population table showing the annual growth rate in the Population Section
#' Updates 1/2019 to remove county_profiles call and PostgreSQL code
#'
#' @param lvl the data level, Counties, Municipalities and region
#' @param listID the list containing place id and Place names
#' @param sYr Start Year
#' @param eYr End year
#' @return kable formatted  table and data file
#' @export
#'
popTable <- function(DBPool,lvl,listID,sYr,eYr) {

  # Collecting place ids from  idList, setting default values
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
 

  #outputs the Population Growth Rate table in the population section..
  
  state <- "Colorado"
  ctynum <- as.numeric(ctyfips)
  placenum <- as.numeric(placefips)
  yrs <- as.character(setYrRange(sYr,eYr))
  
  #State Population and Growth Rate
  stPopGrowth <- "SELECT countyfips, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = 0 AND placefips = 0;"
  popCO <- dbGetQuery(DBPool,stPopGrowth) %>%
    filter(year %in% yrs)%>%
    mutate(name="Colorado",
           totalpopulation=as.numeric(totalpopulation),
           year=as.numeric(year),
           growthRate=percent(signif((((totalpopulation/lag(totalpopulation))^(1/(year-lag(year)))) -1)*100),digits=1),
           Population=comma(totalpopulation))
  mCO <- popCO[,c(2,6,5)]
  
  #County Population and Growth Rate  
  if(lvl == "Counties" || lvl == "Municipalities") {
  ctyPopGrowth <- paste0("SELECT countyfips, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = ",ctynum," AND placefips = 0;") 
  mCty <- dbGetQuery(DBPool,ctyPopGrowth) %>%
    filter(year %in% yrs)%>%
    arrange(countyfips,year)%>%
    mutate(year=as.numeric(year),
           totalpopulation=as.numeric(totalpopulation),
           growthRate=percent(signif((((totalpopulation/lag(totalpopulation))^(1/(year-lag(year)))) -1)*100),digits=1),
           Population=comma(totalpopulation))
  mCty$Population  <- ifelse(mCty$totalpopulation == 0, " ",mCty$Population)
  mCty$name <- ctyname
  mCty <- mCty[,c(2,6,5,4)]
  }
  
  
  
  if(lvl == "Municipalities") { #if a placename is present
    sqlStrPop1 <- paste0("SELECT countyfips, placefips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE placefips = ",placenum,";")
   
    f.popPlace <-  dbGetQuery(DBPool, sqlStrPop1)
    
    
    f.popPlace <- f.popPlace[which(f.popPlace$countyfips != 999), ]  # removing "Total" for multi-county cities
    f.popPlace$totalpopulation <- ifelse(is.na(f.popPlace$totalpopulation),0,f.popPlace$totalpopulation) #Fixing NA values
    f.popPlace$municipalityname <-gsub(' \\([P,p]art\\)','',f.popPlace$municipalityname)
    
    # Adding records for Municipalities incorpropated after the beginning date in the series
    if(min(f.popPlace$year) > sYr) {
      minYr <- min(f.popPlace$year) 
      newRows <- minYr - sYr
      newYr <- matrix(nrow=newRows, ncol=5)
      for(x in 1:newRows) {
        newYr[x,1] <- ctyfips
        newYr[x,2] <- placefips
        newYr[x,3] <- placename
        newYr[x,4] <- as.numeric(sYr + (x - 1))
        newYr[x,5] <- 0
      }
      
      f.newRec <- as.data.frame(newYr,stringsAsFactors=FALSE)  
      names(f.newRec) <- c("countyfips", "placefips", "municipalityname", "year", "totalpopulation")
      f.newRec$year <- as.numeric(f.newRec$year)
      f.newRec$totalpopulation <- as.numeric(f.newRec$totalpopulation)
      f.popPlace <- rbind(f.newRec,f.popPlace)
    }
    
    PP <-  f.popPlace %>% group_by(placefips, municipalityname, year)  %>% summarize(totalpopulation = sum(as.numeric(totalpopulation)))
    
    placX <- PP %>% 
      filter(year %in% yrs)%>%
      arrange(year)
    
    
    placX$Population <- format(placX$totalpopulation,big.mark=",")
    placX$growthRate  <- percent((((placX$totalpopulation/lag(placX$totalpopulation))^(1/(placX$year-lag(placX$year)))) -1)*100,digits=1)
    placX$Population  <- ifelse(placX$totalpopulation == 0, " ",placX$Population)
    mPlace <- as.matrix(placX[,c(3,2,5,6)])
  }

  # Region  
  if(lvl == "Region") {
    popReg <- data.frame()
    for(i in 1:length(ctyfips)) {
      regPopSQL <- paste0("SELECT countyfips, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctynum[i])," AND placefips = 0;")
      regPop <-  dbGetQuery(DBPool, regPopSQL)
      popReg <- bind_rows(popReg,regPop)
    }
    mReg <- popReg %>% 
      group_by(year) %>%
      summarize(totalpopulation=sum(as.numeric(totalpopulation),na.rm=TRUE)) %>%
      filter(year %in% yrs)%>%
      arrange(year)%>%
      mutate(year=as.numeric(year),
             growthRate=percent(signif((((totalpopulation/lag(totalpopulation))^(1/(year-lag(year)))) -1)*100),digits=1),
             Population=comma(totalpopulation))
    mReg$Population  <- ifelse(mReg$totalpopulation == 0, " ",mReg$Population)
  }
  
  if(lvl == "Municipalities") { #if a placename is present
    m.OutTab <- cbind(mPlace,mCty,mCO)
    m.OutTab <- m.OutTab[,c(1,3,4,7,8,10,11)]
  } 
  if(lvl == "Counties") {
    m.OutTab <- cbind(mCty,mCO)
    m.OutTab <- m.OutTab[,c(1,3,4,6,7)] 
  } 
  if(lvl == "Region"){
    m.OutTab <- cbind(mReg,mCO)
    m.OutTab <- m.OutTab[,c(1,4,3,6,7)] 
  }
  
 
  m.OutTab$year <- as.character(m.OutTab$year)
  x.OutTab <- as.matrix(m.OutTab)
  x.OutTab <- gsub("NA%","",x.OutTab)
  #Additional Suppressions
  x.OutTab <- gsub("NaN%","",x.OutTab)
  x.OutTab <- gsub("Inf%","",x.OutTab)
  
  
  
  if(lvl == "Municipalities") {
    names_spaced <- c("Year","Population","Growth Rate","Population","Growth Rate","Population","Growth Rate") 
    tblHead <- c(" " = 1, placename = 2, ctyname = 2, state = 2)
    names(tblHead) <- c(" ", placename, ctyname,state)
  } else {
    names_spaced <- c("Year","Population","Growth Rate","Population","Growth Rate")
    tblHead <- c(" " = 1, ctyname = 2, state = 2)
    names(tblHead) <- c(" ", ctyname,state)
  }
  

  

    # Creating Final Table (kable)
    if(lvl == "Municipalities") {
      OutHTML  <- x.OutTab %>%
        kable(format='html', table.attr='class="myTable"',
              caption = "Population Growth Rate",
              row.names=FALSE,
              align='lrrrrrr',
              col.names = names_spaced,
              escape = FALSE) %>%
        kable_styling(bootstrap_options = "condensed") %>%
        column_spec(1, width = "0.4in") %>%
        column_spec(2, width = "0.5in") %>%
        column_spec(3, width ="0.5in") %>%
        column_spec(4, width = "0.5in") %>%
        column_spec(5, width = "0.5in") %>%
        column_spec(6, width = "0.5in") %>%
        column_spec(7, width = "0.5in") %>%
        add_header_above(header=tblHead)  %>%
        kableExtra::footnote(captionSrc("SDO",""))
    }  else { 
      OutHTML  <- 
        kable(x.OutTab,format='html', table.attr='class="myTable"',
              caption = "Population Growth Rate",
              row.names=FALSE,
              align='lrrrr',
              col.names = names_spaced,
              escape = FALSE) %>%
        kable_styling(bootstrap_options = "condensed") %>%
        column_spec(1, width = "0.4in") %>%
        column_spec(2, width = "0.5in") %>%
        column_spec(3, width = "0.5in") %>%
        column_spec(4, width = "0.5in") %>%
        column_spec(5, width = "0.5in") %>%
        add_header_above(header=tblHead)  %>%
        kableExtra::footnote(captionSrc("SDO",""))
    }
    
    # Creating Final Data Set and Flex table
    f.Out2 <- as.data.frame(x.OutTab)
    if(ncol(f.Out2) == 5) {
      names(f.Out2) <- c("Year",paste0("Population: ",ctyname),paste0("Growth Rate: ",ctyname),
                         "Population: Colorado","Growth Rate: Colorado")
      
      # Creating FlexTable
      FlexOut <- regulartable(as.data.frame(x.OutTab))
      FlexOut <- set_header_labels(FlexOut, year = "Year", 
                                   Population = "Population", growthRate = "Growth Rate",
                                   Population.1 = "Population", growthRate.1 = "Growth Rate")
      FlexOut <- add_header(FlexOut,year ="",Population=ctyname,growthRate="",
                            Population.1="Colorado",growthRate.1="",top=TRUE)
      FlexOut <- add_header(FlexOut,year ="Population Growth Table", top=TRUE)
      FlexOut <- add_footer(FlexOut,year=captionSrc("SDO",""))
      FlexOut <- merge_at(FlexOut,i=1,j = 1:5,part="header")
      FlexOut <- merge_at(FlexOut,i=2,j = 2:3,part="header")
      FlexOut <- merge_at(FlexOut,i=2,j = 4:5,part="header")
      FlexOut <- merge_at(FlexOut, j = 1:5, part = "footer")
      FlexOut <- align(FlexOut,i=1,j = 1, align="left",part="header")
      FlexOut <- align(FlexOut,i=2:3,j = 1:5, align="center",part="header")     
      FlexOut <- align(FlexOut,align="left",part="footer")
      FlexOut <- autofit(FlexOut)
      FlexOut <- width(FlexOut, j = ~ year, width = 1)
    }
    if(ncol(f.Out2) == 7) {
      names(f.Out2) <- c("Year",paste0("Population: ",placename),paste0("Growth Rate: ",placename),
                         paste0("Population: ",ctyname),paste0("Growth Rate: ",ctyname),
                         "Population: Colorado","Growth Rate: Colorado")
      
      # Creating FlexTable
      FlexOut <- regulartable(as.data.frame(x.OutTab))
      FlexOut <- set_header_labels(FlexOut, year = "Year", 
                                   Population = "Population", growthRate = "Growth Rate",
                                   Population.1 = "Population", growthRate.1 = "Growth Rate",
                                   Population.2 = "Population", growthRate.2 = "Growth Rate")
      FlexOut <- add_header(FlexOut,year ="",
                            Population=placename,growthRate="",
                            Population.1=ctyname,growthRate.1="",
                            Population.2="Colorado",growthRate.2="",top=TRUE)
      FlexOut <- add_header(FlexOut,year ="Population Growth Table", top=TRUE)
      FlexOut <- add_footer(FlexOut,year=captionSrc("SDO",""))
      FlexOut <- merge_at(FlexOut,i=1,j = 1:7,part="header")
      FlexOut <- merge_at(FlexOut,i=2,j = 2:3,part="header")
      FlexOut <- merge_at(FlexOut,i=2,j = 4:5,part="header")
      FlexOut <- merge_at(FlexOut,i=2,j = 6:7,part="header")
      FlexOut <- merge_at(FlexOut, j = 1:7, part = "footer")
      FlexOut <- align(FlexOut,i=1,j = 1, align="left",part="header")
      FlexOut <- align(FlexOut,i=2:3,j = 1:7, align="center",part="header")     
      FlexOut <- align(FlexOut,align="left",part="footer")
      FlexOut <- autofit(FlexOut)
      FlexOut <- width(FlexOut, j = ~ year, width = 1)
    }
    

    if(nchar(placename) != 0) {
      OutLATEX <- x.OutTab %>%
        kable(digits=1,
              row.names=FALSE,
              align="lrrrrrr",
              col.names = names_spaced,
              caption="Population Growth Rate",
              format ="latex", booktabs=TRUE) %>%
        kable_styling(latex_options="HOLD_position",font_size=9) %>%
        row_spec(0, align="c") %>%
        column_spec(column=1:7, width="0.4in") %>%
        add_header_above(header=tblHead)  %>%
        kableExtra::footnote(captionSrc("SDO",""),threeparttable=T)
    }  else { 
      OutLATEX <- x.OutTab %>%
        kable(digits=1,
              row.names=FALSE,
              align="lrrrr",
              col.names = names_spaced,
              caption="Population Growth Rate",
              format ="latex", booktabs=TRUE) %>%
        kable_styling(latex_options="HOLD_position",font_size=9) %>%
        row_spec(0, align="c") %>%
        column_spec(column=1:5, width="0.4in") %>%
        add_header_above(header=tblHead)  %>%
        kableExtra::footnote(captionSrc("SDO",""),threeparttable=T)
    }
    
    # Building text
    RowN <- nrow(x.OutTab)
    prevYr <- x.OutTab[RowN-1,1]
    
    # Extracting last growth rates
    if(nchar(placename) != 0) {
      plGR <- gsub("%","",as.character(x.OutTab[RowN,3]))
      ctyGR <- gsub("%","",as.character(x.OutTab[RowN,5]))
      stGR <- gsub("%","",as.character(x.OutTab[RowN,7]))
    } else {
      # Extracting last growth rates
      ctyGR <- gsub("%","",as.character(x.OutTab[RowN,3]))
      stGR <- gsub("%","",as.character(x.OutTab[RowN,5]))
    }
    
    if(nchar(placename) != 0) {#Municipalities
      OutTxt_pl <- paste0("  At the end of ",eYr, " the estimated population of ",placename, " was ", x.OutTab[RowN,2],", ")
      PopChgVal_pl <- as.numeric(gsub(",","",x.OutTab[RowN,2])) - as.numeric(gsub(",","",x.OutTab[RowN-1,2]))
      PopChgFmt_pl <- format(PopChgVal_pl,big.mark=",")
      PopChgTxt_pl <-  ifelse(PopChgVal_pl > 0, paste0("an increase of ",PopChgFmt_pl," over the population in ",prevYr,"."),
                              ifelse(PopChgVal_pl < 0, paste0("a decrease of ",PopChgFmt_pl," over the population in ",prevYr,"."),paste0("did not change between ",prevYr, " and ",eYr,".")
                              ))
      grTxtpl <- paste0("  The growth rate for ",placename," between ",prevYr," and ",eYr, " was ",plGR," percent")
      grTxtpl <- paste0(grTxtpl, " compared to ",ctyGR," percent for ",ctyname," and ",stGR," percent for the State of Colorado.")
      
      outText <- paste0(OutTxt_pl, PopChgTxt_pl,grTxtpl)
    } else {
      OutTxt_cty <- paste0("  At the end of ",eYr, " the estimated population of ",ctyname, " was ", x.OutTab[RowN,2],", ")
      PopChgVal_cty <- as.numeric(gsub(",","",x.OutTab[RowN,2])) - as.numeric(gsub(",","",x.OutTab[RowN-1,2]))
      PopChgFmt_cty <- format(PopChgVal_cty,big.mark=",")
      PopChgTxt_cty <-  ifelse(PopChgVal_cty > 0, paste0("an increase of ",PopChgFmt_cty," over the population in ",prevYr,"."),
                               ifelse(PopChgVal_cty < 0, paste0("a decrease of ",PopChgFmt_cty," over the population in ",prevYr,"."),paste0("did not change between ",prevYr, " and ",eYr,".")
                               ))
      grTxtcty <- paste0("  The growth rate for ",ctyname," between ",prevYr," and ",eYr, " was ",ctyGR," percent")
      grTxtcty <- paste0(grTxtcty, " compared to ",stGR," percent for the State of Colorado.")
      
      outText <- paste0(OutTxt_cty,PopChgTxt_cty,grTxtcty)  
    }
    
    
    
    # bind list
  
    outlist <- list("Htable" = OutHTML, "Ltable" = OutLATEX, "data" = f.Out2,"FlexTable"=FlexOut, "text" = outText)
    return(outlist)
  }
  
