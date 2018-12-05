#' raceTab1 Table showing the percentage values by ethnic/race categories
#'
#'    pulls data from API This table shows a set of histoical comparisons between
#'    the 2000 Census, the 2010 Census and the latest ACS API
#'
#'    This table does not report MOEs for ACS series, because of the lack of cunsus MOEs...
#'
#' @param listID the list containing place id and Place names
#' @param state is the state that the original fips
#' @param ACS Specifies the ACS data set to be used, reads curACS from Shiny program
#' @return kable formatted  table and data file
#' @export
#'
raceTab1 <- function(listID, ACS) {
  # Collecting place ids from  idList, setting default values
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  #  if(listID$PlFilter == "T") {
  #    placefips <- ""
  #    placename <- ""
  #  }
  
  state="08"
  
  #output race tab using pull from API
  if(nchar(placefips) == 0) { # output county table
    Cens20K <- 1
    #call to ACS Race variables
    
    ACSRace=codemog_api(data="b03002", db=ACS, geonum=paste("1", "08", ctyfips, sep=""),meta="no")
    #Converting values to numeric
    ACSRace[,7:ncol(ACSRace)]=as.numeric(as.character(ACSRace[,7:ncol(ACSRace)]))
    
    ACSRace2 <- ACSRace %>%
      select(geoname:b03002012) %>%
      mutate(TotalPop=b03002001,
             Hispanic=b03002012,
             NonHispanic=b03002002,
             NHWhite=b03002003,
             NHBlack=b03002004,
             NHAIAN=b03002005,
             NHAsian=b03002006,
             NHNHOPI=b03002007,
             NHOther=b03002008,
             NHTwo=b03002009,
             HispanicP=percent(round(Hispanic/TotalPop, 3)*100),
             NonHispanicP=percent(round(NonHispanic/TotalPop,3)*100),
             NHWhiteP=percent(round(NHWhite/TotalPop, 3)*100),
             NHBlackP=percent(round(NHBlack/TotalPop, 3)*100),
             NHAIANP=percent(round(NHAIAN/TotalPop,3)*100),
             NHAsianP=percent(round(NHAsian/TotalPop,3)*100),
             NHNHOPIP=percent(round(NHNHOPI/TotalPop, 3)*100),
             NHOtherP=percent(round(NHOther/TotalPop,3)*100),
             NHTwoP=percent(round(NHTwo/TotalPop,3)*100))
    
    
    f.ACSRace <- gather(ACSRace2[, c(1,30:38)], key = "race", value=ACS, HispanicP:NHTwoP)
    f.ACSRace$geoname <- ctyname
    ACSRow <- data.frame(geoname = ctyname,
                         race = "TotalP",
                         ACS = "100.00%")
    f.ACSRace <- rbind(f.ACSRace,ACSRow)
    
    #call to Census 2010 API Race variables
    p9_10=codemog_api(data="p9", geonum=paste("1", state, ctyfips, sep=""),meta="no")
    p9_10[,7:ncol(p9_10)]=as.numeric(as.character(p9_10[,7:ncol(p9_10)]))
    
    p9_10=p9_10%>%
      select(geoname:p9011)%>%
      mutate(TotalPop=p9001, Hispanic=p9002, NonHispanic=p9003, NHWhite=p9005, NHBlack=p9006,
             NHAIAN=p9007, NHAsian=p9008, NHNHOPI=p9009, NHOther=p9010, NHTwo=p9011,
             HispanicP=percent(round(Hispanic/TotalPop, 3)*100),
             NonHispanicP=percent(round(NonHispanic/TotalPop,3)*100),
             NHWhiteP=percent(round(NHWhite/TotalPop, 3)*100),
             NHBlackP=percent(round(NHBlack/TotalPop, 3)*100),
             NHAIANP=percent(round(NHAIAN/TotalPop,3)*100),
             NHAsianP=percent(round(NHAsian/TotalPop,3)*100),
             NHNHOPIP=percent(round(NHNHOPI/TotalPop, 3)*100),
             NHOtherP=percent(round(NHOther/TotalPop,3)*100),
             NHTwoP=percent(round(NHTwo/TotalPop,3)*100)) %>%
      select(-p9001:-p9011)%>%
      gather(race, Census.2010, HispanicP:NHTwoP, -geoname:-geonum)
    
    p9_10 <- p9_10[,c(1,18,19)]
    p9_10$geoname <- ctyname
    
    CensRow <- data.frame(geoname = ctyname,
                          race = "TotalP",
                          Census.2010 = "100.00%")
    p9_10 <- rbind(p9_10,CensRow)
    
    #Call to Census 2000 API
    p4_00=codemog_api(data="p4", db="c2000",geonum=paste("1", state, ctyfips, sep=""),meta="no")
    if(nrow(p4_00) != 0) {
      p4_00[,7:ncol(p4_00)]=as.numeric(as.character(p4_00[,7:ncol(p4_00)]))
      p4_00=p4_00%>%
        select(geoname:p4011)%>%
        mutate(TotalPop=p4001, Hispanic=p4002, NonHispanic=p4003, NHWhite=p4005, NHBlack=p4006,
               NHAIAN=p4007, NHAsian=p4008, NHNHOPI=p4009, NHOther=p4010, NHTwo=p4011,
               HispanicP=percent(round(Hispanic/TotalPop, 3)*100),
               NonHispanicP=percent(round(NonHispanic/TotalPop,3)*100),
               NHWhiteP=percent(round(NHWhite/TotalPop, 3)*100),
               NHBlackP=percent(round(NHBlack/TotalPop, 3)*100),
               NHAIANP=percent(round(NHAIAN/TotalPop,3)*100),
               NHAsianP=percent(round(NHAsian/TotalPop,3)*100),
               NHNHOPIP=percent(round(NHNHOPI/TotalPop, 3)*100),
               NHOtherP=percent(round(NHOther/TotalPop,3)*100),
               NHTwoP=percent(round(NHTwo/TotalPop,3)*100))%>%
        select(-p4001:-p4011)%>%
        gather(race, Census.2000, HispanicP:NHTwoP, -geoname:-geonum)
      
      p4_00 <- p4_00[,c(1,18,19)]
      p4_00$geoname <- ctyname
      
      names(CensRow)[3] <- "Census.2000"
      p4_00 <- rbind(p4_00,CensRow)
      raceTmp <- inner_join(p4_00, p9_10)
    }  else {
      raceTmp <-  p9_10
    }
    f.raceFin <- inner_join(raceTmp, f.ACSRace)
    
  } else{ #output municipality table
    #call to ACS Race variables
    
    ACSRace=codemog_api(data="b03002", db=ACS, geonum=paste("1", "08", placefips, sep=""),meta="no")
    #Converting values to numeric
    ACSRace[,7:ncol(ACSRace)]=as.numeric(as.character(ACSRace[,7:ncol(ACSRace)]))
    
    ACSRace2 <- ACSRace %>%
      select(geoname:b03002012) %>%
      mutate(TotalPop=b03002001,
             Hispanic=b03002012,
             NonHispanic=b03002002,
             NHWhite=b03002003,
             NHBlack=b03002004,
             NHAIAN=b03002005,
             NHAsian=b03002006,
             NHNHOPI=b03002007,
             NHOther=b03002008,
             NHTwo=b03002009,
             HispanicP=percent(round(Hispanic/TotalPop, 3)*100),
             NonHispanicP=percent(round(NonHispanic/TotalPop,3)*100),
             NHWhiteP=percent(round(NHWhite/TotalPop, 3)*100),
             NHBlackP=percent(round(NHBlack/TotalPop, 3)*100),
             NHAIANP=percent(round(NHAIAN/TotalPop,3)*100),
             NHAsianP=percent(round(NHAsian/TotalPop,3)*100),
             NHNHOPIP=percent(round(NHNHOPI/TotalPop, 3)*100),
             NHOtherP=percent(round(NHOther/TotalPop,3)*100),
             NHTwoP=percent(round(NHTwo/TotalPop,3)*100))
    
    
    f.ACSRace <- gather(ACSRace2[, c(1,30:38)], key = "race", value=ACS, HispanicP:NHTwoP)
    f.ACSRace$geoname <- placename
    ACSRow <- data.frame(geoname = placename,
                         race = "TotalP",
                         ACS = "100.00%")
    f.ACSRace <- rbind(f.ACSRace,ACSRow)
    
    #call to Census 2010 API Race variables
    p9_10=codemog_api(data="p9", geonum=paste("1", state, placefips, sep=""),meta="no")
    p9_10[,7:ncol(p9_10)]=as.numeric(as.character(p9_10[,7:ncol(p9_10)]))
    
    p9_10=p9_10%>%
      select(geoname:p9011)%>%
      mutate(TotalPop=p9001, Hispanic=p9002, NonHispanic=p9003, NHWhite=p9005, NHBlack=p9006,
             NHAIAN=p9007, NHAsian=p9008, NHNHOPI=p9009, NHOther=p9010, NHTwo=p9011,
             HispanicP=percent(round(Hispanic/TotalPop, 3)*100),
             NonHispanicP=percent(round(NonHispanic/TotalPop,3)*100),
             NHWhiteP=percent(round(NHWhite/TotalPop, 3)*100),
             NHBlackP=percent(round(NHBlack/TotalPop, 3)*100),
             NHAIANP=percent(round(NHAIAN/TotalPop,3)*100),
             NHAsianP=percent(round(NHAsian/TotalPop,3)*100),
             NHNHOPIP=percent(round(NHNHOPI/TotalPop, 3)*100),
             NHOtherP=percent(round(NHOther/TotalPop,3)*100),
             NHTwoP=percent(round(NHTwo/TotalPop,3)*100)) %>%
      select(-p9001:-p9011)%>%
      gather(race, Census.2010, HispanicP:NHTwoP, -geoname:-geonum)
    
    p9_10 <- p9_10[,c(1,18,19)]
    p9_10$geoname <- placename
    
    CensRow <- data.frame(geoname = placename,
                          race = "TotalP",
                          Census.2010 = "100.00%")
    p9_10 <- rbind(p9_10,CensRow)
    
    #Call to Census 2000 API
    p4_00=codemog_api(data="p4", db="c2000",geonum=paste("1", state, placefips, sep=""),meta="no")
    # Correction for communities founded after 2000
    if(nrow(p4_00) == 1) {
      Cens20K <- 1
      p4_00[,7:ncol(p4_00)]=as.numeric(as.character(p4_00[,7:ncol(p4_00)]))
      p4_00=p4_00%>%
        select(geoname:p4011)%>%
        mutate(TotalPop=p4001, Hispanic=p4002, NonHispanic=p4003, NHWhite=p4005, NHBlack=p4006,
               NHAIAN=p4007, NHAsian=p4008, NHNHOPI=p4009, NHOther=p4010, NHTwo=p4011,
               HispanicP=percent(round(Hispanic/TotalPop, 3)*100),
               NonHispanicP=percent(round(NonHispanic/TotalPop,3)*100),
               NHWhiteP=percent(round(NHWhite/TotalPop, 3)*100),
               NHBlackP=percent(round(NHBlack/TotalPop, 3)*100),
               NHAIANP=percent(round(NHAIAN/TotalPop,3)*100),
               NHAsianP=percent(round(NHAsian/TotalPop,3)*100),
               NHNHOPIP=percent(round(NHNHOPI/TotalPop, 3)*100),
               NHOtherP=percent(round(NHOther/TotalPop,3)*100),
               NHTwoP=percent(round(NHTwo/TotalPop,3)*100))%>%
        select(-p4001:-p4011)%>%
        gather(race, Census.2000, HispanicP:NHTwoP, -geoname:-geonum)
      
      p4_00 <- p4_00[,c(1,18,19)]
      p4_00$geoname <- placename
      
      names(CensRow)[3] <- "Census.2000"
      p4_00 <- rbind(p4_00,CensRow)
    } else {
      Cens20K <- 0
    }
    
    
    # Producing Joined File
    if(Cens20K == 1) {
      raceTmp <- inner_join(p4_00, p9_10)
    } else {
      raceTmp <-  p9_10
    }
    f.raceFin <- inner_join(raceTmp, f.ACSRace)
  }
  
  
  f.raceFin$Race2 <-ifelse(f.raceFin$race == "TotalP","Total Population",
                           ifelse(f.raceFin$race == "HispanicP","Hispanic",
                                  ifelse(f.raceFin$race == "NonHispanicP", "Non-Hispanic",
                                         ifelse(f.raceFin$race == "NHWhiteP","Non-Hispanic White",
                                                ifelse(f.raceFin$race == "NHBlackP","Non-Hispanic Black",
                                                       ifelse(f.raceFin$race == "NHAIANP","Non-Hispanic Native American/Alaska Native",
                                                              ifelse(f.raceFin$race == "NHAsianP","Non-Hispanic Asian",
                                                                     ifelse(f.raceFin$race == "NHNHOPIP","Non-Hispanic Native Hawaiian/Pacific Islander",
                                                                            ifelse(f.raceFin$race == "NHOtherP","Non-Hispanic Other","Non-Hispanic, Two Races")))))))))
  
  if(nrow(p4_00) != 0){
    m.race <- as.matrix(f.raceFin[c(1:4,6,5,7:10), c(6,3,4,5)]) #This is the matrix table
  } else {
    m.race <- as.matrix(f.raceFin[c(1:4,6,5,7:10), c(5,3,4)]) #This is the matrix table
  }
  
  
  
  #Column Names
  ACSName <- paste0("20",substr(ACS,6,7))
  
  if(nrow(p4_00) != 0){
    names_spaced <- c("Race","2000","2010",ACSName)
  } else {
    names_spaced <- c("Race","2010",ACSName)
  }
  
  
  #Span Header
  
  # create vector with colspan
  if(nchar(placefips) == 0) {
    tblHead <- c(" " = 1, ctyname = (ncol(m.race)-1))
    # set vector names
    names(tblHead) <- c(" ", ctyname)
  } else {
    tblHead <- c(" " = 1, placename = (ncol(m.race)-1))
    # set vector names
    names(tblHead) <- c(" ", placename)
  }
  
  
  
  
  if(nrow(p4_00) != 0) { 
    tabHTML <- m.race %>%
      kable(format='html', table.attr='class="cleanTable"',
            digits=1,
            row.names=FALSE,
            align='lrrr',
            caption="Race Trend",
            col.names = names_spaced,
            escape = FALSE)  %>%
      kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 12) %>%
      column_spec(1, width="4in") %>%
      column_spec(2, width="0.5in") %>%
      column_spec(3, width="0.5in") %>%
      column_spec(4, width="0.5in") %>%
      add_indent(c(3:9)) %>%
      add_header_above(header=tblHead) %>%
      add_footnote(c("Source; 2000 Census",
                     "Source: 2010 Census",
                     captionSrc("ACS",ACS)))
    
    race_data <- data.frame(m.race)
    if(nchar(placefips) == 0) {
      race_data$geoname <- ctyname
    } else {
      race_data$geoname <- placename
    }
    race_data <- race_data[,c(5,1:4)]
    names(race_data) <- c("Geography","Race Category","Census 2000", "Census 2010",toupper(ACS))
  } else {
    tabHTML <- m.race %>%
      kable(format='html', table.attr='class="cleanTable"',
            digits=1,
            row.names=FALSE,
            align='lrrr',
            caption="Race Trend",
            col.names = names_spaced,
            escape = FALSE)  %>%
      kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 12) %>%
      column_spec(1, width="4in") %>%
      column_spec(2, width="0.5in") %>%
      column_spec(3, width="0.5in") %>%
      add_indent(c(3:9)) %>%
      add_header_above(header=tblHead) %>%
      add_footnote(c("Source: 2010 Census",
                     captionSrc("ACS",ACS)))
    
    # Output Data
    race_data <- data.frame(m.race)
    if(nchar(placefips) == 0) {
      race_data$geoname <- ctyname
    } else {
      race_data$geoname <- placename
    }
    race_data <- race_data[,c(4,1:3)]
    names(race_data) <- c("Geography","Race Category", "Census 2010",toupper(ACS))
  }
  
  
  #Preparing Flextable
  Acs_Str <- substr(captionSrc("ACS",ACS),29,63)
  tab_date <- substr(captionSrc("ACS",ACS),66,87)
  f.race_data <- race_data[-1]
  
  if(ncol(f.race_data) == 4) {
    names(f.race_data) <- c("Race","Cens00","Cens10","ACS")
  } else {
    names(f.race_data) <- c("Race","Cens10","ACS")
  }
  
  FlexOut <- regulartable(f.race_data)
  if(ncol(f.race_data) == 4) {  
    FlexOut <- set_header_labels(FlexOut, Race = "Race Category", Cens00 = "Census 2000",
                                 Cens10 = "Census 2010", ACS = Acs_Str)
    
  } else {
    FlexOut <- set_header_labels(FlexOut, Race = "Race Category", Cens10 = "Census 2010", ACS = Acs_Str)
  }
  
  if(nchar(placefips) == 0) {
    FlexOut <- add_header(FlexOut,Race=ctyname,top=TRUE)
  } else {
    FlexOut <- add_header(FlexOut,Race=placename,top=TRUE)
  }
  
  FlexOut <- add_header(FlexOut,Race="Race Trend",top=TRUE)
  FlexOut <- add_footer(FlexOut,Race=tab_date)
  FlexOut <- align(FlexOut,i=1:3, j=1, align="left",part="header")
  FlexOut <- align(FlexOut,i=1, align="left",part="footer")
  FlexOut <- align(FlexOut, j=1, align="left", part="body")
  FlexOut <- autofit(FlexOut)
  
  
  if(nrow(p4_00) != 0) {
    # set vector names
    tabLATEX <- kable(m.race, col.names = names_spaced,
                      caption="Race Trend", row.names=FALSE, align=c("l",rep("r",3)),
                      format="latex", booktabs=TRUE)  %>%
      kable_styling(latex_options="HOLD_position") %>%
      column_spec(1,width = "3in") %>%
      column_spec(2,width = "0.4in") %>%
      column_spec(3,width = "0.4in") %>%
      column_spec(4,width = "0.4in") %>%
      add_indent(c(3:9)) %>%
      add_header_above(header=tblHead) %>%
      footnote(c("Source; 2000 Census",
                 "Source: 2010 Census",
                 captionSrc("ACS",ACS)),threeparttable=T) 
    
    #Preparing Text
    if(nchar(placefips) == 0) {
      OutText <- paste0("  The Race Trend table shows the changing racial and ethnic composition of ",ctyname," beginning in 2000 and continuing to the present.")
    } else {
      OutText <- paste0("  The Race Trend table shows the changing racial and ethnic composition of ",placename," beginning in 2000 and continuing to the present.")
    } 
  } else {
    # set vector names
    tabLATEX <- kable(m.race, col.names = names_spaced,
                      caption="Race Trend", row.names=FALSE, align=c("l",rep("r",3)),
                      format="latex", booktabs=TRUE)  %>%
      kable_styling(latex_options="HOLD_position")  %>%
      row_spec(0, align = "c") %>%
      column_spec(1,width = "4in") %>%
      column_spec(c(2:3),width = "0.5in") %>%
      add_indent(c(3:9)) %>%
      add_header_above(header=tblHead) %>%
      footnote(c("Source: 2010 Census",
                 captionSrc("ACS",ACS)),threeparttable=T)
    
    #Preparing Text
    if(nchar(placefips) == 0) {
      OutText <- paste0("The Race Trend table shows the changing racial and ethnic composition of ",ctyname," beginning in 2010 and continuing to the present.")
    } else {
      OutText <- paste0("The Race Trend table shows the changing racial and ethnic composition of ",placename," beginning in 2010 and continuing to the present.")
    }
  }
  
  outList <- list("Htable" = tabHTML, "data" = race_data,"FlexTable"=FlexOut, "Ltable" = tabLATEX,"text" = OutText)
  return(outList)
}


