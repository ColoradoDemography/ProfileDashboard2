#' raceTab2 Table showing the percentage values by ethnic/race categories
#'
#'    pulls data from API This table compares Colorado % to selected geography
#'
#'    This table reports the MOEs and a significance test for each series
#'    comparing the percentages from each table...
#'
#' @param listID the list containing place id and Place names
#' @param ACS Specifies the ACS data set to be used, reads curACS from Shiny program
#' @return kable formatted  table and data file
#' @export

raceTab2 <- function(listID, ACS) {
  # Collecting place ids from  idList, setting default values
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
 # if(listID$PlFilter == "T") {
 #   placefips <- ""
 #   placename <- ""
 # }
  
  
  state="08"

  #output race tab using pull from API
  
  #call to ACS County Race variables
  ACSRaceCTY=codemog_api(data="b03002", db=ACS, geonum=paste("1", "08", ctyfips, sep=""),meta="no")
  #Converting values to numeric
  ACSRaceCTY[,7:ncol(ACSRaceCTY)]=as.numeric(as.character(ACSRaceCTY[,7:ncol(ACSRaceCTY)]))

  ACSRaceCTY2 <- ACSRaceCTY %>%
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
           NHTwo=b03002009)


  f.ACSRaceCTY <- gather(ACSRaceCTY2[, c(20:29)], key = "race", value=ACS, TotalPop:NHTwo)

  ACSRaceCTYMOE=codemog_api(data="b03002_moe", db=ACS, geonum=paste("1", "08", ctyfips, sep=""),meta="no")

  ACSRaceCTYMOE[is.na(ACSRaceCTYMOE)] <- 0
  ACSRaceCTYMOE[,7:ncol(ACSRaceCTYMOE)] <- as.numeric(as.character(ACSRaceCTYMOE[,7:ncol(ACSRaceCTYMOE)]))

  ACSRaceCTYMOE2 <- ACSRaceCTYMOE %>%
    select(geoname:b03002_moe012) %>%
    mutate(TotalPop=b03002_moe001,
           Hispanic=b03002_moe012,
           NonHispanic=b03002_moe002,
           NHWhite=b03002_moe003,
           NHBlack=b03002_moe004,
           NHAIAN=b03002_moe005,
           NHAsian=b03002_moe006,
           NHNHOPI=b03002_moe007,
           NHOther=b03002_moe008,
           NHTwo=b03002_moe009)

  f.ACSRaceCTYMOE_Fin <- gather(ACSRaceCTYMOE2[, c(20:29)], key = "race", value=ACS, TotalPop:NHTwo)
  
  # the county file
  f.county <- left_join(f.ACSRaceCTY,f.ACSRaceCTYMOE_Fin,by="race")
  names(f.county) <- c("Race","Count_County","MOE_County")
  
  total_County <- as.numeric(f.county[which(f.county$Race == "TotalPop"),2])
  f.county$CountPCT_County <- f.county$Count_County/total_County
  f.county$MOEPCT_County <- f.county$MOE_County/total_County


  #call to ACS, State Table
  ACSRaceST=codemog_api(data="b03002", db=ACS, geonum=paste("1", state,  sep=""),meta="no")
  #Converting values to numeric
  ACSRaceST[,7:ncol(ACSRaceST)]=as.numeric(as.character(ACSRaceST[,7:ncol(ACSRaceST)]))

  ACSRaceST2 <- ACSRaceST %>%
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
           NHTwo=b03002009)

  f.ACSRaceST <- gather(ACSRaceST2[, c(20:29)], key = "race", value=ACS, TotalPop:NHTwo)

  # State level MOEs

  ACSRaceSTMOE=codemog_api(data="b03002_moe", db=ACS, geonum=paste("1", state, sep=""),meta="no")
  ACSRaceSTMOE[is.na(ACSRaceSTMOE)] <- 0
  ACSRaceSTMOE[,7:ncol(ACSRaceSTMOE)]=as.numeric(as.character(ACSRaceSTMOE[,7:ncol(ACSRaceSTMOE)]))

  ACSRaceSTMOE2 <- ACSRaceSTMOE %>%
    select(geoname:b03002_moe012) %>%
    mutate(TotalPop=b03002_moe001,
           Hispanic=b03002_moe012,
           NonHispanic=b03002_moe002,
           NHWhite=b03002_moe003,
           NHBlack=b03002_moe004,
           NHAIAN=b03002_moe005,
           NHAsian=b03002_moe006,
           NHNHOPI=b03002_moe007,
           NHOther=b03002_moe008,
           NHTwo=b03002_moe009)

  f.ACSRaceSTMOE_Fin <- gather(ACSRaceSTMOE2[, c(20:29)], key = "race", value=ACS, TotalPop:NHTwo)
  
  # the state file
  f.state <- left_join(f.ACSRaceST,f.ACSRaceSTMOE_Fin,by="race")
  names(f.state) <- c("Race","Count_State","MOE_State")
  
  total_State <- as.numeric(f.state[which(f.state$Race == "TotalPop"),2])
  f.state$CountPCT_State <- f.state$Count_State/total_State
  f.state$MOEPCT_State <- f.state$MOE_State/total_State

if(nchar(placefips) != 0) {
  #call to ACS place Race variables
  ACSRacePL=codemog_api(data="b03002", db=ACS, geonum=paste("1", "08", placefips, sep=""),meta="no")
  #Converting values to numeric
  ACSRacePL[,7:ncol(ACSRacePL)]=as.numeric(as.character(ACSRacePL[,7:ncol(ACSRacePL)]))
  
  ACSRacePL2 <- ACSRacePL %>%
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
           NHTwo=b03002009)
  
  
  f.ACSRacePL <- gather(ACSRacePL2[, c(20:29)], key = "race", value=ACS, TotalPop:NHTwo)
  
  ACSRacePLMOE=codemog_api(data="b03002_moe", db=ACS, geonum=paste("1", "08", placefips, sep=""),meta="no")
  
  ACSRacePLMOE[is.na(ACSRacePLMOE)] <- 0
  ACSRacePLMOE[,7:ncol(ACSRacePLMOE)] <- as.numeric(as.character(ACSRacePLMOE[,7:ncol(ACSRacePLMOE)]))
  
  ACSRacePLMOE2 <- ACSRacePLMOE %>%
    select(geoname:b03002_moe012) %>%
    mutate(TotalPop=b03002_moe001,
           Hispanic=b03002_moe012,
           NonHispanic=b03002_moe002,
           NHWhite=b03002_moe003,
           NHBlack=b03002_moe004,
           NHAIAN=b03002_moe005,
           NHAsian=b03002_moe006,
           NHNHOPI=b03002_moe007,
           NHOther=b03002_moe008,
           NHTwo=b03002_moe009)
  
  f.ACSRacePLMOE_Fin <- gather(ACSRacePLMOE2[, c(20:29)], key = "race", value=ACS, TotalPop:NHTwo)
 
  # the state file
  f.place <- left_join(f.ACSRacePL,f.ACSRacePLMOE_Fin,by="race")
  names(f.place) <- c("Race","Count_Place","MOE_Place")
  
  total_Place <- as.numeric(f.place[which(f.place$Race == "TotalPop"),2])
  f.place$CountPCT_Place <- f.place$Count_Place/total_Place
  f.place$MOEPCT_Place <- f.place$MOE_Place/total_Place
}

if(nchar(placefips) == 0){
  f.raceFin <- left_join(f.county,f.state, by="Race")
  #Calculating the statistical test
  f.raceFin$ZScore <- (abs(f.raceFin$CountPCT_County - f.raceFin$CountPCT_State)/
                         sqrt((f.raceFin$MOEPCT_County^2) + (f.raceFin$MOEPCT_State^2)))
  f.raceFin$Sig_Diff <- ifelse(f.raceFin$ZScore < 1,"No","Yes")
  f.raceFin$Sig_Diff <- ifelse(is.na(f.raceFin$Sig_Diff)," ",f.raceFin$Sig_Diff)
  
  #Formatting Percentage Values
  f.raceFin$CountPCT_County <- percent(f.raceFin$CountPCT_County*100)
  f.raceFin$MOEPCT_County <- percent(f.raceFin$MOEPCT_County*100)
  f.raceFin$CountPCT_State <- percent(f.raceFin$CountPCT_State*100)
  f.raceFin$MOEPCT_State <- percent(f.raceFin$MOEPCT_State*100)
  
  # table Heading
  tblHead <- c(" " = 1, ctyname = 2, "Colorado"  = 2, " " = 1)
  # set vector names
  names(tblHead) <- c(" ", ctyname,"Colorado"," ")
} else {
  f.raceFin <- left_join(f.place,f.county, by="Race")
  #Calculating the statistical test
  f.raceFin$ZScore <- (abs(f.raceFin$CountPCT_Place - f.raceFin$CountPCT_County)/
                         sqrt((f.raceFin$MOEPCT_Place^2) + (f.raceFin$MOEPCT_County^2)))
  f.raceFin$Sig_Diff <- ifelse(f.raceFin$ZScore < 1,"No","Yes")
  f.raceFin$Sig_Diff <- ifelse(is.na(f.raceFin$Sig_Diff)," ",f.raceFin$Sig_Diff)
  
  #Formatting Percentage Values
  f.raceFin$CountPCT_Place <- percent(f.raceFin$CountPCT_Place*100)
  f.raceFin$MOEPCT_Place <- percent(f.raceFin$MOEPCT_Place*100)
  f.raceFin$CountPCT_County <- percent(f.raceFin$CountPCT_County*100)
  f.raceFin$MOEPCT_County <- percent(f.raceFin$MOEPCT_County*100)
  
  # create vector with colspan
  tblHead <- c(" " = 1, placename = 2, ctyname  = 2, " " = 1)
  # set vector names
  names(tblHead) <- c(" ", placename,ctyname," ")
}
  


  #Revising the Levels
  f.raceFin[,1] <-   ifelse(f.raceFin[,1] == "TotalPop", "Total Population",
                            ifelse(f.raceFin[,1] == "Hispanic","Hispanic",
                                   ifelse(f.raceFin[,1] == "NonHispanic", "Non-Hispanic",
                                          ifelse(f.raceFin[,1] == "NHWhite","Non-Hispanic White",
                                                 ifelse(f.raceFin[,1] == "NHBlack","Non-Hispanic Black",
                                                        ifelse(f.raceFin[,1] == "NHAIAN","Non-Hispanic Native American/Alaska Native",
                                                               ifelse(f.raceFin[,1] == "NHAsian","Non-Hispanic Asian",
                                                                      ifelse(f.raceFin[,1] == "NHNHOPI","Non-Hispanic Native Hawaiian/Pacific Islander",
                                                                             ifelse(f.raceFin[,1] == "NHOther","Non-Hispanic Other","Non-Hispanic, Two Races")))))))))




  m.race <- as.matrix(f.raceFin[c(2:10,1),c(1,4,5,8,9,11)]) #This is the matrix table

  #Column Names

  names_spaced <- c("Race","Percentage","MOE","Percentage","MOE","Sig. Diff.?")




  tabHTML <- m.race %>%
    kable(format='html', table.attr='class="cleanTable"',
          digits=1,
          row.names=FALSE,
          align='lrrrrr',
          caption="Race Comparison",
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 12) %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width = "3in") %>%
    column_spec(2, width = "0.5in") %>%
    column_spec(3, width ="0.5in") %>%
    column_spec(4, width ="0.5in") %>%
    column_spec(5, width ="0.5in") %>%
    column_spec(6, width ="0.5in") %>%
    add_indent(c(3:9)) %>%
    add_header_above(header=tblHead) %>%
    kableExtra::footnote(captionSrc("ACS",ACS))


  race_data <- data.frame(m.race)
  
  if(nchar(placefips) == 0) {
  names(race_data)[1] <- "Race Category"
  names(race_data)[2] <- paste0("Percentage: ",ctyname)
  names(race_data)[3] <- paste0("MOE: ",ctyname)
  names(race_data)[4] <- "Percentage: Colorado"
  names(race_data)[5] <- "MOE: Colorado"
  names(race_data)[6] <- "Signficant Difference?"
  } else {
    names(race_data)[1] <- "Race Category"
    names(race_data)[2] <- paste0("Percentage: ",placename)
    names(race_data)[3] <- paste0("MOE: ",placename)
    names(race_data)[4] <- paste0("Percentage: ",ctyname)
    names(race_data)[5] <- paste0("MOE: ",ctyname)
    names(race_data)[6] <- "Signficant Difference?"  
  }

  #Preparing Flextable
  f.race_data <- data.frame(m.race)
  names(f.race_data) <- c("Race","PCT1","MOE1","PCT2","MOE2","Diff")
  FlexOut <- regulartable(f.race_data)
  FlexOut <- set_header_labels(FlexOut, Race = "Race Category", 
                               PCT1="Percent", MOE1="Margin of Error",
                               PCT2="Percent", MOE2="Margin of Error",
                               Diff="Significant Difference?")
  
  if(nchar(placefips) == 0) {
    FlexOut <- add_header(FlexOut,PCT1=ctyname,PCT2="Colorado",top=TRUE)
  } else {
    FlexOut <- add_header(FlexOut,PCT1=placename,PCT2=ctyname,top=TRUE)
  }
  
  FlexOut <- add_header(FlexOut,Race="Race Comparison",top=TRUE)
  FlexOut <- add_footer(FlexOut,Race=captionSrc("ACS",ACS))
  FlexOut <- merge_at(FlexOut,i=1, j = 1:6, part = "header")
  FlexOut <- merge_at(FlexOut,i=2, j = 2:3, part = "header") 
  FlexOut <- merge_at(FlexOut,i=2, j = 4:5, part = "header")
  FlexOut <- merge_at(FlexOut,i=1, j = 1:6, part = "footer")
  FlexOut <- align(FlexOut,i=1:3, j=1, align="left",part="header")
  FlexOut <- align(FlexOut,i=2:3, j=2:6, align="center",part="header")
  FlexOut <- align(FlexOut,i=1, align="left",part="footer")
  FlexOut <- align(FlexOut, j=1, align="left", part="body")
  FlexOut <- autofit(FlexOut)
  FlexOut <- width(FlexOut,j=1, width=3)
  FlexOut <- width(FlexOut,j=2:6, width=1)


    tabLATEX <- m.race %>% kable(
                    col.names = names_spaced,
                    align="lrrrrr",
                    caption="Race Comparison", row.names=FALSE,
                    format="latex", booktabs=TRUE)  %>%
      kable_styling(latex_options="HOLD_position",font_size=10) %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "3in") %>%
      column_spec(2, width = "0.4in") %>%
      column_spec(3, width ="0.4in") %>%
      column_spec(4, width ="0.4in") %>%
      column_spec(5, width ="0.4in") %>%
      column_spec(6, width ="0.4in") %>%
      add_indent(c(3:9)) %>%
      add_header_above(header=tblHead) %>%
      kableExtra::footnote(captionSrc("ACS",ACS),threeparttable = T)

    #Preparing Text
    if(nchar(placefips) == 0) {
      OutText <- paste0("  The Race Comparison table compares the distriburion of ethnic and racial groups in ",ctyname," to the state.")
    } else {
      OutText <- paste0("  The Race Compaison table compares the distriburion of ethnic and racial groups in ",placename," to ",ctyname, ".")
    }


   outList <- list("Htable" = tabHTML, "data" = race_data,"FlexTable"= FlexOut, "Ltable" = tabLATEX,"text" = OutText)
    return(outList)
  }

