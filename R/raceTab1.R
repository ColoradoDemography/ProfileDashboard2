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

  state="08"

  #output race tab using pull from API

    Cens20K <- 1
    #call to ACS Race variables
    #State Values
    ACSRaceST <- codemog_api(data="b03002", db=ACS, geonum=paste0("1", state),meta="no")
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
    
    
    f.ACSRaceST <- gather(ACSRaceST2[, c(1,30:38)], key = "race", value=ACS, HispanicP:NHTwoP)
    f.ACSRaceST$geoname <- "Colorado"
    ACSRowST <- data.frame(geoname = "Colorado",
                         race = "TotalP",
                         ACS = "100.00%")
    f.ACSRaceST <- rbind(f.ACSRaceST,ACSRowST)
    
    ACSRaceCTY <- codemog_api(data="b03002", db=ACS, geonum=paste0("1", state, ctyfips),meta="no")
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
    
    
    f.ACSRaceCTY <- gather(ACSRaceCTY2[, c(1,30:38)], key = "race", value=ACS, HispanicP:NHTwoP)
    f.ACSRaceCTY$geoname <- ctyname
    ACSRowCTY <- data.frame(geoname = ctyname,
                         race = "TotalP",
                         ACS = "100.00%")
    f.ACSRaceCTY <- rbind(f.ACSRaceCTY,ACSRowCTY)
    
    #call to Census 2010 API Race variables

    #State
    p9_10ST <- codemog_api(data="p9", geonum=paste0("1", state),meta="no")
    p9_10ST[,7:ncol(p9_10ST)]=as.numeric(as.character(p9_10ST[,7:ncol(p9_10ST)]))
    
    p9_10ST =p9_10ST  %>%
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
    
    p9_10ST <- p9_10ST[,c(1,18,19)]
    p9_10ST$geoname <- "Colorado"
    
    CensRowST <- data.frame(geoname = "Colorado",
                          race = "TotalP",
                          Census.2010 = "100.00%")
    p9_10ST <- rbind(p9_10ST,CensRowST)
    
    #County
    p9_10CTY <- codemog_api(data="p9", geonum=paste("1", state, ctyfips, sep=""),meta="no")
    p9_10CTY[,7:ncol(p9_10CTY)]=as.numeric(as.character(p9_10CTY[,7:ncol(p9_10CTY)]))
    
    p9_10CTY <- p9_10CTY %>%
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
    
    p9_10CTY <- p9_10CTY[,c(1,18,19)]
    p9_10CTY$geoname <- ctyname
    
    CensRowCTY <- data.frame(geoname = ctyname,
                          race = "TotalP",
                          Census.2010 = "100.00%")
    p9_10CTY <- rbind(p9_10CTY,CensRowCTY)
    
    #Call to Census 2000 API
    #State
    p4_00ST <- codemog_api(data="p4", db="c2000",geonum=paste0("1", state),meta="no")
    p4_00ST[,7:ncol(p4_00ST)]=as.numeric(as.character(p4_00ST[,7:ncol(p4_00ST)]))
    p4_00ST <- p4_00ST %>%
      select(geoname:p4011) %>%
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
    
    p4_00ST <- p4_00ST[,c(1,18,19)]
    p4_00ST$geoname <- "Colorado"
    
    names(CensRowST)[3] <- "Census.2000"
    p4_00ST <- rbind(p4_00ST,CensRowST)
    
  # County  
    
    # Fixing for Broomfield...
    if(ctyfips != "014") {
    p4_00CTY <- codemog_api(data="p4", db="c2000",geonum=paste0("1", state, ctyfips),meta="no")
    p4_00CTY[,7:ncol(p4_00CTY)]=as.numeric(as.character(p4_00CTY[,7:ncol(p4_00CTY)]))
    p4_00CTY <- p4_00CTY %>%
        select(geoname:p4011) %>%
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
      
      p4_00CTY <- p4_00CTY[,c(1,18,19)]
      p4_00CTY$geoname <- ctyname
    } else {
      p4_00CTY <- data.frame(geoname = ctyname,
                race = c("HispanicP","NonHispanicP","NHWhiteP","NHBlack", "NHAIANP","NHAsianP","NHNHOPIP","NHOtherP","NHTwoP"),
                                   Census.2000 = NA)
      
      CensRowCTY$Census.2010 <- " "
    }
      
      
      names(CensRowCTY)[3] <- "Census.2000"
      p4_00CTY <- rbind(p4_00CTY,CensRowCTY)
  
if(nchar(placefips) != 0) { #output municipality table
    #call to ACS Race variables
  
    ACSRaceMUNI=codemog_api(data="b03002", db=ACS, geonum=paste0("1", state, placefips),meta="no")
    #Converting values to numeric
    ACSRaceMUNI[,7:ncol(ACSRaceMUNI)]=as.numeric(as.character(ACSRaceMUNI[,7:ncol(ACSRaceMUNI)]))
    
    ACSRaceMUNI2 <- ACSRaceMUNI %>%
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
    
    
    f.ACSRaceMUNI <- gather(ACSRaceMUNI2[, c(1,30:38)], key = "race", value=ACS, HispanicP:NHTwoP)
    f.ACSRaceMUNI$geoname <- placename
    ACSRowMUNI <- data.frame(geoname = placename,
                         race = "TotalP",
                         ACS = "100.00%")
    f.ACSRaceMUNI <- rbind(f.ACSRaceMUNI,ACSRowMUNI)
    
    #call to Census 2010 API Race variables
    p9_10MUNI  <- codemog_api(data="p9", geonum=paste0("1", state, placefips),meta="no")
    p9_10MUNI[,7:ncol(p9_10MUNI)]=as.numeric(as.character(p9_10MUNI[,7:ncol(p9_10MUNI)]))
    
    p9_10MUNI  <- p9_10MUNI %>%
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
    
    p9_10MUNI <- p9_10MUNI[,c(1,18,19)]
    p9_10MUNI$geoname <- placename
    
    CensRowMUNI <- data.frame(geoname = placename,
                          race = "TotalP",
                          Census.2010 = "100.00%")
    p9_10MUNI <- rbind(p9_10MUNI,CensRowMUNI)
    
    #Call to Census 2000 API
   
    
    
    # Bizarro Adjustment for City of Creede
    if(placefips == "14765") {
       p4_00MUNI <- data.frame(geoname = "City of Creede", state = "8", county = "", place = "14765", tract = "", bg = "",
                     geonum = "10814765", p4001 = 377, p4002 = 6, p4003 = 371, p4004 = 364, p4005 = 361, p4006 = 0,
                     p4007 = 3, p4008 = 0, p4009 = 0, p4010 = 0, p4011 = 7, p4012 = 7, p4013 = 0, p4014 = 3, 
                     p4015 = 0, p4016 = 3, p4017 = 1, p4018 = 0, p4019 = 0, p4020 = 0, p4021 = 0, p4022 = 0,
                     p4023 = 0, p4024 = 0, p4025 = 0, p4026 = 0, p4027 = 0, p4028 = 0, p4029 = 0, p4030 = 0,
                    p4031 = 0, p4032 = 0, p4033 = 0, p4034 = 0, p4035 = 0, p4036 = 0, p4037 = 0, p4038 = 0,
                    p4039 = 0, p4040 = 0, p4041 = 0, p4042 = 0, p4043 = 0, p4044 = 0, p4045 = 0, p4046 = 0,
                    p4047 = 0, p4048 = 0, p4049 = 0, p4050 = 0, p4051 = 0, p4052 = 0, p4053 = 0, p4054 = 0,
                    p4055 = 0, p4056 = 0, p4057 = 0, p4058 = 0, p4059 = 0, p4060 = 0, p4061 = 0, p4062 = 0,
                    p4063 = 0, p4064 = 0, p4065 = 0, p4066 = 0, p4067 = 0, p4068 = 0, p4069 = 0, p4070 = 0,
                    p4071 = 0, p4072 = 0, p4073 = 0)
    } else {
      p4_00MUNI <- codemog_api(data="p4", db="c2000",geonum=paste0("1", state, placefips),meta="no")
     p4_00MUNI[,7:ncol(p4_00MUNI)]=as.numeric(as.character(p4_00MUNI[,7:ncol(p4_00MUNI)]))
    }
    # Correction for communities founded after 2000
      Cens20K <- 1
     
      p4_00MUNI <- p4_00MUNI %>%
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
      
      p4_00MUNI <- p4_00MUNI[,c(1,18,19)]
      p4_00MUNI$geoname <- placename
      
      names(CensRowMUNI)[3] <- "Census.2000"
      p4_00MUNI <- rbind(p4_00MUNI,CensRowMUNI)
    } 
    
 
    # Producing Joined File
      if(nchar(placefips) != 0) {  # MUNI
      f.raceFin <-  plyr::join_all(list(p4_00MUNI,p9_10MUNI, f.ACSRaceMUNI, p4_00CTY,p9_10CTY, f.ACSRaceCTY), by='race', type='left')
    } else {
      f.raceFin <-  plyr::join_all(list(p4_00CTY,p9_10CTY, f.ACSRaceCTY, p4_00ST,p9_10ST, f.ACSRaceST), by='race', type='left')
    }

  f.raceFin <- f.raceFin[,c(1:3,5,7,9,11,13)]
  
  
  f.raceFin$Race2 <-ifelse(f.raceFin$race == "TotalP","Total Population",
                           ifelse(f.raceFin$race == "HispanicP","Hispanic",
                                  ifelse(f.raceFin$race == "NonHispanicP", "Non-Hispanic",
                                         ifelse(f.raceFin$race == "NHWhiteP","Non-Hispanic White",
                                                ifelse(f.raceFin$race == "NHBlackP","Non-Hispanic Black",
                                                       ifelse(f.raceFin$race == "NHAIANP","Non-Hispanic Native American/Alaska Native",
                                                              ifelse(f.raceFin$race == "NHAsianP","Non-Hispanic Asian",
                                                                     ifelse(f.raceFin$race == "NHNHOPIP","Non-Hispanic Native Hawaiian/Pacific Islander",
                                                                            ifelse(f.raceFin$race == "NHOtherP","Non-Hispanic Other","Non-Hispanic, Two Races")))))))))
  
 
# removing NA
 f.raceFin[is.na(f.raceFin)] <- " "
 
   m.race <- as.matrix(f.raceFin[,c(9,3:8)]) #This is the matrix table
  
  
  
  
  #Column Names
  ACSName <- paste0("20",substr(ACS,6,7))
  
  names_spaced <- c("Race",paste0("2000",footnote_marker_number(1)), 
                    paste0("2010",footnote_marker_number(2)),
                    paste0(ACSName,footnote_marker_number(3)),
                    paste0("2000",footnote_marker_number(1)),
                    paste0("2010",footnote_marker_number(2)),
                    paste0(ACSName,footnote_marker_number(3)))
  
  names_spacedL <- c("Race","2000","2010",ACSName,
                    "2000","2010",ACSName)
  
  #Span Header
  
  # create vector with colspan
  if(nchar(placefips) == 0) {
    tblHead <- c(" " = 1, ctyname = 3,"Colorado" = 3)
    # set vector names
    names(tblHead) <- c(" ", ctyname,"Colorado")
  } else {
    tblHead <- c(" " = 1, placename = 3, ctyname = 3)
    # set vector names
    names(tblHead) <- c(" ", placename, ctyname)
  }
  
  
  
  tabHTML <- m.race %>%
    kable(format='html', table.attr='class="cleanTable"',
          digits=1,
          row.names=FALSE,
          align=c("l",rep("r",6)),
          caption="Race Trend",
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 12) %>%
    column_spec(1, width="4in") %>%
    column_spec(2, width="0.5in") %>%
    column_spec(3, width="0.5in") %>%
    column_spec(4, width="0.5in") %>%
    column_spec(5, width="0.5in") %>%
    column_spec(6, width="0.5in") %>%
    column_spec(7, width="0.5in") %>%
    add_indent(c(3:9)) %>%
    add_header_above(header=tblHead) %>%
    kableExtra::footnote(general="Sources",
             number=c("2000 Census",
                   "2010 Census",
                   captionSrc("ACS",ACS)))
             
  
  #LATEX Table
  # set vector names
  tabLATEX <- kable(m.race, col.names = names_spacedL,
                    caption="Race Trend", row.names=FALSE, align=c("l",rep("r",6)),
                    format="latex", booktabs=TRUE)  %>%
    kable_styling(latex_options="HOLD_position") %>%
    column_spec(1, width="3in") %>%
    column_spec(2, width="0.4in") %>%
    column_spec(3, width="0.4in") %>%
    column_spec(4, width="0.4in") %>%
    column_spec(5, width="0.4in") %>%
    column_spec(6, width="0.4in") %>%
    column_spec(7, width="0.4in") %>%
    add_indent(c(3:9)) %>%
    add_header_above(header=tblHead) %>%
    kableExtra:: footnote(general="Sources",
             c("2000: 2000 Census",
                      "2010: 2010 Census",
                      paste0(ACSName,": ",captionSrc("ACS",ACS))),threeparttable = T) 


     # Output Data
    race_data <- data.frame(m.race)
    if(nchar(placefips) == 0) {
      names(race_data) <- c("Race Category", paste0("Census 2000: ", ctyname),
                            paste0("Census 2010: ", ctyname),paste0(toupper(ACS),": ",ctyname),
                            "Census 2000: Colorado","Census 2010: Colorado", paste0(toupper(ACS),": Colorado"))
    } else {
      names(race_data) <- c("Race Category", paste0("Census 2000: ", placename),
                            paste0("Census 2010: ", placename),paste0(toupper(ACS),": ",placename),
                            paste0("Census 2000: ", ctyname),
                            paste0("Census 2010: ", ctyname),paste0(toupper(ACS),": ",ctyname))
      
    }

  
    #Preparing Flextable
    ACSName2 <- toupper(substr(ACS,1,3))
    ACSyr <- paste0("20",substr(ACS,4,5),"-","20",substr(ACS,6,7))
    Acs_Str <- paste0(ACSName2," ",ACSyr)
    tab_date <- substr(captionSrc("ACS",ACS),66,87)
    
    f.raceFlex <- as.data.frame(m.race)
    names(f.raceFlex) <- c("V1","V2","V3","V4","V5","V6","V7")
    FlexOut <- regulartable(f.raceFlex) %>%
      set_header_labels(  V1 = "Race Category", V2 = "Census 2000",
                          V3 = "Census 2010", V4 = Acs_Str,
                          V5 = "Census 2000",
                          V6 = "Census 2010", V7 = Acs_Str)
    
    
    if(nchar(placefips) == 0) {
      FlexOut <- add_header(FlexOut,V2=ctyname,V5="Colorado",top=TRUE)
    } else {
      FlexOut <- add_header(FlexOut,V2=placename,V5=ctyname,top=TRUE)
    }
    
    FlexOut <- FlexOut %>% add_header(V1="Race Trend",top=TRUE) %>%
      add_footer(V1=tab_date) %>%
      merge_at(i=2,j=2:4,part="header") %>%
      merge_at(i=2,j=5:7,part="header") %>%
      align(i=1, j=1, align="left",part="header") %>%
      align(i=2,j=2:4,align="center",part="header") %>%
      align(i=2,j=5:7,align="center",part="header") %>%
      align(i=3,j=1,align="left",part="header") %>%
      align(i=3,j=2:7,align="center",part="header") %>%
      align(i=1, align="left",part="footer") %>%
      align(j=1, align="left", part="body") %>%
      width(j=1, width=3.0) %>%
      width(j=2:7, width=0.8)
  
 
    
    #Preparing Text
    if(nchar(placefips) == 0) {
      OutText <- paste0("  The Race Trend table shows the changing racial and ethnic composition of ",ctyname," beginning in 2000 and continuing to the present.")
    } else {
      OutText <- paste0("  The Race Trend table shows the changing racial and ethnic composition of ",placename," beginning in 2000 and continuing to the present.")
    } 
  
  
  outList <- list("Htable" = tabHTML, "data" = race_data,"FlexTable"=FlexOut, "Ltable" = tabLATEX,"text" = OutText)
  return(outList)
}


