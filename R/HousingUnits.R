#' HousingUnits Summary table for owner-occupied and Rental Housing Units
#' Replaces OOHouse and RT House
#'
#' @param listID the list containing place id and Place names
#' @param ACS Specifies the ACS data set to be used, reads curACS from Shiny program
#' @return kable formatted  table and data file
#' @export
#'

HousingUnits=function(listID, ACS, state="08"){
  
  # Collecting place ids from  idList, setting default values
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  
  
  if(nchar(placefips) == 0) {
    # Raw Place data
    f.b25033 <- codemog_api(data="b25033", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # Population by housing type
    f.b25032 <- codemog_api(data="b25032", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # Units in Structure
    f.b25037 <- codemog_api(data="b25037", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # Year Built
    f.b25010 <- codemog_api(data="b25010", db=ACS, geonum=paste0("1", state, ctyfips),meta="no") # Persons per Household
  } else {
    f.b25033 <- codemog_api(data="b25033", db=ACS, geonum=paste0("1", state, placefips),meta="no") # Population by housing type
    f.b25032 <- codemog_api(data="b25032", db=ACS, geonum=paste0("1", state, placefips),meta="no") # Units in Structure
    f.b25037 <- codemog_api(data="b25037", db=ACS, geonum=paste0("1", state, placefips),meta="no") # Year Built
    f.b25010 <- codemog_api(data="b25010", db=ACS, geonum=paste0("1", state, placefips),meta="no") # Persons per Household
    
  }
  #All Housing Units
  f.AcsALL <- cbind(f.b25033[,c(1,8:20)], f.b25032[,c(8:30)],f.b25037[,c(1,8)],f.b25010[,c(1,8)])
  
  f.AcsALL <- f.AcsALL[,c(1:37,39,41)]
  
  f.AcsALL[,2:39] <-as.numeric(as.character(f.AcsALL[,2:39]))
  
  
  f.AcsALL <- f.AcsALL %>% mutate(
    People_TOT = b25033001,
    People_1 	= b25033003 +  b25033009,
    People_2_4 =	 b25033004 +  b25033010,
    People_5 	 = b25033005 +  b25033011,
    People_MH =	 b25033006 +  b25033012,
    People_OTH =	 b25033007 +  b25033013,
    Units_TOT =	 b25032002 +  b25032013,
    Units_1 	= b25032003 + b25032004 +  b25032014 + b25032015,
    Units_2_4 =	 b25032005 + b25032006 +  b25032016 + b25032017,
    Units_5 	= b25032007 + b25032008 + b25032009 + b25032010 +  b25032018 + b25032019 + b25032020 + b25032021,
    Units_MH 	= b25032011 +  b25032022,
    Units_OTH  =	 b25032012 +  b25032023,
    Med_Yr = b25037001,
    PPH = b25010001)
  
  
  f.AcsALLL <- f.AcsALL[,c(1,40:53)] %>% gather(var, ACS, People_TOT:PPH, -geoname)
  
  f.AcsALL_Fin <- f.AcsALLL[,c(2,3)]
  names(f.AcsALL_Fin) <- c("var","ALL_VAL")
  
  
  #Owner Occupied
  f.AcsOO <- cbind(f.b25033[,c(1,9:14)], f.b25032[,c(9:19)],f.b25037[,c(1,9)],f.b25010[,c(1,9)])
  
  f.AcsOO <- f.AcsOO[,c(1:18,20,22)]
  
  f.AcsOO[,2:20] <-as.numeric(as.character(f.AcsOO[,2:20]))
  
  
  f.AcsOO <- f.AcsOO %>% mutate(
    People_TOT = b25033002,
    People_1 = b25033003,
    People_2_4 = b25033004,
    People_5 =  b25033005,
    People_MH = b25033006,
    People_OTH = b25033007,
    Units_TOT = b25032002,
    Units_1 = b25032003 + b25032004,
    Units_2_4 = b25032005 + b25032006,
    Units_5 = b25032007 + b25032008 + b25032009 + b25032010,
    Units_MH = b25032011,
    Units_OTH = b25032012,
    Med_Yr = b25037002,
    PPH = b25010002)
  
  
  f.AcsOOL <- f.AcsOO[,c(1,21:34)] %>% gather(var, ACS, People_TOT:PPH, -geoname)
  
  f.AcsOO_Fin <-  f.AcsOOL[,c(2,3)]
  names(f.AcsOO_Fin) <- c("var","OO_VAL")
  
  #Rental
  f.AcsRt <- cbind(f.b25033[,c(1,15:20)], f.b25032[,c(20:30)],f.b25037[,c(1,10)],f.b25010[,c(1,10)])
  
  f.AcsRt <- f.AcsRt[,c(1:18,20,22)]
  
  f.AcsRt[,2:20] <-as.numeric(as.character(f.AcsRt[,2:20]))
  
  
  f.AcsRt <- f.AcsRt %>% mutate(
    People_TOT = b25033008,
    People_1 = b25033009,
    People_2_4 = b25033010,
    People_5 = b25033011,
    People_MH = b25033012,
    People_OTH = b25033013,
    Units_TOT = b25032013,
    Units_1 = b25032014 + b25032015,
    Units_2_4 = b25032016 + b25032017,
    Units_5 = b25032018 + b25032019 + b25032020 + b25032021,
    Units_MH = b25032022,
    Units_OTH = b25032023,
    Med_Yr = b25037003,
    PPH = b25010003)
  
  f.AcsRtL <- f.AcsRt[,c(1,21:34)] %>% gather(var, ACS, People_TOT:PPH, -geoname)
  
  
  f.AcsRT_Fin <- f.AcsRtL[,c(2,3)]
  names(f.AcsRT_Fin) <- c("var","RT_VAL")
  
  #Combining Files, calcualting percentages and formatting values
  f.House <- left_join(f.AcsOO_Fin, f.AcsRT_Fin, by="var" ) %>%
    left_join(., f.AcsALL_Fin,by="var") 

  
  f.House$Pct_OO <- ifelse(f.House$ALL_VAL == 0,"",percent((f.House$OO_VAL/f.House$ALL_VAL)*100))
  f.House$Pct_RT <- ifelse(f.House$ALL_VAL == 0,"",percent((f.House$RT_VAL/f.House$ALL_VAL)*100))
  
  
  f.House$OO_VAL_F <-  ifelse(f.House$var == "PPH", formatC(as.numeric(f.House$OO_VAL), format="f", digits=2),
                              ifelse(f.House$var == "Med_Yr", formatC(as.numeric(f.House$OO_VAL), format="f", digits=0), formatC(as.numeric(f.House$OO_VAL),format="f", digits=0, big.mark=",")))
  f.House$RT_VAL_F <-  ifelse(f.House$var == "PPH", formatC(as.numeric(f.House$RT_VAL), format="f", digits=2),
                              ifelse(f.House$var == "Med_Yr", formatC(as.numeric(f.House$RT_VAL), format="f", digits=0), formatC(as.numeric(f.House$RT_VAL),format="f", digits=0, big.mark=",")))
  f.House$ALL_VAL_F <-  ifelse(f.House$var == "PPH", formatC(as.numeric(f.House$ALL_VAL), format="f", digits=2),
                               ifelse(f.House$var == "Med_Yr", formatC(as.numeric(f.House$ALL_VAL), format="f", digits=0), formatC(as.numeric(f.House$ALL_VAL),format="f", digits=0, big.mark=",")))
  f.House$Pct_OO <- ifelse(f.House$var == "PPH","",f.House$Pct_OO)
  f.House$Pct_OO <- ifelse(f.House$var == "Med_Yr","",f.House$Pct_OO)
  f.House$Pct_RT <- ifelse(f.House$var == "PPH","",f.House$Pct_RT)
  f.House$Pct_RT <- ifelse(f.House$var == "Med_Yr","",f.House$Pct_RT)
  
  # Creating Final File
  f.Units <- f.House[c(7:14),c(1,7,5,8,6,9)]
  f.People <- f.House[c(1:6),c(1,7,5,8,6,9)]
  
  f.Units$var <- gsub("Units_","",f.Units$var)
  names(f.Units) <- c("var","OO_Units","Pct_OO_Units","RT_Units","Pct_RT_Units","ALL_Units")
  f.People$var <- gsub("People_","",f.People$var)
  names(f.People) <- c("var","OO_People","Pct_OO_People","RT_People","Pct_RT_People","ALL_People")
  
  
  #f.HouseUnits <- full_join(f.Units,f.People, by = "var")
  f.HouseUnits <- f.Units
  f.HouseUnits[is.na(f.HouseUnits)] <- " "
  
  #Renaming rows and Columns
  f.HouseUnits$var <- ifelse(f.HouseUnits$var =="TOT", "All Housing Units",
                             ifelse(f.HouseUnits$var =="1","Single Unit Buildings",
                                    ifelse(f.HouseUnits$var =="2_4","Buildings with 2 to 4 Units",
                                           ifelse(f.HouseUnits$var =="5","Buildings with 5 or More Units",
                                                  ifelse(f.HouseUnits$var =="MH","Mobile Homes",
                                                         ifelse(f.HouseUnits$var =="OTH","RVs, Boats, Vans, Etc.",
                                                                ifelse(f.HouseUnits$var =="Med_Yr","Median Year of Construction",
                                                                       ifelse(f.HouseUnits$var =="PPH","Average Number of Persons Per Household",""
                                                                       ))))))))
  
  
  #Building table
  m.House <- as.matrix(f.HouseUnits)
  
  
  # Setting up table
  
  #Column Names
  names_spaced <- c("Housing Unit Type","Units","Percent","Units","Percent","Units")
  #Span Header
  if(nchar(placefips) == 0) {
    # create vector with colspan
    tblHead1 <- c(" " = 1, ctyname = 5)
    
    # set vector names
    names(tblHead1) <- c(" ", ctyname)
  } else {
    # create vector with colspan
    tblHead1 <- c(" " = 1, placename = 5)
    
    # set vector names
    names(tblHead1) <- c(" ", placename)
  }
  
  tblHead2 <- c(" " = 1, "Owner-Occupied Units" = 2, "Rental Units" = 2,"All Units" = 1)
  names(tblHead2) <- c(" ","Owner-Occupied Units","Rental Units","All Units")
  
  Housing_tab <- m.House %>%
    kable(format='html', table.attr='class="cleanTable"',
          row.names=FALSE,
          align='lrrrrr',
          caption="Characteristics of Housing Units",
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed") %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width = "2in") %>%
    column_spec(2, width = "0.4in") %>%
    column_spec(3, width ="0.4in") %>%
    column_spec(4, width ="0.4in") %>%
    column_spec(5, width ="0.4in") %>%
    column_spec(6, width ="0.4in") %>%
    add_indent(c(2:6)) %>%
    add_header_above(header=tblHead2) %>%
    add_header_above(header=tblHead1) %>%
    kableExtra::footnote(captionSrc("ACS",ACS))
  
  
  
  tabOut <-  kable(m.House,
                   col.names = names_spaced,
                   align="lrrrrr",
                   caption="Characteristics of Housing Units", row.names=FALSE,
                   format="latex", booktabs=TRUE)  %>%
    kable_styling(latex_options="HOLD_position",font_size=10)  %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width = "2in") %>%
    column_spec(2, width = "0.4in") %>%
    column_spec(3, width ="0.4in") %>%
    column_spec(4, width ="0.4in") %>%
    column_spec(5, width ="0.4in") %>%
    column_spec(6, width ="0.4in") %>%
    add_indent(c(2:6)) %>%
    add_header_above(header=tblHead2) %>%
    add_header_above(header=tblHead1) %>%
    kableExtra::footnote(captionSrc("ACS",ACS),threeparttable = T)
  
  # preparing FlexTable
  
  FlexOut <- regulartable(f.HouseUnits)
  FlexOut <- set_header_labels(FlexOut, var = "Variable", 
                               OO_Units="Units", Pct_OO_Units="Percent", RT_Units="Units",Pct_RT_Units="Percent",ALL_Units ="Units"
                               )
  
  FlexOut <- add_header(FlexOut,var="",OO_Units="Owner-Occupied Units",RT_Units="Rental Units", ALL_Units="All Units",
                         top=TRUE)
  if(nchar(placefips) == 0) {
    FlexOut <- add_header(FlexOut,OO_Units=ctyname,top=TRUE)
  } else {
    FlexOut <- add_header(FlexOut,OO_Units=placename,top=TRUE)
  }
  
  FlexOut <- add_header(FlexOut,var="Characteristics of Housing Units",top=TRUE) %>%
    add_footer(var=captionSrc("ACS",ACS)) %>%
    merge_at(i=1, j = 1:6, part = "header") %>%
    merge_at(i=2, j = 2:6, part = "header") %>%
    merge_at(i=3,j=2:3,part="header") %>%
    merge_at(i=3,j=4:5,part="header") %>%
    merge_at(i=1, j = 1:6, part = "footer") %>%
    align(i=1, j=1:6, align="left",part="header") %>%
    align(i=2, j=1:6, align="center",part="header") %>%
    align(i=3, j=1:6, align="left",part="header") %>%
    align(i=4,j=1,align="left",part="header") %>%
    align(i=4, j=2:6, align="center",part="header") %>%
    align(i=1, align="left",part="footer") %>%
    align(j=1, align="left", part="body") %>%
    autofit() %>%
    width(j=1, width=3) %>%
    width(j=2:6, width=1)
  
  
  # Preparing Final dataset
  f.HouseUnits2 <- f.HouseUnits
  if(nchar(placefips) == 0) {
    f.HouseUnits2$Location <- ctyname
  } else {
    f.HouseUnits2$Location <- placename
  }
  
  f.HouseUnits2 <- f.HouseUnits2[,c(7,1:6)]
  names(f.HouseUnits2)  <-c("Place","Variable","Owner-Occupied Housing Units","Percent",
                            "Rental Housing Units","Percent","All Housing Units"
                            )
  
  
  outList <- list("Htable" = Housing_tab,"Ltable" = tabOut,  "data" = f.HouseUnits2,"FlexTable"=FlexOut)
  return(outList)
  
}