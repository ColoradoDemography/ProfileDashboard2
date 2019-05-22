#' medianAgeTab Creates table showing the Median Age by Gender
#' for a selected place and county or for a copunty and for the state
#' Revised to produce Population Pyramid
#' 
#' @param listID the list containing place id and Place names
#' @param ACS a string identifying the input dataset eg: "acs1115"
#' @param state the State FIPS code, defaluts to "08" for Colorado.
#' @return a formatted table and dataset
#' @export

medianAgeTab <- function(listID, ACS, state="08"){

  # Collecting place ids from  idList, setting default values
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
 

  # Preparing Plot 9/2018
    # extracting data

  if(nchar(placefips) == 0) {  # Output for places
    sexbyage <- codemog_api(data="b01001",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
  }  else {
    sexbyage <- codemog_api(data="b01001",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
  }
  sexbyage[8:ncol(sexbyage)] <- sapply(sexbyage[8:ncol(sexbyage)],as.numeric)

 
  
  
  # Creating decades
  sexbyaged <- sexbyage %>%
    mutate(
            m0009 = b01001003 + b01001004,
            m1019 = b01001005 + b01001006 + b01001007,
            m2029 = b01001008 + b01001009 + b01001010 + b01001011,
            m3039 = b01001012 + b01001013,
            m4049 = b01001014 + b01001015,
            m5059 = b01001016 + b01001017,
            m6069 = b01001018 + b01001019 + b01001020 + b01001021,
            m7079 = b01001022 + b01001023,
            m8000 = b01001024 + b01001025,
            f0009 = b01001027 + b01001028,
            f1019 = b01001029 + b01001030 +	b01001031,
            f2029 = b01001032 + b01001033 +	b01001034 + b01001035,
            f3039 = b01001036 + b01001037,
            f4049 = b01001038 + b01001039,
            f5059 = b01001040 + b01001041,
            f6069 = b01001042 + b01001043 + b01001044 + b01001045,
            f7079 = b01001046 + b01001047,
            f8000 = b01001048 + b01001049,
            pct_m0009 = (m0009/b01001002) * 100,
            pct_m1019 = (m1019/b01001002) * 100,
            pct_m2029 = (m2029/b01001002) * 100,
            pct_m3039 = (m3039/b01001002) * 100,
            pct_m4049 = (m4049/b01001002) * 100,
            pct_m5059 = (m5059/b01001002) * 100,
            pct_m6069 = (m6069/b01001002) * 100,
            pct_m7079 = (m7079/b01001002) * 100,
            pct_m8000 = (m8000/b01001002) * 100,
            pct_f0009 = (f0009/b01001026) * 100,
            pct_f1019 = (f1019/b01001026) * 100,
            pct_f2029 = (f2029/b01001026) * 100,
            pct_f3039 = (f3039/b01001026) * 100,
            pct_f4049 = (f4049/b01001026) * 100,
            pct_f5059 = (f5059/b01001026) * 100,
            pct_f6069 = (f6069/b01001026) * 100,
            pct_f7079 = (f7079/b01001026) * 100,
            pct_f8000 = (f8000/b01001026) * 100
    )  
  
  # Split into male and female files for counts

    malebyageC <- sexbyaged[,c(1:7, 9, 57:65)]
    femalebyageC <- sexbyaged[,c(1:7, 33, 66:74)]
 
  malebyageCL <- malebyageC %>%
    gather(ageLevel, value, b01001002:m8000, factor_key=TRUE) %>%  
    mutate(agecat=ordered(as.factor(ageLevel), 
                          levels=c("m0009", "m1019", "m2029", "m3039", "m4049",
                                   "m5059", "m6069", "m7079", "m8000","b01001002"),
                          labels=c("0 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49",
                                   "50 to 59", "60 to 69", "70 to 79", "80 and over","Total")))
  
  
  femalebyageCL <- femalebyageC %>%
    gather(ageLevel, value, b01001026:f8000, factor_key=TRUE) %>%  
    mutate(agecat=ordered(as.factor(ageLevel), 
                          levels=c("f0009", "f1019", "f2029", "f3039", "f4049",
                                   "f5059", "f6069", "f7079", "f8000","b01001026"),
                          labels=c("0 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49",
                                   "50 to 59", "60 to 69", "70 to 79", "80 and over","Total")))
  
  # Split into male and female files for percentages

    malebyageP <- sexbyaged[,c(1:7, 75:83)]
    femalebyageP <- sexbyaged[,c(1:7, 84:92)]
 
      # find max value
    maxm <- max(malebyageP[8:16])
    maxf <- max(femalebyageP[8:16])
 
  
  maxpct <- max(maxm,maxf)
  
  maxval <- (10*ceiling(maxpct/10)) + 10
  
  
  malebyagePL <- malebyageP %>%
    gather(ageLevel, value, pct_m0009:pct_m8000, factor_key=TRUE) %>%  
    mutate(agecat=ordered(as.factor(ageLevel), 
                          levels=c("pct_m0009", "pct_m1019", "pct_m2029", "pct_m3039", "pct_m4049",
                                   "pct_m5059", "pct_m6069", "pct_m7079", "pct_m8000"),
                          labels=c("0 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49",
                                   "50 to 59", "60 to 69", "70 to 79", "80 and over")))
  malebyagePL$gender <- "Male"
  malebyagePL$value <- malebyagePL$value * -1
  
  femalebyagePL <- femalebyageP %>%
    gather(ageLevel, value, pct_f0009:pct_f8000, factor_key=TRUE) %>%  
    mutate(agecat=ordered(as.factor(ageLevel), 
                          levels=c("pct_f0009", "pct_f1019", "pct_f2029", "pct_f3039", "pct_f4049",
                                   "pct_f5059", "pct_f6069", "pct_f7079", "pct_f8000"),
                          labels=c("0 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49",
                                   "50 to 59", "60 to 69", "70 to 79", "80 and over")))
  femalebyagePL$gender = "Female"
  
  sexbyagePL <- rbind(malebyagePL,femalebyagePL)
  
  sexbyagePL$gender <- factor(sexbyagePL$gender,labels=c("Female","Male"))
  pltTitle <- "Age by Gender"
  
  if(nchar(placefips) == 0) {  # Output for places
    subTitle <- ctyname
  } else {
    subTitle <- placename
  }
  
  barCol <- c("#6EC4E8","#00953A")
  
  pyramid <- ggplot(sexbyagePL, aes(x = agecat, y = value, fill = gender)) + 
    geom_bar(data = subset(sexbyagePL, color="black", gender == "Female"), stat = "identity") + 
    geom_bar(data = subset(sexbyagePL, color="black", gender == "Male"), stat = "identity") + 
    scale_fill_manual(values=barCol, name="Gender") +
    scale_y_continuous(breaks=seq(-maxval,maxval,10),labels = paste0(as.character(c(seq(maxval, 10, -10), seq(0,maxval,10))), "%")) + 
    coord_flip() +
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = captionSrc("ACS",ACS),
         x = "Age Group",
         y= "Percentage of Population") +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          axis.text=element_text(size=12),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          legend.position= "bottom")
  
 
  # Merging files
    maleC <- malebyageCL[,c(1,9,10)]
    maleP <- malebyagePL[,c(1,9,10)]
  
  
  #Modify maleP 
  maleT <- maleP %>% summarize(value = sum(value))
  maleT$agecat <- "Total"
  maleT$geoname <- ctyname
  maleT <- maleT[,c(3,2,1)]
  maleP <- rbind(maleP,maleT)
 
  male <- left_join(maleC,maleP,by="agecat")
  male <- male[,c(1,3,2,5)]
  names(male) <- c("Geography","Age","Count","Percent")
  male$Count <- format(round(male$Count,digits=0),big.mark=",")
  male$Percent <- if_else(is.na(male$Percent),100,male$Percent)
  male$Percent <- percent(abs(male$Percent))
  
  
    femaleC <- femalebyageCL[,c(1,9,10)]
    femaleP <- femalebyagePL[,c(1,9,10)]
  
  femaleT <- femaleP %>% summarize(value = sum(value))
  femaleT$agecat <- "Total"
  femaleT$geoname <- ctyname
  femaleT <- femaleT[,c(3,2,1)]
  femaleP <- rbind(femaleP,femaleT)
  
  female <- left_join(femaleC,femaleP,by="agecat")
  female <- female[,c(1,3,2,5)]
  names(female) <- c("Geography","Age","Count","Percent")
  female$Percent <- if_else(is.na(female$Percent),100,female$Percent)
  female$Count <- format(round(female$Count,digits=0),big.mark=",")
  female$Percent <- percent(abs(female$Percent))
  
  
  sexbyagedf <- left_join(male,female,by="Age")
  sexbyagedf <- sexbyagedf[,c(1:4,6,7)]
  names(sexbyagedf) <- c("Geography","Age", "Male Count","Male Percent","Female Count","Female Percent")
  
  if(nchar(placefips) == 0) {  # Output for places
    sexbyagedf$Geography <- ctyname
  } else {
    sexbyagedf$Geography <- placename
  }
  

  sexbyagedf <- sexbyagedf[c(2:10,1),]
  
 # Median Age

 
  #County  Age
  medAgecty <- codemog_api(data="b01002",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
  medAgectyMOE <- codemog_api(data="b01002_moe",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
  medAgecty2 <- gather(medAgecty[1,8:10])
  medAgecty2$key <- c("Total","Male","Female")
  names(medAgecty2)[2] <- "MedAge_c"

  medAgecty2MOE <- gather(medAgectyMOE[1,8:10])
  medAgecty2MOE$key <- c("Total","Male","Female")
  names(medAgecty2MOE)[2] <- "MOE_c"

  f.ctyAge <- left_join(medAgecty2, medAgecty2MOE, by = "key")

  if(nchar(placefips) != 0) {
    #place  Age
    medAgeplace <- codemog_api(data="b01002",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
    medAgeplaceMOE <- codemog_api(data="b01002_moe",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
    
    medAgeplace2 <- gather(medAgeplace[1,8:10])
    medAgeplace2$key <- c("Total","Male","Female")
    names(medAgeplace2)[2] <- "MedAge_p"
    
    medAgeplace2MOE <- gather(medAgeplaceMOE[1,8:10])
    medAgeplace2MOE$key <- c("Total","Male","Female")
    names(medAgeplace2MOE)[2] <- "MOE_p"
    
    f.placeAge <- left_join(medAgeplace2, medAgeplace2MOE, by = "key")
  }
  
  
  #State Age
  medAgeST  <- codemog_api(data="b01002",db=ACS, geonum=paste("1", state,  sep=""), meta="no")
  medAgeSTMOE  <- codemog_api(data="b01002_moe",db=ACS, geonum=paste("1", state,  sep=""), meta="no")
  
  medAgeST2 <- gather(medAgeST[1,8:10])
  medAgeST2$key <- c("Total","Male","Female")
  names(medAgeST2)[2] <- "MedAge_s"
  
  medAgeST2MOE <- gather(medAgeSTMOE[1,8:10])
  medAgeST2MOE$key <- c("Total","Male","Female")
  names(medAgeST2MOE)[2] <- "MOE_s"
  
  f.stateAge <- left_join(medAgeST2, medAgeST2MOE, by = "key")

  #Creating Copmbined table and Calcualting tests

     if(nchar(placefips) == 0) {
      f.ageTab <- left_join(f.ctyAge, f.stateAge, by = "key")
      #Calculating significant differences
      f.ageTab$MedAge_c <- as.numeric(f.ageTab$MedAge_c)
      f.ageTab$MOE_c <- as.numeric(f.ageTab$MOE_c)
      f.ageTab$MedAge_s <- as.numeric(f.ageTab$MedAge_s)
      f.ageTab$MOE_s <- as.numeric(f.ageTab$MOE_s)
      
      f.ageTab$ZScore <- (abs(f.ageTab$MedAge_c - f.ageTab$MedAge_s)/
                            sqrt((f.ageTab$MOE_c^2) + (f.ageTab$MOE_s^2)))
      f.ageTab$Sig_Diff <- ifelse(f.ageTab$ZScore < 1,"No","Yes")
      f.ageTab$Difference <- ifelse(f.ageTab$Sig_Diff == "Yes", ifelse(f.ageTab$MedAge_c < f.ageTab$MedAge_s,"Younger","Older"),"")
      
      m.ageTab <- as.matrix(f.ageTab[,c(1:5,7,8)])
    } else {
      f.ageTab <- left_join(f.placeAge, f.ctyAge, by = "key")
      #Calculating significant differences
      f.ageTab$MedAge_p <- as.numeric(f.ageTab$MedAge_p)
      f.ageTab$MOE_p <- as.numeric(f.ageTab$MOE_p)
      f.ageTab$MedAge_c <- as.numeric(f.ageTab$MedAge_c)
      f.ageTab$MOE_c <- as.numeric(f.ageTab$MOE_c)
      
      f.ageTab$ZScore <- (abs(f.ageTab$MedAge_p - f.ageTab$MedAge_c)/
                            sqrt((f.ageTab$MOE_p^2) + (f.ageTab$MOE_c^2)))
      f.ageTab$Sig_Diff <- ifelse(f.ageTab$ZScore < 1,"No","Yes")
      f.ageTab$Difference <- ifelse(f.ageTab$Sig_Diff == "Yes", ifelse(f.ageTab$MedAge_p < f.ageTab$MedAge_c,"Younger","Older"),"")
      
      m.ageTab <- as.matrix(f.ageTab[,c(1:5,7,8)])
    }
  

  #Column Names
  if(nchar(placename) == 0)  {
    names_spaced <- c("Gender","Median Age","MOE","Median Age","MOE","Signficant Difference?","Difference from State")
  } else {
    names_spaced <- c("Gender","Median Age","MOE","Median Age","MOE","Signficant Difference?","Difference from County")
  }
  #Span Header
  if(nchar(placefips) == 0) {
    # create vector with colspan
    tblHead <- c(" " = 1, ctyname = 2, "Colorado"  = 2, " " = 2)
    # set vector names
    names(tblHead) <- c(" ", ctyname,"Colorado"," ")
  } else {
    # create vector with colspan
    tblHead <- c(" " = 1, placename = 2, ctyname  = 2, " " = 2)
    # set vector names
    names(tblHead) <- c(" ", placename,ctyname," ")
  }
  
  

  tabHTML <- m.ageTab %>%
    kable(format='html', table.attr='class="cleanTable"',
          digits=1,
          row.names=FALSE,
          align='lrrrrrr',
          caption="Median Age by Gender  Comparison",
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 10) %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width = "0.5in") %>%
    column_spec(2, width = "0.75in") %>%
    column_spec(3, width = "0.75in") %>%
    column_spec(4, width = "0.75in") %>%
    column_spec(5, width = "0.75in") %>%
    column_spec(6, width = "1in") %>%
    column_spec(7, width = "0.75in") %>%
    add_header_above(header=tblHead) %>%
    kableExtra::footnote(captionSrc("ACS",ACS))
  
  # Building FlexTable
  f.Flex <- as.data.frame(m.ageTab)
  names(f.Flex) <- c("Gender","Age.1","MOE.1","Age.2","MOE.2","SD","Diff")
  FlexOut <- regulartable(f.Flex)
  FlexOut <- set_header_labels(FlexOut, Gender = "Gender", 
                               Age.1 = "Estimate", MOE.1 = "Margin of Error", 
                               Age.2 = "Estimate", MOE.2 = "Margin of Error",
                               SD="Signficant Difference?", Diff="Direction of Difference")
  if(nchar(placefips) == 0) {
    FlexOut <- add_header(FlexOut,Gender = "", Age.1= ctyname, MOE.1="",
                             Age.2 = "Colorado", MOE.2 = "",
                             SD = "", Diff = "",top=TRUE)
  } else {
    FlexOut <- add_header(FlexOut,Gender = "", Age.1 = placename, MOE.1 = "",
                          Age.2 = ctyname, MOE.2 ="",
                          SD = "", Diff = "",top=TRUE)
  }
  FlexOut <- add_header(FlexOut,Gender ="Median Age by Gender", top=TRUE)
  FlexOut <- add_footer(FlexOut,Gender=captionSrc("ACS",ACS))
  FlexOut <- merge_at(FlexOut,i=1,j = 1:7,part="header")
  FlexOut <- merge_at(FlexOut,i=2,j = 2:3, part="header")
  FlexOut <- merge_at(FlexOut,i=2,j = 4:5, part="header")
  FlexOut <- merge_at(FlexOut,i=1, j = 1:7, part = "footer")
  FlexOut <- align(FlexOut,i=1,j = 1, align="left",part="header")
  FlexOut <- align(FlexOut,i=2:3,j = 1:7, align="center",part="header")     
  FlexOut <- align(FlexOut,i=1, align="left",part="footer")
  FlexOut <- align(FlexOut, j=1, align="left", part="body")
  FlexOut <- autofit(FlexOut)
  FlexOut <- width(FlexOut, j = 1:5, width = 1)
  

  #preparing Output data
  f.ageTab2 <- f.ageTab[,c(1:5,7,8)]
  

  
if(nchar(placename) == 0)  {
  names(f.ageTab2) <- c("Gender", paste0("Median Age: ",ctyname), paste0("MOE: ",ctyname),
                        "Median Age: Colorado", "MOE: Colorado", "Sig. Difference","Difference from State")
} else {
  names(f.ageTab2) <- c("Gender", paste0("Median Age: ",placename), paste0("MOE: ",placename),
                        paste0("Median Age: ",ctyname), paste0("MOE: ",ctyname), "Sig. Difference","Difference from County")
}
  



  tabLATEX <- m.ageTab %>% kable(digits=1,
                  row.names=FALSE,
                  col.names = names_spaced,
                  align='lrrrrrr',
                  caption="Median Age by Gender Comparison",  
                  format="latex", booktabs=TRUE)  %>%
    kable_styling(latex_options="HOLD_position",font_size=10)  %>%
    row_spec(0, align="c") %>%
    column_spec(1, width = "0.5in") %>%
    column_spec(2, width = "0.5in") %>%
    column_spec(3, width = "0.5in") %>%
    column_spec(4, width = "0.5in") %>%
    column_spec(5, width = "0.5in") %>%
    column_spec(6, width = "0.75in") %>%
    column_spec(7, width = "0.5in") %>%
    add_header_above(header=tblHead) %>%
    kableExtra::footnote(captionSrc("ACS",ACS),threeparttable = T)

  PlAge <- as.numeric(m.ageTab[3,2])
  StAge <- as.numeric(m.ageTab[3,4])
  dval <- ifelse(PlAge > StAge, PlAge - StAge,
          ifelse(PlAge == StAge,0,StAge - PlAge))
 
 
  sDiffa <- m.ageTab[1,6]
  diffDira <- tolower(m.ageTab[1,7])
  sDiffm <- m.ageTab[2,6]
  diffDirm <- tolower(m.ageTab[2,7])
  sDifff <- m.ageTab[3,6]
  diffDirf <- tolower(m.ageTab[3,7])

  if(nchar(placefips) == 0) {
    if(sDiffa == "Yes" & sDiffm == "Yes" && sDifff == "Yes") {
      medText <- paste0(" The median age of ",ctyname," is ",round(dval,digits=1)," years ",diffDira," than the state.")
      medText <- paste0(medText, " Women in ",ctyname," are significantly ",diffDirf, " than women in the state and men in ",ctyname," are significantly ",diffDirm," than men in the state.")
    }
    if(sDiffa == "Yes" & sDiffm == "No" && sDifff == "Yes") {
      medText <- paste0(" The median age of ",ctyname," is ",round(dval,digits=1)," years ",diffDira," than the state.")
      medText <- paste0(medText, " Women in ",ctyname," are significantly ", diffDirf," than women in the state but men are not sigificnatly older or younger than men in the state.")
    }
    if(sDiffa == "Yes" & sDiffm == "Yes" && sDifff == "No") {
      medText <- paste0(" The median age of ",ctyname," is ",round(dval,digits=1)," years ",diffDira," than the state.")
      medText <- paste0(medText, " Women are not significantly older or younger than women in the state but men in ",ctyname," are significantly ",diffDirm," than men in the state.")
    }
    if(sDiffa == "No" & sDiffm == "No" && sDifff == "Yes") {
      medText <- paste0(" The median age of ",ctyname," is not significantly different from the state.")
      medText <- paste0(medText, " Women in ",ctyname," are significantly ", diffDirf," than women in the state but men are not sigificnatly older or younger than men in the state.")
    }
    if(sDiffa == "No" & sDiffm == "Yes" && sDifff == "No") {
      medText <- paste0(" The median age of ",ctyname," is not significantly different from the state.")
      medText <- paste0(medText, " Women are not significantly older or younger than women in the state but men in ",ctyname," are significantly ",diffDirm," than men in the state.")
    }
    if(sDiffa == "Yes" & sDiffm == "No" && sDifff == "No") {
      medText <- paste0(" The median age of ",placename," is  significantly different than the population of ", ctyname,".")
      medText <- paste0(medText, " Women in ",ctyname," are not significantly older or younger than women in the state. Men in ",ctyname," are not significantly older or younger than men in the state.")
    }
    if(sDiffa == "No" & sDiffm == "No" && sDifff == "No") {
      medText <- paste0(" The median age of ",ctyname," is not significantly different than population of the state.")
    }
      } else {
    if(sDiffa == "Yes" & sDiffm == "Yes" && sDifff == "Yes") {
      medText <- paste0(" The median age of ",placename," is ",round(dval,digits=1)," years ",diffDira," than ", ctyname,".")
      medText <- paste0(medText, " Women in ",placename," are significantly ",diffDirf, " than women in ",ctyname," and men in ",placename," are significantly ",diffDirm," than men in ",ctyname,".")
    }
    
    if(sDiffa == "Yes" & sDiffm == "No" && sDifff == "Yes") {
      medText <- paste0(" The median age of ",placename," is ",round(dval,digits=1)," years ",diffDira," than ", ctyname,".")
      medText <- paste0(medText, " Women in ",placename," are significantly ", diffDirf," than women in ", ctyname," but men are not significantly older or younger than men in the county.")
    }
    
    if(sDiffa == "Yes" & sDiffm == "Yes" && sDifff == "No") {
      medText <- paste0("  The median age of ",placename," is ",round(dval,digits=1)," years ",diffDira," than the county.")
      medText <- paste0(medText, " Women are not significantly older or younger than women in ", ctyname," but men in ",placename," are significantly ",diffDirm," than men in the county.")
    }
    if(sDiffa == "No" & sDiffm == "No" && sDifff == "Yes") {
      medText <- paste0(" The median age of ",placename," is not significantly different than the population", ctyname,".")
      medText <- paste0(medText, " Women in ",placename," are significantly ", diffDirf," than women in ", ctyname," but men are not significantly older or younger than men in the county.")
    }
    
    if(sDiffa == "No" & sDiffm == "Yes" && sDifff == "No") {
      medText <- paste0(" The median age of ",placename," is not significantly different than the population of ", ctyname,".")
      medText <- paste0(medText, " Women are not significantly older or younger than women in ", ctyname," but men in ",placename," are significantly ",diffDirm," than men in the county.")
    }
    if(sDiffa == "Yes" & sDiffm == "No" && sDifff == "No") {
          medText <- paste0(" The median age of ",placename," is  significantly different than the population of ", ctyname,".")
          medText <- paste0(medText, " Women are not significantly older or younger than women in ", ctyname,". Men are not significantly older or younger than men in ", ctyname,".")
        }
        
    if(sDiffa == "No" & sDiffm == "No" && sDifff == "No") {
      medText <- paste0(" The median age of ",placename," is not significantly different than the population of ",ctyname,".")
    }
      }
  

  
  outList <- list("plot" = pyramid, "Htable" = tabHTML, "Ltable" = tabLATEX, "text" = medText, "FlexTable" = FlexOut, "data" = sexbyagedf, "data2" = f.Flex)
  return(outList)

}
