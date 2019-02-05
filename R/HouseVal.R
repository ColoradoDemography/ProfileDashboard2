#' HouseVal Table Showing the Median House value for Owner-Occupied housing,
#'     the Median Gross Rent, and Median Costs as a Percentage of Income for
#'     Owners and Renters for a place and the State of Colorado
#'     
#'     Restructured 1/28/2019 to make direct calls to ACS data tables...
#'     OO House Value:  b25077
#'     OO Income percentages b25095
#'     Median gross rent b25064
#'     Rental Income Percentages b25074
#'
#' @param listID the list containing place id and Place names
#' @param ACS Specifies the ACS data set to be used, reads curACS from Shiny program
#' @return kable formatted  table and data file
#' @export
#'

HouseVal <- function(listID, ACS, state="08"){
  
  # Collecting place ids from  idList, setting default values
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
 # if(listID$PlFilter == "T") {
 #   placefips <- ""
 #   placename <- ""
 # }

  #County API Calls
  ctyCode <- paste0("1",state , ctyfips)
  f.ACSCTY077 <- codemog_api(data="b25077",db=ACS,geonum=ctyCode,meta="no") # Value
  f.ACSCTY077moe <- codemog_api(data="b25077_moe",db=ACS,geonum=ctyCode,meta="no") # Margin of Error
 
  f.ACSCTY095 <- codemog_api(data="b25095",db=ACS,geonum=ctyCode,meta="no") # Value
  f.ACSCTY095moe <- codemog_api(data="b25095_moe",db=ACS,geonum=ctyCode,meta="no") # Margin of Error
 
  f.ACSCTY064 <- codemog_api(data="b25064",db=ACS,geonum=ctyCode,meta="no") # Value
  f.ACSCTY064moe <- codemog_api(data="b25064_moe",db=ACS,geonum=ctyCode,meta="no") # Margin of Error
  
  f.ACSCTY074 <- codemog_api(data="b25074",db=ACS,geonum=ctyCode,meta="no") # Value
  f.ACSCTY074moe <- codemog_api(data="b25074_moe",db=ACS,geonum=ctyCode,meta="no") # Margin of Error

  f.ACSCTY <- list(f.ACSCTY077,f.ACSCTY077moe, f.ACSCTY095, f.ACSCTY095moe, 
                    f.ACSCTY064, f.ACSCTY064moe, f.ACSCTY074, f.ACSCTY074moe) %>% reduce(left_join, by = "geoname") %>%
                    select(c(1, 7, 8, 15, 22:94, 101:173, 180, 187, 194:257, 264:327))
  
  
  if(nchar(placefips) == 0) {
    stCode <- paste0("1",state)
    # Raw State data 
    f.ACSST077 <- codemog_api(data="b25077",db=ACS,geonum=stCode,meta="no") # Value
    f.ACSST077moe <- codemog_api(data="b25077_moe",db=ACS,geonum=stCode,meta="no") # Margin of Error
    
    f.ACSST095 <- codemog_api(data="b25095",db=ACS,geonum=stCode,meta="no") # Value
    f.ACSST095moe <- codemog_api(data="b25095_moe",db=ACS,geonum=stCode,meta="no") # Margin of Error
    
    f.ACSST064 <- codemog_api(data="b25064",db=ACS,geonum=stCode,meta="no") # Value
    f.ACSST064moe <- codemog_api(data="b25064_moe",db=ACS,geonum=stCode,meta="no") # Margin of Error
    
    f.ACSST074 <- codemog_api(data="b25074",db=ACS,geonum=stCode,meta="no") # Value
    f.ACSST074moe <- codemog_api(data="b25074_moe",db=ACS,geonum=stCode,meta="no") # Margin of Error
    
    f.ACSST <- list(f.ACSST077,f.ACSST077moe, f.ACSST095, f.ACSST095moe, 
                     f.ACSST064, f.ACSST064moe, f.ACSST074, f.ACSST074moe) %>% reduce(left_join, by = "geoname") %>%
                     select(c(1, 7, 8, 15, 22:94, 101:173, 180, 187, 194:257, 264:327))
  } else {
    # Raw Place data 
    plCode <- paste0("1",state , placefips)
    f.ACSPL077 <- codemog_api(data="b25077",db=ACS,geonum=plCode,meta="no") # Value
    f.ACSPL077moe <- codemog_api(data="b25077_moe",db=ACS,geonum=plCode,meta="no") # Margin of Error
    
    f.ACSPL095 <- codemog_api(data="b25095",db=ACS,geonum=plCode,meta="no") # Value
    f.ACSPL095moe <- codemog_api(data="b25095_moe",db=ACS,geonum=plCode,meta="no") # Margin of Error
    
    f.ACSPL064 <- codemog_api(data="b25064",db=ACS,geonum=plCode,meta="no") # Value
    f.ACSPL064moe <- codemog_api(data="b25064_moe",db=ACS,geonum=plCode,meta="no") # Margin of Error
    
    f.ACSPL074 <- codemog_api(data="b25074",db=ACS,geonum=plCode,meta="no") # Value
    f.ACSPL074moe <- codemog_api(data="b25074_moe",db=ACS,geonum=plCode,meta="no") # Margin of Error
    
    f.ACSPL <- list(f.ACSPL077,f.ACSPL077moe, f.ACSPL095, f.ACSPL095moe, 
                    f.ACSPL064, f.ACSPL064moe, f.ACSPL074, f.ACSPL074moe) %>% reduce(left_join, by = "geoname") %>%
                    select(c(1, 7, 8, 15, 22:94, 101:173, 180, 187, 194:257, 264:327))
  }
 
  #Accounting for Missing value
  
  f.ACSCTY[f.ACSCTY == "-9999"] <- NA
  if(nchar(placefips) == 0) {
    f.ACSST[f.ACSST == "-9999"] <- NA
  }  else {
    f.ACSPL[f.ACSPL == "-9999"] <- NA
  }
  
  
  
  # Assemble County Data
  f.ACSCTY[,3:280] <- sapply(f.ACSCTY[,3:280],as.numeric)
  
  f.ACSCTY <- f.ACSCTY %>% mutate(
    #Values
    Med_val = b25077001,
    OO_3049 = b25095006 + b25095007 + b25095008 + 
      b25095015 + b25095016 + b25095017 + 
      b25095024 + b25095025 + b25095026 + 
      b25095033 + b25095034 + b25095035 + 
      b25095042 + b25095043 + b25095044 + 
      b25095051 + b25095052 + b25095053 + 
      b25095060 + b25095061 + b25095062 + 
      b25095069 + b25095070 + b25095071,
    OO_50 = b25095009 + b25095018 + b25095027 + b25095036 + b25095045 + b25095054 + b25095063 + b25095072,
    PROP_OO_3049 = OO_3049/b25095001,
    PCT_OO_3049 = percent(PROP_OO_3049*100),
    PROP_OO_50 = OO_50/b25095001,
    PCT_OO_50 = percent(PROP_OO_50*100),
    Med_Rent = b25064001,
    RT_3049 = b25074006 + b25074007 + b25074008 + 
      b25074015 + b25074016 + b25074017 + 
      b25074024 + b25074025 + b25074026 + 
      b25074033 + b25074034 + b25074035 + 
      b25074042 + b25074043 + b25074044 + 
      b25074051 + b25074052 + b25074053 + 
      b25074060 + b25074061 + b25074062,
    RT_50 = b25074009 + b25074018 + b25074027 + b25074036 + b25074045 + b25074054 + b25074063,
    PROP_RT_3049 = RT_3049/b25074001,
    PCT_RT_3049 = percent(PROP_RT_3049*100),
    PROP_RT_50 = RT_50/b25074001,
    PCT_RT_50 = percent(PROP_RT_50*100),
    #MOE
    Med_val_moe = b25077_moe001,
    OO_3049_moe = sqrt(b25095_moe006^2 + b25095_moe007^2 + b25095_moe008^2 + 
                         b25095_moe015^2 + b25095_moe016^2 + b25095_moe017^2 + 
                         b25095_moe024^2 + b25095_moe025^2 + b25095_moe026^2 + 
                         b25095_moe033^2 + b25095_moe034^2 + b25095_moe035^2 + 
                         b25095_moe042^2 + b25095_moe043^2 + b25095_moe044^2 + 
                         b25095_moe051^2 + b25095_moe052^2 + b25095_moe053^2 + 
                         b25095_moe060^2 + b25095_moe061^2 + b25095_moe062^2 + 
                         b25095_moe069^2 + b25095_moe070^2 + b25095_moe071^2),
    OO_50_moe = sqrt(b25095_moe009^2 + b25095_moe018^2 + b25095_moe027^2 + b25095_moe036^2 + 
                       b25095_moe045^2 + b25095_moe054^2 + b25095_moe063^2 + b25095_moe072^2),
    PROP_OO_3049_moe = OO_3049_moe/b25095001,
    PCT_OO_3049_moe = percent(PROP_OO_3049_moe*100),
    PROP_OO_50_moe = OO_50_moe/b25095001,
    PCT_OO_50_moe = percent(PROP_OO_50_moe*100),
    Med_Rent_moe = b25064_moe001,
    RT_3049_moe = sqrt(b25074_moe006^2 + b25074_moe007^2 + b25074_moe008^2 + 
                         b25074_moe015^2 + b25074_moe016^2 + b25074_moe017^2 + 
                         b25074_moe024^2 + b25074_moe025^2 + b25074_moe026^2 + 
                         b25074_moe033^2 + b25074_moe034^2 + b25074_moe035^2 + 
                         b25074_moe042^2 + b25074_moe043^2 + b25074_moe044^2 + 
                         b25074_moe051^2 + b25074_moe052^2 + b25074_moe053^2 + 
                         b25074_moe060^2 + b25074_moe061^2 + b25074_moe062^2),
    RT_50_moe = sqrt(b25074_moe009^2 + b25074_moe018^2 + b25074_moe027^2 + 
                       b25074_moe036^2 + b25074_moe045^2 + b25074_moe054^2 + b25074_moe063^2),
    PROP_RT_3049_moe = RT_3049_moe/b25074001,
    PCT_RT_3049_moe = percent(PROP_RT_3049_moe*100),
    PROP_RT_50_moe = RT_50_moe/b25074001,
    PCT_RT_50_moe = percent(PROP_RT_50_moe*100)
  ) 
  
  
  f.ACSCTY_val <- f.ACSCTY[,c(2,281:294)]
  f.ACSCTY_moe <- f.ACSCTY[,c(2,295:308)]
  
  f.ACSCTY_valL <- as.data.frame(t(f.ACSCTY_val))
  names(f.ACSCTY_valL)[1] <- "CTY_VAL"
  f.ACSCTY_valL <- rownames_to_column( f.ACSCTY_valL,"value")
  
  f.ACSCTY_moeL <- as.data.frame(t(f.ACSCTY_moe))
  names(f.ACSCTY_moeL)[1] <- "CTY_MOE"
  f.ACSCTY_moeL <- rownames_to_column( f.ACSCTY_moeL,"value")
  f.ACSCTY_moeL$value <- gsub("_moe","",f.ACSCTY_moeL$value)
  
  f.ACSCTY_Fin <- left_join(f.ACSCTY_valL,f.ACSCTY_moeL, by="value")
  
  
  if(nchar(placefips) == 0) {
    # Assemble State Data
    f.ACSST[,3:280] <- sapply(f.ACSST[,3:280],as.numeric)
    
    f.ACSST <- f.ACSST %>% mutate(
      #Values
      Med_val = b25077001,
      OO_3049 = b25095006 + b25095007 + b25095008 + 
        b25095015 + b25095016 + b25095017 + 
        b25095024 + b25095025 + b25095026 + 
        b25095033 + b25095034 + b25095035 + 
        b25095042 + b25095043 + b25095044 + 
        b25095051 + b25095052 + b25095053 + 
        b25095060 + b25095061 + b25095062 + 
        b25095069 + b25095070 + b25095071,
      OO_50 = b25095009 + b25095018 + b25095027 + b25095036 + b25095045 + b25095054 + b25095063 + b25095072,
      PROP_OO_3049 = OO_3049/b25095001,
      PCT_OO_3049 = percent(PROP_OO_3049*100),
      PROP_OO_50 = OO_50/b25095001,
      PCT_OO_50 = percent(PROP_OO_50*100),
      Med_Rent = b25064001,
      RT_3049 = b25074006 + b25074007 + b25074008 + 
        b25074015 + b25074016 + b25074017 + 
        b25074024 + b25074025 + b25074026 + 
        b25074033 + b25074034 + b25074035 + 
        b25074042 + b25074043 + b25074044 + 
        b25074051 + b25074052 + b25074053 + 
        b25074060 + b25074061 + b25074062,
      RT_50 = b25074009 + b25074018 + b25074027 + b25074036 + b25074045 + b25074054 + b25074063,
      PROP_RT_3049 = RT_3049/b25074001,
      PCT_RT_3049 = percent(PROP_RT_3049*100),
      PROP_RT_50 = RT_50/b25074001,
      PCT_RT_50 = percent(PROP_RT_50*100),
      #MOE
      Med_val_moe = b25077_moe001,
      OO_3049_moe = sqrt(b25095_moe006^2 + b25095_moe007^2 + b25095_moe008^2 + 
                           b25095_moe015^2 + b25095_moe016^2 + b25095_moe017^2 + 
                           b25095_moe024^2 + b25095_moe025^2 + b25095_moe026^2 + 
                           b25095_moe033^2 + b25095_moe034^2 + b25095_moe035^2 + 
                           b25095_moe042^2 + b25095_moe043^2 + b25095_moe044^2 + 
                           b25095_moe051^2 + b25095_moe052^2 + b25095_moe053^2 + 
                           b25095_moe060^2 + b25095_moe061^2 + b25095_moe062^2 + 
                           b25095_moe069^2 + b25095_moe070^2 + b25095_moe071^2),
      OO_50_moe = sqrt(b25095_moe009^2 + b25095_moe018^2 + b25095_moe027^2 + b25095_moe036^2 + 
                         b25095_moe045^2 + b25095_moe054^2 + b25095_moe063^2 + b25095_moe072^2),
      PROP_OO_3049_moe = OO_3049_moe/b25095001,
      PCT_OO_3049_moe = percent(PROP_OO_3049_moe*100),
      PROP_OO_50_moe = OO_50_moe/b25095001,
      PCT_OO_50_moe = percent(PROP_OO_50_moe*100),
      Med_Rent_moe = b25064_moe001,
      RT_3049_moe = sqrt(b25074_moe006^2 + b25074_moe007^2 + b25074_moe008^2 + 
                           b25074_moe015^2 + b25074_moe016^2 + b25074_moe017^2 + 
                           b25074_moe024^2 + b25074_moe025^2 + b25074_moe026^2 + 
                           b25074_moe033^2 + b25074_moe034^2 + b25074_moe035^2 + 
                           b25074_moe042^2 + b25074_moe043^2 + b25074_moe044^2 + 
                           b25074_moe051^2 + b25074_moe052^2 + b25074_moe053^2 + 
                           b25074_moe060^2 + b25074_moe061^2 + b25074_moe062^2),
      RT_50_moe = sqrt(b25074_moe009^2 + b25074_moe018^2 + b25074_moe027^2 + 
                         b25074_moe036^2 + b25074_moe045^2 + b25074_moe054^2 + b25074_moe063^2),
      PROP_RT_3049_moe = RT_3049_moe/b25074001,
      PCT_RT_3049_moe = percent(PROP_RT_3049_moe*100),
      PROP_RT_50_moe = RT_50_moe/b25074001,
      PCT_RT_50_moe = percent(PROP_RT_50_moe*100)
    )
    
    
    f.ACSST_val <- f.ACSST[,c(2,281:294)]
    f.ACSST_moe <- f.ACSST[,c(2,295:308)]
    
    f.ACSST_valL <- as.data.frame(t(f.ACSST_val))
    names(f.ACSST_valL)[1] <- "ST_VAL"
    f.ACSST_valL <- rownames_to_column( f.ACSST_valL,"value")
    
    f.ACSST_moeL <- as.data.frame(t(f.ACSST_moe))
    names(f.ACSST_moeL)[1] <- "ST_MOE"
    f.ACSST_moeL <- rownames_to_column( f.ACSST_moeL,"value")
    f.ACSST_moeL$value <- gsub("_moe","",f.ACSST_moeL$value)
    f.ACSST_Fin <- left_join(f.ACSST_valL,f.ACSST_moeL, by="value")
  } else {
    # Assemble Place Data
    #Raw values
    f.ACSPL[,3:280] <- sapply(f.ACSPL[,3:280],as.numeric)
    
    f.ACSPL <- f.ACSPL %>% mutate(
      #Values
      #Values
      Med_val = b25077001,
      OO_3049 = b25095006 + b25095007 + b25095008 + 
        b25095015 + b25095016 + b25095017 + 
        b25095024 + b25095025 + b25095026 + 
        b25095033 + b25095034 + b25095035 + 
        b25095042 + b25095043 + b25095044 + 
        b25095051 + b25095052 + b25095053 + 
        b25095060 + b25095061 + b25095062 + 
        b25095069 + b25095070 + b25095071,
      OO_50 = b25095009 + b25095018 + b25095027 + b25095036 + b25095045 + b25095054 + b25095063 + b25095072,
      PROP_OO_3049 = OO_3049/b25095001,
      PCT_OO_3049 = percent(PROP_OO_3049*100),
      PROP_OO_50 = OO_50/b25095001,
      PCT_OO_50 = percent(PROP_OO_50*100),
      Med_Rent = b25064001,
      RT_3049 = b25074006 + b25074007 + b25074008 + 
        b25074015 + b25074016 + b25074017 + 
        b25074024 + b25074025 + b25074026 + 
        b25074033 + b25074034 + b25074035 + 
        b25074042 + b25074043 + b25074044 + 
        b25074051 + b25074052 + b25074053 + 
        b25074060 + b25074061 + b25074062,
      RT_50 = b25074009 + b25074018 + b25074027 + b25074036 + b25074045 + b25074054 + b25074063,
      PROP_RT_3049 = RT_3049/b25074001,
      PCT_RT_3049 = percent(PROP_RT_3049*100),
      PROP_RT_50 = RT_50/b25074001,
      PCT_RT_50 = percent(PROP_RT_50*100),
      #MOE
      Med_val_moe = b25077_moe001,
      OO_3049_moe = sqrt(b25095_moe006^2 + b25095_moe007^2 + b25095_moe008^2 + 
                           b25095_moe015^2 + b25095_moe016^2 + b25095_moe017^2 + 
                           b25095_moe024^2 + b25095_moe025^2 + b25095_moe026^2 + 
                           b25095_moe033^2 + b25095_moe034^2 + b25095_moe035^2 + 
                           b25095_moe042^2 + b25095_moe043^2 + b25095_moe044^2 + 
                           b25095_moe051^2 + b25095_moe052^2 + b25095_moe053^2 + 
                           b25095_moe060^2 + b25095_moe061^2 + b25095_moe062^2 + 
                           b25095_moe069^2 + b25095_moe070^2 + b25095_moe071^2),
      OO_50_moe = sqrt(b25095_moe009^2 + b25095_moe018^2 + b25095_moe027^2 + b25095_moe036^2 + 
                         b25095_moe045^2 + b25095_moe054^2 + b25095_moe063^2 + b25095_moe072^2),
      PROP_OO_3049_moe = OO_3049_moe/b25095001,
      PCT_OO_3049_moe = percent(PROP_OO_3049_moe*100),
      PROP_OO_50_moe = OO_50_moe/b25095001,
      PCT_OO_50_moe = percent(PROP_OO_50_moe*100),
      Med_Rent_moe = b25064_moe001,
      RT_3049_moe = sqrt(b25074_moe006^2 + b25074_moe007^2 + b25074_moe008^2 + 
                           b25074_moe015^2 + b25074_moe016^2 + b25074_moe017^2 + 
                           b25074_moe024^2 + b25074_moe025^2 + b25074_moe026^2 + 
                           b25074_moe033^2 + b25074_moe034^2 + b25074_moe035^2 + 
                           b25074_moe042^2 + b25074_moe043^2 + b25074_moe044^2 + 
                           b25074_moe051^2 + b25074_moe052^2 + b25074_moe053^2 + 
                           b25074_moe060^2 + b25074_moe061^2 + b25074_moe062^2),
      RT_50_moe = sqrt(b25074_moe009^2 + b25074_moe018^2 + b25074_moe027^2 + 
                         b25074_moe036^2 + b25074_moe045^2 + b25074_moe054^2 + b25074_moe063^2),
      PROP_RT_3049_moe = RT_3049_moe/b25074001,
      PCT_RT_3049_moe = percent(PROP_RT_3049_moe*100),
      PROP_RT_50_moe = RT_50_moe/b25074001,
      PCT_RT_50_moe = percent(PROP_RT_50_moe*100)
    )
    
    
    f.ACSPL_val <- f.ACSPL[,c(2,281:294)]
    f.ACSPL_moe <- f.ACSPL[,c(2,295:308)]
    
    f.ACSPL_valL <- as.data.frame(t(f.ACSPL_val))
    names(f.ACSPL_valL)[1] <- "PL_VAL"
    f.ACSPL_valL <- rownames_to_column( f.ACSPL_valL,"value")
    
    f.ACSPL_moeL <- as.data.frame(t(f.ACSPL_moe))
    names(f.ACSPL_moeL)[1] <- "PL_MOE"
    f.ACSPL_moeL <- rownames_to_column( f.ACSPL_moeL,"value")
    f.ACSPL_moeL$value <- gsub("_moe","",f.ACSPL_moeL$value)
    f.ACSPL_Fin <- left_join(f.ACSPL_valL,f.ACSPL_moeL, by="value")
  } 
  
  # Joining Fles and calculating tests
  if(nchar(placefips) == 0) {
    f.HouseVal <- left_join(f.ACSCTY_Fin,f.ACSST_Fin, by="value")
    
  } else {
    f.HouseVal <- left_join(f.ACSPL_Fin,f.ACSCTY_Fin, by="value")
  }
  
  m.HouseVal <- as.matrix(f.HouseVal)
  m.test <- matrix(nrow=15, ncol=2)
  #calculating statistical test
  for(i in c(2,5,7,9,12,14)) {
    m.test[i,1] <-abs(as.numeric(m.HouseVal[i,2]) - as.numeric(m.HouseVal[i,4]))/sqrt((as.numeric(m.HouseVal[i,3])^2) + (as.numeric(m.HouseVal[i,5])^2))
    m.test[i,2] <- ifelse(m.test[i,1] < 1,"No","Yes")
  }
  for(i in c(5,7,12,14)) {
    m.test[i+1,1] = m.test[i,1]
    m.test[i+1,2] = m.test[i,2]
  }
  m.HouseVal <- cbind(m.HouseVal,m.test)
  
  # Bulding Renter and owner-occupied matices for table 
  m.rental <- m.HouseVal[c(9,13,15),c(1:5,7)]  
  m.oocc <- m.HouseVal[c(2,6,8),c(1:5,7)] 
  
  m.rental[1,1] <- "Median Gross Rent of Rental Households (Current Dollars)"
  m.rental[2,1] <- "Percentage of Rental Households paying 30-49% of income on housing"
  m.rental[3,1] <- "Percentage of Rental Households paying 50% or more of income on housing"
  
  m.oocc[1.1] <- "Median Value of Owner-Occupied Households (Current Dollars)"
  m.oocc[2,1] <- "Percentage of Owner-Occupied Households paying 30-49% of income on housing"
  m.oocc[3,1] <- "Percentage of Owner-Occupied Households paying 50% or more of income on housing"
  
  #formatting values
  m.rental[1,2:5] <- paste0("$",formatC(as.numeric(m.rental[1,2:5]),format="f",digits=0,big.mark=","))
  m.oocc[1,2:5] <- paste0("$",formatC(as.numeric(m.oocc[1,2:5]),format="f",digits=0,big.mark=","))
  
  #Creating output data set
  m.FinTab <- rbind(m.oocc,m.rental)
  f.HouseVal_Fin <- as.data.frame(m.FinTab)
  

  if(nchar(placefips) == 0) {
    names(f.HouseVal_Fin) <- c("Variable",paste0("Value: ",ctyname), paste0("MOE: ",ctyname),
                               paste0("Value: Colorado"), paste0("MOE: Colorado"), "Siginficant Difference?")
  } else {
    names(f.HouseVal_Fin) <- c("Variable",paste0("Value: ",placename), paste0("MOE: ",placename),
                               paste0("Value: ",ctyname), paste0("MOE: ",ctyname), "Siginficant Difference?")
    
  }
  
  # Setting up table
  
  #Column Names
  names_spaced <- c("Variable","Value","MOE","Value","MOE","Sig. Diff.?")
  #Span Header
  
  # create vector with colspan
  if(nchar(placefips) == 0) {
    tblHead1 <- c(" " = 1, ctyname = 2, "Colorado" = 2, " " = 1)
    
    # set vector names
    names(tblHead1) <- c(" ", ctyname, "Colorado", " ")
  }  else {
    tblHead1 <- c(" " = 1, placename = 2, ctyname = 2, " " = 1)
    
    # set vector names
    names(tblHead1) <- c(" ", placename, ctyname, " ")
  }
  
  
      Housing_tab1 <- m.oocc %>%
      kable(format='html', table.attr='class="cleanTable"',
            row.names=FALSE,
            align='lrrrrr',
            caption="Comparative Owner-Occupied Housing Values",
            col.names = names_spaced,
            escape = FALSE)  %>%
      kable_styling(bootstrap_options = "condensed",full_width = F,font_size=11) %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "3in") %>%
      column_spec(column=2:6, width = "0.33in") %>%
      add_header_above(header=tblHead1) %>%
      footnote(captionSrc("ACS",ACS))
    
    Housing_tab2 <- m.rental %>%
      kable(format='html', table.attr='class="cleanTable"',
            row.names=FALSE,
            align='lrrrrr',
            caption="Comparative Rental Housing Values",
            col.names = names_spaced,
            escape = FALSE)  %>%
      kable_styling(bootstrap_options = "condensed",full_width = F,font_size=11) %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "3in") %>%
      column_spec(2:6, width = "0.33in") %>%
      add_header_above(header=tblHead1) %>%
      footnote(captionSrc("ACS",ACS))
    
    #Building FlexTables
    # Owner-Occupied
    OO <- data.frame(m.oocc)
    names(OO) <- c("V1","V2","V3","V4","V5","V6")
    FTOO <- regulartable(OO)
    FTOO <- set_header_labels(FTOO, V1 = "Variable", 
                                 V2="Value", V3="Margin of Error",
                                 V4="Value",V5="Margin of Error",
                                 V6= "Significant Difference?"
    )
    
    if(nchar(placefips) == 0) {
      FTOO <- add_header(FTOO,V2=ctyname,V4="Colorado",top=TRUE)
    } else {
      FTOO <- add_header(FTOO,V2=placename,V4=ctyname,top=TRUE)
    }
    
    FTOO <- add_header(FTOO,V1="Comparative Owner-Occupied Housing Values",top=TRUE)
    FTOO <- add_footer(FTOO,V1=captionSrc("ACS",ACS))
    FTOO <- merge_at(FTOO,i=1, j = 1:6, part = "header")
    FTOO <- merge_at(FTOO,i=2,j=2:3,part="header")
    FTOO <- merge_at(FTOO,i=2,j=4:5,part="header")
    FTOO <- merge_at(FTOO,i=1, j = 1:6, part = "footer")
    FTOO <- align(FTOO,i=1, j=1, align="left",part="header")
    FTOO <- align(FTOO,i=2:3, j=2:6, align="center",part="header")
    FTOO <- align(FTOO,i=1, align="left",part="footer")
    FTOO <- align(FTOO, j=1, align="left", part="body")
    FTOO <- autofit(FTOO)
    FTOO <- width(FTOO,j=1, width=3)
    FTOO <- width(FTOO,j=2:6, width=1)
    
    # Rental
    RT <- data.frame(m.rental)
    names(RT) <- c("V1","V2","V3","V4","V5","V6")
    FTRT <- regulartable(RT)
    FTRT <- set_header_labels(FTRT, V1 = "Variable", 
                              V2="Value", V3="Margin of Error",
                              V4="Value",V5="Margin of Error",
                              V6= "Significant Difference?"
    )
    
    if(nchar(placefips) == 0) {
      FTRT <- add_header(FTRT,V2=ctyname,V4="Colorado",top=TRUE)
    } else {
      FTRT <- add_header(FTRT,V2=placename,V4=ctyname,top=TRUE)
    }
    
    FTRT <- add_header(FTRT,V1="Comparative Rental Housing Values",top=TRUE)
    FTRT <- add_footer(FTRT,V1=captionSrc("ACS",ACS))
    FTRT <- merge_at(FTRT,i=1, j = 1:6, part = "header")
    FTRT <- merge_at(FTRT,i=2,j=2:3,part="header")
    FTRT <- merge_at(FTRT,i=2,j=4:5,part="header")
    FTRT <- merge_at(FTRT,i=1, j = 1:6, part = "footer")
    FTRT <- align(FTRT,i=1, j=1, align="left",part="header")
    FTRT <- align(FTRT,i=2:3, j=2:6, align="center",part="header")
    FTRT <- align(FTRT,i=1, align="left",part="footer")
    FTRT <- align(FTRT, j=1, align="left", part="body")
    FTRT <- autofit(FTRT)
    FTRT <- width(FTRT,j=1, width=3)
    FTRT <- width(FTRT,j=2:6, width=1)
    
    Housing_tab3 <-  kable(m.oocc,
                           col.names = names_spaced,
                           align=c("lrrrrr"),
                           caption="Comparison of Owner-Occupied Housing Values", row.names=FALSE,
                           format="latex", booktabs=TRUE)  %>%
      kable_styling(latex_options="HOLD_position",font_size=10)  %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "3in") %>%
      column_spec(2:6, width = "0.33in") %>%
      add_header_above(header=tblHead1) %>%
      footnote(captionSrc("ACS",ACS))
    
    Housing_tab4 <-  kable(m.rental,
                           col.names = names_spaced,
                           align=c("lrrrrr"),
                           caption="Comparison of Rental Housing Values", row.names=FALSE,
                           format="latex", booktabs=TRUE)  %>%
      kable_styling(latex_options="HOLD_position",font_size=10) %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "3in") %>%
      column_spec(2:6, width = "0.33in") %>%
      add_header_above(header=tblHead1) %>%
      footnote(captionSrc("ACS",ACS))
    
   
    
    outList <- list("HtableOO" = Housing_tab1, "HtableRT" = Housing_tab2, "data" = f.HouseVal_Fin,
                    "FlexTableOO" = FTOO,"FlexTableRT" = FTRT, "LtableOO" = Housing_tab3, "LtableRT" = Housing_tab4)
    return(outList)
  }

