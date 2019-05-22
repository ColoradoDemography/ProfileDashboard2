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
    Med_val = paste0("$",formatC(as.numeric(b25077001),format="f",digits=0,big.mark=",")),
    OO_3049 = b25095006 + b25095007 + b25095008 + 
      b25095015 + b25095016 + b25095017 + 
      b25095024 + b25095025 + b25095026 + 
      b25095033 + b25095034 + b25095035 + 
      b25095042 + b25095043 + b25095044 + 
      b25095051 + b25095052 + b25095053 + 
      b25095060 + b25095061 + b25095062 + 
      b25095069 + b25095070 + b25095071,
    OO_50 = b25095009 + b25095018 + b25095027 + b25095036 + b25095045 + b25095054 + b25095063 + b25095072,
    OO_GE30 = OO_3049 + OO_50,
    PCT_OO_GE30 = percent((OO_GE30/b25095001) * 100),
    PCT_OO_3049 = percent((OO_3049/b25095001)*100),
    PCT_OO_50 = percent((OO_50/b25095001)*100),
    Med_Rent = paste0("$",formatC(as.numeric(b25064001),format="f",digits=0,big.mark=",")),
    RT_3049 = b25074006 + b25074007 + b25074008 + 
      b25074015 + b25074016 + b25074017 + 
      b25074024 + b25074025 + b25074026 + 
      b25074033 + b25074034 + b25074035 + 
      b25074042 + b25074043 + b25074044 + 
      b25074051 + b25074052 + b25074053 + 
      b25074060 + b25074061 + b25074062,
    RT_50 = b25074009 + b25074018 + b25074027 + b25074036 + b25074045 + b25074054 + b25074063,
    RT_GE30 = RT_3049 + RT_50,
    PCT_RT_GE30 = percent((RT_GE30/b25074001)*100),
    PCT_RT_3049 = percent((RT_3049/b25074001)*100),
    PCT_RT_50 = percent((RT_50/b25074001)*100)
  ) 
  
  
  f.ACSCTY_val <- f.ACSCTY[,c(2,281:294)]
  
  
  f.ACSCTY_valL <- as.data.frame(t(f.ACSCTY_val))
  names(f.ACSCTY_valL)[1] <- "CTY_VAL"
  f.ACSCTY_valL <- rownames_to_column( f.ACSCTY_valL,"value")
  
  
  f.ACSCTY_Fin <- f.ACSCTY_valL
  
  
  if(nchar(placefips) == 0) {
    # Assemble State Data
    f.ACSST[,3:280] <- sapply(f.ACSST[,3:280],as.numeric)
    
    f.ACSST <- f.ACSST %>% mutate(
      #Values
      Med_val = paste0("$",formatC(as.numeric(b25077001),format="f",digits=0,big.mark=",")),
      OO_3049 = b25095006 + b25095007 + b25095008 + 
        b25095015 + b25095016 + b25095017 + 
        b25095024 + b25095025 + b25095026 + 
        b25095033 + b25095034 + b25095035 + 
        b25095042 + b25095043 + b25095044 + 
        b25095051 + b25095052 + b25095053 + 
        b25095060 + b25095061 + b25095062 + 
        b25095069 + b25095070 + b25095071,
      OO_50 = b25095009 + b25095018 + b25095027 + b25095036 + b25095045 + b25095054 + b25095063 + b25095072,
      OO_GE30 = OO_3049 + OO_50,
      PCT_OO_GE30 = percent((OO_GE30/b25095001) * 100),
      PCT_OO_3049 = percent((OO_3049/b25095001)*100),
      PCT_OO_50 = percent((OO_50/b25095001)*100),
      Med_Rent = paste0("$",formatC(as.numeric(b25064001),format="f",digits=0,big.mark=",")),
      RT_3049 = b25074006 + b25074007 + b25074008 + 
        b25074015 + b25074016 + b25074017 + 
        b25074024 + b25074025 + b25074026 + 
        b25074033 + b25074034 + b25074035 + 
        b25074042 + b25074043 + b25074044 + 
        b25074051 + b25074052 + b25074053 + 
        b25074060 + b25074061 + b25074062,
      RT_50 = b25074009 + b25074018 + b25074027 + b25074036 + b25074045 + b25074054 + b25074063,
      RT_GE30 = RT_3049 + RT_50,
      PCT_RT_GE30 = percent((RT_GE30/b25074001)*100),
      PCT_RT_3049 = percent((RT_3049/b25074001)*100),
      PCT_RT_50 = percent((RT_50/b25074001)*100)
    )
    
    
    f.ACSST_val <- f.ACSST[,c(2,281:294)]
    
    
    f.ACSST_valL <- as.data.frame(t(f.ACSST_val))
    names(f.ACSST_valL)[1] <- "ST_VAL"
    f.ACSST_valL <- rownames_to_column( f.ACSST_valL,"value")
    
    f.ACSST_Fin <- f.ACSST_valL
  } else {
    # Assemble Place Data
    #Raw values
    f.ACSPL[,3:280] <- sapply(f.ACSPL[,3:280],as.numeric)
    
    f.ACSPL <- f.ACSPL %>% mutate(
      #Values
      Med_val = paste0("$",formatC(as.numeric(b25077001),format="f",digits=0,big.mark=",")),
      OO_3049 = b25095006 + b25095007 + b25095008 + 
        b25095015 + b25095016 + b25095017 + 
        b25095024 + b25095025 + b25095026 + 
        b25095033 + b25095034 + b25095035 + 
        b25095042 + b25095043 + b25095044 + 
        b25095051 + b25095052 + b25095053 + 
        b25095060 + b25095061 + b25095062 + 
        b25095069 + b25095070 + b25095071,
      OO_50 = b25095009 + b25095018 + b25095027 + b25095036 + b25095045 + b25095054 + b25095063 + b25095072,
      OO_GE30 = OO_3049 + OO_50,
      PCT_OO_GE30 = percent((OO_GE30/b25095001) * 100),
      PCT_OO_3049 = percent((OO_3049/b25095001)*100),
      PCT_OO_50 = percent((OO_50/b25095001)*100),
      Med_Rent = paste0("$",formatC(as.numeric(b25064001),format="f",digits=0,big.mark=",")),
      RT_3049 = b25074006 + b25074007 + b25074008 + 
        b25074015 + b25074016 + b25074017 + 
        b25074024 + b25074025 + b25074026 + 
        b25074033 + b25074034 + b25074035 + 
        b25074042 + b25074043 + b25074044 + 
        b25074051 + b25074052 + b25074053 + 
        b25074060 + b25074061 + b25074062,
      RT_50 = b25074009 + b25074018 + b25074027 + b25074036 + b25074045 + b25074054 + b25074063,
      RT_GE30 = RT_3049 + RT_50,
      PCT_RT_GE30 = percent((RT_GE30/b25074001)*100),
      PCT_RT_3049 = percent((RT_3049/b25074001)*100),
      PCT_RT_50 = percent((RT_50/b25074001)*100)
    )
    
    
    f.ACSPL_val <- f.ACSPL[,c(2,281:294)]
    
    f.ACSPL_valL <- as.data.frame(t(f.ACSPL_val))
    names(f.ACSPL_valL)[1] <- "PL_VAL"
    f.ACSPL_valL <- rownames_to_column( f.ACSPL_valL,"value")
    
    f.ACSPL_Fin <- f.ACSPL_valL
  } 
  
  # Joining Fles 
  if(nchar(placefips) == 0) {
    f.HouseVal <- left_join(f.ACSCTY_Fin,f.ACSST_Fin, by="value")
    f.HouseVal <- f.HouseVal[c(2,6:8,9,13:15),]
  } else {
    f.HouseVal <- left_join(f.ACSPL_Fin,f.ACSCTY_Fin, by="value")
    f.HouseVal <- f.HouseVal[c(2,6:8,9,13:15),]
  }
  

  # Revising Value Names
  f.HouseVal$value <- ifelse(f.HouseVal$value == "Med_val","Median Value of Owner-Occupied Households (Current Dollars)",
                             ifelse(f.HouseVal$value == "PCT_OO_GE30","Percentage of Owner-Occupied Households paying 30% or more of income on housing",
                                    ifelse(f.HouseVal$value == "PCT_OO_3049","Percentage of Owner-Occupied Households paying 30-49% of income on housing",
                                           ifelse(f.HouseVal$value == "PCT_OO_50","Percentage of Owner-Occupied Households paying 50% or more of income on housing",     
                                                  ifelse(f.HouseVal$value == "Med_Rent","Median Gross Rent of Rental Households (Current Dollars)",
                                                         ifelse(f.HouseVal$value == "PCT_RT_GE30","Percentage of Rental Households paying 30% or more of income on housing",
                                                                ifelse(f.HouseVal$value == "PCT_RT_3049","Percentage of Rental Households paying 30-49% of income on housing",
                                                                       "Percentage of Rental Households paying 50% or more of income on housing")))))))     
  
  
  
  
  
  # Setting up table

  m.HouseVal <- as.matrix(f.HouseVal)
  
  #Column Names
  names_spaced <- c("Variable","Value","Value")
  #Span Header
  
  # create vector with colspan
  if(nchar(placefips) == 0) {
    tblHead1 <- c(" " = 1, ctyname = 1, "Colorado" = 1)
    
    # set vector names
    names(tblHead1) <- c(" ", ctyname, "Colorado")
  }  else {
    tblHead1 <- c(" " = 1, placename = 1, ctyname = 1)
    
    # set vector names
    names(tblHead1) <- c(" ", placename, ctyname)
  }
  
  
  Housing_tab1 <- m.HouseVal  %>%
    kable(format='html', table.attr='class="cleanTable"',
          row.names=FALSE,
          align='lrr',
          caption="Comparative Housing Values",
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size=11) %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width = "3in") %>%
    column_spec(column=2:3, width = "0.33in") %>%
    add_header_above(header=tblHead1) %>%
    kableExtra::footnote(captionSrc("ACS",ACS))
  
  # Building Latex Tables
  Housing_tab2 <- m.HouseVal  %>%
    kable(col.names = names_spaced,
          align=c("lrr"),
          caption="Comparative Housing Values", row.names=FALSE,
          format="latex", booktabs=TRUE)  %>%
    kable_styling(latex_options="HOLD_position",font_size=10)  %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width = "4in") %>%
    column_spec(2:3, width = "0.33in") %>%
    add_header_above(header=tblHead1) %>%
    kableExtra::footnote(captionSrc("ACS",ACS)) 
  
  #Building FlexTables
  f.HouseVal2 <- f.HouseVal
  names(f.HouseVal2) <- c("V1","V2","V3")
  
  Flextable <- regulartable(f.HouseVal2)
  Flextable <- set_header_labels(Flextable, V1 = "Variable", 
                                 V2="Value", V3="Value")
  
  if(nchar(placefips) == 0) {
    Flextable <- add_header(Flextable,V2=ctyname,V3="Colorado",top=TRUE)
  } else {
    Flextable <- add_header(Flextable,V2=placename,V3=ctyname,top=TRUE)
  }
  
  Flextable <- Flextable %>%
    add_header(V1="Comparative Housing Values",top=TRUE) %>%
    add_footer(V1=captionSrc("ACS",ACS)) %>%
    merge_at(i=1, j = 1:3, part = "header") %>%
    merge_at(i=1, j = 1:3, part = "footer") %>%
    align(i=1, j=1, align="left",part="header") %>%
    align(i=2:3, j=2:3, align="center",part="header") %>%
    align(i=1, align="left",part="footer") %>%
    align( j=1, align="left", part="body") %>%
    autofit() %>%
    width(j=1, width=4) %>%
    width(j=2:3, width=1) 
  
  if(nchar(placefips) == 0) {
    names(f.HouseVal) <- c("Value",ctyname,"Colorado")
  } else {
    names(f.HouseVal) <- c("Value",placename,ctyname)
  }
  
  
  outList <- list("Htable" = Housing_tab1, "Ltable" = Housing_tab2, 
                  "FlexTable" = Flextable, "data" = f.HouseVal)
  return(outList)
}