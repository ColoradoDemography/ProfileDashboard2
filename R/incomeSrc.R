#' incomeSrc Displays a table showinf sources of incomes
#'
#' @param level the data level from input$level
#' @param listID the list containing place id and Place names
#' @param ACS Specifies the ACS data set to be used, reads curACS from Shiny program
#' @return kable formatted  table and data file
#' @export

incomeSrc <- function(level, listID, ACS) {
  # Collecting place ids from  idList, setting default values
  # Currently Output Counties
  state <- "08"
  
  fipslist <- listID$ctyNum
  fipsname <- listID$ctyName
 
  
 #Gathering Data

  f.b19051 <- codemog_api(data="b19051",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  f.b19051m <- codemog_api(data="b19051_moe",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  
  f.b19054 <- codemog_api(data="b19054",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  f.b19054m <- codemog_api(data="b19054_moe",db=ACS,geonum=paste0("1",state , fipslist),meta="no")  
  
  f.b19055 <- codemog_api(data="b19055",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  f.b19055m <- codemog_api(data="b19055_moe",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  
  
  f.b19056 <- codemog_api(data="b19056",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  f.b19056m <- codemog_api(data="b19056_moe",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  
  
  f.b19057 <- codemog_api(data="b19057",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  f.b19057m <- codemog_api(data="b19057_moe",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  
  
  f.b19059 <- codemog_api(data="b19059",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  f.b19059m <- codemog_api(data="b19059_moe",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  
  f.Households <- list( f.b19051, f.b19051m, f.b19054, f.b19054m, 
                        f.b19055, f.b19055m, f.b19056, f.b19056m, 
                        f.b19057, f.b19057m, f.b19059, f.b19059m) %>% reduce(left_join, by = "geonum")
  
  f.Households[f.Households == -9999] <- NA
  
  f.b20003 <- codemog_api(data="b20003",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  f.b20003m <- codemog_api(data="b20003_moe",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  
  f.b19061 <- codemog_api(data="b19061",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  f.b19061m <- codemog_api(data="b19061_moe",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  
  f.b19064 <- codemog_api(data="b19064",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  f.b19064m <- codemog_api(data="b19064_moe",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  
  f.b19065 <- codemog_api(data="b19065",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  f.b19065m <- codemog_api(data="b19065_moe",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  
  f.b19066 <- codemog_api(data="b19066",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  f.b19066m <- codemog_api(data="b19066_moe",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  
  f.b19067 <- codemog_api(data="b19067",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  f.b19067m <- codemog_api(data="b19067_moe",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  
  f.b19069 <- codemog_api(data="b19069",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  f.b19069m <- codemog_api(data="b19069_moe",db=ACS,geonum=paste0("1",state , fipslist),meta="no")
  
  f.Incomes  <- list(f.b20003, f.b20003m, f.b19061, f.b19061m,
                        f.b19064, f.b19064m, f.b19065, f.b19065m,
                        f.b19066, f.b19066m, f.b19067, f.b19067m, 
                        f.b19069, f.b19069m ) %>% reduce(left_join, by = "geonum")
  
  f.Incomes[f.Incomes == -9999] <- NA
  
  #County Level Household Income Counts
  incC51 <- f.Households[c("b19051001", "b19051002", "b19051003")]
  incC54 <- f.Households[c("b19054001", "b19054002", "b19054003")]
  incC55 <- f.Households[c("b19055001", "b19055002", "b19055003")]
  incC56 <- f.Households[c("b19056001", "b19056002", "b19056003")]
  incC57 <- f.Households[c("b19057001", "b19057002", "b19057003")]
  incC59 <- f.Households[c("b19059001", "b19059002", "b19059003")]
  
  names(incC51) <- c("total", "in", "out")
  names(incC54) <- c("total", "in", "out")
  names(incC55) <- c("total", "in", "out")
  names(incC56) <- c("total", "in", "out")
  names(incC57) <- c("total", "in", "out")
  names(incC59) <- c("total", "in", "out")
  
  incCty <- do.call("rbind", list(incC51, incC54, incC55, incC56, incC57,  incC59))
  incCty$ID <- seq.int(nrow(incCty))
  incCty$type <- c("With earnings","With interest, dividends or net rental income", 
                   "With Social Security income", "With Supplemental Security Income (SSI)","With cash public assistance income", 
                   "With retirement income")
  
  # County Level MOE Counts
  incC51_moe <- f.Households[c("b19051_moe001", "b19051_moe002", "b19051_moe003")]
  incC54_moe <- f.Households[c("b19054_moe001", "b19054_moe002", "b19054_moe003")]
  incC55_moe <- f.Households[c("b19055_moe001", "b19055_moe002", "b19055_moe003")]
  incC56_moe <- f.Households[c("b19056_moe001", "b19056_moe002", "b19056_moe003")]
  incC57_moe <- f.Households[c("b19057_moe001", "b19057_moe002", "b19057_moe003")]
  incC59_moe <- f.Households[c("b19059_moe001", "b19059_moe002", "b19059_moe003")] 
  
  names(incC51_moe) <- c("total_moe", "in_moe", "out_moe")
  names(incC54_moe) <- c("total_moe", "in_moe", "out_moe")
  names(incC55_moe) <- c("total_moe", "in_moe", "out_moe")
  names(incC56_moe) <- c("total_moe", "in_moe", "out_moe")
  names(incC57_moe) <- c("total_moe", "in_moe", "out_moe")
  names(incC59_moe) <- c("total_moe", "in_moe", "out_moe")
  
  incCty_moe <- do.call("rbind", list(incC51_moe, incC54_moe, incC55_moe, incC56_moe, incC57_moe,  incC59_moe))
  incCty_moe$ID <- seq.int(nrow(incCty_moe))
  incCty_moe$type <- c("With earnings","With interest, dividends or net rental income", 
                       "With Social Security income", "With Supplemental Security Income (SSI)","With cash public assistance income", 
                       "With retirement income")
  
  ctyHH <- as.numeric(unique(incCty$total))
  ctyHH_moe <- as.numeric(unique(incCty_moe$total_moe))
  
  #combining files
  incCty2 <- incCty[,c(4,5,2)]
  incCty_moe2 <- incCty_moe[,c(4,5,2)]
  incCtyHH <- left_join(incCty2, incCty_moe2, by="ID")
  incCtyHH <- incCtyHH[,c(1:3,5)]
  names(incCtyHH) <- c("ID","type","total","total_moe")
  totRow <- data.frame(ID = 0,
                       type="All Households",
                       total=ctyHH,
                       total_moe=ctyHH_moe)
  incCtyHH <- rbind(totRow,incCtyHH)
  
  incCtyHH$pct <- percent((as.numeric(incCtyHH$total)/as.numeric(ctyHH))*100)
  incCtyHH$pct_moe <- percent((as.numeric(incCtyHH$total_moe)/as.numeric(ctyHH))*100)
  
  # County HH Income Value
  mincC61 <- f.Incomes[c("geonum","b19061001")]
  mincC64 <- f.Incomes[c("geonum","b19064001")]
  mincC65 <- f.Incomes[c("geonum","b19065001")]
  mincC66 <- f.Incomes[c("geonum","b19066001")]
  mincC67 <- f.Incomes[c("geonum","b19067001")]
  mincC69 <- f.Incomes[c("geonum","b19069001")]
  
  
  names(mincC61)[2] <- "Agg_Income"
  names(mincC64)[2] <- "Agg_Income"
  names(mincC65)[2] <- "Agg_Income"
  names(mincC66)[2] <- "Agg_Income"
  names(mincC67)[2] <- "Agg_Income"
  names(mincC69)[2] <- "Agg_Income"
  
  
  mincCty <- do.call("rbind", list(mincC61, mincC64, mincC65, mincC66, mincC67,  mincC69))
  mincCty$ID <- seq.int(nrow(mincCty))
  mincCty$type <- c("With earnings","With interest, dividends or net rental income", 
                    "With Social Security income", "With Supplemental Security Income (SSI)","With cash public assistance income", 
                    "With retirement income")
  
  mincC61_moe <- f.Incomes[c("geonum","b19061_moe001")]
  mincC64_moe <- f.Incomes[c("geonum","b19064_moe001")]
  mincC65_moe <- f.Incomes[c("geonum","b19065_moe001")]
  mincC66_moe <- f.Incomes[c("geonum","b19066_moe001")]
  mincC67_moe <- f.Incomes[c("geonum","b19067_moe001")]
  mincC69_moe <- f.Incomes[c("geonum","b19069_moe001")]
  
  
  
  names(mincC61_moe)[2] <- "Agg_Income_moe"
  names(mincC64_moe)[2] <- "Agg_Income_moe"
  names(mincC65_moe)[2] <- "Agg_Income_moe"
  names(mincC66_moe)[2] <- "Agg_Income_moe"
  names(mincC67_moe)[2] <- "Agg_Income_moe"
  names(mincC69_moe)[2] <- "Agg_Income_moe"
  
  
  mincCty_moe <- do.call("rbind", list(mincC61_moe, mincC64_moe, mincC65_moe, mincC66_moe, mincC67_moe,  mincC69_moe))
  mincCty_moe$ID <- seq.int(nrow(mincCty_moe))
  mincCty_moe$type <- c("With earnings","With interest, dividends or net rental income", 
                        "With Social Security income", "With Supplemental Security Income (SSI)","With cash public assistance income", 
                        "With retirement income")
  
  # Creating Summary values
  mincCTot <- as.numeric(f.Incomes$b20003001)
  mincCTot_moe <- as.numeric(f.Incomes$b20003_moe001)
  
  
  #combining files
  mincCty2 <- mincCty[,c(3,4,2)]
  mincCty_moe2 <- mincCty_moe[,c(3,4,2)]
  mincCtyHH <- left_join(mincCty2, mincCty_moe2, by="ID")
  mincCtyHH <- mincCtyHH[,c(1:3,5)]
  names(mincCtyHH) <- c("ID","type","Agg_Income","Agg_Income_moe")
  totRow <- data.frame(ID = 0,
                       type="All Households",
                       Agg_Income=mincCTot,
                       Agg_Income_moe=mincCTot_moe)
  mincCtyHH <- rbind(totRow,mincCtyHH)
  
  
  # Producing final File
  mincCtyF <- mincCtyHH[,c(1,3,4)]
  incCtyFin <- left_join(incCtyHH, mincCtyF, by ="ID")
  
  incCtyFin$avg_income <- paste0("$",format(round(as.numeric(incCtyFin$Agg_Income)/as.numeric(incCtyFin$total),digits=0),big.mark=","))
  incCtyFin$avg_income_moe <- paste0("$",format(round(as.numeric(incCtyFin$Agg_Income_moe)/as.numeric(incCtyFin$total),digits=0),big.mark=","))
  
  
  # Building table
  totalHH <- format(as.numeric(incCtyFin[1,3]),big.mark=",")
  totalHH_moe <- format(as.numeric(incCtyFin[1,4]),big.mark=",")
  
  m.IncFin <- as.matrix(incCtyFin)
  
  for(i in 1:nrow(m.IncFin)) {
    if(is.na(m.IncFin[i,7])){
      m.IncFin[i,9] = ""
      m.IncFin[i,10] = ""
    }
  }
  m.IncFin <- m.IncFin[,c(2,5,6,9,10)]
  
  m.IncFin[1,2] <- totalHH
  m.IncFin[1,3] <- totalHH_moe
  
  
  
  # table Heading
  tblHead1 <- c(fipsname = 5)
  # set vector names
  names(tblHead1) <- c(fipsname)
  
  tblHead2 <- c(" " = 1,"Total Households" = 2,"Mean Income" = 2)
  names(tblHead2) <- c(" ","Total Households","Mean Income")
  
  names_spaced <- c("Income Source","Estimate","MOE", "Estimate","MOE")
  
 
    inc_tabH <- m.IncFin %>%
      kable(format='html', table.attr='class="cleanTable"',
            digits=1,
            row.names=FALSE,
            align='lrrrr',
            caption="Household Income Source(s)",
            col.names = names_spaced,
            escape = FALSE)  %>%
      kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 12) %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width="3in") %>%
      column_spec(2, width="0.75in") %>%
      column_spec(3, width="0.75in") %>%
      column_spec(4, width="0.75in") %>%
      column_spec(5, width="0.75in") %>%
      add_indent(c(2:7)) %>%
      add_header_above(header=tblHead2) %>%
      add_header_above(header=tblHead1) %>%
      kableExtra::footnote(captionSrc("ACS",ACS))
    
    f.outData <- as.data.frame(m.IncFin)
    names(f.outData) <- c(paste0("Income Source: ",fipsname),"Households: Estimate",
                          "Households: MOE", "Average Income: Estimate", "Average Income; MOE")
    
    #FlexTable
    
    Ft <- data.frame(m.IncFin)
    names(Ft) <- c("V1","V2","V3","V4","V5")
    FlexOut <- regulartable(Ft)
    
    FlexOut <- set_header_labels(FlexOut, V1 = "Income Source(s)", 
                                V2 = "Estimate", V3 = "Margin of Error", 
                                 V4 = "Estimate", V5 = "Margin of Error")
    
      FlexOut <- add_header(FlexOut,V1 = "", V2 = "Total Households", V3 = "",
                            V4 = "Mean Income", V5 ="",
                            top=TRUE)
   
    FlexOut <- add_header(FlexOut,V1 =paste0("Household Income Source(s): ",fipsname), top=TRUE)
    FlexOut <- add_footer(FlexOut,V1=captionSrc("ACS",ACS))
    FlexOut <- merge_at(FlexOut,i=1,j = 1:5,part="header")
    FlexOut <- merge_at(FlexOut,i=2,j = 2:3, part="header")
    FlexOut <- merge_at(FlexOut,i=2,j = 4:5, part="header")
    FlexOut <- merge_at(FlexOut,i=1, j = 1:5, part = "footer")
    FlexOut <- align(FlexOut,i=1,j = 1, align="left",part="header")
    FlexOut <- align(FlexOut,i=2:3,j = 1:5, align="center",part="header")     
    FlexOut <- align(FlexOut,i=1, align="left",part="footer")
    FlexOut <- align(FlexOut, j=1, align="left", part="body")
    FlexOut <- autofit(FlexOut)
    FlexOut <- width(FlexOut,j = 1, width = 2)
    FlexOut <- width(FlexOut, j = 2:5, width = 1)
    
    
    inc_tabL <- m.IncFin %>%
      kable(
        row.names=FALSE,
        align='lrrrr',
        caption="Household Income Source(s)",
        col.names = names_spaced,
        format="latex", booktabs=TRUE)  %>%
      kable_styling(latex_options="HOLD_position",font_size=10) %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width="3in") %>%
      column_spec(2, width="0.5in") %>%
      column_spec(3, width="0.5in") %>%
      column_spec(4, width="0.5in") %>%
      column_spec(5, width="0.5in") %>%
      add_indent(c(2:7)) %>%
      add_header_above(header=tblHead2) %>%
      add_header_above(header=tblHead1) %>%
      kableExtra::footnote(captionSrc("ACS",ACS),threeparttable = T)
    
    # Text
    
    OutText <- paste0("The Houselold Income Source(s) Table shows household income sources and amounts for housholds in ", fipsname,".")
    OutText <- paste0(OutText,"  Households will have multiple sources of income, so this table is not mutually exclusive.")
    OutText <- paste0(OutText,"  Mean income values reflect values from the cited source.")
    
    
    outList <- list("Htable" = inc_tabH, "data" = f.outData,"FlexTable"=FlexOut, "Ltable" = inc_tabL, "text" = OutText)
    return(outList)
  }
