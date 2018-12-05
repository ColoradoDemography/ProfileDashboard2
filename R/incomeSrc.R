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
  
  if(level == "Municipalities") {
    if(listID$PlFilter == "F") {
      fipslist <- listID$plNum
      fipsname <- listID$plName
    } else {
      fipslist <- listID$ctyNum
      fipsname <- listID$ctyName
    }
  }
  if(level == "Counties") {
    fipslist <- listID$ctyNum
    fipsname <- listID$ctyName
  }
  
  hhSQL <- paste0("SELECT * FROM data.incomesrc_hh WHERE (geonum = 108", fipslist," and acs = '",ACS,"');")
  incSQL <- paste0("SELECT * FROM data.incomesrc_inc WHERE (geonum = 108", fipslist," and acs = '",ACS,"');")
  
  # Call to Postrgres  
  pw <- {
    "demography"
  }
  
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <- dbConnect(drv, dbname = "dola",
                   host = "104.197.26.248", port = 5433,
                   user = "codemog", password = pw)
  rm(pw) # removes the password
  
  f.Households <- dbGetQuery(con,hhSQL)
  f.Incomes <- dbGetQuery(con,incSQL)
  
  
  #closing the connections
  dbDisconnect(con)
  dbUnloadDriver(drv)
  rm(con)
  rm(drv)
  
  f.Households[f.Households == -9999] <- NA
  f.Incomes[f.Incomes == -9999] <- NA
  
  #County Level Household Income Counts
  incC51 <- f.Households[c(3:5)]
  incC54 <- f.Households[c(6:8)]
  incC55 <- f.Households[c(9:11)]
  incC56 <- f.Households[c(12:14)]
  incC57 <- f.Households[c(15:17)]
  incC59 <- f.Households[c(18:20)]
  
  names(incC51)[c(1,2,3)] <- c("total", "in", "out")
  names(incC54)[c(1,2,3)] <- c("total", "in", "out")
  names(incC55)[c(1,2,3)] <- c("total", "in", "out")
  names(incC56)[c(1,2,3)] <- c("total", "in", "out")
  names(incC57)[c(1,2,3)] <- c("total", "in", "out")
  names(incC59)[c(1,2,3)] <- c("total", "in", "out")
  
  incCty <- do.call("rbind", list(incC51, incC54, incC55, incC56, incC57,  incC59))
  incCty$ID <- seq.int(nrow(incCty))
  incCty$type <- c("With earnings","With interest, dividends or net rental income", 
                   "With Social Security income", "With Supplemental Security Income (SSI)","With cash public assistance income", 
                   "With retirement income")
  
  # County Level MOE Counts
  incC51_moe <- f.Households[c(21:23)]
  incC54_moe <- f.Households[c(24:26)]
  incC55_moe <- f.Households[c(27:29)]
  incC56_moe <- f.Households[c(30:32)]
  incC57_moe <- f.Households[c(33:35)]
  incC59_moe <- f.Households[c(36:38)] 
  
  names(incC51_moe)[c(1,2,3)] <- c("total_moe", "in_moe", "out_moe")
  names(incC54_moe)[c(1,2,3)] <- c("total_moe", "in_moe", "out_moe")
  names(incC55_moe)[c(1,2,3)] <- c("total_moe", "in_moe", "out_moe")
  names(incC56_moe)[c(1,2,3)] <- c("total_moe", "in_moe", "out_moe")
  names(incC57_moe)[c(1,2,3)] <- c("total_moe", "in_moe", "out_moe")
  names(incC59_moe)[c(1,2,3)] <- c("total_moe", "in_moe", "out_moe")
  
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
  incCtyHH <- merge(incCty2, incCty_moe2, by="ID")
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
  mincC61 <- f.Incomes[c(2,10)]
  mincC64 <- f.Incomes[c(2,11)]
  mincC65 <- f.Incomes[c(2,12)]
  mincC66 <- f.Incomes[c(2,13)]
  mincC67 <- f.Incomes[c(2,14)]
  mincC69 <- f.Incomes[c(2,15)]
  
  
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
  
  mincC61_moe  <- f.Incomes[c(2,23)]
  mincC64_moe  <- f.Incomes[c(2,24)]
  mincC65_moe  <- f.Incomes[c(2,25)]
  mincC66_moe  <- f.Incomes[c(2,26)] 
  mincC67_moe <- f.Incomes[c(2,27)]
  mincC69_moe <- f.Incomes[c(2,28)]
  
  
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
  mincCTot <- as.numeric(f.Incomes[1,3])
  mincCTot_moe <- as.numeric(f.Incomes[1,16])
  
  
  #combining files
  mincCty2 <- mincCty[,c(3,4,2)]
  mincCty_moe2 <- mincCty_moe[,c(3,4,2)]
  mincCtyHH <- merge(mincCty2, mincCty_moe2, by="ID")
  mincCtyHH <- mincCtyHH[,c(1:3,5)]
  names(mincCtyHH) <- c("ID","type","Agg_Income","Agg_Income_moe")
  totRow <- data.frame(ID = 0,
                       type="All Households",
                       Agg_Income=mincCTot,
                       Agg_Income_moe=mincCTot_moe)
  mincCtyHH <- rbind(totRow,mincCtyHH)
  
  
  # Producing final File
  mincCtyF <- mincCtyHH[,c(1,3,4)]
  incCtyFin <- merge(incCtyHH, mincCtyF, by ="ID")
  
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
      footnote(captionSrc("ACS",ACS))
    
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
      footnote(captionSrc("ACS",ACS),threeparttable = T)
    
    # Text
    
    OutText <- paste0("The Houselold Income Source(s) Table shows household income sources and amounts for housholds in ", fipsname,".")
    OutText <- paste0(OutText,"  Households will have multiple sources of income, so this table is not mutually exclusive.")
    OutText <- paste0(OutText,"  Mean income values reflect values from the cited source.")
    
    
    outList <- list("Htable" = inc_tabH, "data" = f.outData,"FlexTable"=FlexOut, "Ltable" = inc_tabL, "text" = OutText)
    return(outList)
  }
