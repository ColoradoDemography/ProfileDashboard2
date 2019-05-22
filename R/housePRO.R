#' housePRO  Produces the housing table
#'  CO Housing Unit Table
#'
#'  This function compares housing occupancy and vacancy rates for a place to the state
#'
#' @param listID the list containing place id and Place names
#' @param HH Specifies the HH data set to be used, reads curHH from Shiny program
#' @return kable formatted  table and data file
#' @export
#'

housePRO=function(DBPool,listID, curYr){

  # Collecting place ids from  idList, setting default values
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
#  if(listID$PlFilter == "T") {
#    placefips <- ""
#    placename <- ""
#  }
state <- "08"

  if(nchar(placefips) == 0) {
    # Building county data table
    HHSQL <- paste0("SELECT countyfips, year, totalpopulation, householdpopulation, groupquarterspopulation,  householdsize, totalhousingunits, vacanthousingunits, vacancyrate FROM estimates.county_profiles  WHERE (countyfips = ", ctyfips ," and year = ", curYr,");")
  } else {
    # Building Place data table
    HHSQL <- paste0("SELECT countyfips, placefips, year, totalpopulation, householdpopulation, groupquarterspopulation,  householdsize, totalhousingunits, vacanthousingunits, vacancyrate FROM estimates.muni_pop_housing WHERE (placefips = ", placefips," and year = ", curYr,");")
  }

f.hhP <- dbGetQuery(DBPool, HHSQL)


# Preparing data
if(nchar(placefips) == 0) {
  # Building county data table
  f.hhP$occupiedhousingunits <- f.hhP$totalhousingunits - f.hhP$vacanthousingunits
  f.hhP <- f.hhP[,c(1,2,7,10,8,9,3:6)]
  f.HHPl <- f.hhP %>% gather(housing, count, totalhousingunits:householdsize, factor_key=TRUE)
  f.HHPl <- f.HHPl[,c(3,4)]
}  else {
    f.hhP <- f.hhP[which(as.numeric(f.hhP$countyfips) != 999),]
    f.hhP$occupiedhousingunits <- f.hhP$totalhousingunits - f.hhP$vacanthousingunits
    
    f.HHPl <- f.hhP %>% summarize(totalpopulation	 = sum(totalpopulation),
                                  householdpopulation	 = sum( householdpopulation),
                                  groupquarterspopulation	 = sum( groupquarterspopulation),
                                  householdsize	 = sum(  householdsize),
                                  totalhousingunits	 = sum( totalhousingunits),
                                  occupiedhousingunits	 = sum(occupiedhousingunits),
                                  vacanthousingunits	 = sum( vacanthousingunits)) %>%
                        mutate(vacancyrate = (vacanthousingunits/totalhousingunits) *100) 
    
     f.HHPl <- f.HHPl[,c(5:8,1:4)] %>%           
                        gather(housing, count, totalhousingunits:householdsize, factor_key=TRUE)
  }
  

f.HHPl$housing  <- ifelse(f.HHPl$housing == "totalpopulation", "Total Population",
                   ifelse(f.HHPl$housing == "householdpopulation", "Household Population",         
                   ifelse(f.HHPl$housing == "groupquarterspopulation", "Group Quarters Population",
                   ifelse(f.HHPl$housing == "totalhousingunits", "Total Housing Units",
                   ifelse(f.HHPl$housing == "householdsize", "Persons per Household",       
                   ifelse(f.HHPl$housing == "occupiedhousingunits", "Occupied Housing Units",
                   ifelse(f.HHPl$housing == "vacanthousingunits", "Vacant Housing Units","Vacancy Rate")))))))


 
  f.HHPl[c(1:3,5:7),2] <- comma(as.numeric(f.HHPl[c(1:3,5:7),2]))
  f.HHPl[8,2] <- round(as.numeric(f.HHPl[8,2]), 2)
  f.HHPl[4,2] <- percent(as.numeric(f.HHPl[4,2]))

  m.House <- as.matrix(f.HHPl)

  

  # Setting up table

  #Column Names
  names_spaced <- c("Housing Type","Value")
  #Span Header

  if(nchar(placefips) == 0) {
  # create vector with colspan
  tblHead1 <- c(" " = 1, ctyname = 2)

  # set vector names
  names(tblHead1) <- c(" ", ctyname)
  tabTitle <- paste0("Housing Units: ",ctyname, ", ",curYr)
  } else {
    # create vector with colspan
    tblHead1 <- c(" " = 1, placename = 2)
    
    # set vector names
    names(tblHead1) <- c(" ", placename)
    tabTitle <- paste0("Housing Units: ",placename,", ",curYr)
  }

  
  Htable <- m.House %>%
    kable(format='html', table.attr='class="cleanTable"',
          row.names=FALSE,
          align='lr',
          caption=tabTitle,
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 12) %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width = "3.5in") %>%
    column_spec(2, width ="0.5in") %>%
    add_indent(c(2,3,4,6,7,8)) %>%
   # add_header_above(header=tblHead1) %>%
    kableExtra::footnote(captionSrc("SDO",curYr))
  
  # preparing FlexTable
  
  FlexOut <- regulartable(f.HHPl)
  FlexOut <- set_header_labels(FlexOut, housing = "Housing Type", 
                               count="Value")
  
  if(nchar(placefips) == 0) {
    FlexOut <- add_header(FlexOut,count=ctyname,top=TRUE)
  } else {
    FlexOut <- add_header(FlexOut,count=placename,top=TRUE)
  }
  
  FlexOut <- add_header(FlexOut,housing=tabTitle,top=TRUE)
  FlexOut <- add_footer(FlexOut,housing=captionSrc("SDO",curYr))
  FlexOut <- merge_at(FlexOut,i=1, j = 1:2, part = "header")
  FlexOut <- merge_at(FlexOut,i=2, j = 2, part = "header") 
  FlexOut <- merge_at(FlexOut,i=1, j = 1:2, part = "footer")
  FlexOut <- align(FlexOut,i=1:3, j=1, align="left",part="header")
  FlexOut <- align(FlexOut,i=2:3, j=2, align="center",part="header")
  FlexOut <- align(FlexOut,i=1, align="left",part="footer")
  FlexOut <- align(FlexOut, j=1, align="left", part="body")
  FlexOut <- autofit(FlexOut)
  FlexOut <- width(FlexOut,j=1, width=3)
  FlexOut <- width(FlexOut,j=2, width=1)

   Ltable <- m.House %>% kable(
        col.names = names_spaced,
        align="lrr",
        caption=tabTitle, row.names=FALSE,
        format="latex", booktabs=TRUE)  %>%
        kable_styling(latex_options="HOLD_position", font_size=10) %>%
        column_spec(1, width = "3.5in") %>%
        column_spec(2, width ="0.5in") %>%
        add_indent(c(2,3,4,6,7,8)) %>%
        add_header_above(header=tblHead1) %>%
        kableExtra::footnote(captionSrc("SDO",curYr),threeparttable = T)

  names(f.HHPl) <- c(tabTitle,"Value")  

   outList <- list("Htable" = Htable, "Ltable" = Ltable,"data" = f.HHPl,"FlexTable" = FlexOut)
   return(outList)
  }

