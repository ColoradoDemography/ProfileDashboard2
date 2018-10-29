#' housePRO  Produces the housing table
#'  CO Housing Unit Table
#'
#'  This function compares housing occupancy and vacancy rates for a place to the state
#'
#' @param listID the list containing place id and Place names
#' @param ACS Specifies the ACS data set to be used, reads curACS from Shiny program
#' @return kable formatted  table and data file
#' @export
#'

housePRO=function(listID, ACS){
  
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
    # Building ACS county data table
    f.b25001 <- codemog_api(data="b25001", db=ACS, geonum=paste("1", state, ctyfips, sep=""),meta="no")
    f.b25003 <- codemog_api(data="b25003", db=ACS, geonum=paste("1", state, ctyfips, sep=""),meta="no")
    f.b25004 <- codemog_api(data="b25004", db=ACS, geonum=paste("1", state, ctyfips, sep=""),meta="no")
  } else {
    # Building ACS Place data table
    f.b25001 <- codemog_api(data="b25001", db=ACS, geonum=paste("1", state, placefips, sep=""),meta="no")
    f.b25003 <- codemog_api(data="b25003", db=ACS, geonum=paste("1", state, placefips, sep=""),meta="no")
    f.b25004 <- codemog_api(data="b25004", db=ACS, geonum=paste("1", state, placefips, sep=""),meta="no")
  }


  f.AcsPl <- cbind(f.b25001[,c(1,8)], f.b25003[,8:10],f.b25004[,8:15])

  f.AcsPl[,2:13]=as.numeric(as.character(f.AcsPl[,2:13]))

  f.AcsPl <- f.AcsPl %>% rename(Total=b25001001, Occupied=b25003001, Vacant=b25004001,
                                Owner = b25003002, Renter = b25003003, Seasonal = b25004006)%>%
    mutate(Other = sum(b25004002, b25004003, b25004004, b25004005, b25004007,b25004008))

  f.AcsPlace <- f.AcsPl[,c(1:6,11,14)] %>%
    gather(var, ACS, Total:Other, -geoname)
  
  


  #Finalizing place table
  f.AcsPLFin <- f.AcsPlace
  names(f.AcsPLFin) <- c("HCat", "geoname","PL_Value")
  #Calculating  proportions
  PL_Tot <- as.numeric(f.AcsPLFin[1,3])
  f.AcsPLFin$PL_VAL_Prop <- f.AcsPLFin$PL_Value/PL_Tot
  f.AcsPLFin$PL_VAL_PCT <- percent(f.AcsPLFin$PL_VAL_Prop *100)



  # Assembling Combined Tab: f.longtab

  f.longTab <- f.AcsPLFin[,c(2,3,5)]

  f.longTab$geoname <- ifelse(f.longTab$geoname == "Total","Total Housing Units",
                           ifelse(f.longTab$geoname == "Occupied","Occupied Housing Units",
                                  ifelse(f.longTab$geoname == "Owner", "Owner-Occupied Units",
                                         ifelse(f.longTab$geoname == "Renter", "Renter-Occupied Units",
                                                ifelse(f.longTab$geoname == "Vacant", "Vacant Housing Units",
                                                       ifelse(f.longTab$geoname == "Seasonal","Seasonal Units","All Other Vacant Units"))))))

  #Reordering Table and prepating output

  f.HouseTab <- f.longTab
  f.HouseTab[2] <- comma(f.HouseTab[,2])

  m.House <- as.matrix(f.HouseTab)

  names(f.HouseTab) <- c("Housing Type", paste0("Housing Units: ",ctyname),
                         paste0("Percentage: ",ctyname))

  # Setting up table

  #Column Names
  names_spaced <- c("Housing Type","Count","Percent")
  #Span Header

  if(nchar(placefips) == 0) {
  # create vector with colspan
  tblHead1 <- c(" " = 1, ctyname = 2)

  # set vector names
  names(tblHead1) <- c(" ", ctyname)
  } else {
    # create vector with colspan
    tblHead1 <- c(" " = 1, placename = 2)
    
    # set vector names
    names(tblHead1) <- c(" ", placename)
    
  }

  
  Htable <- m.House %>%
    kable(format='html', table.attr='class="cleanTable"',
          row.names=FALSE,
          align='lrr',
          caption="Housing Units",
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 12) %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width = "3.5in") %>%
    column_spec(2, width ="0.5in") %>%
    column_spec(3, width ="0.5in") %>%
    add_indent(c(3,4,6,7)) %>%
    add_header_above(header=tblHead1) %>%
    footnote(captionSrc("ACS",ACS))
  
  # preparing FlexTable
  f.house_data <- data.frame(m.House)
  FlexOut <- regulartable(f.house_data)
  FlexOut <- set_header_labels(FlexOut, geoname = "Housing Type", 
                               PL_Value="Count", PL_VAL_PCT="Percent"
                               )
  
  if(nchar(placefips) == 0) {
    FlexOut <- add_header(FlexOut,PL_Value=ctyname,top=TRUE)
  } else {
    FlexOut <- add_header(FlexOut,PL_Value=placename,top=TRUE)
  }
  
  FlexOut <- add_header(FlexOut,geoname="Housing Units",top=TRUE)
  FlexOut <- add_footer(FlexOut,geoname=captionSrc("ACS",ACS))
  FlexOut <- merge_at(FlexOut,i=1, j = 1:3, part = "header")
  FlexOut <- merge_at(FlexOut,i=2, j = 2:3, part = "header") 
  FlexOut <- merge_at(FlexOut,i=1, j = 1:3, part = "footer")
  FlexOut <- align(FlexOut,i=1:3, j=1, align="left",part="header")
  FlexOut <- align(FlexOut,i=2:3, j=2:3, align="center",part="header")
  FlexOut <- align(FlexOut,i=1, align="left",part="footer")
  FlexOut <- align(FlexOut, j=1, align="left", part="body")
  FlexOut <- autofit(FlexOut)
  FlexOut <- width(FlexOut,j=1, width=3)
  FlexOut <- width(FlexOut,j=2:3, width=1)

   Ltable <- m.House %>% kable(
        col.names = names_spaced,
        align="lrr",
        caption="Housing Units", row.names=FALSE,
        format="latex", booktabs=TRUE)  %>%
        kable_styling(latex_options="HOLD_position") %>%
        column_spec(1, width = "3.5in") %>%
        column_spec(2, width ="0.5in") %>%
        column_spec(3, width ="0.5in") %>%
        add_indent(c(3,4,6,7)) %>%
        add_header_above(header=tblHead1) %>%
        footnote(captionSrc("ACS",ACS))


   outList <- list("Htable" = Htable, "Ltable" = Ltable,"data" = f.HouseTab,"FlexTable" = FlexOut)
   return(outList)
  }

