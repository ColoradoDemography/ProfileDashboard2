#' residentialLF Creates a two plots showing residential labor force participation
#'
#' Plot 1: a line chart showing forcasted trend line for persons in the labor force and
#'     persons age 16 and older.
#' Plor2: a bar chart comparing forcasted labor force participation for a place and Colorado
#'
#' @param listID the list containing place id and Place names
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphics and associated data sets
#' @export

residentialLF <- function(DBPool,listID, curyr, base=10){
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }
  #fips is the 3-digit character string



  LFSQLPL <- paste0("SELECT * FROM estimates.labor_force_participation WHERE area_code = ",as.numeric(ctyfips), ";")
  

  # Read data files
  f.LFPlace <- dbGetQuery(DBPool, LFSQLPL)


  # Summing by Participation Year

  f.LFPlaceSum <- f.LFPlace %>%
    group_by(population_year) %>%
    summarize(LForce = sum(laborforce),
              Pop16P = sum(cni_pop_16pl)) %>% 
            mutate(LFGrowth=percent(signif((((LForce/lag(LForce))^(1/(population_year-lag(population_year)))) -1)*100),digits=1),
                   P16Growth=percent(signif((((Pop16P/lag(Pop16P))^(1/(population_year-lag(population_year)))) -1)*100),digits=1))
  
  f.LFPlaceSum$Series <-  ifelse(f.LFPlaceSum$population_year > curyr,"Forecast","Estimate")
  f.LFPlaceSum <-  f.LFPlaceSum[which( f.LFPlaceSum$population_year >= 2010 &  f.LFPlaceSum$population_year <= 2040),]


  f.LFPlaceSum$LFGrowth <- gsub("NA%","", f.LFPlaceSum$LFGrowth)
  f.LFPlaceSum$P16Growth <- gsub("NA%","", f.LFPlaceSum$P16Growth)

  f.LFPlaceSum$LForce <-format(round(f.LFPlaceSum$LForce,digits=0),big.mark=",",scientific=FALSE)
  f.LFPlaceSum$Pop16P <-format(round(f.LFPlaceSum$Pop16P,digits=0),big.mark=",",scientific=FALSE)
  
  f.LFPlaceSum <-  f.LFPlaceSum[,c(1,6,2,4,3,5)]
  names(f.LFPlaceSum) <- c("Year","Type","Labor Force","Annual Growth Rate: Labor Force","Persons Age 16+","Annual Growth Rate: Persons Age 16+")
  m.forecast <- as.matrix(f.LFPlaceSum[seq(1, nrow(f.LFPlaceSum), 5), ])
  
  f.flex <- as.data.frame(m.forecast)
  
   #Producing the tables
   pltTitle <- "Forecast Resident Labor Force and Population, Age 16 +"
   
   # set vector names
   tblHead <- c(ctyname = 6)
   names(tblHead) <- ctyname
   
   names_spaced <- c("Year","Type","Labor Force","Annual Growth Rate: Labor Force","Persons Age 16+","Annual Growth Rate: Persons Age 16+")
   
   tabHTML <- m.forecast %>%
     kable(format='html', table.attr='class="cleanTable"',
           digits=1,
           row.names=FALSE,
           align="llrrrr",
           caption= pltTitle,
           col.names = names_spaced,
           escape = FALSE)  %>%
     kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 12) %>%
     column_spec(1, width="0.5in") %>%
     column_spec(2, width="0.5in") %>%
     column_spec(3, width="1in") %>%
     column_spec(4, width="1in") %>%
     column_spec(5, width="1in") %>%
     column_spec(6, width="1in") %>%
     add_header_above(header=tblHead) %>%
     kableExtra::footnote(captionSrc("SDO",""))
   
   
   #LATEX Table
   # set vector names
   tabLATEX <- kable(m.forecast, col.names = names_spaced,
                     caption= pltTitle, 
                     row.names=FALSE, align="llrrrr",
                     format="latex", booktabs=TRUE)  %>%
     kable_styling(latex_options="HOLD_position") %>%
     column_spec(1, width="0.5in") %>%
     column_spec(2, width="0.5in") %>%
     column_spec(3, width="1in") %>%
     column_spec(4, width="1in") %>%
     column_spec(5, width="1in") %>%
     column_spec(6, width="1in") %>%
     add_header_above(header=tblHead) %>%
     kableExtra::footnote(captionSrc("SDO",""),threeparttable = T) 
   
   #Flextable
   
   FlexOut <- regulartable(f.flex) %>% 
     add_header(Year=paste0(pltTitle,": ",ctyname),top=TRUE) %>%
     add_footer(Year=captionSrc("SDO","")) %>%
     merge_at(i=1,j=1:3,part="header") %>%
     merge_at(i=1,j=1:3,part="footer") %>%
     align(i=1, j=1, align="left",part="header") %>%
     width(j=1:6, width=1.0) 


   # Final Data
   f.flex$geoname <- ctyname
   f.flex <- f.flex[,c(7,1:6)]
   names(f.flex) <- c("Place","Year","Type","Labor Force","Annual Growth Rate: Labor Force","Persons Age 16+","Annual Growth Rate: Persons Age 16+")
   
#Text
  OutText <- paste0("This table compares the forecast residential labor force to the forecast population of person age 16 and older for ",ctyname,".")

  outList <- list("Htable"= tabHTML,"Ltable" = tabLATEX , "FlexTable" = FlexOut, "data" = f.flex, "text" = OutText)

  return(outList)
}
