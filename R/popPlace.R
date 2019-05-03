#'  popPlace : Populates the input$unit field using information from the PostGres estimates database.
#'  return a data frame with the placefips, countyfips, placename and totalPopulation  Revised 1/23/2019
#'
#' @param level identifies the level to be used (State, Planning Regions, Counties, Municipalities)
#'    taken from the input$level parameter from the dashboard
#'
#' @export


popPlace <- function(DBPool,curyr) {
  #Regions
    regList <- list(
      "Denver PMSA",
      "Denver-Boulder Metro Area",
      "Denver-Boulder-Greely CMSA",
      "10 County Denver Metro Area",
      "Central Mountains",	
      "Eastern Plains",
      "Front Range",
      "San Luis Valley",
      "Western Slope",
      "Region  1: Northern Eastern Plains",
      "Region  2: Northern Front Range",
      "Region  3: Denver Metropolitan Area",
      "Region  4: Southern Front Range",
      "Region  5: Central Eastern Plains",
      "Region  6: Southern Eastern Plains",
      "Region  7: Pueblo County",
      "Region  8: San Juan Valley",
      "Region  9: Southern Western Slope",
      "Region 10: Central Western Slope",
      "Region 11: Northern Western Slope",
      "Region 12: Northern Mountains",
      "Region 13: Central Mountains",
      "Region 14: Southern Mountains")
 
  
  # Create Connection Strings
  clookupStr <- paste0("SELECT countyfips, placefips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE year = ", curyr, " and placefips = 0;")
  plookupStr <- paste0("SELECT countyfips, placefips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE year = ", curyr, ";")
  mlookupStr <- paste0("SELECT countyfips, placefips,  year, totalpopulation FROM estimates.county_muni_timeseries WHERE year = ", curyr, " and placefips != 0 and placefips != 99990 and countyfips = 999;")
 

    # f.cLookup contains the county records
    f.cLookup <- dbGetQuery(DBPool, clookupStr)
    
    f.pLookup <- dbGetQuery(DBPool, plookupStr)
    
    #f.mLookup is the multi county cities
    f.mLookup <- dbGetQuery(DBPool, mlookupStr)
    
  
 # Counties   
    f.cLookup <- f.cLookup[c(2:nrow(f.cLookup)),]
    f.cLookup[,3] <- sapply(f.cLookup[,3], function(x) simpleCap(x))
   

 # Municialities

    #removing errant records...
    f.pLookup <- f.pLookup[which(f.pLookup$placefips != 0),] #remove State Records
    f.pLookup <- f.pLookup[which(f.pLookup$countyfips != 999),] # County total records for multiple places
    f.pLookup <- f.pLookup[which(f.pLookup$placefips != 99990),] #Remove Unincoprpoated Areas
    f.pLookup <- f.pLookup[which(!is.na(f.pLookup$placefips)),] #Remove Disbanded Areas
    
    f.pLookup$municipalityname <- gsub(' \\(Part\\)','',f.pLookup$municipalityname)
    f.pLookup$municipalityname <- gsub('Sprgs','Springs',f.pLookup$municipalityname)
    f.pLookup$municipalityname <- gsub('/G','',f.pLookup$municipalityname)
    
    #merging f.pLookup and f.mLookup and updating totalpopulation value
    f.mLookup <- f.mLookup[,c(2,4)]

    f.pLookupFin <- left_join(f.pLookup,f.mLookup,by="placefips")
    f.pLookupFin$cty_Pop <- f.pLookupFin$totalpopulation.x  # this is the potions of the population in each portion

    f.pLookupFin$totalpopulation <- ifelse(is.na(f.pLookupFin$totalpopulation.y),f.pLookupFin$totalpopulation.x,f.pLookupFin$totalpopulation.y)
    f.pLookupFin <- f.pLookupFin[,c(2,1,3,4,8,7)]
    
    # merging counties and municipals
    f.cty <- f.cLookup[,c(1,3)]

    f.plac <- left_join(f.pLookupFin,f.cty,by="countyfips")
    names(f.plac)[3] <- "municipalityname"
    names(f.plac)[7] <- "countyname"
    f.plac <- f.plac[,c(2,1,3:7)]
    f.plac <- f.plac[order(f.plac$municipalityname),]
 
    
    
  loc <- list("Counties" = f.cLookup, "Munis" = f.plac, "Region" = regList)
  return(loc)
}

