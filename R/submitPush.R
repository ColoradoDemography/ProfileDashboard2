#' submitPush Writes a script tag with a dataLayer.push command  on clicking of the "View Profile" button
#'
#' @param lvl the selected level (Counties or Municipalities)
#' @param unit the selected location
#' @param topics the contents of the outChk checkboxes
#' @export
#'
submitPush <- function(lvl,unit,topics) {

  lvlStr <- paste0("'DataLevel' : '",lvl,"',")
  unitStr <-paste0("'Location : '",unit,"',")
  
  if("stats" %in% topics) {
    statsStr <- "'BasicStatistics' : 'yes',"
  }else {
    statsStr <- "'BasicStatistics' : 'no',"
  }  
  if("popf" %in% topics) {
    popfStr <- "'PopulationForecast' : 'yes',"
  }else {
    popfStr <- "'PopulationForecast'  : 'no',"
  }  
  if("pop" %in% topics) {
    popStr <- "'AgeCharacteristics' : 'yes',"
  }else {
    popStr <- "'AgeCharacteristics' : 'no',"
  }
  if("popc" %in% topics) {
    popcStr <- "'PopulationCharacteristics' : 'yes',"
  }else {
    popcStr <- "'PopulationCharacteristics' : 'no',"
  }
  if("housing" %in% topics) {
    housingStr <- "'Housing' : 'yes',"
  }else {
    housingStr <- "'Housing' : 'no',"
  }  
  if("comm" %in% topics) {
    commStr <- "'Commuting' : 'yes',"
  }else {
    commStr <- "'Commuting : 'no',"
  }  
  if("emplind" %in% topics) {
    emplindStr <- "'EmploymentIndustry' : 'yes',"
  }else {
    emplindStr <- "'EmploymentIndustry' : 'no',"
  } 
  if("emply" %in% topics) {
    emplyStr <- "'EmploymentCharacteristics'  : 'yes'"
  }else {
    emplyStr <- "'EmploymentCharacteristics' : 'no'"
  }  
    
  #assembling script
  outstr <-paste0("<script>window.dataLayer = window.dataLayer || [];",
    "window.dataLayer.push({ event: 'ProfileSubmit',",
    lvlStr,unitStr,statsStr,popfStr,
    popStr,popcStr,housingStr,commStr,emplindStr,emplyStr,
    "}); </script>")
  
  return(outstr)
}