#' API Wrapper to call Colorado County Single Year of Age Data
#'
#' This API wrapper returns a dataframe of county-level age data.
#'
#' The functions takes county and year and can return single year of age data and grouped data.
#'
#' Grouping options are as follows:
#'    opt1= Group by year
#'    opt2= Group by county and year
#'    opt3= Group by age and year
#'
#' Data are estimates or forecasts depending on the most recent vintage, they are noted as such in the returned data.
#'
#' @param fips_list Numeric FIPS code(s) for the county (0 for the state) (no leading 0's)
#' @param year_list Numeric list of years between 1990 and 2050
#' @param group string taking values of opt1, opt2, opt3, see descriptiong what what each does
#' @importFrom jsonlite fromJSON
#' @import tidyr
#' @import dplyr
#'
#' @export

county_sya = function(fips_list, year_list, group="none"){
# subject to change, but this is the server URL
url_stub = "https://gis.dola.colorado.gov/lookups/sya?"

# Type checks for each list so that we don't get any text sent to the API for a SQL Injection.
if (!is.numeric(fips_list))
  stop("FIPS should be numeric.")
if (!is.numeric(year_list))
  stop("Years should be numeric.")

if (any(year_list < 1985) || any(year_list > 3000))
  stop("One or more year is out of range. Years should be between 1990 and 2050")


# Checks for two special call types 300 is for all counties, 0 is for state total, puts proper list
# together for call
suppressWarnings(if (!is.list(fips_list) && (fips_list == 300 | fips_list == 0)) {
  fips=c(1,3,5,7,9,11,13,14,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97,99,101,103,105,107,109,111,113,115,117,119,121,123,125)
} else {

  fips = fips_list
})

# Checks for special call types 3000 is for all years puts proper list together for call
 suppressWarnings(if (!is.list(year_list) && year_list == 3000) {
  year = c(1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,
           2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,
          2024,2025,2026,2027,2028,2029,2030,2031,2032,2033,2034,2035,2036,2037,2038,2039,2040,
           2041,2042,2043,2044,2045,2046,2047,2048,2049,2050)

} else {

  year = year_list
 })
# Creates the URL for the API call


suppressWarnings(if (!is.list(fips_list) && fips_list == 0) {
  call = paste0(url_stub, "county=", paste(fips,collapse = ","), "&year=",paste(year, collapse = ","), "&choice=single", "&group=opt3")
  data = jsonlite::fromJSON(call, simplifyVector = TRUE)
  data$countyfips=rep(0, length(data$year))
  data$county=rep("Colorado", length(data$year))

}else{
  call = paste0(url_stub, "county=", paste(fips, collapse = ","), "&year=", paste(year, collapse = ","),"&choice=single")
  # Makes the API call and converts the JSON to a data frame
  data = jsonlite::fromJSON(call, simplifyVector = TRUE)
})

# tells the function what to return and changes it from a dplyr tbl object back to a generic data
# frame
return(as.data.frame(data))


}
