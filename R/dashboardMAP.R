#' dashboardMAP Creates a simple map that highlights a Colorado County or place
#'   Modified from cp_countymap  AB 2/2018
#'   Revised 3/2018 to account for standalone JSON dataset
#'
#' This function creates a map to be used in the profile process,
#'    If a planning region is selected, the plannign region is colored in
#'    If a county is selected, the county is colored in and the planning region is outlined
#'    if a place is selected, the county is outlined and a dagger is posted at the center of the place.
#'
#'
#' @param listID the list containing place id and Place names
#' @export

dashboardMAP <- function(DBPool,lvl,listID){
  # Collecting place ids from  idList, setting default values
  ctyList <- listID$ctyList
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  #if(listID$PlFilter == "T") {
  #  placefips <- ""
  #  placename <- ""
  #}
  
  
  if(lvl == "Municipalities"){
  
    f.muni <- dbGetQuery(DBPool, paste0("SELECT placefp, x, y FROM bounds.place_centroids WHERE  placefp = '",placefips, "';"))
    
    f.muni$lat <- as.numeric(f.muni$y)
    f.muni$long <- as.numeric(f.muni$x)
  }
  
  
  #Pulls the COunty Outlines
  
  #Accessing JSON file, with Counties 
  data_file <- "www/County_GEN_2014.geojson"
  data_json <- geojson_read(data_file, what = "sp")
  
  
  
  #gj=readOGR(data_json, "OGRGeoJSON", verbose=FALSE)
  gj=fortify(data_json)
  
  gj1 <- data.frame()
  
  for(i in 1:length(ctyList)) {
    #Pulls the County to Highlight
    cty1 <- data_json[which(data_json@data[["COUNTYFP"]] == ctyList[i]),]
    cty2 <- fortify(cty1)
    gj1 <- rbind(gj1,cty2)
  }
  
  
  m <- ggplot()+
    geom_map(data=gj, map=gj,
             aes(x=long, y=lat, map_id=id), 
             fill=rgb(239,239,239, max=255), color=rgb(92,102,112, max=255), size=.25) +
    geom_map(data=gj1, map=gj1,
             aes(x=long, y=lat, map_id=id),
             fill=rgb(0,149,58, max=255), color=rgb(92,102,112, max=255), size=.25)+
    coord_map(project="albers", lat0=40, lat1=39) +
    theme_map()+
    theme(panel.background=element_rect(fill=rgb(239,239,239, max=255), color=rgb(239,239,239, max=255)),
          plot.background=element_rect(fill=rgb(239,239,239, max=255), color=rgb(239,239,239, max=255)))
  
  if(lvl =="Municipalities"){  # Adding point for center of municipality
    m <- m + geom_point(data=f.muni, aes(x=long, y=lat,shape="16", color="#655003c"),size=2) + 
      theme(legend.position="none")
  }
  
  return(m)
}