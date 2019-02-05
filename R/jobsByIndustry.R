#' jobsByIndustry Creates a Chart showing the Total estimated jobs by sector for
#'   a place and for Colorado
#'
#' Uses State Demography Office data to create a chart compaing jobs by major sector
#' for a selected place, compared to Colorado.
#'
#' @param listID the list containing place id and Place names
#' @param curyr The maximum year value, from CurYr
#' @param base is the abse text size for the ggplot2 object and codemog_theme(), defaults to base = 10
#' @return  ggplot graphic and data file
#' @export
#'
jobsByIndustry <- function(DBPool,listID, curyr, base=10){
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }
  
  options(warn=-1)  # Suppressing warning messages produced by VennDiagram

  # Read data files
  f.jobsPL <- dbGetQuery(DBPool, paste0("SELECT * FROM estimates.jobs_by_sector WHERE (area_code = ", as.character(as.numeric(ctyfips)),
                                     " AND population_year = ", as.character(curyr), ");"))
  f.jobsST <- dbGetQuery(DBPool, paste0("SELECT * FROM estimates.jobs_by_sector WHERE (area_code = 0 AND population_year = ",
                                     as.character(curyr), ");"))

  f.jobsPL$total_jobs <- if_else(is.na(f.jobsPL$total_jobs),0,f.jobsPL$total_jobs)
  f.jobsPL$sector_name <- gsub("&nbsp;","",f.jobsPL$sector_name)
  f.jobsPL$sector_id <- if_else(nchar(f.jobsPL$sector_id) == 4,paste0("0",f.jobsPL$sector_id), f.jobsPL$sector_id)

  # Assigning and extracginf major Categories
  f.jobsPL$sector_agg <- if_else(f.jobsPL$sector_id == "01000",1,
                                 if_else(f.jobsPL$sector_id == "02000",1,
                                         if_else(f.jobsPL$sector_id == "03000",1,
                                                 if_else(f.jobsPL$sector_id == "04000",1,
                                                         if_else(f.jobsPL$sector_id == "05000",1,
                                                                 if_else(f.jobsPL$sector_id == "06000",1,
                                                                         if_else(f.jobsPL$sector_id == "07000",1,
                                                                                 if_else(f.jobsPL$sector_id == "08000",1,
                                                                                         if_else(f.jobsPL$sector_id == "09000",1,
                                                                                                 if_else(f.jobsPL$sector_id == "10000",1,
                                                                                                         if_else(f.jobsPL$sector_id == "10150",1,
                                                                                                                 if_else(f.jobsPL$sector_id == "11000",1,
                                                                                                                         if_else(f.jobsPL$sector_id == "11025",1,
                                                                                                                                 if_else(f.jobsPL$sector_id == "11050",1,
                                                                                                                                         if_else(f.jobsPL$sector_id == "12000",1,
                                                                                                                                                 if_else(f.jobsPL$sector_id == "12015",1,
                                                                                                                                                         if_else(f.jobsPL$sector_id == "13000",1,
                                                                                                                                                                 if_else(f.jobsPL$sector_id == "13015",1,
                                                                                                                                                                         if_else(f.jobsPL$sector_id == "14000",1,
                                                                                                                                                                                 if_else(f.jobsPL$sector_id == "15000",1,0))))))))))))))))))))



  f.jobsPLMain <- f.jobsPL[which(f.jobsPL$sector_agg == 1),]

  # Updating category names
  f.jobsPLMain$sector_name <- if_else(f.jobsPLMain$ sector_name == "Accommodation and food","Accomodation and Food Services",
                                      if_else(f.jobsPLMain$ sector_name == "Admin and waste","Adminstration and Waste Services",
                                              if_else(f.jobsPLMain$ sector_name == "Arts","Arts, Entertainment and Recreation",
                                                      if_else(f.jobsPLMain$ sector_name == "Education","Educational Services",
                                                              if_else(f.jobsPLMain$ sector_name == "Finance activities","Finance and Insurance",
                                                                      if_else(f.jobsPLMain$ sector_name == "Fince activities","Finance and Insurance",
                                                                      if_else(f.jobsPLMain$ sector_name == "Health Services","Healthcare and Social Assistance",
                                                                              if_else(f.jobsPLMain$ sector_name == "Management of companies and enterprise","Management of Companies",
                                                                                      if_else(f.jobsPLMain$ sector_name == "Other services, except public administration","Other Services",
                                                                                              if_else(f.jobsPLMain$ sector_name == "Professional and business services","Professional and Technical Services",
                                                                                                      if_else(f.jobsPLMain$ sector_name == "Real estate","Real Estate and Rental and Leasing",
                                                                                                              if_else(f.jobsPLMain$ sector_name == "Wholesale trade","Wholesale Trade",
                                                                                                                      if_else(f.jobsPLMain$ sector_name == "Transportation and warehousing","Transportation and Warehousing",f.jobsPLMain$sector_name
                                                                                                                      )))))))))))))


  f.jobsPLMain <- f.jobsPLMain %>%
    mutate(PLTotal = sum(total_jobs),
           total_c = comma(round(total_jobs,digits=0)),
           prop_jobs = (total_jobs/PLTotal)*100,
           pct_jobs = percent(prop_jobs))


  f.jobsPLMain$geoname <- ctyname
  f.jobsPLMain <- f.jobsPLMain[which(f.jobsPLMain$prop_jobs > 0),]  # removing blank categoies
  f.jobsPLMainFin <- f.jobsPLMain[,c(4,5,10,11,12)]



  f.jobsChart <- f.jobsPLMainFin

  f.jobsChart$sector_name <- factor(f.jobsChart$sector_name, levels=f.jobsChart$sector_name[order(f.jobsChart$prop_jobs)], ordered=TRUE)
  

  pltTitle <- paste0(as.character(curyr)," Share of Jobs\nby Industry")
  subTitle <- ctyname  #The is the county Name...
  axs <- setAxis(f.jobsChart$prop_jobs)


  p.jobs <- ggplot(f.jobsChart, aes(x=sector_name, y=prop_jobs)) +
    geom_bar(stat="identity", position="dodge", fill= "#6EC4E8")+
    geom_text(mapping=aes(x=sector_name, y=prop_jobs, label=pct_jobs),
              hjust = -0.5, size = 4,
              position = position_dodge(width = 1),
              inherit.aes = TRUE) +
    scale_y_continuous(limits=c(axs$minBrk,axs$maxBrk), breaks=axs$yBrk, expand = c(0, 0), label=percent)  +
    theme_codemog(base_size=base) + coord_flip() +
    theme(axis.text.x=element_text(angle=0))+
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = captionSrc("SDO",""),
         x = "Job Sector",
         y = "Percentage") +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text = element_text(size=10),
          panel.grid.major = element_line(colour = "gray80"))



  # Building Output data
  
  f.jobsFin <- f.jobsPLMainFin
  f.jobsFin$Geography <- ctyname
  f.jobsFin <- f.jobsFin[order(f.jobsFin$sector_id),]
  f.jobsFin <- f.jobsFin[,c(6,2,3,5)]
  names(f.jobsFin) <- c("Geography","Job Sector", "Number of Jobs", "Percentage of Jobs")
  
  # Generating text
  OutText1 <- paste0(" The total estimated jobs are subdivided into 3 categories:")
  OutText1 <- paste0(OutText1," \\begin{itemize} ")
  OutText1 <- paste0(OutText1,"\\item \\textit{Direct Basic:} jobs that bring outside dollars into the community by selling goods or services outside the county, such as manufacturing or engineering services,")
  OutText1 <- paste0(OutText1,"\\item \\textit{Indirect Basic:} jobs that are created as the result of goods and services purchased by direct basic such as accounting services or raw material inputs, and ")
  OutText1 <- paste0(OutText1,"\\item \\textit{Local (Resident) Services:}  jobs that are supported when income earned from the base industries is spent locally at retailers or are supported by local tax dollars to provide services like education and public safety.")
  OutText1 <- paste0(OutText1,"\\end{itemize}")
  
  OutText2 <- paste0("This plot shows the jobs by industry profile for ",ctyname,".")
  OutText2 <- paste0(OutText2,"  The relative rank of high-paying sectors, such as mining, information and finacial and insurance services")
  OutText2 <- paste0(OutText2," versus mid-range jobs (e.g., contsruction, health casre and government)")
  OutText2 <- paste0(OutText2,"  and lower-paying industrices such as retail trade and accomodation and food services,")
  OutText2 <- paste0(OutText2,"  will have an impact on a counties' overall economic health.") 
  
  outList <- list("plot"=p.jobs,"data"=f.jobsFin,"text1"=OutText1,"text2"=OutText2)
  return(outList)

}
