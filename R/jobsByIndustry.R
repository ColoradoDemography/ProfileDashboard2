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
  
  # Read data files
  f.jobsPL <- dbGetQuery(DBPool, paste0("SELECT * FROM estimates.jobs_by_sector WHERE (area_code = ", as.character(as.numeric(ctyfips)),
                                     " AND population_year = ", as.character(curyr), ");"))
 
  f.jobsPL$total_jobs <- if_else(is.na(f.jobsPL$total_jobs),0,f.jobsPL$total_jobs)
  
  

  # Assigning and extracting major Categories
  f.jobsPL <- f.jobsPL %>%
     mutate(sector_id = ifelse(grepl("-",sector_id),str_sub(sector_id,1,2),sector_id),
            sector_id = str_pad(sector_id,3,pad="0"),
            sector_id = ifelse(sector_id == "010", "000", sector_id))
  f.jobsTOT <- f.jobsPL %>% filter(sector_id == "000") %>%
    mutate(PLTotal = total_jobs) %>% select(PLTotal)
 
 
  f.jobsPLMain <- bind_cols(f.jobsPL, f.jobsTOT) %>%
    mutate(total_c = comma(round(total_jobs,digits=0)),
           prop_jobs = (total_jobs/PLTotal)*100,
           pct_jobs = percent(prop_jobs))


  f.jobsPLMain$geoname <- ctyname
  f.jobsPLMain <- f.jobsPLMain[which(f.jobsPLMain$prop_jobs > 0),]  # removing blank categoies
  f.jobsPLMainFin <- f.jobsPLMain %>% arrange(sector_id) %>%
        select(geoname, sector_name,total_c,pct_jobs)

  f.jobsChart <- f.jobsPLMain %>% filter(sector_id != "000") %>%
       mutate(sector_name = ifelse(grepl("Waste",sector_name),"Administrative and Waste Management",sector_name)) %>%
       arrange(prop_jobs)
  
  jobs_lab <- unlist(f.jobsChart$sector_name)
  
   f.jobsChart$sector_name <- factor(f.jobsChart$sector_name, levels = jobs_lab)
 
  pltTitle <- paste0(as.character(curyr)," Share of Jobs\nby Industry")
  subTitle <- ctyname  #The is the county Name...
  axs <- setAxis(f.jobsChart$prop_jobs)


  p.jobs <- ggplot(f.jobsChart, aes(x=sector_name, y=prop_jobs)) +
    geom_bar(stat="identity", position="dodge", fill= "#6EC4E8")+
    geom_text(mapping=aes(x=sector_name, y=prop_jobs, label=pct_jobs),
              hjust = -0.5, size = 2,
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
          axis.text = element_text(size=8),
          panel.grid.major = element_line(colour = "gray80"))

  # Building Output data
  
  f.jobsFin <- f.jobsPLMainFin
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
