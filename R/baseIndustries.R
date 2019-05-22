#' baseIndustries outputs the base_industries job plot and data
#'
#'   This includes code to output data for the Denver-Boulder MSA when Adams, Arapahoe, Boulder,
#'     Broomfield, Denver, Douglas, or Jefferson County are selected.
#'
#' @param  fips numeric, county-level FIPS code
#' @param  ctyname County Name string, from input$unit
#' @param  curyr the current year
#' @return ggplot2 graphic and data file
#' @export

baseIndustries <- function(DBPool,listID, curyr, oType,base=10){
  

  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }
  #fips is the 3-digit character string

  # creating alternative fips code for Denver MSA
  if(ctyfips %in% c("001", "005", "013", "014", "031", "035", "059")) {
    ctyfips = "500"
    ctyname = "Denver-Boulder MSA"
    fipslist = c(1, 5, 13, 14, 31, 35, 59)
  } else {
    fipslist = as.numeric(ctyfips)
  }

  jobsSQL <- paste0("SELECT * FROM estimates.base_analysis WHERE fips = '",ctyfips, "';")
  LFSQLPL <- paste0("SELECT * FROM estimates.labor_force_participation WHERE area_code in (")
  for(i in 1:length(fipslist)){
    LFSQLPL <- paste0(LFSQLPL,fipslist[i], ", ")
  }
  LFSQLPL <- substr(LFSQLPL,1,nchar(LFSQLPL)-2)
  LFSQLPL <- paste0(LFSQLPL, ");")

  # Read data files

  f.jobsBase <- dbGetQuery(DBPool, jobsSQL)
  f.LFPlace <- dbGetQuery(DBPool, LFSQLPL)
  
  # Adding Population Age 16 + and total Population for Table

  f.LFPlace <- f.LFPlace[which(f.LFPlace$population_year == curyr),c(1:6,8)]

  f.LFPlaceSum <- f.LFPlace %>%
    summarize(Pop16P = comma(ceiling(sum(cni_pop_16pl))))

  f.LFPlaceT <- as.data.frame(t(f.LFPlaceSum))
  f.LFPlaceT$Type <- "Total Population, 16+"
  f.LFPlaceT$pct <- " "
  f.LFPlaceT <-  f.LFPlaceT[,c(2,1,3)]

  # convert datasets to long

  f.jobsBaseL <- gather(f.jobsBase, industry, jobs, employment:direct_basic_emp)

  # Table data
  f.jobsBaseTab <- f.jobsBaseL[which(f.jobsBaseL$industry %in% c("employment",  "direct_basic_emp", "ib_emp", "wrkr_lrs_emp")),]




  f.jobsBaseL$industry <- if_else(f.jobsBaseL$industry == "agri_emp","Agriculture",
                                  if_else(f.jobsBaseL$industry == "mining_emp","Mining",
                                          if_else(f.jobsBaseL$industry == "manuf_emp","Manufacturing",
                                                  if_else(f.jobsBaseL$industry == "govt_emp","Government",
                                                          if_else(f.jobsBaseL$industry == "regl_serv_emp","Regional Services",
                                                                  if_else(f.jobsBaseL$industry == "tourism_emp","Tourism",
                                                                          if_else(f.jobsBaseL$industry == "other_hhd_emp","Other Household",
                                                                                  if_else(f.jobsBaseL$industry == "retiree_emp","Retiree(s)",
                                                                                          if_else(f.jobsBaseL$industry == "commuter_emp", "Commuter",
                                                                                                  if_else(f.jobsBaseL$industry == "other_inc_emp","Other Income","drop"))))))))))



  # Building Plot Data
  f.jobsBaseFin <- f.jobsBaseL[which(f.jobsBaseL$industry != "drop"),]

  f.jobsBaseFin <-  f.jobsBaseFin %>%
    mutate (prop = jobs/sum(jobs) * 100,
            pct = percent(prop))


  #sorting records
  f.jobsBaseFin <- f.jobsBaseFin[order(-f.jobsBaseFin$prop),]
  f.jobsBaseFin$industry <- factor(f.jobsBaseFin$industry,
                                   levels=f.jobsBaseFin$industry[order(-f.jobsBaseFin$prop)], ordered=TRUE)


  pltTitle <- paste0(curyr," Base Industries\n(without Indirect)")
  cPallette <- c( "Agriculture"  = "deepskyblue4",
                  "Manufacturing" = 	"deepskyblue3",
                  "Mining" =	"deepskyblue2",
                  "Government" = "darkseagreen4",
                  "Regional Services" = 	"darkseagreen3",
                  "Commuter" = "darkseagreen3",
                  "Tourism" =	"darkseagreen1",
                  "Other Household"= "gold3",
                  "Other Income" =	"gold2",
                  "Retiree(s)" =	"gold1"
  )
  
  axs <- setAxis(f.jobsBaseFin$prop)
  
  BaseBars <-  f.jobsBaseFin %>%
    ggplot(aes(x=industry, y=prop, fill=industry), color="black")+
    geom_bar(stat="identity", position="dodge")+
    geom_text(mapping=aes(x=industry, y=prop, label=pct),
              vjust = -0.75, size = 3,
              position = position_dodge(width = 1),
              inherit.aes = TRUE) +
    geom_hline(yintercept=0, size=1.05) +
    scale_fill_manual( values = cPallette) +
    scale_y_continuous(limits=c(axs$minBrk,axs$maxBrk), breaks=axs$yBrk,  label=percent) +
    theme_codemog(base_size=base) +
    labs(title = pltTitle,
         subtitle = ctyname,
         caption = captionSrc("SDO",""),
         x = "Base Indistries",
         y= "Percentage") +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          axis.text.x  = element_text(angle=90, vjust=-0.5),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          axis.text = element_text(size=12),
          legend.position= "none")


  # Table data
  f.jobsBaseTab$pos <- if_else(f.jobsBaseTab$industry  == "employment",4,
                               if_else(f.jobsBaseTab$industry == "ib_emp", 2,
                                       if_else(f.jobsBaseTab$industry == "direct_basic_emp",1,3)))

  f.jobsBaseTab$industry <- if_else(f.jobsBaseTab$industry  == "employment","Total Employment",
                                    if_else(f.jobsBaseTab$industry == "ib_emp", "Indirect Basic Employment",
                                            if_else(f.jobsBaseTab$industry == "direct_basic_emp","Direct Basic Employment","Local Services Employment")))

  totalEmp <- as.numeric(f.jobsBaseTab[1,6])

  f.jobsBaseTab$pct <- percent((f.jobsBaseTab$jobs/totalEmp)*100)
  f.jobsBaseTab$jobs <- format(round(f.jobsBaseTab$jobs,digits=0),big.mark=",")

  #Generating Table
  m.jobs <-   as.matrix(f.jobsBaseTab[order(f.jobsBaseTab$pos),c(5,6,8)])
  m.tot <- as.matrix(f.LFPlaceT)

  m.jobs <- rbind(m.jobs,m.tot)

  names_spaced <- c("Employment Type","Number of Jobs","Percentage")

  #Table Output
    jobsTabH <- m.jobs %>%
      kable(format='html', table.attr='class="cleanTable"',
            row.names=FALSE,
            align='lrr',
            caption=paste0("Jobs by Sector: ",ctyname, ", ",curyr),
            col.names = names_spaced,
            escape = FALSE)  %>%
      kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 12) %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "2in") %>%
      column_spec(2, width = "0.75in") %>%
      column_spec(3, width = "0.75in") %>%
      kableExtra::footnote(captionSrc("SDO",""))

    jobsTabL <- m.jobs%>% kable(digits=1,
                               row.names=FALSE,
                               align='lrrr',
                               caption=paste0("Jobs by Sector: ",ctyname, ", ",curyr),
                               format="latex", booktabs=TRUE,
                               col.names = names_spaced)  %>%
      kable_styling(latex_options="HOLD_position") %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width = "2in") %>%
      column_spec(2, width = "0.75in") %>%
      column_spec(3, width = "0.75in") %>%
      kableExtra::footnote(captionSrc("SDO",""))
 
  #preparing data

  f.jobsBaseFin <- f.jobsBaseFin[,c(4:6,8)]
  f.jobsBaseFin$cname <- ctyname
  f.jobsBaseFin$jobs <- format(round(f.jobsBaseFin$jobs,digits=0),big.mark=",",scientific=FALSE)
  names(f.jobsBaseFin) <- c("Geography","Industry","Jobs","Percentage")
    
  
  # Text
    OutText <- paste0("Similar to the industry employment, areas with large amounts of diversity in their base industries")
    OutText <- paste0(OutText," tend to suffer less during downturns and recover more quickly.")
    OutText <- paste0(OutText," \\textit{Regional Services} is a diverse base industry that encompasses all services and goods that a region")
    OutText <- paste0(OutText," sells to those in surrounding areas; examples include specialized health care, construction, air")
    OutText <- paste0(OutText," or rail transportation, and large item retail purchases like autos or appliances.")
    OutText <- paste0(OutText," \\textit{Retirees} are considered basic since they spend money from social security or other pensions, Medicare and savings.")
    OutText <- paste0(OutText," \\textit{Government} typically only includes employment in Federal Government and State Government.")
    OutText <- paste0(OutText," \\textit{Tourism} not only includes traditional tourist services like accommodation and food, but also includes 2nd homes,")
    OutText <- paste0(OutText," property management and transportation of tourists by airlines, car rental, car sharing and shuttles.")
    
  #FlexTable
   
    Ft <- data.frame(m.jobs)
    names(Ft) <- c("V1","V2","V3")
    Flextab <- regulartable(Ft)
    
    Flextab <- set_header_labels(Flextab, V1 = "Employment Type", 
                                  V2="Number of Jobs", V3="Percent"
    )
    
    Flextab <- add_header(Flextab,V1=paste0("Jobs by Sector: ",ctyname, ", ",curyr),top=TRUE)
    Flextab <- add_footer(Flextab,V1=captionSrc("SDO",""))
    Flextab <- merge_at(Flextab,i=1, j = 1:3, part = "header")
    Flextab <- merge_at(Flextab,i=1, j = 1:3, part = "footer")
    Flextab <- align(Flextab,i=1, align="left",part="header")
    Flextab <- align(Flextab,i=2, j=1, align="left",part="header")
    Flextab <- align(Flextab,i=2, j=2:3, align="center",part="header")
    Flextab <- align(Flextab,i=1, align="left",part="footer")
    Flextab <- align(Flextab, j=1, align="left", part="body")
    Flextab <- autofit(Flextab)
    Flextab <- width(Flextab,j=1, width=3)
    Flextab <- width(Flextab,j=2:3, width=1)
    
    f.jobsBaseTab$Geography <- ctyname
    f.jobsBaseTab <- f.jobsBaseTab[order(f.jobsBaseTab$pos),]
    f.jobsBaseTab <- f.jobsBaseTab[,c(9,5,6,8)]
    names(f.jobsBaseTab) <- c("Geography","Employment Type","Job Estimate","Percent")
    
  outList <- list("plot" = BaseBars,"Htable" = jobsTabH, "Ltable" = jobsTabL, "data1"=f.jobsBaseFin, "data2" = f.jobsBaseTab, "text" = OutText, "FlexTable" = Flextab)
  return(outList)
}
