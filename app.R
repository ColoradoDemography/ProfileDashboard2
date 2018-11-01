#' Colorado Demographic Profiles
#' @author  Adam Bickford, Colorado State Demography Office, November 2017-March 2018
#' Release Version 2.0 10/31/2018

rm(list = ls())

library(tidyverse, quietly=TRUE)
library(readr)
library(readxl, quietly=TRUE)
library(scales, quietly=TRUE)
library(codemogAPI, quietly=TRUE)
library(codemogProfile, quietly=TRUE)
library(codemogLib)
library(knitr, quietly=TRUE)
library(kableExtra, quietly=TRUE)
library(RPostgreSQL, quietly=TRUE)
library(rmarkdown)
library(shiny, quietly=TRUE)
library(shinydashboard, quietly=TRUE)
library(shinyjs, quietly=TRUE)
library(VennDiagram)
library(rgdal)
library(geojsonio)
library(gridExtra)
library(ggthemes)
library(maptools)

if (!require("officer"))
{
  install.packages("officer",dep=TRUE)
}
library(officer)


if (!require("flextable"))
{
  install.packages("flextable",dep=TRUE)
}
library(flextable)
source("R/ageForecastPRO.R")
source("R/agePlotPRO.R")
source("R/baseIndustries.R")
source("R/boxContent.R")
source("R/captionSrc.R")
source("R/chkID.R")
source("R/cocPlot.R")
source("R/codemog_cdp.r")
source("R/dashboardMAP.R")
source("R/downloadObj.R")
source("R/downloadObjUI.R")
source("R/educPRO.R")
source("R/GenerateVenn.R")
source("R/houseEstPRO.R")
source("R/housePRO.R")
source("R/HouseVal.R")
source("R/incomePRO.R")
source("R/incomeSrc.R")
source("R/jobMigration.R")
source("R/jobsByIndustry.R")
source("R/jobsPlot.R")
source("R/jobsPopForecast.R")
source("R/listTofips.R")
source("R/medianAgeTab.R")
source("R/migbyagePRO.R")
source("R/OOHouse.R")
source("R/percent.R")
source("R/pop_timeseries.R")
source("R/popForecast.R")
source("R/popPlace.R")
source("R/popTable.R")
source("R/raceTab1.R")
source("R/raceTab2.R")
source("R/residentialLF.R")
source("R/roundUpNice.R")
source("R/RTHouse.R")
source("R/setAxis.R")
source("R/setYrRange.R")
source("R/simpleCap.R")
source("R/statsTable1.R")
source("R/submitPush.R")
source("R/submitReport.R")
source("R/tabList.R")
source("R/tabTitle.R")
source("R/TempFil.R")
source("R/weeklyWages.R")




# The GLOBAL Variables  Add Additional lists items as sections get defined
# Current ACS database
curACS <- "acs1216"
curYr <- 2016
fipslist <<- ""
tDir <- tempdir()  #Setting Temporary Directory location for Reporting

#Basic Statistics
stats.list <<- list()

# Population Change
popa1 <<- list()
popa2 <<- list()
popa3 <<- list()
popa4 <<- list()
popa.list <<- list()

#Population Forecast
popf1 <<- list()
popf2 <<- list()
popf3 <<- list()
popf4 <<- list()
popf.list <<- list()


#Population Characteristics
popc1 <<- list()
popc2 <<- list()
popc3 <<- list()
popc4 <<- list()
popc.list <<- list()

#Housing and Household Characteristics
poph1 <<- list()
poph2 <<- list()
poph3 <<- list()
poph4 <<- list()
poph5 <<- list() # Housing Values
poph.list <<- list()

#Commuting (Transit)
popt1 <<- list()
popt2 <<- list()  # This is for the jobs and Migration chart
popt.list <<- list()

#Employment by Industry
popei1 <<- list()
popei2 <<- list()
popei3 <<- list()
popei.list <<- list()

#Employment and Demographic Forecast
popem1 <<- list()
popem2 <<- list()
popem3 <<- list()
popem4 <<- list()
popem.list <<- list()

#Prepping Matrix of filenames
fileMat <- TempFil()

# Structure of user Interface
ui <-
  dashboardPage( skin="green", 
                 title= "Colorado Demographic Profiles",
                 dashboardHeader(title = span(img(src="ShieldOnly_LRG.png", height = 70, align = "top"),"Colorado Demographic Profiles"), titleWidth=550), #dashboardHeader
                 dashboardSidebar( width = 300,  useShinyjs(),
                                   # data level Drop down
                                   selectInput("level", "Select Data Level" ,
                                               choices=c("Select a Data Level","Counties","Municipalities")  #Enabled in V1
                                   ),
                                   
                                   # profile Unit dropdown
                                   selectInput("unit", "Select Location" ,choices=""),
                                   # Comparison dropdown 1  Disabled in V1
                                   #   selectizeInput("comp", "Select Comparison" ,choices=""),
                                   #   # Comparison dropdown 2
                                   #    selectizeInput("comp2","Select Custom Comparisons",choices ="", multiple=TRUE),  Disabled in V1
                                   #Output Content Checkboxes
                                   checkboxGroupInput("outChk", "Select the Data Elements to display:",
                                                      choices = c("Basic Statistics" = "stats",
                                                                  "Population Trends" = "popf",
                                                                  "Population Characteristics: Age" = "pop",
                                                                  "Population Characteristics: Income, Education and Race"= "popc",
                                                                  "Housing and Households" = "housing",
                                                                  "Commuting and Job Growth" = "comm",
                                                                  "Employment by Industry"="emplind",
                                                                  "Employment Forecast and Wage Information"="emply"
                                                      ),
                                                      selected =  c("stats","popf","pop","popc",
                                                                    "housing","comm", "emplind","emply")
                                   ),
                                   
                                   #Action Button
                                   actionButton("profile","View Profile"),
                                   #   actionButton("comparison","View Comparison"),  Disabled in V1
                                   actionButton("contact","Contact SDO",onclick ="window.open('https://goo.gl/forms/xvyxzq6DGD46rMo42', '_blank')"),
                                   downloadButton("outputPDF", label="Download PDF Report",
                                                  style="color: black; background-color: gray90; border-color: black")
                                   
                                   
                 ), #dashboardSidebar
                 dashboardBody(  tags$head( 
                   tags$meta(name="keywords", content="Colorado, demographic, county, community, municiplaity, city, population, housing, household, age, median income, jobs, wages"),
                   includeScript("www/dL_init.js"),
                   includeScript("www/tag_manager.js"), #writes GTM connection
                   tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css"),  #Link to CSS...
                   tags$title("Colorado Demographic Profiles") #,
                   # includeScript("www/dataL.js") # This is the linkage to the dataLayer Output code
                 ),
                 tags$body(includeHTML("www/tag_body.js")),  # for non-JS instances
                 tags$style(HTML("
                                 .box.box-solid.box-primary>.box-header {
                                 color:#fffff;
                                 background:#C9C6C5
                                 }
                                 .box.box-solid.box-primary{
                                 color: #ffffff;
                                 border-bottom-color:#C9C6C5;
                                 border-left-color:#C9C6C5;
                                 border-right-color:#C9C6C5;
                                 border-top-color:#C9C6C5;
                                 }   ")),
                fluidRow(uiOutput("ui")
                 )
                
                 ) #dashboardBody
                
                 ) # dashboardPage/ui


# Server Management Function
server <- function(input, output, session) {
  infoSrc <- matrix(" ",nrow=8,ncol=2)
  infoSrc[1,1] <- "<b>Basic Statistics</b>"
  infoSrc[1,2] <- "Summary Table and Map"
  infoSrc[2,1] <- "<b>Population Trends</b>"
  infoSrc[2,2] <- "Population Estimates and Forecasts"
  infoSrc[3,1] <- "<b>Population Characteristics: Age</b>"
  infoSrc[3,2] <- "Population Estimates and Migration by Age"
  infoSrc[4,1] <- "<b>Population Characteristics: Income, Education and Race</b>"
  infoSrc[4,2] <- "Population Estimates by Income, Educational Attainment and Race"
  infoSrc[5,1] <- "<b>Housing and Households</b>"
  infoSrc[5,2] <- "Housing Units, Costs and Unit Characteristics"
  infoSrc[6,1] <- "<b>Commuting and Job Growth</b>"
  infoSrc[6,2] <- "Commuting Patterns and Job Growth and Migration"
  infoSrc[7,1] <- "<b>Employment by Industry</b>"
  infoSrc[7,2] <- "Employment Data by Industry"
  infoSrc[8,1] <- "<b>Employment Forecast and Wage Information</b>"
  infoSrc[8,2] <- "Employment Forecasts, Wage and Income Sources"
  
  infoTab <-  kable(infoSrc, format='html', table.attr='class="cleanTab"',align='l',linesep = "") %>%
    kable_styling(bootstrap_options ="condensed", full_width = F) %>%
    column_spec(1, width = "4in") 
  infoTab <- gsub("&lt;","<",infoTab)
  infoTab <- gsub("&gt;",">",infoTab)
  
  #Creating data Source Links Table
  linkSrc <- matrix(" ", nrow=6, ncol=5)
  linkSrc[1,1]  <- "<b>Data Dashboard</b>"
  linkSrc[2,1]  <- "<a href='https://gis.dola.colorado.gov/apps/demographic_dashboard/' target='_blank'>Demographic Dashboard</a>"
  linkSrc[3,1]  <- "<a href='https://gis.dola.colorado.gov/apps/netmigration_dashboard/' target='_blank'>Net Migration Dashboard</a>"
  
  linkSrc[4,1] <- "<b>Data Lookup Pages</b>"
  linkSrc[5,1] <- "<a href='https://demography.dola.colorado.gov/population/data/profile-county/' target='_blank'>County Data Lookup</a>"
  linkSrc[6,1] <- "<a href='https://demography.dola.colorado.gov/population/data/profile-regions/' target='_blank'>Regional Data Lookup</a>"
  
  linkSrc[1,2]  <- "<b>Maps and GIS data</b>" 
  linkSrc[2,2]  <- "<a href='https://demography.dola.colorado.gov/gis/map-gallery/' target='_blank'>Interactive Map Gallery</a>"
  linkSrc[3,2]  <- "<a href='https://demography.dola.colorado.gov/gis/thematic-maps/#thematic-maps' target='_blank'>Thematic Maps</a>"
  linkSrc[4,2]  <- "<a href='https://demography.dola.colorado.gov/demography/region-reports-2014/#colorado-planning-region-reports' target='_blank'>Region Reports</a>"
  linkSrc[5,2] <- "<a href='https://demography.dola.colorado.gov/gis/gis-data/#gis-data' target='_blank'>GIS Data Downloads</a>"
  linkSrc[6,2] <- "<a href='https://demography.dola.colorado.gov/gis/gis-data/#gis-data' target='_blank'>Links to GIS Data and DOLA Grants</a>"
  
  
  linkSrc[1,3]  <- "<b>Population Data</b>"
  linkSrc[2,3]  <- "<a href='https://demography.dola.colorado.gov/population/' target='_blank'>Population Estimates and Forecasts</a>"
  linkSrc[3,3]  <- "<a href='https://demography.dola.colorado.gov/births-deaths-migration/' target='_blank'>Births Deaths and Migration</a>"
  linkSrc[4,3]  <- "<a href='https://demography.dola.colorado.gov/economy-labor-force/' target='_blank'>Economy and Labor Force</a>"
  linkSrc[5,3]  <- "<a href='https://demography.dola.colorado.gov/housing-and-households/' target='_blank','>Housing and Households</a>"
  
  linkSrc[1,4] <- "<b>Census and ACS Data</b>"
  linkSrc[2,4] <- "<a href='https://demography.dola.colorado.gov/data/#census-data-tools' target='_blank'>Census Data Tools</a>"
  linkSrc[3,4] <- "<a href='https://demography.dola.colorado.gov/census-acs/' target='_blank'>Census Data Page</a>"
  linkSrc[1,5]  <- "<b>Publications</b>"
  linkSrc[2,5]  <- "<a href='https://demography.dola.colorado.gov/demography/publications-and-presentations/#publications-and-presentations' target='_blank'>Publications and Reports</a>"
  linkSrc[3,5]  <- "<a href='https://demography.dola.colorado.gov/crosstabs/' target='_blank'>Crosstabs</a>"
  linkSrc[4,5]  <- "<a href='https://demography.dola.colorado.gov/demography/publications-and-presentations/#annual-demography-summit-2017' target='_blank'>Annual Summit</a>"
  
  
  linkTab <-  kable(linkSrc, format='html', table.attr='class="cleanTab"',align='l',linesep = "") %>%
    kable_styling(bootstrap_options ="condensed") %>%
    column_spec(1, width = "2.25in") %>%
    column_spec(2, width = "2.25in") %>%
    column_spec(3, width = "2.25in") %>%
    column_spec(4, width = "2.25in") %>%
    column_spec(5, width = "2.25in")
  
  linkTab <- gsub("&lt;","<",linkTab)
  linkTab <- gsub("&gt;",">",linkTab)
  
  frontPgBox1 <- box(width=11,tags$div(tags$b("Welcome to the State Demography Office (SDO) Colorado Demographic Profiles Website"), tags$br(),
                                       "This tool provides summary plots and data describing Counties and Incorporated Municipalities in Colorado.", tags$br(),
                                       tags$em("Profile Contents:"),
                                       HTML(infoTab),
                                       "To create a profile:",tags$br(),
                                       tags$ul(
                                         tags$li("Select a Data Level and Location using the dropdown boxes."),
                                         tags$li("Select specific Data Elements to display using the checkboxes."),
                                         tags$li("Click on the 'View Profile' button to display the selected profile.")
                                       ), 
                                       "You can download the plots and underlying data for each display by selecting the 'Sources and Downloads' 
                                       panel of each display box.", tags$br(),
                                       tags$em(tags$b("Notes:")), tags$br(), 
                                       tags$ul(
                                         tags$li("Profiles are available for Counties and Incorporated Municipalites.  
                                                 Please contact SDO for information on other geographies and places."),
                                         tags$li("Producing the requested outputs may take up to 3 minutes, depending on your request and your connection speed."),
                                         tags$li("Downloading any report, plot or data object will open a new browser window while the 
                                                 object is being processed and downloaded.  This window will close once the object processing is completed."),
                                         tags$li("Downloaded objects will be saved in the 'Download' location supported by your browser.")
                                         )))
  frontPgBox2 <-  box(width=11, tags$div(
    tags$b("Links to other SDO Data Sources:"),
    HTML(linkTab)))
  
  frontPg <- list(frontPgBox1,frontPgBox2)
  shinyjs::hide("outputPDF")
  
  output$ui <- renderUI(frontPg)
  # updates Dropdown boxes and selects data level and unit
  CountyList <- popPlace("Counties",curYr)
  PlaceList <- popPlace("Municipalities",curYr)
  #RegionList <- popPlace("Region",curYr)

  observeEvent(input$level, ({
    shinyjs::hide("outputPDF")
    
    #clears the comp2 dropdown on change
    updateSelectInput(session, "comp2", choices = "")
    if(input$level == "Select a Data Level") { #the initial state of the dropdowns
      outUnit <- ""
      outComp <- ""
    }
  #  if(input$level == "Region") {  # Added 9/18
  #           outUnit <-  RegionList
  #        }
    if(input$level == "Counties") {
      outUnit <- unique(as.list(CountyList[,3]))
  #    outComp <- c("Selected County Only", "Counties in Planning Region", "Custom List of Counties (Select Below)","State")
    }
    if(input$level == "Municipalities") {  
      outUnit <- unique(as.list(PlaceList[,3]))
  #    outComp <- c("Selected Municipality Only", "Similar Municipalities", "County", "Custom List of Municipalities (Select Below)", "State")
    }
    
    updateSelectInput(session, "unit", choices = outUnit)
    #    updateSelectInput(session, "comp", choices = outComp)
  }))  #observeEvent input$level
  
  # Event for Comparison selection
  observeEvent(input$comp, {
    shinyjs::hide("outputPDF")
    
    if((input$level == "Counties") && (input$comp == "Custom List of Counties (Select Below)")){
      # Creating custom list
      custList <- as.list(CountyList[which(CountyList$municipalityname != input$unit),3])
      updateSelectInput(session, "comp2", choices = custList)
    }
    # Disabled in V1
    #                if((input$level == "Municipalities") && (input$comp == "Custom List of Municipalities (Select Below)")){
    #                  # Creating custom list
    #                  custList <- as.list(unique(PlaceList[which(PlaceList$municipalityname != input$unit),3]))
    #                  updateSelectInput(session, "comp2", choices = custList)
    #                }
  }) #observeEvent input$comp
  
  # Event for click on profile button
  observeEvent(input$profile,  {

    shinyjs::hide("outputPDF")
 
    dLout <- submitPush(input$level,input$unit,input$outChk)  # Generate dataLayer Command
    session$sendCustomMessage("handler1",dLout)  #Sends dataLayer command to dataL.js script
    
    outputList <<- list()
    output$ui <- renderUI(outputList)
    
    #creating the input FIPS list to generate data
    if(input$unit == "") {
      lnError <- tags$h2("Please specify a Data Level and a Profile to display")
      outputList <<- list(lnError)
    }  else {
      withProgress(message = 'Generating Profile', value = 0, {  # Initialize Progress bar
        #Building fipslist
        if(input$level == "Counties") {
          fipslist <<- listTofips(CountyList,input$level,input$unit)
        } 
        if(input$level == "Municipalities") { 
          fipslist <<- listTofips(PlaceList,input$level,input$unit)
        }
        
        if(input$level == "Region") {  
          fipslist <<- listTofips(RegionList,input$level,input$unit)
        }
        #Generate profile UI objects
        
        svals <- reactiveValues(a=NULL,b=NULL,c=NULL)
        placeName <- simpleCap(input$unit)
        ln1 <- tags$h1(placeName)
        #creating ids and output flags for multiple counties and small places
        idList <- chkID(lvl=input$level,fipslist= fipslist,plName=placeName,ctyList=CountyList, plList=PlaceList)
        
        #stats; Basic Statistics
        if("stats" %in% input$outChk) {
          stats.text <- tags$h2("Basic Statistics")
          stat_List <- statsTable1(lvl=input$level,listID=idList,sYr=2010,eYr=curYr,ACS=curACS)
          stat_map <- dashboardMAP(lvl=input$level,listID=idList)
          # creating output files
          #HTML table
          dput(stat_List$Htable,fileMat[1])

          #Latex File
          dput(stat_List$Ltable, fileMat[2])
          
          #Plain Text
          dput(stat_List$text,fileMat[3])
          
          #Images
          ggsave(fileMat[4],stat_map, device="png")
          ggsave(fileMat[5],stat_map, device="png")
          
          img_List1 <- list(src = fileMat[4], contentType = 'image/png', width = 500, height = 300)
          
          Stats.info <- tags$div(class="dInfo","Individual plots and data may be downloaded by selecting the 'Sources and Downloads' tabl in each display box.",tags$br(),
                                 "Note: County data is displayed for municipalities and places with fewer than 200 people.",tags$br(), tags$br(),
                                 "General information is available here:", tags$br(),
                                 tags$ul(
                                   tags$li(tags$a(href="https://demography.dola.colorado.gov/data/","State Demography Office Data",target="_blank")),
                                   tags$li(tags$a(href="https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml","U.S. Census Bureau American Community Survey",target="_blank"),
                                   tags$br(),tags$br(),downloadObjUI("statstabl")
                                   )))
          
          stats.box0 <- box(width=12,ln1)
          stats.box1 <- tabBox(width=8, height=350,
                               tabPanel("Table",tags$div(class="Row1Tab",HTML(dget(fileMat[1])))),
                               tabPanel("Information",Stats.info))
          stats.box2 <- box(width=4, height=350,renderImage({img_List1}))
          
          
          #building List
          stats.list <<- list(stats.box0, stats.box1, stats.box2)
          
          incProgress()
        }
        # Population Forecasts
        
        
        if("popf" %in% input$outChk){
          #Chart/Table Objects
          popf1 <<- popTable(lvl=input$level,listID=idList,sYr=1990,eYr=curYr)
          popf2 <<- pop_timeseries(lvl=input$level,listID=idList,endyear=curYr,base=12)
          popf3 <<- popForecast(listID=idList)
          popf4 <<- cocPlot(listID=idList,lyr=curYr)
          
          # creating output files
          #HTML table
          dput(popf1$Htable,fileMat[6])

          #Latex File
          dput(popf1$Ltable, fileMat[7])
          
          #Plain Text
          dput(popf1$text,fileMat[8])
          
          #Images
          ggsave(fileMat[9],popf2$plot, device="png")
          ggsave(fileMat[10],popf2$plot, device="png")
          ggsave(fileMat[11],popf3$plot, device="png")
          ggsave(fileMat[12],popf3$plot, device="png")
          
          dput(popf3$text,fileMat[13])
          ggsave(fileMat[14],popf4$plot, device="png")
          ggsave(fileMat[15],popf4$plot, device="png")
          dput(popf4$text,fileMat[16])
        
          
          img_List2 <- list(src = fileMat[9], contentType = 'image/png', width = 500, height = 300)
          img_List3 <- list(src = fileMat[11], contentType = 'image/png', width = 500, height = 300)
          img_List4 <- list(src = fileMat[14], contentType = 'image/png', width = 500, height = 300)
          
          
          #infobox Objects
          if(input$level == "Counties") {
            popf1.info <- tags$div(boxContent(title= "Population Growth Estimates",
                                              description = "The Population Growth Table compares population growth for a place to the State.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                              urlList = list(c("SDO Demographic Profiles -County","https://demography.dola.colorado.gov/population/data/profile-county/")) ),
                                   tags$br(),
                                   downloadObjUI("popf1tabl"),downloadObjUI("popf1data"))
            
            popf2.info <- tags$div(boxContent(title= "Population Growth Data",
                                              description = "The Population Growth Plot shows the growth of the total population for a selected location.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                              urlList = list(c("SDO Demographic Profiles -County","https://demography.dola.colorado.gov/population/data/profile-county/")) ),
                                   tags$br(), downloadObjUI("popf2plot"),  downloadObjUI("popf2data"))
            
            popf3.info <- tags$div(boxContent(title= "Population Forecast",
                                              description = "The Population Forecast plot shows the estimated population growth between 2010 and 2025 for the selected county.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                              urlList = list(c("SDO Population Totals for Colorado Counties","https://demography.dola.colorado.gov/population/population-totals-counties/#population-totals-for-colorado-counties")) ),
                                   tags$br(), downloadObjUI("popf3plot"), downloadObjUI("popf3data"))
            
            popf4.info <- tags$div(boxContent(title= "Components of Change",
                                              description = "The Components of Change Plot shows the estimated births, deaths and net migration values for a selected place between 2010 and the present.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                              urlList=list(c("SDO Components of Change Estimates","https://demography.dola.colorado.gov/births-deaths-migration/data/components-change/"))),
                                   tags$br(), downloadObjUI("popf4plot"), downloadObjUI("popf4data"))
          }
          
          if(input$level == "Municipalities") {
            popf1.info <- tags$div(boxContent(title= "Population Growth Estimates",
                                              description = "The Population Growth Table compares population growth for a place to the State.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = "F", 
                                              urlList = list(c("SDO Demographic Profiles -County","https://demography.dola.colorado.gov/population/data/profile-county/")) ),
                                   tags$br(),
                                   downloadObjUI("popf1tabl"),downloadObjUI("popf1data"))
            
            popf2.info <- tags$div(boxContent(title= "Population Growth Data",
                                              description = "The Population Growth Plot shows the growth of the total population for a selected location.",
                                              MSA= "F", stats = "F", muni = "T", multiCty = idList$multiCty, PlFilter = "F", 
                                              urlList = list(c("SDO Demographic Profiles -County","https://demography.dola.colorado.gov/population/data/profile-county/")) ),
                                   tags$br(), downloadObjUI("popf2plot"),  downloadObjUI("popf2data"))
            
            popf3.info <- tags$div(boxContent(title= "Population Forecast",
                                              description = "The Population Forecast plot shows the estimated population growth between 2010 and 2025 for the selected county.",
                                              MSA= "F", stats = "F", muni = "T", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                              urlList = list(c("SDO Population Totals for Colorado Counties","https://demography.dola.colorado.gov/population/population-totals-counties/#population-totals-for-colorado-counties")) ),
                                   tags$br(), downloadObjUI("popf3plot"), downloadObjUI("popf3data"))
            
            popf4.info <- tags$div(boxContent(title= "Components of Change",
                                              description = "The Components of Change Plot shows the estimated births, deaths and net migration values for a selected place between 2010 and the present.",
                                              MSA= "F", stats = "F", muni = "T", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                              urlList=list(c("SDO Components of Change Estimates","https://demography.dola.colorado.gov/births-deaths-migration/data/components-change/"))),
                                   tags$br(), downloadObjUI("popf4plot"), downloadObjUI("popf4data"))
          }
          # Bind to boxes
          popf1.box <- tabBox(width=6, height=400,
                              tabPanel("Table",tags$div(class="cleanTab", HTML(dget(fileMat[6])))),
                              tabPanel("Sources and Downloads",popf1.info))
          popf2.box <- tabBox(width=6, height=400,
                              tabPanel("Plot", renderImage({img_List2})),
                              tabPanel("Sources and Downloads",popf2.info))
          popf3.box <- tabBox(width=6, height=400,
                              tabPanel("Plot",renderImage({img_List3})),
                              tabPanel("Sources and Downloads", popf3.info))
          popf4.box <- tabBox(width=6, height=400,
                              tabPanel("Plot",renderImage({img_List4})),
                              tabPanel("Sources and Downloads",popf4.info))
          
          
          #Append to List
          popf.list <<- list(popf1.box,popf2.box,popf3.box,popf4.box)
          incProgress()
        }  # popf
        
        
        #pop: Population Table, County Time Series, Population by Age, Median Age
        if("pop" %in% input$outChk){
          #Generate tables, plots and text...
          
          popa1 <<- agePlotPRO(listID=idList, ACS=curACS, yrs=curYr)
          popa2 <<- medianAgeTab(listID=idList, ACS=curACS)
          popa3 <<- ageForecastPRO(listID=idList,sYr=2010,mYr=2015,eYr=2025,base=12)
          popa4 <<- migbyagePRO(listID=idList)
          
          ggsave(fileMat[17],popa1$plot, device="png")
          ggsave(fileMat[18],popa1$plot, device="png")
          dput(popa1$text,fileMat[19])
          
          ggsave(fileMat[20],popa2$plot, device="png")
          ggsave(fileMat[21],popa2$plot, device="png")
          
          dput(popa2$Htable,fileMat[22])

          dput(popa2$Ltable, fileMat[23])
          dput(popa2$text, fileMat[24])
          
          ggsave(fileMat[25],popa3$plot, device="png")
          ggsave(fileMat[26],popa3$plot, device="png")
          dput(popa3$text,fileMat[27])
          
          ggsave(fileMat[28],popa4$plot, device="png")
          ggsave(fileMat[29],popa4$plot, device="png")
          dput(popa4$text,fileMat[30])
          
          img_List5 <- list(src = fileMat[17], contentType = 'image/png', width = 500, height = 300)
          img_List6 <- list(src = fileMat[20], contentType = 'image/png', width = 500, height = 300)
          img_List7 <- list(src = fileMat[25], contentType = 'image/png', width = 500, height = 300)
          img_List8 <- list(src = fileMat[28], contentType = 'image.png', width = 500, height = 300)
          
          
          #Info Boxes
          if(input$level == "Counties") {   
            popa1.info <- tags$div(boxContent(title= "Population by Age",
                                              description = "The Population by Age Plot displays age categories for a single year.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                              urlList = list(c("SDO County Single-Year of Age Forecasts","https://demography.dola.colorado.gov/population/data/sya-county/"),
                                                             c("SDO Age Visualization Chart","https://demography.dola.colorado.gov/Age-Animation-Bars/")) ),
                                   tags$br(),
                                   downloadObjUI("popa1plot"), downloadObjUI("popa1data"))
            
            popa2.info <- tags$div(boxContent(title= "Age by Gender, Median Age Data",
                                              description = "The Age by Gender Plor and Median Age Table compares the median age by gender for a location to the state.",
                                              MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                              urlList = list(c("American Community Survey American Fact Finder, Series B01002","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                   tags$br(),
                                   downloadObjUI("popa2plot"),downloadObjUI("popa2tabl"),downloadObjUI("popa2data"))
            
            popa3.info <- tags$div(boxContent(title= "Population Forecast by Age",
                                              description = "The Population Forecast by Age Plot displays the age distribution between 2010 and 2025.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                              urlList = list(c("SDO County Single-Year of Age Forecasts","https://demography.dola.colorado.gov/population/data/sya-county/")) ),
                                   tags$br(),
                                   downloadObjUI("popa3plot"), downloadObjUI("popa3data"))
            
            popa4.info <- tags$div(boxContent(title= "Net Migration by Age",
                                              description = "The Net Migration by Age Plot compares the net migration rate by age group between 2000 and 2010 for a selected place and the state ",
                                              MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                              urlList = list(c("SDO Net Migration by Age Comparison","https://gis.dola.colorado.gov/apps/netmigration_dashboard/")) ),
                                   tags$br(),
                                   downloadObjUI("popa4plot"), downloadObjUI("popa4data"))
          }
          
          if(input$level == "Municipalities") {          
            popa1.info <- tags$div(boxContent(title= "Population by Age",
                                              description = "The Population by Age Plot displays age categories for a single year.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = "F", 
                                              urlList = list(c("American Community Survey American Fact Finder, Series B01001","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                   tags$br(),
                                   downloadObjUI("popa1plot"), downloadObjUI("popa1data"))
            
            popa2.info <- tags$div(boxContent(title= "Age by Gender, Median Age Data",
                                              description = "The Age by Gender Plor and Median Age Table compares the median age by gender for a location to the state.",
                                              MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                              urlList = list(c("American Community Survey American Fact Finder, Series B01002","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                   tags$br(),
                                   downloadObjUI("popa2plot"),downloadObjUI("popa2tabl"),downloadObjUI("popa2data"))
            
            popa3.info <- tags$div(boxContent(title= "Population Forecast by Age",
                                              description = "The Population Forecast by Age Plot displays the age distribution between 2010 and 2025 .",
                                              MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                              urlList = list(c("SDO County Single-Year of Age Forecasts","https://demography.dola.colorado.gov/population/data/sya-county/")) ),
                                   tags$br(),
                                   downloadObjUI("popa3plot"), downloadObjUI("popa3data"))
            
            popa4.info <- tags$div(boxContent(title= "Net Migration by Age",
                                              description = "The Net Migration by Age Plot compares the net migration rate by age group between 2000 and 2010 for a selected place and the state ",
                                              MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                              urlList = list(c("SDO Net Migration by Age Comparison","https://gis.dola.colorado.gov/apps/netmigration_dashboard/")) ),
                                   tags$br(),
                                   downloadObjUI("popa4plot"), downloadObjUI("popa4data"))
          }
          
          # Bind to boxes
          popa1.box <- tabBox(width=6, height=400,
                              tabPanel("Plot", renderImage({img_List5})),
                              tabPanel("Sources and Downloads",popa1.info))
          popa2.box <- tabBox(width=6, height=400,
                              tabPanel("Plot", renderImage({img_List6})),
                         #    tabPanel("Table", tags$div(class="cleanTab", HTML(popa2$table))),
                              tabPanel("Sources and Downloads",popa2.info))
          popa3.box <- tabBox(width=6, height=400,
                              tabPanel("Plot",renderImage({img_List7})),
                              tabPanel("Sources and Downloads", popa3.info))
          popa4.box <- tabBox(width=6, height=400,
                              tabPanel("Plot",renderImage({img_List8})),
                              tabPanel("Sources and Downloads",popa4.info))
          
          
          #Append to List
          popa.list <<- list(popa1.box,popa2.box,popa3.box,popa4.box)
          incProgress()
        }
        
        
        # Population Chatacteristics
        if("popc" %in% input$outChk){
          #Generate tables, plots and text...
          popc1 <<- incomePRO(listID=idList, ACS=curACS)
          popc2 <<- educPRO(listID=idList, ACS=curACS)
          popc3 <<- raceTab1(listID=idList, ACS=curACS)
          popc4 <<- raceTab2(listID=idList, ACS=curACS)
          
          #Income
          ggsave(fileMat[31],popc1$plot, device="png")
          ggsave(fileMat[32],popc1$plot, device="png")
          dput(popc1$text,fileMat[33])
          
          # Education
          ggsave(fileMat[34],popc2$plot, device="png")
          ggsave(fileMat[35],popc2$plot, device="png")
          
          #Race 1
          dput(popc3$Htable,fileMat[36])

          dput(popc3$Ltable, fileMat[37])
          dput(popc3$text, fileMat[38])
          
          #Race 2
          dput(popc4$Htable,fileMat[39])

          dput(popc4$Ltable, fileMat[40])
          dput(popc4$text, fileMat[41])
          
          img_List9 <- list(src = fileMat[31], contentType = 'image/png', width = 500, height = 300)
          img_List10 <- list(src = fileMat[34], contentType = 'image/png', width = 500, height = 300)
          
          #Contents of Information Tabs
          popc1.info <- tags$div(boxContent(title= "Household Income",
                                            description = "The Household Income Disctibution Plot compares the distribution of household income for a selected location to the state.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = "F", 
                                            urlList = list(c("SDO American Community Survey API","http://coloradodemography.github.io/CensusAPI/"),
                                                           c("American Community Survey American Fact Finder, Series B19001","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("popc1plot"),  downloadObjUI("popc1data"))
          
          popc2.info <- tags$div(boxContent(title= "Education Attainment",
                                            description= "The Educational Attainment Plot compares the categories of educational attaiment for adults aged 25 and older for a selected location to the State.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = "F", 
                                            urlList = list(c("SDO American Community Survey API","http://coloradodemography.github.io/CensusAPI/"),
                                                           c("American Community Survey American Fact Finder, Series B15003","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("popc2plot"),  downloadObjUI("popc2data"))
          
          
          popc3.info <- tags$div(boxContent(title= "Racial Identification Trend",
                                            description= "The Race Trend Table shows changes in the distribution of racial idenification since the 2000 Census.",
                                            MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = "F", 
                                            urlList = list(c("SDO American Community Survey API","http://coloradodemography.github.io/CensusAPI/"),
                                                           c("American Community Survey American Fact Finder, Series B03002","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("popc3tabl"),downloadObjUI("popc3data"))
          
          popc4.info <- tags$div(boxContent(title= "Racial Identification Comparison",
                                            description= "The Race Comparison Table compares the distribution of racial idenification of a place to the State.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = "F", 
                                            urlList = list(c("SDO American Community Survey API","http://coloradodemography.github.io/CensusAPI/"),
                                                           c("American Community Survey American Fact Finder, series B03002","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("popc4tabl"),downloadObjUI("popc4data"))
          
          
          # Bind to boxes
          popc1.box <- tabBox(width=6, height=400,
                              tabPanel("Plot",renderImage({img_List9})),
                              tabPanel("Sources and Downloads",popc1.info))
          popc2.box <- tabBox(width=6, height=400,
                              tabPanel("Plot",renderImage({img_List10})),
                              tabPanel("Sources and Downloads",popc2.info))
          popc3.box <- tabBox(width=6, height=500,
                              tabPanel("Table",tags$div(class="cleanTab",HTML(dget(fileMat[36])))),
                              tabPanel("Sources and Downloads",popc3.info))
          popc4.box <- tabBox(width=6, height=500,
                              tabPanel("Table",tags$div(class="cleanTab",HTML(dget(fileMat[39])))),
                              tabPanel("Sources and Downloads",popc4.info))
          
          
          #Append to List
          popc.list <<- list(popc1.box,popc2.box,popc3.box,popc4.box)
          incProgress()
        }
        
        # Housing
        if("housing" %in% input$outChk){
          #Generate tables, plots and text...
          poph1 <<- houseEstPRO(listID=idList,curYr=curYr)
          poph2 <<- housePRO(listID=idList, ACS=curACS) # Housing Unit Table
          poph3 <<- OOHouse(listID=idList,ACS=curACS)  # Chars of Owner Occupied Housing
          poph4 <<- RTHouse(listID=idList,ACS=curACS)  # Chars of Rental Housing
          poph5 <<- HouseVal(listID=idList,ACS=curACS) # Comparative Value of Housing both OO and Rental

          #Housing Estimate
          ggsave(fileMat[42],poph1$plot, device="png")
          ggsave(fileMat[43],poph1$plot, device="png")
          dput(poph1$text,fileMat[44])
          
          img_List11 <- list(src = fileMat[42], contentType = 'image/png', width = 500, height = 300)
          
          #Housing Unit Table
          dput(poph2$Htable,fileMat[45])

          dput(poph2$Ltable, fileMat[46])
          
          #Housing Value Owner Occupied
          dput(poph5$HtableOO,fileMat[47])

          dput(poph5$LtableOO, fileMat[48])
   
          #Housing Value Rental
          dput(poph5$HtableRT,fileMat[49])

          dput(poph5$LtableRT, fileMat[50])
          
          #Housing Characteristics Owner Occupied
          dput(poph3$Htable,fileMat[51])

          dput(poph3$Ltable, fileMat[52])
          
          #Housing Value Rental
          dput(poph4$Htable,fileMat[53])

          dput(poph4$Ltable, fileMat[54])
          
          #Contents of Information Tabs
          poph1.info <- tags$div(boxContent(title= "Household Projection",
                                            description = "The household projection displays the estimated number of households between 2010 and 2050.",
                                            MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = "F", 
                                            urlList = list(c("SDO Household Projections --County","https://demography.dola.colorado.gov/housing-and-households/data/household-projections/")) ),
                                 tags$br(),
                                 downloadObjUI("poph1plot"),  downloadObjUI("poph1data"))
          
          poph2.info <- tags$div(boxContent(title= "Housing Type Table",
                                            description= "The Housing Type Table compares the categories of housing types for a selected place to the State.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = "F", 
                                            urlList = list(c("SDO Housing Time Series","https://demography.dola.colorado.gov/population/data/muni-pop-housing/"),
                                                           c("American Community Survey American Fact Finder, Series B25001, B25003, and B25004","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("poph2tabl"),downloadObjUI("poph2data"))
          
          
          poph3.info <- tags$div(boxContent(title= "Characteristics of Owner-Occupied Housing",
                                            description= "The Owner-Occupied Housing Table displays the characteristics of owner-occupied housing in a selected place.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = "F", 
                                            urlList = list(c("American Community Survey American Fact Finder, Series B25010, B25032, B25033, and B25037","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("poph3tabl"),downloadObjUI("poph3data"))
          
          poph4.info <- tags$div(boxContent(title= "Characteristics of Rental Housing",
                                            description= "The Rental Housing Table displays the characteristics of rental housing in a selected place.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = "F", 
                                            urlList = list(c("American Community Survey American Fact Finder, Series B25010, B25032, B25033, and B25037","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("poph4tabl"),downloadObjUI("poph4data"))
          poph5.info <- tags$div(boxContent(title= "Comparative Owner-Occupied Housing Values",
                                            description= "The Comparative Housing Table compares the economic characteristics of  owner-occupied and rental housing in a selected place to the State.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = "F", 
                                            urlList = list(c("American Community Survey American Fact Finder, Series B25077 and B25092","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("poph5tabl"),downloadObjUI("poph5data"))
          
          poph6.info <- tags$div(boxContent(title= "Comparative Rental Housing Values",
                                            description= "The Comparative Housing Table compares the economic characteristics of  owner-occupied and rental housing in a selected place to the State.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = idList$multiCty, PlFilter = "F", 
                                            urlList = list(c("American Community Survey American Fact Finder, Series B25066 and B25071","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("poph6tabl"),downloadObjUI("poph6data"))
          
          
          # Bind to boxes
          poph1.box <- tabBox(width=6, height=400,
                              tabPanel("Plot",renderImage({img_List11})),
                              tabPanel("Sources and Downloads",poph1.info))
          poph2.box <- tabBox(width=6, height=400,
                              tabPanel("Table",tags$div(class="cleanTab",HTML(dget(fileMat[45])))),
                              tabPanel("Sources and Downloads",poph2.info))
          poph5.box <- tabBox(width=6, height = 325,
                              tabPanel("Table",tags$div(class="cleanTab",HTML(dget(fileMat[47])))),
                              tabPanel("Sources and Downloads",poph5.info))
          poph6.box <- tabBox(width=6, height = 325,
                              tabPanel("Table",tags$div(class="cleanTab",HTML(dget(fileMat[49])))),
                              tabPanel("Sources and Downloads",poph6.info))
          poph3.box <- tabBox(width=6, height=350,
                              tabPanel("Table",tags$div(class="cleanTab",HTML(dget(fileMat[51])))),
                              tabPanel("Sources and Downloads",poph3.info))
          poph4.box <- tabBox(width=6, height=350,
                              tabPanel("Table",tags$div(class="cleanTab",HTML(dget(fileMat[53])))),
                              tabPanel("Sources and Downloads",poph4.info))
          
          
          #Append to List
          poph.list <<- list(poph1.box,poph2.box, poph5.box, poph6.box, poph3.box,poph4.box)
          incProgress()
        }
        
        # Commuting
        if("comm" %in% input$outChk){
          
          #Generate tables, plots and text...
          
          popt1 <<- GenerateVenn(listID=idList)
          popt2 <<- jobMigration(listID=idList,maxyr = curYr)
          
          #Venn Diagram
          ggsave(fileMat[55],popt1$plot, device="png")
          ggsave(fileMat[56],popt1$plot, device="png")
          img_List12 <- list(src = fileMat[55], contentType = 'image/png', width = 500, height = 300)
          
          
          #Live table HTML, Flex Table and Latex table
          #HTML Table
      
          dput(popt1$liveTabH,fileMat[57])

          #Latex Table
          dput(popt1$liveTabL, fileMat[58])
          
          #Work table HTML, Flex Table and Latex table
          #HTML Table
          dput(popt1$workTabH,fileMat[59])

          #Latex Table
          dput(popt1$workTabL, fileMat[60])
          
          #Jobs and Migration
          ggsave(fileMat[61],popt2$plot, device="png")
          ggsave(fileMat[62],popt2$plot, device="png")
          dput(popt2$text, fileMat[63])
          img_List13 <- list(src = fileMat[61], contentType = 'image/png', width = 500, height = 300)
          
          #Contents of Information Tabs
          popt1.info <- tags$div(boxContent(title= "Commuting Patterns Plot",
                                            description = "The Communting Patterns plot shows the number of people working and living in a specified location.",
                                            MSA= "T", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                            urlList = list(c("U.s. Census Bureau On the Map Data","https://onthemap.ces.census.gov/")) ),
                                 tags$br(),
                                 downloadObjUI("popt1plot"))
          
          popt2.info <- tags$div(boxContent(title= "Work Outside Table",
                                            description= "The work outside table shows the top ten work locations for prople living in an area but working somewhere else.",
                                            MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                            urlList = list(c("U.s. Census Bureau On the Map Data","https://onthemap.ces.census.gov/")) ),
                                 tags$br(),
                                 downloadObjUI("popt3tabl"),downloadObjUI("popt3data"))
          
          
          popt3.info <- tags$div(boxContent(title= "Live Outside Table",
                                            description= "The live outside table shows the top ten residential locations for people working in an area but living somewhere else.",
                                            MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                            urlList = list(c("U.s. Census Bureau On the Map Data","https://onthemap.ces.census.gov/")) ),
                                 tags$br(),
                                 downloadObjUI("popt2tabl"),downloadObjUI("popt2data"))
          
          popt4.info <- tags$div(boxContent(title= "Jobs and Net Migration Plot",
                                            description= "The jobs and net migration plot shows the trend between jobs and net migration for a selected place.",
                                            MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                            urlList = list(c("SDO Net Migration by Age Comparison","https://gis.dola.colorado.gov/apps/netmigration_dashboard/"),
                                                           c("Bureau of Economic Analysis Jobs Data","https://www.bea.gov/index.htm"))),
                                 tags$br(),
                                 downloadObjUI("popt4plot"), downloadObjUI("popt4data"))
          
          # Bind to boxes
          
          popt1.box <- tabBox(width=6, height=400,
                              tabPanel("Plot",renderImage({img_List12})),
                              tabPanel("Sources and Downloads",popt1.info))
          popt2.box <- tabBox(width=6, height=400,
                              tabPanel("Table",tags$div(class="cleanTab",HTML(dget(fileMat[57])))),
                              tabPanel("Sources and Downloads",popt3.info))
          popt3.box <- tabBox(width=6, height=400,
                              tabPanel("Table",tags$div(class="cleanTab",HTML(dget(fileMat[59])))),
                              tabPanel("Sources and Downloads",popt2.info))
          popt4.box <- tabBox(width=6, height=400,
                              tabPanel("Plot",renderImage({img_List13})),
                              tabPanel("Sources and Downloads",popt4.info))
          
          #Append to List
          popt.list <<- list(popt1.box,popt4.box,popt2.box,popt3.box)
          incProgress()
        }  #Commuting
        
        #Employment by Industry
        if("emplind" %in% input$outChk){
          #Generate tables, plots and text...
          popei1 <<- jobsPlot(listID=idList, maxyr = curYr)
          popei2 <<- jobsByIndustry(listID=idList, curyr = curYr)
          popei3 <<- baseIndustries(listID=idList, curyr = curYr)
          
          #JobsPlot
          ggsave(fileMat[64],popei1$plot, device="png")
          ggsave(fileMat[65],popei1$plot, device="png")
          dput(popei1$text, fileMat[66])
          img_List14 <- list(src = fileMat[64], contentType = 'image/png', width = 500, height = 300)
          
          #Jobs by Industry
          ggsave(fileMat[67],popei2$plot, device="png")
          ggsave(fileMat[68],popei2$plot, device="png")
          dput(popei2$text1, fileMat[69])
          dput(popei2$text2, fileMat[70])
          img_List15 <- list(src = fileMat[67], contentType = 'image/png', width = 500, height = 300)
          
          #base Industries
          #Plot
          ggsave(fileMat[71],popei3$plot, device="png")
          ggsave(fileMat[72],popei3$plot, device="png")
          img_List16 <- list(src = fileMat[71], contentType = 'image/png', width = 500, height = 300)
          
          # HTML Table
          dput(popei3$Htable, fileMat[73])
          
          #latex Table
          dput(popei2$Ltable, fileMat[74])
          #Text
          dput(popei2$text, fileMat[75])
          
          
          
          #Contents of Information Tabs
          popei1.info <- tags$div(boxContent(title= "Estimated Jobs",
                                             description = "The Jobs Estimate Plot shows the estimated number of jobs to 2040.",
                                             MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                             urlList = list(c("Jobs by Sector (NAICS)","https://demography.dola.colorado.gov/economy-labor-force/data/jobs-by-sector/#jobs-by-sector-naics"))),
                                  tags$br(),
                                  downloadObjUI("popei1plot"),  downloadObjUI("popei1data"))
          
          popei2.info <- tags$div(boxContent(title= "Jobs by Sector / Economic Industry Mix",
                                             description= "Comparing the share of jobs by industry to a larger area helps to get a better understanding of the industries that higher or lower employment concentrations.  The industry mix can also help inform the average weekly wages as industries such as retail trade or 
                                             accommodation and food pay considerably lower wages than professional and technical services or mining.",
                                             MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                             urlList = list(c("SDO Base Industries Summary","https://drive.google.com/file/d/1Ag0JdOo8XATTBiNuh80BTiuqLV4Kv72T/view"),
                                                            c("Jobs by Sector (NAICS)","https://demography.dola.colorado.gov/economy-labor-force/data/jobs-by-sector/#jobs-by-sector-naics"))),
                                  tags$br(),
                                  downloadObjUI("popei2plot"), downloadObjUI("popei2data"))
          
          
          popei3.info <- tags$div(boxContent(title= "Base Industries Plot",
                                             description= "The Base Industries plot shows which industries drive the county economy by bringing in dollars from outside the area.  A county with a diversity of base industries with similar shares of employment will 
                                             generally be more resilient than one that is dominated by one large industry.",
                                             MSA= "T", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                             urlList = list(c("SDO Base Industries Summary","https://drive.google.com/file/d/1Ag0JdOo8XATTBiNuh80BTiuqLV4Kv72T/view"),
                                                            c("SDO Base industries Anaysis","https://demography.dola.colorado.gov/economy-labor-force/data/base-analysis/#base-industries-analysis"))),
                                  tags$br(),
                                  downloadObjUI("popei3plot"), downloadObjUI("popei3data"))
          
          popei4.info <- tags$div(boxContent(title= "Base Industries Table",
                                             description= "The Base Industries Table summarizes the number of jobs in indirect basic employment, direct basic employment and local services sectors.",
                                             MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                             urlList = list(c("SDO Base Industries Summary","https://drive.google.com/file/d/1Ag0JdOo8XATTBiNuh80BTiuqLV4Kv72T/view"),
                                                            c("SDO Base Industries Anaysis","https://demography.dola.colorado.gov/economy-labor-force/data/base-analysis/#base-industries-analysis"))),
                                  tags$br(),
                                  downloadObjUI("popei4tabl"),downloadObjUI("popei4data"))
          # Bind to boxes
          popei1.box <- tabBox(width=6, height=400,
                               tabPanel("Plot",renderImage({img_List14})),
                               tabPanel("Sources and Downloads",popei1.info))
          popei2.box <- tabBox(width=6, height=400,
                               tabPanel("Plot",renderImage({img_List15})),
                               tabPanel("Sources and Downloads",popei2.info))
          popei3.box <- tabBox(width=6, height=400,
                               tabPanel("Plot",renderImage({img_List16})),
                               tabPanel("Sources and Downloads",popei3.info))
          popei4.box <- tabBox(width=6, height=400,
                               tabPanel("Table",tags$div(class="cleanTab",HTML(dget(fileMat[73])))),
                               tabPanel("Sources and Downloads",popei4.info))
          
          #Append to List
          popei.list <<- list(popei1.box,popei2.box,popei3.box,popei4.box)
          incProgress()
        }  #Employment by Industry
        
        #Employment and Demographic Forecast
        if("emply" %in% input$outChk){
          #Generate tables, plots and text...
          popem1 <<- jobsPopForecast(listID=idList,curyr=curYr)
          popem2 <<- weeklyWages(listID=idList)
          popem3 <<- residentialLF(listID=idList,curyr=curYr)
          popem4 <<- incomeSrc(level=input$level,listID=idList,ACS=curACS)  

          #JobsPopForecast
          ggsave(fileMat[76],popem1$plot, device="png")
          ggsave(fileMat[77],popem1$plot, device="png")
          dput(popem1$text, fileMat[78])
          img_List17 <- list(src = fileMat[76], contentType = 'image/png', width = 500, height = 300)
          
          #weeklyWages
          ggsave(fileMat[79],popem2$plot, device="png")
          ggsave(fileMat[80],popem2$plot, device="png")
          dput(popem2$text, fileMat[81])
          img_List18 <- list(src = fileMat[79], contentType = 'image/png', width = 500, height = 300)
          
          #residentialLF
          ggsave(fileMat[82],popem3$plot, device="png")
          ggsave(fileMat[83],popem3$plot, device="png")
          dput(popem3$text, fileMat[84])
          img_List19 <- list(src = fileMat[82], contentType = 'image/png', width = 500, height = 300)
          
          #incomeSrc
          # HTML Table
          dput(popem4$Htable, fileMat[85])
          
          #latex Table
          dput(popem4$Ltable, fileMat[86])
          #Text
          dput(popem4$text, fileMat[87])
          
          
          
          #Contents of Information Tabs
          popem1.info <- tags$div(boxContent(title= "Jobs and Population Forecast Plot",
                                             description = "The Jobs and Population Forecast Plot displays the growth rate in local jpbs and population.",
                                             MSA= "F", stats = "F", muni = "T", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                             urlList = list(c("SDO Economic Forecasts"," https://demography.dola.colorado.gov/economy-labor-force/economic-forecasts/#economic-forecasts"),
                                                            c("SDO Jobs Forecasts","https://demography.dola.colorado.gov/economy-labor-force/data/labor-force/#labor-force-participation"))),
                                  tags$br(),
                                  downloadObjUI("popem1plot"), downloadObjUI("popem1data"))
          
          popem2.info <- tags$div(boxContent(title= "Average Weekly wages",
                                             description = "The Average Weekly Wages plot shows the trend in average wages from 2010 to the present for a selected place and the state.",
                                             MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                             urlList = list(c("Department of Labor and Employment Quarterly Census of Employment and Wages","https://www.colmigateway.com/gsipub/index.asp?docid=372") )),
                                  tags$br(),
                                  downloadObjUI("popem2plot"),  downloadObjUI("popem2data"))
          
          
          popem3.info <- tags$div(boxContent(title= "Residential Labor Force Participation Line Plot",
                                             description = "The Residential Labor Force Line plot shows the trend in total labor gorce participation from 2010 to the present for a selected place and the state.",
                                             MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                             urlList = list(c("SDO Labor Force Participation Data","https://demography.dola.colorado.gov/economy-labor-force/data/labor-force/#labor-force-participation"))),
                                  tags$br(),
                                  downloadObjUI("popem3plot"), downloadObjUI("popem3data"))
          
          popem4.info <- tags$div(boxContent(title= "Household Income Sources(s) Table",
                                             description = "The Houselold Income Source(s) Table shows household income sources and amounts for housholds in a selected place or county.  
                                             Households will have multiple sources of income, so this table is not mutually exclusive. Mean income values reflect values from the cited source.",
                                             MSA= "F", stats = "F", muni = "F", multiCty = idList$multiCty, PlFilter = idList$PlFilter, 
                                             urlList = list(c("American Community Survey American Fact Finder, Series B19051 to B19070","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                  tags$br(),
                                  downloadObjUI("popem4tabl"),downloadObjUI("popem4data"))
          
          
          # Bind to boxes
          popem1.box <- tabBox(width=6, height=400,
                               tabPanel("Plot",renderImage({img_List17})),
                               tabPanel("Sources and Downloads",popem1.info))
          popem2.box <- tabBox(width=6, height=400,
                               tabPanel("Table",renderImage({img_List18})),
                               tabPanel("Sources and Downloads",popem2.info))
          popem3.box <- tabBox(width=6, height=400,
                               tabPanel("Plot",renderImage({img_List19})),
                               tabPanel("Sources and Downloads",popem3.info))
          popem4.box <- tabBox(width=6, height=400,
                               tabPanel("Table",tags$div(class="cleanTab",HTML(dget(fileMat[85])))),
                               tabPanel("Sources and Downloads",popem4.info))
          
          
          #Append to List
          popem.list <<- list(popem1.box,popem2.box,popem3.box,popem4.box)
          incProgress()
        }  #Employment and Demographic Forecast
        
        
        
       
        incProgress()       
        shinyjs::show("outputPDF") 
      }) #Progress Bar
    }#if input$unit == ""
    
    # Output UI...
    
    if(length(outputList) == 0) {
      tabs <- lapply(1:length(input$outChk), function(i) {  # this determines the number of tabs needed
        id <- paste0("tab", i)
        tabPanel(
          title = tabTitle(input$outChk[[i]]), tabList(input$outChk[[i]])
        ) # TabPanel
      })
    }  else {
      tabs <- outputList
    }
    output$ui  <- renderUI({ do.call(tabsetPanel, tabs) }) #renderUI
    
    #Event to output PDF documents
    
    output$outputPDF <- downloadHandler(
      
      filename <- function() {
        paste0(input$unit," Community Profile Report ",as.character(Sys.Date()),".pdf")
      },
      content <- function(file) {
        #Generate Report
      #  if(file.exists("SDO_Report.tex")) file.remove("SDO_Report.tex")
      #  if(file.exists("SDO_Report.pdf")) file.remove("SDO_Report.pdf")
        
        tempReport   <- "./SDO_Report.Rnw" 
        tempTex   <- "SDO_Report.tex"     
        incProgress()
        
        # Set up parameters to pass to Rnw document
        outChk <- input$outChk
        olistID <- idList
        olevel <- input$level
        ocurACS <- curACS
        ocurYr <- curYr
        placelist <- PlaceList
        filemat <- fileMat
 
        #knitting file and copy to final document
        
        knit(input=tempReport,output=tempTex)

        tools::texi2pdf(tempTex)
        tempPDF <- "SDO_Report.pdf"
        file.rename(tempPDF, file) # move pdf to file for downloading
        # unlink(tDir)
      } #Content
    ) #Download Handler
    
    
    
    #Event to outload plots and data files
    
    #Basic Statistics
    # Check this
    callModule(downloadObj, id = "statstabl", simpleCap(input$unit), "statstabl", stat_List$FlexTable)
    
    #Population Forecast
    
    callModule(downloadObj, id = "popf1tabl", simpleCap(input$unit), "popf1tabl", popf1$FlexTable)
    callModule(downloadObj, id = "popf1data", simpleCap(input$unit), "popf1data", popf1$data)
    
    callModule(downloadObj, id = "popf2plot", simpleCap(input$unit),"popf2plot", popf2$plot)
    callModule(downloadObj, id = "popf2data", simpleCap(input$unit),"popf2data", popf2$data)
    
    callModule(downloadObj, id = "popf3plot", simpleCap(input$unit), "popf3plot", popf3$plot)
    callModule(downloadObj, id = "popf3data", simpleCap(input$unit), "popf3data", popf3$data)
    
    callModule(downloadObj, id = "popf4plot", simpleCap(input$unit), "popf4plot", popf4$plot)
    callModule(downloadObj, id = "popf4data", simpleCap(input$unit), "popf4data", popf4$data)
    
    #Age
    callModule(downloadObj, id = "popa1plot", simpleCap(input$unit),"popa1plot", popa1$plot)
    callModule(downloadObj, id = "popa1data", simpleCap(input$unit),"popa1data", popa1$data)
    
    callModule(downloadObj, id = "popa2plot", simpleCap(input$unit),"popa2plot", popa2$plot)
    callModule(downloadObj, id = "popa2tabl", simpleCap(input$unit),"popa2tabl", popa2$FlexTable)
    callModule(downloadObj, id = "popa2data", simpleCap(input$unit),"popa2data", popa2$data)
    
    callModule(downloadObj, id = "popa3plot", simpleCap(input$unit), "popa3plot", popa3$plot)
    callModule(downloadObj, id = "popa3data", simpleCap(input$unit), "popa3data", popa3$data)
    
    callModule(downloadObj, id = "popa4plot", simpleCap(input$unit), "popa4plot", popa4$plot)
    callModule(downloadObj, id = "popa4data", simpleCap(input$unit), "popa4data", popa4$data)
    
    #Population Characteristics
    callModule(downloadObj, id = "popc1plot", simpleCap(input$unit),"popc1plot", popc1$plot)
    callModule(downloadObj, id = "popc1data", simpleCap(input$unit),"popc1data", popc1$data)
    
    callModule(downloadObj, id = "popc2plot", simpleCap(input$unit),"popc2plot", popc2$plot)
    callModule(downloadObj, id = "popc2data", simpleCap(input$unit),"popc2data", popc2$data)
    
    callModule(downloadObj, id = "popc3tabl", simpleCap(input$unit), "popc3tabl", popc3$FlexTable)
    callModule(downloadObj, id = "popc3data", simpleCap(input$unit), "popc3data", popc3$data)
    
    
    callModule(downloadObj, id = "popc4tabl", simpleCap(input$unit), "popc4tabl", popc4$FlexTable)
    callModule(downloadObj, id = "popc4data", simpleCap(input$unit), "popc4data", popc4$data)
    
    #Housing
    callModule(downloadObj, id = "poph1plot", simpleCap(input$unit),"poph1plot", poph1$plot)
    callModule(downloadObj, id = "poph1data", simpleCap(input$unit),"poph1data", poph1$data)
    
    callModule(downloadObj, id = "poph2tabl", simpleCap(input$unit),"poph2tabl", poph2$FlexTable)
    callModule(downloadObj, id = "poph2data", simpleCap(input$unit),"poph2data", poph2$data)
    
    callModule(downloadObj, id = "poph3tabl", simpleCap(input$unit),"poph3tabl", poph3$FlexTable)
    callModule(downloadObj, id = "poph3data", simpleCap(input$unit), "poph3data", poph3$data)
    
    callModule(downloadObj, id = "poph4tabl", simpleCap(input$unit),"poph4tabl", poph4$FlexTable)
    callModule(downloadObj, id = "poph4data", simpleCap(input$unit), "poph4data", poph4$data)
    
    callModule(downloadObj, id = "poph5tabl", simpleCap(input$unit),"poph5tabl", poph5$FlexTableOO)
    callModule(downloadObj, id = "poph5data", simpleCap(input$unit), "poph5data", poph5$data)
    
    callModule(downloadObj, id = "poph6tabl", simpleCap(input$unit),"poph6tabl", poph5$FlexTableRT)
    callModule(downloadObj, id = "poph6data", simpleCap(input$unit), "poph6data", poph5$data)
    
    #commuting
    callModule(downloadObj, id = "popt1plot", simpleCap(input$unit),"popt1plot", popt1$plot)
    
    callModule(downloadObj, id = "popt2tabl", simpleCap(input$unit),"popt2tabl", popt1$FlexLive)
    callModule(downloadObj, id = "popt2data", simpleCap(input$unit),"popt2data", popt1$data1)
    
    callModule(downloadObj, id = "popt3tabl", simpleCap(input$unit),"popt3tabl", popt1$FlexWork)
    callModule(downloadObj, id = "popt3data", simpleCap(input$unit),"popt3data", popt1$data2)
    
    callModule(downloadObj, id = "popt4plot", simpleCap(input$unit),"popt4plot", popt2$plot)
    callModule(downloadObj, id = "popt4data", simpleCap(input$unit),"popt4data", popt2$data)
    
    #Employment by Industry
    callModule(downloadObj, id = "popei1plot", simpleCap(input$unit),"popei1plot", popei1$plot)
    callModule(downloadObj, id = "popei1data", simpleCap(input$unit),"popei1data", popei1$data)
    callModule(downloadObj, id = "popei2plot", simpleCap(input$unit),"popei2plot", popei2$plot)
    callModule(downloadObj, id = "popei2data", simpleCap(input$unit),"popei2data", popei2$data)
    callModule(downloadObj, id = "popei3plot", simpleCap(input$unit),"popei3plot", popei3$plot)
    callModule(downloadObj, id = "popei3data", simpleCap(input$unit),"popei3data", popei3$data1)
    callModule(downloadObj, id = "popei4tabl", simpleCap(input$unit),"popei4tabl", popei3$FlexTable)
    callModule(downloadObj, id = "popei4data", simpleCap(input$unit),"popei4data", popei3$data2)
    
    #Employment and Demographic Forecast
    callModule(downloadObj, id = "popem1plot", simpleCap(input$unit),"popem1plot", popem1$plot)
    callModule(downloadObj, id = "popem1data", simpleCap(input$unit),"popem1data", popem1$data)
    callModule(downloadObj, id = "popem2plot", simpleCap(input$unit),"popem2plot", popem2$plot)
    callModule(downloadObj, id = "popem2data", simpleCap(input$unit),"popem2data", popem2$data)
    callModule(downloadObj, id = "popem3plot", simpleCap(input$unit),"popem3plot", popem3$plot)
    callModule(downloadObj, id = "popem3data", simpleCap(input$unit),"popem3data", popem3$data)
    
    callModule(downloadObj, id = "popem4tabl", simpleCap(input$unit),"popem4tabl", popem4$FlexTable)
    callModule(downloadObj, id = "popem4data", simpleCap(input$unit),"popem4data", popem4$data)
    
  }) #observeEvent input$profile
  
 
  
  
  
}  #server



shinyApp(ui = ui, server = server)
