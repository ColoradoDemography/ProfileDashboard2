#' GenerateVenn Generates a Venn diagram using LODES data
#' V3 revised 9/4/2019 AB
#' @param listID Id list with fips and location names
#' @return ggplot2 graphic, formatted datatables, and datasets
#' @export
#'
GenerateVenn <- function(DBPool,listID){

  # Collecting place ids from  idList, setting default values

  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName

if(nchar(placefips) != 0) {
  sumSQL <- paste0("SELECT * FROM data.otm_place_summary WHERE fips = '",placefips,"' ;")
  placeSQL <- paste0("SELECT * FROM data.otm_place_place WHERE fips = '",placefips,"' ;")
} else {
  sumSQL <- paste0("SELECT * FROM data.otm_county_summary WHERE fips = '",ctyfips,"' ;")
  placeSQL <- paste0("SELECT * FROM data.otm_county_place WHERE fips = '",ctyfips,"' ;")
}
  # reading Data 
   f.summary <- dbGetQuery(DBPool, sumSQL)
   f.place <- dbGetQuery(DBPool, placeSQL)

     f.summary$livein_workout <- as.numeric(f.summary$livein_workout)
     f.summary$liveout_workin <- as.numeric(f.summary$liveout_workin)
     f.summary$livein_workin <-  as.numeric(f.summary$livein_workin)
  

  rawVenn <- euler(c("A" = f.summary$liveout_workin, "B" = f.summary$livein_workout, "A&B" = f.summary$livein_workin ))
  cols <- c("lightblue1", "lightyellow1","olivedrab1")
  
  
  Vdiag <- plot(rawVenn,
             #   quantities = format(rawVenn$original.values, big.mark = ",",scientific = FALSE),
                fill = cols,
                fill_opacity = 0.5, border = "black",
                labels=FALSE,
                counts = TRUE)
  
  #Building Legend
    legstr1 <- paste0("Employed in Selected Area, Live Outside: ", format(f.summary$liveout_workin, big.mark = ",",scientific = FALSE) )
    legstr2 <- paste0("Live in Selected Area, Employed Outside: ", format(f.summary$livein_workout, big.mark = ",",scientific = FALSE) )
    legstr3 <- paste0("Employed and Live in Selected Area: ", format(f.summary$livein_workin, big.mark = ",",scientific = FALSE) )

  
  if(nchar(placefips) != 0) {
      plot_title <- paste0(placename,": All Jobs, 2017")
  } else {
    plot_title <- paste0(ctyname,": All Jobs, 2017")
  }
  
  
  lg <- legendGrob(labels=c(legstr1,
                            legstr2,
                            legstr3),
                   pch=rep(19,length(c(legstr1,
                                       legstr2,
                                       legstr3))),
                   
                   gp=gpar(col=cols, fill="gray", fontsize=12),
                   byrow=TRUE)
  
  
  g <- gTree(children = gList(Vdiag))

  #outVenn is the final VennDiagram
  #Formatting citation
  captionGrob = textGrob(captionSrc("LODES",""),
                       gp=gpar(fontsize=12),
                       x = unit(1, "npc"),
                       hjust = 1,
                       vjust = 0)
  titleGrob <- textGrob(plot_title, gp=gpar(fontsize=12,fontface="bold"))
  
  gridvenn <- arrangeGrob(titleGrob, g, lg, captionGrob, nrow=4, heights=unit(c(1,2,1,1),"in"))
  
  
  
  outVenn <- as.ggplot(gridvenn)

  # Finalizing the output data sets
  #selecting the top 10 places

if(nchar(placefips) != 0) {
  f.work_fin <- f.place[which(f.place$type == 2),c(5:7)]
  f.live_fin <- f.place[which(f.place$type == 1),c(5:7)]
} else {
  f.work_fin <- f.place[which(f.place$type == 1),c(5:7)]
  f.live_fin <- f.place[which(f.place$type == 2),c(5:7)]
}

  names(f.work_fin) <- c("Location","Count","Percent")
  f.work_sum <- f.work_fin %>%
      summarize(Count = sum(Count),
                Percent = sum(Percent))
  f.work_sum$Location <- "Total"
  f.work_sum <- f.work_sum[,c(3,1,2)]
  f.work_fin <- rbind(f.work_fin,f.work_sum)
  
  f.work_fin$Count <- format(f.work_fin$Count,big.mark=",")
  f.work_fin$Percent <- percent(f.work_fin$Percent)
  f.work_fin$Location <- gsub("City City","City",f.work_fin$Location)



  names(f.live_fin) <- c("Location","Count","Percent")
  f.live_sum <- f.live_fin %>%
    summarize(Count = sum(Count),
              Percent = sum(Percent))
  f.live_sum$Location <- "Total"
  f.live_sum <- f.live_sum[,c(3,1,2)]
  f.live_fin <- rbind(f.live_fin,f.live_sum)
  
  f.live_fin$Count <- format(f.live_fin$Count,big.mark=",")
  f.live_fin$Percent <- percent(f.live_fin$Percent)
  f.live_fin$Location <- gsub("City City","City",f.live_fin$Location)



  # Formatting Work Output table.
  names_spaced <- c("Location","Count","Percent")
  
  if(nchar(placefips) != 0) {
    capstr1 <- paste0("Employees in ",placename," living elsewhere")
    capstr2 <- paste0("Residents of ",placename," working elsewhere")
    tabCap <- paste0("Commuting Patterns for ",placename)
  } else {
    capstr2 <- paste0("Employees in ",ctyname," living elsewhere")
    capstr1 <- paste0("Residents of ",ctyname," working elsewhere")
    tabCap <- paste0("Commuting Patterns for ",ctyname)
  }
  
  m.work <- as.matrix(f.work_fin)
  m.live <- as.matrix(f.live_fin)

  workTabH <- m.work %>%
    kable(format='html', table.attr='class="cleanTable"',
          row.names=FALSE,
          align='lrr',
          caption=capstr1,
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = T) %>%
    column_spec(1, width = "3in") %>%
    column_spec(2, width = "1in") %>%
    column_spec(3, width = "1in") %>%
    kableExtra::footnote(captionSrc("LODES",""))

  #formatting Live output table
  liveTabH <- m.live %>%
    kable(format='html', table.attr='class="cleanTable"',
          row.names=FALSE,
          align='lrr',
          caption=capstr2,
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = T) %>%
    column_spec(1, width = "3in") %>%
    column_spec(2, width = "1in") %>%
    column_spec(3, width = "1in") %>%
    kableExtra::footnote(captionSrc("LODES",""))
  
 #Creating Latex and Flextables
  

  f.work_fin$id <- 0
  for(i in 1:nrow(f.work_fin)) {
    f.work_fin[i,4] <- i
  }
  f.live_fin$id <- 0
  for(i in 1:nrow(f.live_fin)) {
    f.live_fin[i,4] <- i
  }
  
  f.comb <- full_join(f.work_fin,f.live_fin,"id")
  f.comb <- f.comb[,c(1:3,5:7)]
  m.comb <- as.matrix(f.comb)
 
 # Formatting Work Output table.
 names_spacedL <- c("Location","Count","Percent","Location","Count","Percent")
 
 tblHead <- c(capstr1 = 3,capstr2 = 3)
 # set vector names
 names(tblHead) <- c(capstr1,capstr2)
 
  combTabL <-kable(m.comb,
                  col.names = names_spacedL,
                 row.names=FALSE,
                 align='lrrlrr',
                 caption=tabCap,
                 format="latex", booktabs=TRUE) %>%
    kable_styling(latex_options="hold_position",font_size=9)  %>%
    column_spec(1, width="2in") %>%
    column_spec(2, width = "0.5in") %>%
    column_spec(3, width = "0.5in") %>%
    column_spec(4, width="2in") %>%
    column_spec(5, width = "0.5in") %>%
    column_spec(6, width = "0.5in") %>%
    add_header_above(header=tblHead) %>%
    kableExtra::footnote(captionSrc("LODES",""),threeparttable = T)

  
 
  #Building FlexTable
  Ft1 <- data.frame(m.comb)
  names(Ft1) <- c("V1","V2","V3","V4","V5","V6")
  Flexcomb <- regulartable(Ft1) %>%
              set_header_labels(V1 = "Location", V2="Count", V3="Percent",
                                V4 = "Location", V5="Count", V6="Percent" ) %>%
              add_header(V1=capstr1,V4=capstr2,top=TRUE) %>%
              add_footer(V1=captionSrc("LODES","")) %>%
              merge_at(i=1, j = 1:3, part = "header") %>%
              merge_at(i=1, j = 4:6, part = "header") %>%
              merge_at(i=1, j = 1:6, part = "footer") %>%
              align(j=2:3, align="center", part="header") %>%
              align(i=2, j=1, align="left",part="header") %>%
              align(i=2, j=2:3, align="center",part="header") %>%
              align(i=1, align="left",part="footer") %>%
              align(j=1, align="left", part="body") %>%
              align(j=4, align="left", part="body") %>%
              width(j=1, width=2.5) %>%
              width(j=2:3, width=0.8) %>%
              width(j=4, width=2.5) %>%
              width(j=5:6, width=0.8)
  
 #Building output data set
  
  f.data_out <- f.comb
  
  if(nchar(placefips) != 0) {
    f.data_out$Geography <- placename
  } else {
    f.data_out$Geography <- ctyname
  }
  f.data_out <- f.data_out[,c(7,1:6)]
  
  
  names(f.data_out) <- c("Geography",capstr2, "Number","Percentage",capstr1, "Number","Percentage")
  
  # Binding List for Output
  outList <- list("plot" = outVenn, "liveTabH" = liveTabH, "data1" = f.summary,
                  "workTabH" = workTabH, "data2" = f.data_out,
                  "Flexcomb" = Flexcomb, "combTabL" = combTabL)
 
  return(outList)
  }

