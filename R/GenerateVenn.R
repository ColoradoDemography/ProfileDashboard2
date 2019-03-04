#' GenerateVenn Generates a Venn diagram using LODES data
#' V2 revised 2/15/2018 AB
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
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }
  
  options(warn=-1)  # Suppressing warning messages produced by VennDiagram

if(nchar(placefips) != 0) {
  sumSQL <- paste0("SELECT * FROM data.otm_place_summary WHERE fips = '",placefips,"' ;")
  placeSQL <- paste0("SELECT * FROM data.otm_place_place WHERE fips = '",placefips,"' ;")
} else {
  sumSQL <- paste0("SELECT * FROM data.otm_county_summary WHERE fips = '",ctyfips,"' ;")
  placeSQL <- paste0("SELECT * FROM data.otm_county_place WHERE fips = '",ctyfips,"' ;")
}
 
    f.summary <- dbGetQuery(DBPool, sumSQL)
    f.place <- dbGetQuery(DBPool, placeSQL)


  if(nchar(placefips) != 0) {
    location <- paste0(placename,"\n","All Jobs, ",as.character(f.summary$year))
  } else {
    location <- paste0(ctyname,"\n","All Jobs, ",as.character(f.summary$year))
  }
  
# FIX THIS IN THE FUTURE
  lin_wout <- as.numeric(f.summary$workin_liveout)
  lout_win <- as.numeric(f.summary$livein_workout)
  lin_win <-  as.numeric(f.summary$livein_workin)

  region1 <- lout_win + lin_win #Live outside, work in
  region2 <- lin_wout + lin_win #Live in, work outside
  
  crossRegion <- lin_win 

 
  # By default, VennDiagram outputs the larger Region value in the left hand postion.
  # This code block insures that the diagram is correct
  if(lin_wout >= lout_win){
    diag <- draw.pairwise.venn(region1, region2, crossRegion, inverted = TRUE,
                               lty = rep("solid", 2), cat.col = rep("black", 2),
                               cex = 1, cat.cex = 1, cat.default.pos= "text", ext.text = FALSE,
                               fill = c("chartreuse4", "aquamarine2"), alpha = rep(0.5, 2),
                               euler.d=TRUE,scaled=TRUE, ind = FALSE, print.mode="raw")
  } else{
    diag <- draw.pairwise.venn(region1, region2, crossRegion, inverted = FALSE,
                               lty = rep("solid", 2), cat.col = rep("black", 2),
                               cex = 1, cat.cex = 1,  cat.default.pos= "text", ext.text  = FALSE,
                               fill = c("chartreuse4", "aquamarine2"), alpha = rep(0.5, 2),
                               euler.d=TRUE,scaled=TRUE, ind = FALSE, print.mode="raw")
  }


  # Formatting the labels for the output diagram
  # Change labels for first three text grobs
  # hard-coded three, but it would be the number of text labels
  # minus the number of groups passed to venn.diagram

 
  idx <- sapply(diag, function(i) grepl("text", i$name))  # Identifying the text grobs

  for(i in 1:3){
    diag[idx][[i]]$label <-
      format(as.numeric(diag[idx][[i]]$label), big.mark=",", scientific=FALSE)
    if(diag[idx][[i]]$label == "NA") {
      diag[idx][[i]]$label <- ""
    }
  } #End I Loop


  #Building Legend
  
  if(nchar(placefips) != 0) {
    legstr1 <- paste0("Employees in ",placename," living elsewhere")
    legstr2 <- paste0("Residents of ",placename," working elsewhere")
    legstr3 <- paste0("Employed and Live in ",placename)
  } else {
    legstr1 <- paste0("Employees in ",ctyname," living elsewhere")
    legstr2 <- paste0("Residents of ",ctyname," working elsewhere")
    legstr3 <- paste0("Employed and Live in ",ctyname)
  }
  
  cols <- c("chartreuse4", "aquamarine2","aquamarine3")
  lg <- legendGrob(labels=c(legstr1,
                            legstr2,
                            legstr3),
                   pch=rep(19,length(c(legstr1,
                                       legstr2,
                                       legstr3))),

                   gp=gpar(col=cols, fill="gray", fontsize=12),
                   byrow=TRUE)

  g <- gTree(children = gList(diag))


  #outVenn is the final VennDiagram
  #Formatting citation
  sub.label = textGrob(captionSrc("LODES",""),
                       gp=gpar(fontsize=12),
                       x = unit(1, "npc"),
                       hjust = 1,
                       vjust = 0)

    outVenn <- arrangeGrob(g, lg, nrow=3, ncol=1, heights=c(4,1,2),
                         top=textGrob(location, gp=gpar(fontsize=15,font=8)), sub=sub.label)

  options(warn=0)  # restoring Warning Messages
  # Finalizing the output data sets
  #selecting the top 10 places

  f.work_fin <- f.place[which(f.place$type == 1),c(5:7)]
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


  f.live_fin <- f.place[which(f.place$type == 2),c(5:7)]
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
    capstr1 <- paste0("Employees in ",ctyname," living elsewhere")
    capstr2 <- paste0("Residents of ",ctyname," working elsewhere")
    tabCap <- paste0("Commuting Patterns for ",ctyname)
  }
  
  m.work <- as.matrix(f.work_fin)
  m.live <- as.matrix(f.live_fin)

  workTabH <- m.work %>%
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
    footnote(captionSrc("LODES",""))

  #formatting Live output table
  liveTabH <- m.live %>%
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
    footnote(captionSrc("LODES",""))
  
 #Creating Latex and Flextables
 m.comb <- cbind(m.work,m.live)
 
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
    footnote(captionSrc("LODES",""),threeparttable = T)

  
 
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
 
  if(nchar(placefips) != 0) {
       f.live_fin$Geography <- placename
       f.work_fin$Geography <- placename
  } else {
       f.live_fin$Geography <- ctyname
       f.work_fin$Geography <- ctyname
  }
  f.live_fin <- f.live_fin[,c(4,1:3)]
  f.work_fin <- f.work_fin[,c(4,1:3)]
  
  names(f.live_fin) <- c("Geography","Residence Location", "Number","Percentage")
  names(f.work_fin) <- c("Geography","Work Location", "Number","Percentage")
  
  # Binding List for Output
  outList <- list("plot" = outVenn, "liveTabH" = liveTabH, "data1" = f.live_fin,
                  "workTabH" = workTabH, "data2" = f.work_fin,
                  "Flexcomb" = Flexcomb, "combTabL" = combTabL)
 
  return(outList)
  }

