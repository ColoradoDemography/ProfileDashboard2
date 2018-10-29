#' SubmitReport Writes a script tag with a dataLayer.push command  on clicking of the outputPDF button
#'
#' @export
#'
submitReport <- function() {

  outstr <-paste0("<script>dataLayer.push({'report': true, 'event': 'downloadReport'})</script>")
  return(outstr)
}