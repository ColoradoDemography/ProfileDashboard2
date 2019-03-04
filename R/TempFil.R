#' TempFil Outputs a matrix of temporary file names and directories
#'
#' @param filemat Temporary file matrix
#' @return Matrix of filename vectors
#' @export

TempFil <- function(oDir) {
  oMatrix <- matrix(data=NA,nrow=90)

  x <- oDir
  # Copying RMD File      
  file.copy("SDO_Report.Rmd",oDir)
  oMatrix[88] <- file.path(paste0(oDir,"/","SDO_Report.Rmd"))
  
  # Location of PDF File
  oMatrix[89] <- file.path(paste0(oDir,"/","SDO_REPORT.pdf"))

  
 #Copying Dola Image
  file.copy("www/co_dola_div_localgov_300_rgb.png",oDir)
  oMatrix[90] <- file.path(paste0(oDir,"/","co_dola_div_localgov_300_rgb.png"))

  
  
  for(i in 1:87) {
    if(i %in% c(4, 5, 9, 10, 11, 12, 14, 15, 17, 18, 20, 21, 25, 26, 28, 29, 31, 32, 34, 
                35, 42, 43, 55, 56, 61, 62, 64, 65, 67, 68, 71, 72, 79, 80, 85, 86)) { #PNG files
      oMatrix[i] <- tempfile(tmpdir=oDir,fileext=".png")
    } 
    if(i %in% c(1, 6, 22, 36, 39, 45, 47, 49, 51, 53, 57, 59, 73, 76, 82)) { #HTML files
      oMatrix[i] <- tempfile(tmpdir=oDir,fileext=".htm")
    }
    if(i %in% c(2, 7, 23, 37, 40, 46, 48, 50, 52, 54, 58, 60, 74, 77, 83)) { #Latex files
      oMatrix[i] <- tempfile(tmpdir=oDir,fileext=".lat")
    }
    if(i %in% c(3, 8, 13, 16, 19, 24, 27, 30, 33, 38, 41, 44, 63, 66, 69, 70, 75, 78, 81, 84, 87)) { # Text files
      oMatrix[i] <- tempfile(tmpdir=oDir,fileext=".txt")
    }
  }
 
return(oMatrix)  
}