#' TempFil Outputs a matrix of temporary file names and directories
#'
#' @param filemat tTempoary file matrix
#' @return Matric of filename vectors
#' @export

TempFil <- function() {

  oMatrix <- matrix(data=NA,nrow=101)
  oDir <- tempdir()
  for(i in 1:101) {
    if(i %in% c(5, 6, 11, 12, 13, 14, 16, 17, 19, 20, 22, 23, 28, 29, 31, 32, 34, 35, 37, 38,
                47, 48, 65, 66, 73, 74, 76, 77, 79, 80, 83, 84, 89, 90, 92, 93, 95, 96)) { #PNG files
      oMatrix[i] <- tempfile(tmpdir=oDir,fileext=".png")
    } 
    if(i %in% c(1, 7, 24, 39, 43, 50, 53, 56, 59, 62, 67, 70, 85, 98)) { #HTML files
      oMatrix[i] <- tempfile(tmpdir=oDir,fileext=".htm")
    }
    if(i %in% c(2, 8, 25, 40, 44, 51, 54, 57, 60, 63, 68, 71, 86, 99)) { # Flextable files
      oMatrix[i] <- tempfile(tmpdir=oDir,fileext=".docx") 
    }
    
    if(i %in% c(3, 9, 26, 41, 45, 52, 55, 58, 61, 64, 69, 72, 87, 100)) { #Latex files
      oMatrix[i] <- tempfile(tmpdir=oDir,fileext=".lat")
    }
    if(i %in% c(4, 10, 15, 18, 21, 27, 30, 33, 36, 42, 46, 49, 75, 78, 81, 82, 88, 91, 94, 97, 101)) { # Text files
      oMatrix[i] <- tempfile(tmpdir=oDir,fileext=".txt")
    }
  }
  
return(oMatrix)  
}