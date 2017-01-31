## Libraries
  library(tidyverse)

#______________________________________________________________________________________
# read one upas file
# file <- "data/log/PS108LOG_170116_123618_000000_000000_BLR02___________HHH.txt"
  load_upas_file <- function(file){
    ware_info <- read_csv(file, col_names = "versions",
    											n_max = 1, col_types = NULL)
    sample_info <- read_csv(file, col_names = TRUE,
    											n_max = 1, skip = 1, col_types = NULL)
    col_names <- read_csv(file, col_names = FALSE,
    											n_max = 1, skip = 4, col_types = NULL)	
    units <- read_csv(file, col_names = as.character(col_names[1,]),
    									    n_max = 1, skip = 3, col_types = NULL)
    data <- read_csv(file, col_names = TRUE, skip = 4)
 # parse header info to main data file
    
 # convert data classes
 
 # rename columns
 
 # return
    return(data)
}
#______________________________________________________________________________________

#______________________________________________________________________________________
# Load multifile folders
# out <- load_multifile("data/log", "*")
load_multifile <- function(fldr, pattern){
 # list files to load
  filelist <- list.files(fldr, pattern = pattern, full.names = TRUE, ignore.case = TRUE)

 # loop files
  for(i in 1:length(filelist)){
  	ifelse(i==1, out <- load_upas_file(filelist[i]), out <- rbind(out, load_upas_file(filelist[i])))
  }

 # return
  return(out)
}
#______________________________________________________________________________________

