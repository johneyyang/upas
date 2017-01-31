## Libraries
  library(tidyverse)

##______________________________________________________________________________________
## read one upas file
## file <- "data/log/PS108LOG_170116_123618_000000_000000_BLR02___________HHH.txt"
  load_upas_file <- function(file){
  ware_info <- read_csv(file, col_names = "versions", n_max = 1)
  sample_info <- read_csv(file, col_names = TRUE, n_max = 1, skip = 1)
  col_names <- read_csv(file, col_names = FALSE, n_max = 1, skip = 4)	
  units <- read_csv(file, col_names = as.character(col_names[1,]), n_max = 1, skip = 3)
  data <- read_csv(file, col_names = TRUE, skip = 4)	
  }
##______________________________________________________________________________________