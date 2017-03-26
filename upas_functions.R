#_______________________________________________________________________________
## Libraries
  library(tidyverse)
#_______________________________________________________________________________

#_______________________________________________________________________________
# read one upas file
# file <- "data/log/PS108LOG_170116_123618_000000_000000_BLR02___________HHH.txt"
  load_upas_file <- function(file){
    software_info <- read_csv(file, col_names = "versions",
                              n_max = 1, col_types = NULL)
    sample_info <- read_csv(file, col_names = TRUE,
                            n_max = 1, skip = 1, col_types = NULL)
    col_names <- read_csv(file, col_names = FALSE,
                          n_max = 1, skip = 4, col_types = NULL)
    units <- read_csv(file, col_names = as.character(col_names[1,]),
                      n_max = 1, skip = 3, col_types = NULL)
    data <- read_csv(file, col_names = TRUE, skip = 4)
 # parse file name
  data <- dplyr::mutate(data, sample = strsplit(basename(file), "_")[[1]][6])

 # parse header info to main data file
  data <- dplyr::mutate(data, start = as.POSIXct(as.character(sample_info$StartTime[1]),
                                                 format = "%y%m%d%H%M%S"),
                              end = as.POSIXct(as.character(sample_info$EndTime[1]),
                                               format = "%y%m%d%H%M%S"))
 # convert data classes
  data <- dplyr::mutate(data, datetime = as.POSIXct(as.character(timestr),
                                                    format = "%y%m%d%H%M%S"))
 # rename columns
  data <- dplyr::rename(data, flow = volflow,
                              vol = sampledVol,
                              t_oc = bme_temp,
                              p_kpa = bme_press,
                              rh_pct = bme_rh,
                              den = atmoRho,
                              dp = dpSDPu25,
                              t_oc_sd = tempSDPu25,
                              bat_v = bVolt,
                              bat_fuel = bFuel,
                              gps_lat = gpslatitude,
                              gps_lon = gpslongitude,
                              gps_date = gpsUTCDate,
                              gps_time = gpsUTCTime,
                              gps_sat = gpssatellites,
                              gps_alt = gpsaltitude)
 # return
  return(data)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load multifile folders
# out <- load_multifile("data/log", "*")
# saveRDS(data, "upas_india.rds")
load_multifile <- function(fldr, pattern){
 # list files to load
  filelist <- list.files(fldr, pattern = pattern, full.names = TRUE, ignore.case = TRUE)

 # loop files
  for(i in 1:length(filelist)){
    ifelse(i==1,
           out <- load_upas_file(filelist[i]),
           out <- rbind(out, load_upas_file(filelist[i])))
  }

 # return
  return(out)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Clean gps
upas_gps <- function(df){
  out <- dplyr::mutate(df, gps_lat = replace(gps_lat, gps_sat < 3, NA)) %>%
         dplyr::mutate(gps_lon = replace(gps_lon, gps_sat < 3, NA)) %>%
         dplyr::mutate(gps_alt = replace(gps_alt, gps_sat < 3, NA)) %>%
         dplyr::mutate(gps_date = replace(gps_date, gps_sat < 3, NA)) %>%
         dplyr::mutate(gps_time = replace(gps_time, gps_sat < 3, NA))
  # return
  return(out)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Process upas
upas_process <- function(df){
  out <- upas_gps(df)
 # return
  return(out)
}
#_______________________________________________________________________________

