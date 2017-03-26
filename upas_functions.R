#_______________________________________________________________________________
## Libraries
  library(tidyverse)
  library(scales)
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
                              lat = gpslatitude,
                              lng = gpslongitude,
                              gps_date = gpsUTCDate,
                              gps_time = gpsUTCTime,
                              gps_sat = gpssatellites,
                              gps_alt = gpsaltitude,
                              id = sample)
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
  out <- dplyr::mutate(df, lat = replace(lat, gps_sat < 3, NA)) %>%
         dplyr::mutate(lng = replace(lng, gps_sat < 3, NA)) %>%
         dplyr::mutate(gps_alt = replace(gps_alt, gps_sat < 3, NA)) %>%
         dplyr::mutate(gps_date = replace(gps_date, gps_sat < 3, NA)) %>%
         dplyr::mutate(gps_time = replace(gps_time, gps_sat < 3, NA))
  # return
  return(out)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Process upas
# upas <- upas_process(upas)
upas_process <- function(df){
  out <- upas_gps(df)
 # return
  return(out)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Plot PM
# plot_pm(upas, "BLR02")
plot_pm <- function(df, id_inst){
 # filter
  out <- dplyr::filter(df, id == id_inst)
 # plot
  p <- ggplot(out, aes(x = datetime, y = dp)) +
       geom_point() +
       theme_minimal() +
       xlab("") +
       ylab("Pressure drop (Pa)") +
       ylim(0, round_up(max(out$dp, na.rm = TRUE)))
 # return
  return(p)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Plot met
# plot_met(upas, "BLR02")
plot_met <- function(df, id_inst){
 # filter
  out <- dplyr::filter(df, id == id_inst) %>%
         dplyr::select(t_oc, rh_pct, p_kpa, datetime) 
 # plot t
  p_t <- ggplot(out, aes(x = datetime, y = t_oc)) +
         geom_point(color = "indianred3") +
         theme_minimal() +
         xlab("") +
         ylab("Temperature (oC)") +
         ylim(0, round_up(max(out$t_oc, na.rm = TRUE))) +
         theme(legend.position = "hide")
  # plot rh
  p_rh <- ggplot(out, aes(x = datetime, y = rh_pct)) +
          geom_point(color = "lightskyblue3") +
          theme_minimal() +
          xlab("") +
          ylab("Relative humidity (%)") +
          ylim(0, round_up(max(out$rh_pct, na.rm = TRUE))) +
          theme(legend.position = "hide")

  # plot p
  p_p <- ggplot(out, aes(x = datetime, y = p_kpa)) +
         geom_point(color = "mediumpurple2") +
         theme_minimal() +
         xlab("") +
         ylab("Atmospheric pressure (kPa)") +
         ylim(round(min(out$p_kpa, na.rm = TRUE) -50, -2),
              round(max(out$p_kpa, na.rm = TRUE) + 50, -2)) +
         theme(legend.position = "hide")
  # combine
  p <- multiplot(p_t, p_rh, p_p, cols = 1)

 # return
  return(p)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Plot operation
# plot_op(upas, "BLR02")
plot_op <- function(df, id_inst){
 # filter
 out <- dplyr::filter(df, id == id_inst) %>%
  dplyr::select(flow, vol, bat_v, bat_fuel, datetime) 
 # plot flow
 p_flow <- ggplot(out, aes(x = datetime, y = flow)) +
  geom_point(color = "indianred3") +
  theme_minimal() +
  xlab("") +
  ylab("Flow (Liters/min)") +
  ylim(floor(min(out$flow)),
       ceiling(max(out$flow))) +
  theme(legend.position = "hide")
 # plot sampled volume
 p_vol <- ggplot(out, aes(x = datetime, y = vol)) +
  geom_point(color = "wheat3") +
  theme_minimal() +
  xlab("") +
  ylab("Sampled volume (Liters)") +
  ylim(0, round_up(max(out$vol, na.rm = TRUE))) +
  theme(legend.position = "hide")
 
 # plot battery voltage
 p_batv <- ggplot(out, aes(x = datetime, y = bat_v)) +
  geom_point(color = "palegreen4") +
  theme_minimal() +
  xlab("") +
  ylab("Battery status (V)") +
  ylim(round(min(out$bat_v, na.rm = TRUE) -50, -2),
       round(max(out$bat_v, na.rm = TRUE) + 50, -2)) +
  theme(legend.position = "hide")
 # plot battery fuel
 p_batf <- ggplot(out, aes(x = datetime, y = bat_fuel)) +
  geom_point(color = "lightsteelblue4") +
  theme_minimal() +
  xlab("") +
  ylab("Battery fuel level (?)") +
  ylim(round(min(out$bat_fuel, na.rm = TRUE) -50, -2),
       round(max(out$bat_fuel, na.rm = TRUE) + 50, -2)) +
  theme(legend.position = "hide")
 # combine
 p <- multiplot(p_flow, p_vol, p_batv, p_batf, cols = 1)
 
 # return
 return(p)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# round down order of magnitude
  round_down <- function(x) suppressWarnings(ifelse(is.na(10^floor(log10(x))),
                                   x,
                                   10^floor(log10(x))))
# round down order of magnitude
  round_up <- function(x) suppressWarnings(ifelse(is.na(10^ceiling(log10(x))),
                                   x,
                                   10^ceiling(log10(x))))
#_______________________________________________________________________________

#_______________________________________________________________________________
# Multiple plot function from R cookbook
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
   library(grid)
   
   # Make a list from the ... arguments and plotlist
   plots <- c(list(...), plotlist)
   
   numPlots = length(plots)
   
   # If layout is NULL, then use 'cols' to determine layout
   if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
   }
   
   if (numPlots==1) {
    print(plots[[1]])
    
   } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
     # Get the i,j matrix positions of the regions that contain this subplot
     matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
     
     print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                     layout.pos.col = matchidx$col))
    }
   }
  }
#_______________________________________________________________________________
  
#_______________________________________________________________________________
# map gps data
  upas_map <- function(df, id_list = "", var = ""){
   # clean
    df <- dplyr::filter(df, !is.na(lat))
   # color pallete
    color <- colorFactor(rainbow(length(unique(upas$id))), df$id)
  # plot
    leaflet(df) %>%
    addProviderTiles(providers$Stamen.TonerLite,
                    options = providerTileOptions(noWrap = TRUE)) %>%
    addCircleMarkers(radius = 5,
                     color=~color(id),
                     stroke = FALSE, fillOpacity = 0.5)
  }
#_______________________________________________________________________________