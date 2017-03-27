# libraries
 library(tidyverse)
 library(scales)
 library(leaflet)
# source files
  source("upas_functions")

  
#_______________________________________________________________________________
# select met data
  data_met <- function(df, id_inst){
   # filter by id
   if(id_inst != "all"){
    out <- dplyr::filter(df, id == id_inst) %>%
     dplyr::select(t_oc, rh_pct, p_kpa, datetime)
   }
   
   if(id_inst == "all"){
    out <- dplyr::select(df, t_oc, rh_pct, p_kpa, datetime)
   }
  # return
    return(out)
  }
#_______________________________________________________________________________

#_______________________________________________________________________________
# Plot met
# plot_met(upas, "BLR02")
  plot_met <- function(df){
   # plot t
   p_t <- ggplot(df, aes(x = datetime, y = t_oc)) +
    geom_point(color = "indianred3") +
    theme_minimal() +
    xlab("") +
    ylab("Temperature (oC)") +
    ylim(0, round_up(max(df$t_oc, na.rm = TRUE))) +
    theme(legend.position = "hide")
   # plot rh
   p_rh <- ggplot(df, aes(x = datetime, y = rh_pct)) +
    geom_point(color = "lightskyblue3") +
    theme_minimal() +
    xlab("") +
    ylab("Relative humidity (%)") +
    ylim(0, round_up(max(df$rh_pct, na.rm = TRUE))) +
    theme(legend.position = "hide")
   
   # plot p
   p_p <- ggplot(df, aes(x = datetime, y = p_kpa)) +
    geom_point(color = "mediumpurple2") +
    theme_minimal() +
    xlab("") +
    ylab("Atmospheric pressure (kPa)") +
    ylim(round(min(df$p_kpa, na.rm = TRUE) -50, -2),
         round(max(df$p_kpa, na.rm = TRUE) + 50, -2)) +
    theme(legend.position = "hide")
   # combine
   p <- multiplot(p_t, p_rh, p_p, cols = 1)

   # return
   return(p)
  }
#_______________________________________________________________________________
  
#_______________________________________________________________________________
# select op data
  data_op <- function(df, id_inst){
   # filter by id
   if(id_inst != "all"){
    out <- dplyr::filter(df, id == id_inst) %>%
           dplyr::select(flow, vol, bat_v, bat_fuel, datetime) 
   }

   if(id_inst == "all"){
    out <- dplyr::select(df, flow, vol, bat_v, bat_fuel, datetime) 
   }
   # return
   return(out)
  }
#_______________________________________________________________________________  

#_______________________________________________________________________________
# Plot operation
# plot_op(upas, "BLR02")
  plot_op <- function(df){
    # plot flow
    p_flow <- ggplot(df, aes(x = datetime, y = flow)) +
     geom_point(color = "indianred3") +
     theme_minimal() +
     xlab("") +
     ylab("Flow (Liters/min)") +
     ylim(floor(min(df$flow)),
          ceiling(max(df$flow))) +
     theme(legend.position = "hide")
    # plot sampled volume
    p_vol <- ggplot(df, aes(x = datetime, y = vol)) +
     geom_point(color = "wheat3") +
     theme_minimal() +
     xlab("") +
     ylab("Sampled volume (Liters)") +
     ylim(0, round_up(max(df$vol, na.rm = TRUE))) +
     theme(legend.position = "hide")
    # plot battery voltage
    p_batv <- ggplot(df, aes(x = datetime, y = bat_v)) +
     geom_point(color = "palegreen4") +
     theme_minimal() +
     xlab("") +
     ylab("Battery status (V)") +
     ylim(round(min(df$bat_v, na.rm = TRUE) -50, -2),
          round(max(df$bat_v, na.rm = TRUE) + 50, -2)) +
     theme(legend.position = "hide")
    # plot battery fuel
    p_batf <- ggplot(df, aes(x = datetime, y = bat_fuel)) +
     geom_point(color = "lightsteelblue4") +
     theme_minimal() +
     xlab("") +
     ylab("Battery fuel level (?)") +
     ylim(round(min(df$bat_fuel, na.rm = TRUE) -50, -2),
          round(max(df$bat_fuel, na.rm = TRUE) + 50, -2)) +
     theme(legend.position = "hide")
  # combine
    p <- multiplot(p_flow, p_vol, p_batv, p_batf, cols = 1)
  # return
    return(p)
  }
#_______________________________________________________________________________