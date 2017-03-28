#_______________________________________________________________________________
# libraries
  library(shiny)
  library(tidyverse)
  library(leaflet)
  library(scales)
#_______________________________________________________________________________

#_______________________________________________________________________________
# ui function
fluidPage(
  #_____________________________________________________________________________
  # top row
  fluidRow(
    column(2, p(), "UPAS test app", p()
    ),
    column(5,
         offset = 0,
         fileInput("file1", "", multiple = TRUE, accept = ".txt", width = "100%")
    )
  ),
  #_____________________________________________________________________________
  # end top row

  #_____________________________________________________________________________
  # tab panels
  fluidRow(column(10, offset = 1,
  tabsetPanel(type = "tabs", 
  tabPanel("map",
    selectInput("map_color",
                label = "",
                choices = c("id", "dp", "t", "rh"),
                selected = 1),
    leafletOutput("upasmap", height = 900),
    p()
    ),
  tabPanel("met",
    fluidRow(
    column(1),
    column(2, offset = 1,
    selectInput("select_id_met", label = "", 
      choices = "id_list", 
      selected = 1))),
    fluidRow(plotOutput("plot_met", height = "700px"))
    ),
  tabPanel("op",
    fluidRow(
    column(1),
    column(2, offset = 1,
    selectInput("select_id_op", label = "", 
    choices = "id_list", 
    selected = 1))),
    fluidRow(plotOutput("plot_op", height = "700px"))
    ),
  tabPanel("pm",
    fluidRow(
    column(1),
    column(2, offset = 1,
    selectInput("select_id_pm", label = "", 
    choices = "id_list", 
    selected = 1))),
    fluidRow(plotOutput("plot_pm", height = "700px"))
    )
    )
   )
  )
#_______________________________________________________________________________
# close ui
)
#_______________________________________________________________________________