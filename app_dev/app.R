library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
 fluidRow(
  column(2, p(), "UPAS test app", p()
  ),
  column(2, offset = 2, p(), actionButton("recalc", "load data"), p()
  )      
 ),
  fluidRow(
    tabsetPanel(type = "tabs", 
                tabPanel("map", 
                        leafletOutput("upasmap", height = 900),
                        p()),
                        
                tabPanel("instrument", "hjvgv"), 
                tabPanel("info", "hbhj")
    )
  )
)

server <- function(input, output, session){

  points <- eventReactive(input$recalc, {
  cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
}, ignoreNULL = FALSE)

  output$upasmap <- renderLeaflet({
    upas_map(upas)
  })
}

shinyApp(ui, server)