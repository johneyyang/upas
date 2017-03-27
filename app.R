library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
 fluidRow(
  column(2, p(), "UPAS test app", p()
  ),
  column(2, offset = 2, p(), "load button here", p()
  )      
 ),
  fluidRow(
    leafletOutput("upasmap"),
    p(),
    actionButton("recalc", "New points")
  )
)

server <- function(input, output, session){

  points <- eventReactive(input$recalc, {
  cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
}, ignoreNULL = FALSE)

  output$upasmap <- renderLeaflet({
    leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE)
  ) %>%
    addMarkers(data = points())
  })
}

shinyApp(ui, server)