library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
id_list <- c("all", unique(upas$id))

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
                        p()
                        ),
                tabPanel("met",
                  fluidRow(
                    column(1),
                    column(2, offset = 1,
                         selectInput("select_met_id", label = "", 
                                     choices = id_list, 
                                     selected = 1))),
                  fluidRow(plotOutput("plot_met"))
                       ),
                tabPanel("op",
                         fluidRow(
                          column(1),
                          column(2, offset = 1,
                                 selectInput("select_op_id", label = "", 
                                             choices = id_list, 
                                             selected = 1))),
                   fluidRow(plotOutput("plot_op"))
                         )
    )
  )
)

server <- function(input, output, session){
 # load test data
 upas <- load_multifile("data", "*")
 # clean test data
 upas <- upas_process(upas)
 # id_list
 id_list <- unique(upas$id)
 
#________________________________________________________
# map
  output$upasmap <- renderLeaflet({
    upas_map(upas)
  })
#________________________________________________________

#________________________________________________________
# met data to plot
  met_data <- reactive({
    data_met(upas, input$select_met_id)
  })
#________________________________________________________

#________________________________________________________
# op data to plot
  op_data <- reactive({
    data_op(upas, input$select_op_id)
  })
#________________________________________________________

#________________________________________________________
 # plot met
  output$plot_met <- renderPlot({
    print(plot_met(met_data()))
  })
#________________________________________________________

#________________________________________________________
# plot op
  output$plot_op <- renderPlot({
     print(plot_op(op_data()))
})
#________________________________________________________

}
#________________________________________________________
shinyApp(ui, server)