library(shiny)
library(shinyWidgets)
library(shinycssloaders)

source("controles oit.R")

####

ui <- fluidPage(
  h1("Respuesta de los gobiernos estatales ante los efectos de la pandemia por COVID-19 en México"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "seledo",
                  label = "Seleccione un estado",
                  choices = opciones_indicador)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfica 2", withSpinner(plotlyOutput("gra2", height = "80vh"))),
        tabPanel("Gráfica 4", withSpinner(plotlyOutput("gra4", height = "80vh"))),
        tabPanel("Gráfica 5", withSpinner(plotlyOutput("gra5", height = "80vh"))),
        tabPanel("Gráfica 6", withSpinner(plotlyOutput("gra6", height = "80vh"))),
        tabPanel("Gráfica 8", withSpinner(plotlyOutput("gra8", height = "80vh")))
      )
    )
  )
)

server <- function(input, output, session) {

  output$gra2 <- renderPlotly(
    {
      gen_grafica2(
        ent_seleccionada = req(input$seledo
        ))
    })
  output$gra4 <- renderPlotly(
    {
      gen_grafica4(
        ent_seleccionada =  req(input$seledo
      ))
    })
  output$gra5 <- renderPlotly(
    {
      gen_grafica5(
        ent_seleccionada = req(input$seledo
      ))
    })

  output$gra6 <- renderPlotly(
    {
      gen_grafica6(
        ent_seleccionada = req(input$seledo
      ))
    })
  output$gra8 <- renderPlotly(
    {
      gen_grafica8(
        ent_seleccionada = req(input$seledo
      ))
    })
}

shinyApp(ui, server)

