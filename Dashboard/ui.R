library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader("Análise de crédito"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(plotOutput("PLOT1")),
      box(
        title = "Anos",
        sliderInput("idade_anos", 
                    "Selecione a faixa de idade:",
                    min = 1, max = 100, value = 1)
      )
    )
  )
)

server <- function(input, output) {
  # dados
  dadosSocio <- read.csv("data/dadosSocio.csv")
  dadosPatr <- read.csv("data/dadosPatr.csv")
  dadosHist <- read.csv("data/dadosHist.csv")
  
  output$PLOT1 <- renderPlot({
    P1 <- dadosSocio[1:input$idade_anos,]  # Substituí seq_len por 1:
    ggplot(P1, aes(x = alguma_coluna)) + 
      geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
      labs(title = "Histograma", x = "Alguma Coluna", y = "Frequência")
  }) 
}

shinyApp(ui, server)
