library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)


ui <- grid_page(
  layout = c(
    "area1   area1  area10  ",
    "sidebar plotly area8   ",
    "area2   .      bluePlot",
    "area3   area4  bluePlot"
  ),
  row_sizes = c(
    "100px",
    "0.99fr",
    "0.99fr",
    "1.02fr"
  ),
  col_sizes = c(
    "250px",
    "0.58fr",
    "1.42fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    card_header(
      selectInput(
        inputId = "atributos_socio",
        label = "Atributo sociodemográfico:",
        choices = list(
          "Idade" = "idade",
          "Genero" = "genero",
          "Estado civil" = "estado_civil",
          "Tempo de moradia na residência atual" = "res_atual_desde",
          "Estrangeiro" = "estrangeiro",
          "Situação ocupacional" = "status_ocupacional",
          "empo de permanencia no emprego atual" = "tempo_ocupacao"
        )
      )
    ),
    card_body(
      sliderInput(
        inputId = "inputId",
        label = "Slider Input",
        min = 0,
        max = 10,
        value = 5,
        width = "100%"
      )
    )
  ),
  grid_card_plot(area = "bluePlot"),
  grid_card(area = "plotly", card_body()),
  grid_card(area = "area4", card_body()),
  grid_card(
    area = "area1",
    card_body(h2(strong("Análise de perfil")), "Lorem Ipsum")
  ),
  grid_card(
    area = "area8",
    card_body(
      "Interactive Plot",
      plotlyOutput(
        outputId = "atributos_soci",
        width = "100%",
        height = "100%"
      )
    )
  ),
  grid_card(
    area = "area10",
    card_body(tabsetPanel()),
    card_footer(
      strong("Selecione as opções desejadas:
      
      ")
    )
  ),
  grid_card(
    area = "area2",
    full_screen = TRUE,
    card_header(
      selectInput(
        inputId = "patrimonio",
        label = "Cenário patrimonial: ",
        choices = list(
          "Habitação" = "habitacao",
          "Propriedades" = "propriedade",
          "Posse de telefone celular" = "posse_celular",
          "Propósito" = "proposito",
          "Status de reserva" = "status_reserva",
          "Nº de responsáveis pela renda" = "n_resp_man_bens"
        )
      )
    )
  ),
  grid_card(
    area = "area3",
    full_screen = TRUE,
    card_header(
      selectInput(
        inputId = "hist_cred",
        label = "Histórico de crédito:",
        choices = list(
          "Status da conta corrente" = "status_conta",
          "Histórico de crédito" = "hist_credito",
          "Crédito atual" = "atual_credi",
          "Taxa do parcelamento com relação a renda disponivel" = "tx_par_rendadisp",
          "Devedores/fiadores" = "dev_fiad",
          "Planos de parcelamento" = "plan_parcelamentos",
          "Nº de emprestimos/linhas de crédito" = "em_lin_cred"
        )
      )
    )
  )
)


server <- function(input, output) {
   
  output$atributos_soci <- renderPlotly({
    # generate bins based on input$bins from ui.R
    plot_ly(x = ~ faithful[, 2], type = "histogram")
  })
  
  output$bluePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "steelblue", border = "white")
  })
  
  output$myTable <- renderDT({
    head(faithful, input$numRows)
  })
}

shinyApp(ui, server)
  

