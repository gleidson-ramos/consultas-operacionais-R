library(shiny)
library(bs4Dash)
library(plotly)
library(DBI)

source("conexao.R")

ui <- bs4DashPage(
  header = bs4DashNavbar(disable = TRUE),

  sidebar = bs4DashSidebar(disable = TRUE),

  body = bs4DashBody(
    fluidRow(
      bs4Card(
        title = "Distribuição das Prioridades",
        width = 6,
        status = "danger",
        solidHeader = TRUE,
        plotlyOutput("donut")
      ),

      bs4Card(
        title = "Prioridades por Status",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput("stack")
      )
    ),

    fluidRow(
      bs4Card(
        title = "Ocorrências Urgentes Para Ano Atual",
        width = 12,
        status = "warning",
        solidHeader = TRUE,
        plotlyOutput("urgentes")
      )
    )
  ),

  footer = bs4DashFooter()

)

server <- function(input, output){
  output$donut <- renderPlotly({

    dados <- dbGetQuery(con,"
    SELECT prioridade, COUNT(*) quantidade FROM ocorrencia
    GROUP BY prioridade")

    plot_ly(
      dados,
      labels = ~prioridade,
      values = ~quantidade,
      type = "pie",
      hole = .60
    )
  })

  output$stack <- renderPlotly({
    dados <- dbGetQuery(con,"SELECT s.nome_status, o.prioridade,
    COUNT(*) quantidade
    FROM ocorrencia o
    JOIN status_ocorrencia s
    ON s.id_status=o.id_status
    GROUP BY s.nome_status, o.prioridade")

    plot_ly(
      dados,
      x = ~nome_status,
      y = ~quantidade,
      color = ~prioridade,
      type = "bar"
    ) |>
      layout(
        barmode = "stack",
        xaxis = list(title = "Status"),
        yaxis = list(title = "Quantidade")
        )
  })

  output$urgentes <- renderPlotly({
    dados <- dbGetQuery(con,"SELECT DATE(data_ocorrencia) dia,
    COUNT(*) quantidade FROM ocorrencia
    WHERE prioridade = 'URGENTE'
    AND YEAR(data_ocorrencia) = YEAR(CURDATE())
    GROUP BY DATE(data_ocorrencia)
    ORDER BY dia")
    
    plot_ly(
      dados,
      x = ~dia,
      y = ~quantidade,
      type = "bar",
      marker = list(
        color = "#E74C3C",
        line = list(color = "#922B21", width = 1)
      )
    ) %>%
      layout(
        xaxis = list(title = "Data"),
        yaxis = list(title = "Quantidade de Ocorrências")
      )
  })

}

shinyApp(ui, server)
