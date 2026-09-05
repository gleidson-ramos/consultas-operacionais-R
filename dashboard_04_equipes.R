library(shiny)
library(bs4Dash)
library(plotly)
library(DBI)
library(RMySQL)

source("conexao.R")

ui <- bs4DashPage(

  header = bs4DashNavbar(disable = TRUE),

  sidebar = bs4DashSidebar(disable = TRUE),

  body = bs4DashBody(
    fluidRow(
      bs4Card(
        title = "Atendimentos por Responsável",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput("responsavel_atendimento", height = 450)
      ),

      bs4Card(
        title = "Volume Removido por Responsável",
        width = 6,
        status = "success",
        solidHeader = TRUE,
        plotlyOutput("volume_responsavel", height = 450)
      )
    ),

    fluidRow(
      bs4Card(
        title = "Tempo Médio de Atendimento",
        width = 12,
        status = "warning",
        solidHeader = TRUE,
        plotlyOutput("tempo_medio", height = 500)
      )
    )
  ),
  footer = bs4DashFooter()

)

server <- function(input, output, session){
  output$responsavel_atendimento <- renderPlotly({
    dados <- dbGetQuery(con,"SELECT r.nome, COUNT(*) atendimentos
    FROM atendimento_coleta a
    JOIN responsavel r ON r.id_responsavel = a.id_responsavel
    GROUP BY r.nome
    ORDER BY atendimentos DESC")

    plot_ly(
      dados,
      x = ~reorder(nome, atendimentos),
      y = ~atendimentos,
      type = "bar"
    ) %>%
      layout(
        xaxis = list(title = "Responsável"),
        yaxis = list(title = "Quantidade de Atendimentos")
      )

  })

  output$volume_responsavel <- renderPlotly({

    dados <- dbGetQuery(con,"SELECT r.nome,
    SUM(IFNULL(a.volume_removido,0)) volume_total
    FROM atendimento_coleta a
    JOIN responsavel r ON r.id_responsavel = a.id_responsavel
    GROUP BY r.nome
    ORDER BY volume_total DESC")

    plot_ly(
      dados,
      x = ~reorder(nome, volume_total),
      y = ~volume_total,
      type = "bar"
    ) %>%
      layout(
        xaxis = list(title = "Responsável"),
        yaxis = list(title = "Volume Total")
      )

  })

  output$tempo_medio <- renderPlotly({
    dados <- dbGetQuery(con,"SELECT r.nome,
    AVG(TIMESTAMPDIFF(HOUR, a.data_inicio, a.data_fim)) tempo_medio
    FROM atendimento_coleta a
    JOIN responsavel r ON r.id_responsavel = a.id_responsavel
    WHERE a.data_inicio IS NOT NULL AND a.data_fim IS NOT NULL
    GROUP BY r.nome
    ORDER BY tempo_medio DESC")

    plot_ly(
      dados,
      x = ~reorder(nome, tempo_medio),
      y = ~tempo_medio,
      type = "bar"
    ) %>%
      layout(
        xaxis = list(title = "Responsável"),
        yaxis = list(title = "Tempo Médio (Em Horas)")
      )
  })
}

shinyApp(ui, server)
