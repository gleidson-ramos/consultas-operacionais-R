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
        title = "Top Tipos de Resíduos",
        width = 6,
        status = "success",
        solidHeader = TRUE,
        plotlyOutput("top_residuos", height = 450)
      ),
      
      bs4Card(
        title = "Categorias de Resíduos",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput("categorias", height = 450)
      )
    ),
    
    fluidRow(
      bs4Card(
        title = "Periculosidade dos Resíduos",
        width = 6,
        status = "danger",
        solidHeader = TRUE,
        plotlyOutput("periculosidade", height = 450)
      ),
      
      bs4Card(
        title = "Volume Estimado por Tipo",
        width = 6,
        status = "warning",
        solidHeader = TRUE,
        plotlyOutput("volume_tipo", height = 450)
      )
    )
  ),
  
  footer = bs4DashFooter()
)

server <- function(input, output, session){
  

  # TOP TIPOS DE RESÍDUOS
  output$top_residuos <- renderPlotly({
    
    dados <- dbGetQuery(con, "
      SELECT
        tr.nome_tipo,
        SUM(orr.quantidade) AS quantidade
      FROM ocorrencia_residuo orr
      INNER JOIN tipo_residuo tr
        ON tr.id_tipo_residuo = orr.id_tipo_residuo
      GROUP BY tr.nome_tipo
      ORDER BY quantidade DESC
    ")
    
    plot_ly(
      dados,
      x = ~quantidade,
      y = ~reorder(nome_tipo, quantidade),
      type = "bar",
      orientation = "h"
    ) %>%
      layout(
        xaxis = list(title = "Quantidade"),
        yaxis = list(title = "Tipo de Resíduo")
      )
    
  })
  

  # CATEGORIAS DE RESÍDUOS
  output$categorias <- renderPlotly({
    
    dados <- dbGetQuery(con, "
      SELECT
        tr.categoria,
        SUM(orr.quantidade) AS quantidade
      FROM ocorrencia_residuo orr
      INNER JOIN tipo_residuo tr
        ON tr.id_tipo_residuo = orr.id_tipo_residuo
      GROUP BY tr.categoria
      ORDER BY quantidade DESC
    ")
    
    plot_ly(
      dados,
      labels = ~categoria,
      values = ~quantidade,
      type = "pie",
      hole = 0.45
    )
    
  })
  

  # PERICULOSIDADE
  output$periculosidade <- renderPlotly({
    
    dados <- dbGetQuery(con, "
    SELECT
      tr.periculosidade,
      SUM(IFNULL(orr.volume_estimado,0)) AS volume
    FROM ocorrencia_residuo orr
    INNER JOIN tipo_residuo tr
      ON tr.id_tipo_residuo = orr.id_tipo_residuo
    GROUP BY tr.periculosidade
    ORDER BY volume DESC
  ")
    
    plot_ly(
      dados,
      x = ~periculosidade,
      y = ~volume,
      type = 'bar',
      color = ~periculosidade,
      text = ~round(volume, 2),
      textposition = "auto"
    ) %>%
      layout(
        xaxis = list(title = "Nível de Periculosidade"),
        yaxis = list(title = "Volume Estimado"),
        showlegend = FALSE
      )
    
  })
  

  # VOLUME ESTIMADO POR TIPO

  
  output$volume_tipo <- renderPlotly({
    
    dados <- dbGetQuery(con, "
      SELECT
        tr.nome_tipo,
        SUM(IFNULL(orr.volume_estimado,0)) AS volume
      FROM ocorrencia_residuo orr
      INNER JOIN tipo_residuo tr
        ON tr.id_tipo_residuo = orr.id_tipo_residuo
      GROUP BY tr.nome_tipo
      ORDER BY volume DESC
    ")
    
    plot_ly(
      dados,
      x = ~nome_tipo,
      y = ~volume,
      type = "bar"
    ) %>%
      layout(
        xaxis = list(title = "Tipo de Resíduo"),
        yaxis = list(title = "Volume Estimado")
      )
    
  })
  
}

shinyApp(ui, server)