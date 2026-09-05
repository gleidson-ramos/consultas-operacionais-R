library(shiny)
library(bs4Dash)
library(DBI)
library(RMySQL)
library(leaflet)
library(plotly)
library(dplyr)

source("conexao.R")

ui <- bs4DashPage(

  header = bs4DashNavbar(disable = TRUE),

  sidebar = bs4DashSidebar(disable = TRUE),

  body = bs4DashBody(
    fluidRow(
      bs4ValueBoxOutput("total_regioes", width = 3),
      bs4ValueBoxOutput("total_bairros", width = 3),
      bs4ValueBoxOutput("total_pontos", width = 3),
      bs4ValueBoxOutput("pontos_ativos", width = 3)
    ),

    fluidRow(
      bs4Card(
        title = "Mapa dos Pontos por Região Administrativa",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        maximizable = TRUE,
        leafletOutput("mapa", height = 700)
      )
    ),

    fluidRow(
      bs4Card(
        title = "Quantidade de Pontos por Região",
        width = 6,
        status = "success",
        solidHeader = TRUE,
        plotlyOutput("grafico_regiao", height = 450)
      ),
      bs4Card(
        title = "Distribuição Percentual",
        width = 6,
        status = "warning",
        solidHeader = TRUE,
        plotlyOutput("pizza_regiao", height = 450)
      )
    ),
    fluidRow(
      bs4Card(
        title = "Resumo por Região Administrativa",
        width = 12,
        status = "info",
        solidHeader = TRUE,
        tableOutput("tabela_regioes")
      )
    )
  ),

  footer = bs4DashFooter()
)


server <- function(input, output, session){

  output$total_regioes <- renderbs4ValueBox({
    valor <- dbGetQuery(con, "SELECT COUNT(*) total FROM regiao_administrativa")
    bs4ValueBox(valor$total, "Regiões", icon = icon("globe"), color = "primary", gradient = TRUE)
  })

  output$total_bairros <- renderbs4ValueBox({
    valor <- dbGetQuery(con, "SELECT COUNT(*) total FROM bairro")
    bs4ValueBox(valor$total, "Bairros", icon = icon("city"), color = "success", gradient = TRUE)
  })

  output$total_pontos <- renderbs4ValueBox({
    valor <- dbGetQuery(con, "SELECT COUNT(*) total FROM ponto_monitorado")
    bs4ValueBox(valor$total, "Pontos", icon = icon("map-marker-alt"), color = "warning", gradient = TRUE)
  })

  output$pontos_ativos <- renderbs4ValueBox({
    valor <- dbGetQuery(con, "SELECT COUNT(*) total FROM ponto_monitorado WHERE ativo = 1")
    bs4ValueBox(valor$total, "Pontos Ativos", icon = icon("check-circle"), color = "danger", gradient = TRUE)
  })

  dados_mapa <- reactive({
    dbGetQuery(con, "
      SELECT
        ra.nome_regiao,
        b.nome_bairro,
        l.nome_logradouro,
        p.id_ponto,
        p.latitude,
        p.longitude,
        p.ativo
      FROM ponto_monitorado p
      INNER JOIN logradouro l ON l.id_logradouro = p.id_logradouro
      INNER JOIN bairro b ON b.id_bairro = l.id_bairro
      INNER JOIN regiao_administrativa ra ON ra.id_regiao = b.id_regiao
    ")
  })

  output$mapa <- renderLeaflet({
    dados <- dados_mapa()

    pal <- colorFactor(
      palette = c("#1abc9c","#3498db","#9b59b6","#e74c3c",
                  "#f39c12","#2ecc71","#34495e","#16a085","#8e44ad"),
      domain = dados$nome_regiao
    )

    leaflet(dados) |>
      addProviderTiles(providers$CartoDB.DarkMatter) |>
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 8,
        fillColor = ~pal(nome_regiao),
        color = "#FFFFFF",
        weight = 1,
        fillOpacity = 0.9,
        popup = ~paste0(
          "<b>Região:</b> ", nome_regiao,
          "<br><b>Bairro:</b> ", nome_bairro,
          "<br><b>Logradouro:</b> ", nome_logradouro,
          "<br><b>Ponto:</b> ", id_ponto,
          "<br><b>Ativo:</b> ", ifelse(ativo == 1, "Sim", "Não")
        ),
        clusterOptions = markerClusterOptions()
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~nome_regiao,
        title = "Regiões Administrativas"
      )
  })

  output$grafico_regiao <- renderPlotly({
    dados <- dbGetQuery(con, "
    SELECT ra.nome_regiao, COUNT(*) quantidade
    FROM ponto_monitorado p
    INNER JOIN logradouro l ON l.id_logradouro = p.id_logradouro
    INNER JOIN bairro b ON b.id_bairro = l.id_bairro
    INNER JOIN regiao_administrativa ra ON ra.id_regiao = b.id_regiao
    GROUP BY ra.nome_regiao
    ORDER BY quantidade DESC")
    
    plot_ly(
      dados,
      x = ~quantidade,
      y = ~nome_regiao,
      type = "bar",
      orientation = "h",
      color = ~nome_regiao
    ) %>%
      layout(
        xaxis = list(title = "Quantidade de Pontos"),
        yaxis = list(title = "Região Administrativa")
      )
  })

  output$pizza_regiao <- renderPlotly({
    dados <- dbGetQuery(con, "
      SELECT ra.nome_regiao, COUNT(*) quantidade
      FROM ponto_monitorado p
      INNER JOIN logradouro l ON l.id_logradouro = p.id_logradouro
      INNER JOIN bairro b ON b.id_bairro = l.id_bairro
      INNER JOIN regiao_administrativa ra ON ra.id_regiao = b.id_regiao
      GROUP BY ra.nome_regiao")

    plot_ly(
      dados,
      labels = ~nome_regiao,
      values = ~quantidade,
      type = "pie",
      hole = 0.55
    )
  })

  output$tabela_regioes <- renderTable({
    
    dados <- dbGetQuery(con, "
    SELECT
      ra.nome_regiao AS Regiao,
      COUNT(DISTINCT b.id_bairro) AS Bairros,
      COUNT(DISTINCT p.id_ponto) AS Pontos,
      SUM(CASE WHEN p.ativo = 1 THEN 1 ELSE 0 END) AS Pontos_Ativos
      FROM regiao_administrativa ra
      LEFT JOIN bairro b ON b.id_regiao = ra.id_regiao
      LEFT JOIN logradouro l ON l.id_bairro = b.id_bairro
      LEFT JOIN ponto_monitorado p ON p.id_logradouro = l.id_logradouro
      GROUP BY ra.nome_regiao
      ORDER BY Pontos DESC")
    
    dados$Bairros <- as.integer(dados$Bairros)
    dados$Pontos <- as.integer(dados$Pontos)
    dados$Pontos_Ativos <- as.integer(dados$Pontos_Ativos)
    
    names(dados)[names(dados) == "Pontos_Ativos"] <- "Pontos Ativos"
    
    dados
  })
}


shinyApp(ui, server)
