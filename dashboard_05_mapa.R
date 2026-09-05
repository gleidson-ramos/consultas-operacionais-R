library(shiny)
library(bs4Dash)
library(DBI)
library(RMySQL)
library(leaflet)

source("conexao.R")

ui <- bs4DashPage(

  header = bs4DashNavbar(disable = TRUE),

  sidebar = bs4DashSidebar(disable = TRUE),

  body = bs4DashBody(

    fluidRow(
      bs4Card(
        title = "Pontos Monitorados",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        maximizable = TRUE,
        closable = FALSE,
        leafletOutput("mapa", height = 700)
      )
    ),

    fluidRow(
      bs4Card(
        title = "Pontos por Bairro",
        width = 6,
        status = "success",
        solidHeader = TRUE,
        tableOutput("bairro_resumo")
      ),

      bs4Card(
        title = "Resumo Geral",
        width = 6,
        status = "warning",
        solidHeader = TRUE,
        tableOutput("resumo")
      )
    )
  ),

  footer = bs4DashFooter()

)

server <- function(input, output, session){

  output$mapa <- renderLeaflet({
    dados <- dbGetQuery(con,"
      SELECT
        p.id_ponto, p.latitude, p.longitude, b.nome_bairro, l.nome_logradouro
      FROM ponto_monitorado p
      JOIN logradouro l ON l.id_logradouro = p.id_logradouro
      JOIN bairro b ON b.id_bairro = l.id_bairro
    ")

    leaflet(dados) |>
      addProviderTiles(
        providers$CartoDB.DarkMatter
      ) |>

      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 8,
        popup = ~paste(
          "<b>Ponto:</b>", id_ponto,
          "<br><b>Bairro:</b>", nome_bairro,
          "<br><b>Logradouro:</b>", nome_logradouro
        ),
        stroke = FALSE,
        fillOpacity = 0.8
      )
  })

  output$bairro_resumo <- renderTable({
    dados <- dbGetQuery(con, "SELECT b.nome_bairro,
    COUNT(*) AS pontos
    FROM ponto_monitorado p
    JOIN logradouro l ON l.id_logradouro = p.id_logradouro
    JOIN bairro b ON b.id_bairro = l.id_bairro
    GROUP BY b.nome_bairro
    ORDER BY pontos DESC
  ")
    dados$pontos <- as.integer(dados$pontos)
    names(dados) <- c("Bairro", "Quantidade de Pontos")
    dados
  })

  output$resumo <- renderTable({
    dados <- dbGetQuery(con, "SELECT COUNT(*) AS total_pontos,
    SUM(CASE WHEN ativo = 1 THEN 1 ELSE 0 END) AS ativos
    FROM ponto_monitorado")
    
    dados$total_pontos <- as.integer(dados$total_pontos)
    dados$ativos <- as.integer(dados$ativos)
    names(dados) <- c("Total de Pontos", "Pontos Ativos")
    dados
  })

}

shinyApp(ui, server)
