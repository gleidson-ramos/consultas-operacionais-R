library(shiny)
library(bs4Dash)
library(plotly)
library(leaflet)
library(DBI)
library(RMySQL)

source("conexao.R")

# ─────────────────────────────────────────────
# PERÍODO GLOBAL
# ─────────────────────────────────────────────
data_ini <- as.Date("2024-12-10")
data_fim <- as.Date("2024-12-17")

data_ini_sql <- paste0(data_ini, " 00:00:00")
data_fim_sql <- paste0(data_fim + 1, " 00:00:00")

# ─────────────────────────────────────────────
# BASE ÚNICA DO DASHBOARD (SUA SQL AJUSTADA)
# ─────────────────────────────────────────────
base_pontos <- function() {
  sprintf("
    SELECT
        p.id_ponto AS Ponto,
        l.tipo_logradouro AS Logradouro,
        l.nome_logradouro AS Nome,
        b.nome_bairro AS Bairro,
        MAX(v.condicao_local) AS Condicao,
        MAX(v.nivel_acumulo) AS Nivel,
        MAX(v.data_vistoria) AS UltimaVistoria
    FROM ponto_monitorado p
    LEFT JOIN vistoria_ponto v ON v.id_ponto = p.id_ponto
    LEFT JOIN logradouro l ON l.id_logradouro = p.id_logradouro
    LEFT JOIN bairro b ON b.id_bairro = l.id_bairro
    WHERE p.ativo = 1
    GROUP BY
        p.id_ponto,
        l.tipo_logradouro,
        l.nome_logradouro,
        b.nome_bairro
    HAVING MAX(v.data_vistoria) >= '%s'
       AND MAX(v.data_vistoria) <  '%s'
  ", data_ini_sql, data_fim_sql)
}

ui <- bs4DashPage(
  header = bs4DashNavbar(disable = TRUE),
  sidebar = bs4DashSidebar(disable = TRUE),
  
  body = bs4DashBody(
    
    fluidRow(
      bs4ValueBoxOutput("vb_total", width = 4),
      bs4ValueBoxOutput("vb_alto", width = 4),
      bs4ValueBoxOutput("vb_sem_atendimento", width = 4)
    ),
    
    fluidRow(
      bs4Card(
        title = "Pontos no Período",
        width = 8,
        status = "primary",
        solidHeader = TRUE,
        tableOutput("tabela")
      ),
      
      bs4Card(
        title = "Distribuição de Acúmulo",
        width = 4,
        status = "warning",
        solidHeader = TRUE,
        plotlyOutput("grafico", height = 320)
      )
    ),
    
    fluidRow(
      bs4Card(
        title = "Mapa de Pontos",
        width = 12,
        status = "danger",
        solidHeader = TRUE,
        leafletOutput("mapa", height = 550)
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ─────────────────────────────
  # TOTAL PONTOS
  # ─────────────────────────────
  output$vb_total <- renderbs4ValueBox({
    
    sql <- sprintf("SELECT COUNT(*) total FROM (%s) t", base_pontos())
    res <- dbGetQuery(con, sql)
    
    bs4ValueBox(
      value = res$total,
      subtitle = "Pontos no Período",
      icon = icon("map"),
      color = "primary"
    )
  })
  
  # ─────────────────────────────
  # ACÚMULO ALTO
  # ─────────────────────────────
  output$vb_alto <- renderbs4ValueBox({
    
    sql <- sprintf("
      SELECT COUNT(*) total FROM (
        SELECT * FROM (%s) t
        WHERE Nivel IN ('ALTO','Alto')
      ) x
    ", base_pontos())
    
    res <- dbGetQuery(con, sql)
    
    bs4ValueBox(
      value = res$total,
      subtitle = "Acúmulo Alto",
      icon = icon("exclamation-triangle"),
      color = "warning"
    )
  })
  
  
  # ─────────────────────────────
  # SEM ATENDIMENTO (simplificado)
  # ─────────────────────────────
  output$vb_sem_atendimento <- renderbs4ValueBox({
    
    sql <- sprintf("
      SELECT COUNT(*) total FROM (
        SELECT * FROM (%s) t
        WHERE Nivel IN ('ALTO','CRITICO','Alto','Crítico')
      ) x
    ", base_pontos())
    
    res <- dbGetQuery(con, sql)
    
    bs4ValueBox(
      value = res$total,
      subtitle = "Nível Críticos",
      icon = icon("map-marker-alt"),
      color = "danger"
    )
  })
  
  # ─────────────────────────────
  # TABELA
  # ─────────────────────────────
  output$tabela <- renderTable({
    
    dbGetQuery(con, base_pontos())
  })
  
  # ─────────────────────────────
  # GRÁFICO
  # ─────────────────────────────
  output$grafico <- renderPlotly({
    
    sql <- sprintf("
      SELECT Nivel, COUNT(*) qtd
      FROM (%s) t
      GROUP BY Nivel
    ", base_pontos())
    
    dados <- dbGetQuery(con, sql)
    
    if (nrow(dados) == 0) {
      plot_ly() %>% layout(title = "Sem dados")
    } else {
      plot_ly(dados, labels = ~Nivel, values = ~qtd, type = "pie", hole = 0.5)
    }
  })
  
  # ─────────────────────────────
  # MAPA
  # ─────────────────────────────
  output$mapa <- renderLeaflet({
    
    sql <- sprintf("
      SELECT *
      FROM (%s) t
    ", base_pontos())
    
    dados <- dbGetQuery(con, sql)
    
    leaflet(dados) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(
        lng = ~NULL,
        lat = ~NULL,
        radius = 0
      )
  })
}

shinyApp(ui, server)