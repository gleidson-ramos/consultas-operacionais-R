library(shiny)
library(bs4Dash)
library(plotly)
library(leaflet)
library(DBI)
library(RMySQL)

source("conexao.R")

ui <- bs4DashPage(
  header = bs4DashNavbar(disable = TRUE),

  sidebar = bs4DashSidebar(disable = TRUE),

  body = bs4DashBody(

    # ── Value Boxes ──────────────────────────────────────────────────────────
    fluidRow(
      bs4ValueBoxOutput("vistorias_hoje",       width = 3),
      bs4ValueBoxOutput("acumulo_alto",         width = 3),
      bs4ValueBoxOutput("sem_vistoria_7dias",   width = 3),
      bs4ValueBoxOutput("pontos_sem_atend",     width = 3)
    ),

    # ── Tabela de vistorias do dia + gráfico acúmulo ─────────────────────────
    fluidRow(
      bs4Card(
        title      = "Vistorias Realizadas Hoje",
        width      = 8,
        status     = "primary",
        solidHeader = TRUE,
        div(
          style = "overflow-x: auto;",
          tableOutput("tabela_vistorias_hoje")
        )
      ),

      bs4Card(
        title      = "Nível de Acúmulo — Hoje",
        width      = 4,
        status     = "warning",
        solidHeader = TRUE,
        plotlyOutput("grafico_acumulo", height = 320)
      )
    ),

    # ── Mapa: pontos críticos sem atendimento ────────────────────────────────
    fluidRow(
      bs4Card(
        title       = "Pontos com Acúmulo Alto Sem Atendimento Agendado",
        width       = 12,
        status      = "danger",
        solidHeader = TRUE,
        maximizable = TRUE,
        closable    = FALSE,
        leafletOutput("mapa_criticos", height = 550)
      )
    ),

    # ── Pontos sem vistoria recente ──────────────────────────────────────────
    fluidRow(
      bs4Card(
        title      = "Pontos Sem Vistoria nos Últimos 7 Dias",
        width      = 12,
        status     = "warning",
        solidHeader = TRUE,
        div(
          style = "overflow-x: auto;",
          tableOutput("tabela_sem_vistoria")
        )
      )
    )
  ),

  footer = bs4DashFooter()
)

server <- function(input, output, session) {

  # ── Value Boxes ─────────────────────────────────────────────────────────────

  output$vistorias_hoje <- renderbs4ValueBox({
    valor <- dbGetQuery(con, "
      SELECT COUNT(*) total FROM vistoria_ponto
      WHERE DATE(data_vistoria) = CURDATE()
    ")
    bs4ValueBox(
      value    = valor$total,
      subtitle = "Vistorias Hoje",
      icon     = icon("search-location"),
      color    = "primary",
      gradient = TRUE
    )
  })

  output$acumulo_alto <- renderbs4ValueBox({
    valor <- dbGetQuery(con, "
      SELECT COUNT(*) total FROM vistoria_ponto
      WHERE DATE(data_vistoria) = CURDATE()
        AND nivel_acumulo IN ('ALTO', 'CRITICO', 'Alto', 'Crítico')
    ")
    bs4ValueBox(
      value    = valor$total,
      subtitle = "Acúmulo Alto/Crítico Hoje",
      icon     = icon("exclamation-triangle"),
      color    = "danger",
      gradient = TRUE
    )
  })

  output$sem_vistoria_7dias <- renderbs4ValueBox({
    valor <- dbGetQuery(con, "
      SELECT COUNT(*) total FROM ponto_monitorado p
      WHERE p.ativo = 1
        AND p.id_ponto NOT IN (
          SELECT id_ponto FROM vistoria_ponto
          WHERE data_vistoria >= DATE_SUB(CURDATE(), INTERVAL 7 DAY)
        )
    ")
    bs4ValueBox(
      value    = valor$total,
      subtitle = "Pontos Sem Vistoria (7 dias)",
      icon     = icon("clock"),
      color    = "warning",
      gradient = TRUE
    )
  })

  output$pontos_sem_atend <- renderbs4ValueBox({
    valor <- dbGetQuery(con, "
      SELECT COUNT(DISTINCT v.id_ponto) total
      FROM vistoria_ponto v
      WHERE v.nivel_acumulo IN ('ALTO','CRITICO','Alto','Crítico')
        AND v.data_vistoria >= DATE_SUB(CURDATE(), INTERVAL 3 DAY)
        AND v.id_ponto NOT IN (
          SELECT o.id_ponto FROM ocorrencia o
          JOIN atendimento_coleta a ON a.id_ocorrencia = o.id_ocorrencia
          WHERE a.data_agendada >= CURDATE()
            AND a.status_atendimento NOT IN ('CANCELADO','Cancelado')
        )
    ")
    bs4ValueBox(
      value    = valor$total,
      subtitle = "Críticos Sem Atendimento",
      icon     = icon("map-marker-alt"),
      color    = "danger",
      gradient = TRUE
    )
  })

  # ── Tabela: vistorias realizadas hoje ────────────────────────────────────────

  output$tabela_vistorias_hoje <- renderTable({
    dados <- dbGetQuery(con, "
      SELECT
        v.id_vistoria                          AS ID,
        TIME(v.data_vistoria)                  AS Hora,
        b.nome_bairro                          AS Bairro,
        l.nome_logradouro                      AS Logradouro,
        v.nivel_acumulo                        AS `Nível Acúmulo`,
        v.condicao_local                       AS `Condição Local`,
        r.nome                                 AS Responsável,
        IFNULL(v.observacao, '—')              AS Observação
      FROM vistoria_ponto v
      JOIN ponto_monitorado p  ON p.id_ponto      = v.id_ponto
      JOIN logradouro l        ON l.id_logradouro  = p.id_logradouro
      JOIN bairro b            ON b.id_bairro      = l.id_bairro
      JOIN responsavel r       ON r.id_responsavel = v.id_responsavel
      WHERE DATE(v.data_vistoria) = CURDATE()
      ORDER BY v.data_vistoria DESC
    ")
    if (nrow(dados) == 0) {
      data.frame(Mensagem = "Nenhuma vistoria registrada hoje.")
    } else {
      dados
    }
  })

  # ── Gráfico: distribuição do nível de acúmulo hoje ───────────────────────────

  output$grafico_acumulo <- renderPlotly({
    dados <- dbGetQuery(con, "
      SELECT nivel_acumulo, COUNT(*) quantidade
      FROM vistoria_ponto
      WHERE DATE(data_vistoria) = CURDATE()
      GROUP BY nivel_acumulo
    ")
    if (nrow(dados) == 0) {
      plot_ly() %>%
        layout(title = "Sem dados hoje")
    } else {
      plot_ly(
        dados,
        labels = ~nivel_acumulo,
        values = ~quantidade,
        type   = "pie",
        hole   = 0.55
      ) %>%
        layout(showlegend = TRUE)
    }
  })

  # ── Mapa: pontos críticos sem atendimento agendado ───────────────────────────

  output$mapa_criticos <- renderLeaflet({
    dados <- dbGetQuery(con, "
      SELECT DISTINCT
        p.id_ponto,
        p.latitude,
        p.longitude,
        b.nome_bairro,
        l.nome_logradouro,
        v.nivel_acumulo,
        v.condicao_local,
        v.data_vistoria
      FROM vistoria_ponto v
      JOIN ponto_monitorado p ON p.id_ponto      = v.id_ponto
      JOIN logradouro l       ON l.id_logradouro  = p.id_logradouro
      JOIN bairro b           ON b.id_bairro      = l.id_bairro
      WHERE v.nivel_acumulo IN ('ALTO','CRITICO','Alto','Crítico')
        AND v.data_vistoria >= DATE_SUB(CURDATE(), INTERVAL 3 DAY)
        AND p.id_ponto NOT IN (
          SELECT o.id_ponto FROM ocorrencia o
          JOIN atendimento_coleta a ON a.id_ocorrencia = o.id_ocorrencia
          WHERE a.data_agendada >= CURDATE()
            AND a.status_atendimento NOT IN ('CANCELADO','Cancelado')
        )
    ")

    mapa <- leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter)

    if (nrow(dados) == 0) {
      mapa %>%
        addControl("<b>Nenhum ponto crítico sem atendimento.</b>",
                   position = "topright")
    } else {
      mapa %>%
        addCircleMarkers(
          data        = dados,
          lng         = ~longitude,
          lat         = ~latitude,
          radius      = 10,
          fillColor   = "#E74C3C",
          color       = "#FFFFFF",
          weight      = 1,
          fillOpacity = 0.9,
          popup       = ~paste0(
            "<b>Ponto:</b> ",       id_ponto,
            "<br><b>Bairro:</b> ",  nome_bairro,
            "<br><b>Local:</b> ",   nome_logradouro,
            "<br><b>Acúmulo:</b> ", nivel_acumulo,
            "<br><b>Condição:</b> ", condicao_local,
            "<br><b>Vistoria:</b> ", format(as.POSIXct(data_vistoria), "%d/%m %H:%M")
          )
        )
    }
  })

  # ── Tabela: pontos sem vistoria nos últimos 7 dias ───────────────────────────

  output$tabela_sem_vistoria <- renderTable({
    dados <- dbGetQuery(con, "
      SELECT
        p.id_ponto                                         AS Ponto,
        b.nome_bairro                                      AS Bairro,
        l.nome_logradouro                                  AS Logradouro,
        IFNULL(p.referencia, '—')                          AS Referência,
        IFNULL(MAX(v.data_vistoria), 'Nunca vistoriado')   AS `Última Vistoria`
      FROM ponto_monitorado p
      JOIN logradouro l ON l.id_logradouro = p.id_logradouro
      JOIN bairro b     ON b.id_bairro     = l.id_bairro
      LEFT JOIN vistoria_ponto v ON v.id_ponto = p.id_ponto
      WHERE p.ativo = 1
      GROUP BY p.id_ponto, b.nome_bairro, l.nome_logradouro, p.referencia
      HAVING MAX(v.data_vistoria) < DATE_SUB(CURDATE(), INTERVAL 7 DAY)
          OR MAX(v.data_vistoria) IS NULL
      ORDER BY `Última Vistoria` ASC
    ")
    if (nrow(dados) == 0) {
      data.frame(Mensagem = "Todos os pontos foram vistoriados nos últimos 7 dias.")
    } else {
      dados$Ponto <- as.integer(dados$Ponto)
      dados
    }
  })
}

shinyApp(ui, server)
