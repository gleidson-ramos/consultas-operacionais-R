library(shiny)
library(bs4Dash)
library(plotly)
library(DBI)
library(RMySQL)

source("conexao.R")

DATA_REF <- "2025-06-12"

ui <- bs4DashPage(
  header  = bs4DashNavbar(disable = TRUE),
  sidebar = bs4DashSidebar(disable = TRUE),
  
  body = bs4DashBody(
    
    # ── Value Boxes ────────────────────────────────────────────────────────
    fluidRow(
      bs4ValueBoxOutput("vb_total",   width = 3),
      bs4ValueBoxOutput("vb_agendado",   width = 3),
      bs4ValueBoxOutput("vb_andamento",   width = 2),
      bs4ValueBoxOutput("vb_atrasados",   width = 2),
      bs4ValueBoxOutput("vb_finalizados", width = 2)
    ),
    
    # ── Agendados para o dia ───────────────────────────────────────────────
    fluidRow(
      bs4Card(
        title       = paste("Atendimentos Agendados —", DATA_REF),
        width       = 8,
        status      = "primary",
        solidHeader = TRUE,
        div(style = "overflow-x: auto;", tableOutput("tabela_agendados"))
      ),
      
      bs4Card(
        title       = "Atendimentos por Prioridade",
        width       = 4,
        status      = "warning",
        solidHeader = TRUE,
        plotlyOutput("grafico_prioridade", height = 420)
      )
    ),
    
  ),
  
  footer = bs4DashFooter()
)

server <- function(input, output, session) {
  
  # ── Value Boxes ──────────────────────────────────────────────────────────
  output$vb_total <- renderbs4ValueBox({
    v <- dbGetQuery(con, paste0("
      SELECT COUNT(*) total FROM atendimento_coleta
      WHERE DATE(data_agendada) = '", DATA_REF, "'
    "))
    bs4ValueBox(
      value    = v$total,
      subtitle = "Total de Atendimentos",
      icon     = icon("calendar-check"),
      color    = "primary",
      gradient = TRUE
    )
  })
  
  output$vb_agendado <- renderbs4ValueBox({
    v <- dbGetQuery(con, paste0("
      SELECT COUNT(*) total FROM atendimento_coleta
      WHERE DATE(data_agendada) = '", DATA_REF, "'
      AND status_atendimento = 'AGENDADO'
    "))
    bs4ValueBox(
      value    = v$total,
      subtitle = "Aguardando Atendimento",
      icon     = icon("clock"),
      color    = "warning",
      gradient = TRUE
    )
  })
  
  output$vb_andamento <- renderbs4ValueBox({
    v <- dbGetQuery(con, paste0("
      SELECT COUNT(*) total FROM atendimento_coleta
      WHERE DATE(data_agendada) = '", DATA_REF, "'
      AND status_atendimento = 'EM_EXECUÇÃO'
    "))
    bs4ValueBox(
      value    = v$total,
      subtitle = "Em Andamento",
      icon     = icon("truck"),
      color    = "info",
      gradient = TRUE
    )
  })
  
  output$vb_atrasados <- renderbs4ValueBox({
    v <- dbGetQuery(con, paste0("
      SELECT COUNT(*) total FROM atendimento_coleta
      WHERE DATE(data_agendada) = '", DATA_REF, "'
      AND status_atendimento = 'CANCELADO'
    "))
    bs4ValueBox(
      value    = v$total,
      subtitle = "Cancelados",
      icon     = icon("exclamation-circle"),
      color    = "danger",
      gradient = TRUE
    )
  })
  
  output$vb_finalizados <- renderbs4ValueBox({
    v <- dbGetQuery(con, paste0("
      SELECT COUNT(*) total FROM atendimento_coleta
      WHERE DATE(data_agendada) = '", DATA_REF, "'
      AND status_atendimento = 'FINALIZADO'
    "))
    bs4ValueBox(
      value    = v$total,
      subtitle = "Finalizados",
      icon     = icon("check-circle"),
      color    = "success",
      gradient = TRUE
    )
  })
  
  # ── Gráfico: atendimentos por prioridade ────────────────────────────────────
  
  output$grafico_prioridade <- renderPlotly({
    dados <- dbGetQuery(con, paste0("
      SELECT
      	o.prioridade, COUNT(*) quantidade
      FROM atendimento_coleta a
      JOIN ocorrencia o       ON o.id_ocorrencia  = a.id_ocorrencia
      WHERE DATE(a.data_agendada) = '2025-06-12'
      group BY o.prioridade
    "))
    if (nrow(dados) == 0) {
      plot_ly() %>% layout(title = "Sem dados")
    } else {
      plot_ly(
        dados,
        labels = ~prioridade,
        values = ~quantidade,
        type   = "pie",
        hole   = 0.55
      ) %>%
        layout(showlegend = TRUE)
    }
  })
  
  # ── Tabela: agendados do dia ─────────────────────────────────────────────
  
  output$tabela_agendados <- renderTable({
    dados <- dbGetQuery(con, paste0("
      SELECT
        a.id_atendimento                              AS ID,
        TIME(a.data_agendada)                         AS `Hora Agendada`,
        r.nome                                        AS Responsável,
        l.tipo_logradouro 							  AS Logradouro,
        l.nome_logradouro                             AS Nome,
        b.nome_bairro                                 AS Bairro,
        o.prioridade                                  AS Prioridade,
        a.status_atendimento                          AS Status
      FROM atendimento_coleta a
      JOIN ocorrencia o       ON o.id_ocorrencia  = a.id_ocorrencia
      JOIN ponto_monitorado p ON p.id_ponto        = o.id_ponto
      JOIN logradouro l       ON l.id_logradouro   = p.id_logradouro
      JOIN bairro b           ON b.id_bairro       = l.id_bairro
      JOIN responsavel r      ON r.id_responsavel  = a.id_responsavel
      WHERE DATE(a.data_agendada) = '", DATA_REF, "'
      AND a.status_atendimento <> 'FINALIZADO'
      AND a.status_atendimento <> 'CANCELADO'
      ORDER BY a.data_agendada ASC
    "))
    if (nrow(dados) == 0) {
      data.frame(Mensagem = "Nenhum atendimento agendado para este dia.")
    } else {
      dados$ID <- as.integer(dados$ID)
      dados
    }
  })


}

shinyApp(ui, server)