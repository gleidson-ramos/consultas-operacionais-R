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
    
    # ── Value Boxes ──────────────────────────────────────────────────────────
    fluidRow(
      bs4ValueBoxOutput("agendados_hoje",    width = 3),
      bs4ValueBoxOutput("em_andamento",      width = 3),
      bs4ValueBoxOutput("atrasados",         width = 3),
      bs4ValueBoxOutput("finalizados_hoje",  width = 3)
    ),
    
    # ── Fila do dia: atendimentos agendados ──────────────────────────────────
    fluidRow(
      bs4Card(
        title       = "Atendimentos Agendados para Hoje",
        width       = 12,
        status      = "primary",
        solidHeader = TRUE,
        div(
          style = "overflow-x: auto;",
          tableOutput("tabela_agendados")
        )
      )
    ),
    
    # ── Em andamento + Atrasados ─────────────────────────────────────────────
    fluidRow(
      bs4Card(
        title       = "Em Andamento Agora",
        width       = 6,
        status      = "info",
        solidHeader = TRUE,
        div(
          style = "overflow-x: auto;",
          tableOutput("tabela_andamento")
        )
      ),
      
      bs4Card(
        title       = "Atrasados",
        width       = 6,
        status      = "danger",
        solidHeader = TRUE,
        div(
          style = "overflow-x: auto;",
          tableOutput("tabela_atrasados")
        )
      )
    ),
    
    # ── Ocorrências urgentes sem responsável ─────────────────────────────────
    fluidRow(
      bs4Card(
        title       = "Ocorrências Urgentes / Alta Sem Responsável",
        width       = 6,
        status      = "danger",
        solidHeader = TRUE,
        div(
          style = "overflow-x: auto;",
          tableOutput("tabela_sem_responsavel")
        )
      ),
      
      bs4Card(
        title       = "Carga Atual por Responsável",
        width       = 6,
        status      = "warning",
        solidHeader = TRUE,
        plotlyOutput("grafico_carga", height = 350)
      )
    )
  ),
  
  footer = bs4DashFooter()
)

server <- function(input, output, session) {
  
  # ── Value Boxes ─────────────────────────────────────────────────────────────
  
  output$agendados_hoje <- renderbs4ValueBox({
    valor <- dbGetQuery(con, "
      SELECT COUNT(*) total FROM atendimento_coleta
      WHERE DATE(data_agendada) = '2025-06-12'
    ")
    bs4ValueBox(
      value    = valor$total,
      subtitle = "Agendados Hoje",
      icon     = icon("calendar-check"),
      color    = "primary",
      gradient = TRUE
    )
  })
  
  output$em_andamento <- renderbs4ValueBox({
    valor <- dbGetQuery(con, "
      SELECT COUNT(*) total FROM atendimento_coleta
      WHERE data_inicio IS NOT NULL
        AND data_fim IS NULL
    ")
    bs4ValueBox(
      value    = valor$total,
      subtitle = "Em Andamento",
      icon     = icon("truck"),
      color    = "info",
      gradient = TRUE
    )
  })
  
  output$atrasados <- renderbs4ValueBox({
    valor <- dbGetQuery(con, "
      SELECT COUNT(*) total FROM atendimento_coleta
      WHERE data_agendada < '2025-06-12 23:59:59'
        AND data_fim IS NULL
        AND status_atendimento NOT IN ('CANCELADO','Cancelado')
    ")
    bs4ValueBox(
      value    = valor$total,
      subtitle = "Atrasados",
      icon     = icon("exclamation-circle"),
      color    = "danger",
      gradient = TRUE
    )
  })
  
  output$finalizados_hoje <- renderbs4ValueBox({
    valor <- dbGetQuery(con, "
      SELECT COUNT(*) total FROM atendimento_coleta
      WHERE DATE(data_fim) = '2025-06-12'
    ")
    bs4ValueBox(
      value    = valor$total,
      subtitle = "Finalizados Hoje",
      icon     = icon("check-circle"),
      color    = "success",
      gradient = TRUE
    )
  })
  
  # ── Tabela: agendados para hoje ──────────────────────────────────────────────
  
  output$tabela_agendados <- renderTable({
    dados <- dbGetQuery(con, "
      SELECT
        a.id_atendimento                          AS ID,
        TIME(a.data_agendada)                     AS `Hora Agendada`,
        r.nome                                    AS Responsável,
        b.nome_bairro                             AS Bairro,
        l.nome_logradouro                         AS Logradouro,
        o.prioridade                              AS Prioridade,
        a.status_atendimento                      AS Status
      FROM atendimento_coleta a
      JOIN ocorrencia o        ON o.id_ocorrencia  = a.id_ocorrencia
      JOIN ponto_monitorado p  ON p.id_ponto        = o.id_ponto
      JOIN logradouro l        ON l.id_logradouro   = p.id_logradouro
      JOIN bairro b            ON b.id_bairro       = l.id_bairro
      JOIN responsavel r       ON r.id_responsavel  = a.id_responsavel
      WHERE DATE(a.data_agendada) = '2025-06-12'
      ORDER BY a.data_agendada ASC
    ")
    if (nrow(dados) == 0) {
      data.frame(Mensagem = "Nenhum atendimento agendado para hoje.")
    } else {
      dados$ID <- as.integer(dados$ID)
      dados
    }
  })
  
  # ── Tabela: em andamento ─────────────────────────────────────────────────────
  
  output$tabela_andamento <- renderTable({
    dados <- dbGetQuery(con, "
      SELECT
        a.id_atendimento                                        AS ID,
        r.nome                                                  AS Responsável,
        b.nome_bairro                                           AS Bairro,
        l.nome_logradouro                                       AS Logradouro,
        o.prioridade                                            AS Prioridade,
        TIME(a.data_inicio)                                     AS `Início`,
        CONCAT(
          TIMESTAMPDIFF(HOUR, a.data_inicio, '2025-06-12 23:59:59'), 'h ',
          MOD(TIMESTAMPDIFF(MINUTE, a.data_inicio, '2025-06-12 23:59:59'), 60), 'min'
        )                                                       AS `Tempo Decorrido`
      FROM atendimento_coleta a
      JOIN ocorrencia o       ON o.id_ocorrencia = a.id_ocorrencia
      JOIN ponto_monitorado p ON p.id_ponto       = o.id_ponto
      JOIN logradouro l       ON l.id_logradouro  = p.id_logradouro
      JOIN bairro b           ON b.id_bairro      = l.id_bairro
      JOIN responsavel r      ON r.id_responsavel = a.id_responsavel
      WHERE DATE(a.data_agendada) = '2025-06-12'
        AND a.data_inicio IS NOT NULL
        AND a.data_fim IS NULL
      ORDER BY a.data_inicio ASC
    ")
    if (nrow(dados) == 0) {
      data.frame(Mensagem = "Nenhum atendimento em andamento.")
    } else {
      dados$ID <- as.integer(dados$ID)
      dados
    }
  })
  
  # ── Tabela: atrasados ────────────────────────────────────────────────────────
  
  output$tabela_atrasados <- renderTable({
    dados <- dbGetQuery(con, "
      SELECT
        a.id_atendimento                                          AS ID,
        r.nome                                                    AS Responsável,
        b.nome_bairro                                             AS Bairro,
        o.prioridade                                              AS Prioridade,
        a.status_atendimento                                      AS Status,
        DATE_FORMAT(a.data_agendada, '%d/%m %H:%i')              AS `Agendado Para`,
        CONCAT(
          TIMESTAMPDIFF(HOUR, a.data_agendada, '2025-06-12 23:59:59'), 'h ',
          MOD(TIMESTAMPDIFF(MINUTE, a.data_agendada, '2025-06-12 23:59:59'), 60), 'min'
        )                                                         AS `Atraso`
      FROM atendimento_coleta a
      JOIN ocorrencia o       ON o.id_ocorrencia = a.id_ocorrencia
      JOIN ponto_monitorado p ON p.id_ponto       = o.id_ponto
      JOIN logradouro l       ON l.id_logradouro  = p.id_logradouro
      JOIN bairro b           ON b.id_bairro      = l.id_bairro
      JOIN responsavel r      ON r.id_responsavel = a.id_responsavel
      WHERE DATE(a.data_agendada) = '2025-06-12'
        AND a.data_agendada < '2025-06-12 23:59:59'
        AND a.data_fim IS NULL
        AND a.status_atendimento NOT IN ('CANCELADO','Cancelado')
      ORDER BY a.data_agendada ASC
    ")
    if (nrow(dados) == 0) {
      data.frame(Mensagem = "Nenhum atendimento atrasado.")
    } else {
      dados$ID <- as.integer(dados$ID)
      dados
    }
  })
  
  # ── Tabela: ocorrências urgentes/alta sem responsável ────────────────────────
  
  output$tabela_sem_responsavel <- renderTable({
    dados <- dbGetQuery(con, "
      SELECT
        o.id_ocorrencia                                           AS ID,
        DATE_FORMAT(o.data_abertura, '%d/%m %H:%i')              AS `Aberta Em`,
        b.nome_bairro                                            AS Bairro,
        l.nome_logradouro                                        AS Logradouro,
        o.prioridade                                             AS Prioridade,
        CONCAT(
          TIMESTAMPDIFF(HOUR, o.data_abertura, '2025-06-12 23:59:59'), 'h em aberto'
        )                                                        AS `Tempo Aberta`
      FROM ocorrencia o
      JOIN ponto_monitorado p ON p.id_ponto      = o.id_ponto
      JOIN logradouro l       ON l.id_logradouro  = p.id_logradouro
      JOIN bairro b           ON b.id_bairro      = l.id_bairro
      WHERE o.id_responsavel IS NULL
        AND o.data_encerramento IS NULL
        AND o.prioridade IN ('URGENTE','ALTA')
        AND DATE(o.data_abertura) <= '2025-06-12'
      ORDER BY
        FIELD(o.prioridade,'URGENTE','ALTA'),
        o.data_abertura ASC
    ")
    if (nrow(dados) == 0) {
      data.frame(Mensagem = "Nenhuma ocorrência urgente sem responsável.")
    } else {
      dados$ID <- as.integer(dados$ID)
      dados
    }
  })
  
  # ── Gráfico: carga atual por responsável ─────────────────────────────────────
  
  output$grafico_carga <- renderPlotly({
    dados <- dbGetQuery(con, "
      SELECT
        r.nome,
        SUM(CASE WHEN a.data_fim IS NULL
                  AND a.status_atendimento NOT IN ('CANCELADO','Cancelado')
             THEN 1 ELSE 0 END) AS atendimentos_pendentes,
        SUM(CASE WHEN o2.data_encerramento IS NULL
             THEN 1 ELSE 0 END) AS ocorrencias_abertas
      FROM responsavel r
      LEFT JOIN atendimento_coleta a ON a.id_responsavel = r.id_responsavel
        AND DATE(a.data_agendada) = '2025-06-12'
      LEFT JOIN ocorrencia o2 ON o2.id_responsavel = r.id_responsavel
        AND DATE(o2.data_abertura) <= '2025-06-12'
        AND o2.data_encerramento IS NULL
      GROUP BY r.nome
      HAVING atendimentos_pendentes > 0 OR ocorrencias_abertas > 0
      ORDER BY atendimentos_pendentes DESC
    ")
    
    if (nrow(dados) == 0) {
      plot_ly() %>% layout(title = "Sem pendências no momento")
    } else {
      plot_ly(dados, x = ~reorder(nome, atendimentos_pendentes)) %>%
        add_bars(
          y    = ~atendimentos_pendentes,
          name = "Atendimentos pendentes",
          marker = list(color = "#3498DB")
        ) %>%
        add_bars(
          y    = ~ocorrencias_abertas,
          name = "Ocorrências em aberto",
          marker = list(color = "#E67E22")
        ) %>%
        layout(
          barmode = "group",
          xaxis   = list(title = "Responsável"),
          yaxis   = list(title = "Quantidade"),
          legend  = list(orientation = "h", y = -0.25)
        )
    }
  })
}

shinyApp(ui, server)