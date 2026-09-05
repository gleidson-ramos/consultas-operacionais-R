# 📊 Consultas Operacionais: Dashboards utilizando R
Conjunto de dashboards interativos para monitoramento operacional de coleta de resíduos e ocorrências urbanas, desenvolvido com **R Shiny** e **bs4Dash**, integrado a um banco de dados **MySQL**.

## 📁 Estrutura do Projeto
```bash
Consultas Operacionais/
├── conexao.R                        # Configuração da conexão com o banco de dados
├── pacotes_instalados.R             # Script para instalação dos pacotes necessários
│
├── dashboard_01_operacoes.R         # Visão geral das ocorrências
├── dashboard_02_prioridades.R       # Análise de prioridades das ocorrências
├── dashboard_03_residuos.R          # Análise dos tipos e volumes de resíduos
├── dashboard_04_equipes.R           # Desempenho das equipes e responsáveis
├── dashboard_05_mapa.R              # Mapa dos pontos monitorados
├── dashboard_06_fila_dia.R          # Fila de atendimentos do dia
├── dashboard_06_fila_dia_v2.R       # Versão 2 da fila do dia (refatorada)
├── dashboard_06_mapa_regioes.R      # Mapa por regiões
├── dashboard_7_vistorias_hoje.R     # Vistorias realizadas hoje
├── dashboard_7_vistorias_hoje_v2.R  # Versão 2 das vistorias (simplificada)
└── dashboard_8_atendimentos_hoje.R  # Atendimentos em tempo real
```
## 🛠️ Tecnologias Utilizadas

| Pacote | Função |
|---|---|
| `shiny` | Framework para aplicações web interativas em R |
| `bs4Dash` | Interface com componentes Bootstrap 4 (cards, value boxes) |
| `plotly` | Gráficos interativos |
| `leaflet` | Mapas interativos |
| `leaflet.extras` | Funcionalidades extras para mapas |
| `DBI` | Interface genérica para bancos de dados |
| `RMySQL` | Driver de conexão com MySQL |
| `dplyr` | Manipulação de dados |


## 🚀 Como Executar
### Instale os pacotes R necessários
Abra o R ou RStudio e execute:
```r
source("pacotes_instalados.R")
```

### Configure a conexão com o banco de dados
Edite o arquivo `conexao.R` com as suas credenciais:
```r
con <- dbConnect(
  MySQL(),
  dbname = "laboratorio_cidades_sustentaveis",
  host   = "localhost",
  port   = 3306,
  user   = "seu_usuario",      # ← altere aqui
  password = "sua_senha"       # ← altere aqui
)
```

### Execute um dashboard
No RStudio, abra o arquivo desejado (ex: `dashboard_01_operacoes.R`) e clique em **Run App**, ou execute via console:
```r
shiny::runApp("dashboard_01_operacoes.R")
```

## 🗄️ Banco de Dados

O projeto utiliza o banco `laboratorio_cidades_sustentaveis` com as seguintes tabelas principais:

| Tabela | Descrição |
|---|---|
| `ocorrencia` | Ocorrências de descarte irregular |
| `status_ocorrencia` | Status possíveis para uma ocorrência |
| `atendimento_coleta` | Registros de coletas realizadas |
| `responsavel` | Equipes e responsáveis |
| `ponto_monitorado` | Pontos georreferenciados de monitoramento |
| `vistoria_ponto` | Vistorias realizadas nos pontos |
| `tipo_residuo` | Tipos e categorias de resíduos |
| `ocorrencia_residuo` | Resíduos associados a cada ocorrência |
| `logradouro` | Logradouros cadastrados |
| `bairro` | Bairros da cidade |

## 📄 Sobre o Projeto
Projeto desenvolvido na disciplina de Tópicos Especiais em Banco de Dados, utilizando dados fictícios provenientes de um banco de dados MySQL de uma cidade sustentável. A partir desses dados, foram desenvolvidos dashboards interativos com diferentes visualizações e indicadores, com o objetivo de apoiar a gestão operacional dos serviços de coleta de resíduos e saneamento urbano.