# R/estilo.R — identidade visual compartilhada por TODAS as abas.

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)
# ---- Paleta (bandeira da Itália) -------------------------------------------
cores <- c(
  verde        = "#008C45",
  verde_escuro = "#006B35",
  branco       = "#F4F5F0",
  vermelho     = "#CD212A",
  grafite      = "#1F2A24",
  cinza        = "#5C6B62"
)

cores_tipos <- c(
  "Locomotivas a vapor e elétricas"      = "#006B35",
  "Automotrizes e locomotivas elétricas" = "#4FB07D",
  "Carruagens e reboques"                = "#9AA9A0",
  "Vagões de correio e bagagem"          = "#2B3A32",
  "Carros de passeio"                    = "#CD212A",
  "Outros veículos motorizados"          = "#E8878C"
)


tema_app <- bs_theme(
  version   = 5,
  bg        = "#FAFAF7",
  fg        = cores[["grafite"]],
  primary   = cores[["verde"]],
  secondary = cores[["cinza"]],
  danger    = cores[["vermelho"]],
  base_font = font_google("Figtree", local = FALSE)
)

css_app <- "
.navbar { background-color: #008C45 !important; border-bottom: 0; }
.navbar .navbar-brand,
.navbar .nav-link,
.navbar-dark .navbar-nav .nav-link { color: #FFFFFF !important; }
.navbar .nav-link.active { font-weight: 600; box-shadow: inset 0 -3px 0 #F4F5F0; }
.faixa-tricolor {
  height: 5px;
  background: linear-gradient(90deg, #008C45 0 33.33%, #F4F5F0 33.33% 66.66%, #CD212A 66.66% 100%);
  border-bottom: 1px solid #D9DDD8;
}
.card-header { font-weight: 600; }
.fonte { color: #5C6B62; font-size: 0.85rem; }
"

# ---- Tema ggplot -----------------------------------------------------------
theme_app <- function() {
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      axis.title         = element_text(colour = cores[["cinza"]]),
      plot.background    = element_rect(fill = NA, colour = NA)
    )
}

# ---- Formatação pt-BR ------------------------------------------------------
fmt <- function(x, d = 0) {
  format(round(x, d), big.mark = ".", decimal.mark = ",",
         nsmall = d, scientific = FALSE, trim = TRUE)
}

rotulo_num <- scales::label_number(big.mark = ".", decimal.mark = ",")

# ---- Tabelas (DT em português) ---------------------------------------------
dt_opcoes <- list(
  pageLength = 8, dom = "tip", scrollX = TRUE,
  language = list(
    info = "_START_ a _END_ de _TOTAL_", infoEmpty = "Sem registros",
    emptyTable = "Sem dados para os filtros escolhidos",
    paginate = list(`next` = "Próximo", previous = "Anterior")
  )
)

tabela_dt <- function(df, colunas_num = character(0), digitos = 0) {
  t <- DT::datatable(df, rownames = FALSE, class = "compact stripe",
                     options = dt_opcoes)
  if (length(colunas_num) > 0) {
    t <- DT::formatRound(t, colunas_num, digits = digitos,
                         mark = ".", dec.mark = ",")
  }
  t
}

# ---- Helpers de análise ----------------------------------------------------
# Média móvel centrada (as pontas ficam NA)
media_movel <- function(x, k) {
  if (k <= 1) return(x)
  as.numeric(stats::filter(x, rep(1 / k, k), sides = 2))
}

# Faixas das guerras mundiais; devolve NULL se nenhuma cair no período
guerras <- data.frame(
  nome   = c("1ª Guerra Mundial", "2ª Guerra Mundial"),
  inicio = c(1914, 1939),
  fim    = c(1918, 1945)
)

camada_guerras <- function(periodo) {
  g <- guerras[guerras$fim >= periodo[1] & guerras$inicio <= periodo[2], ]
  if (nrow(g) == 0) return(NULL)
  geom_rect(
    data = g,
    aes(xmin = inicio, xmax = fim, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE, fill = cores[["vermelho"]], alpha = 0.08
  )
}
