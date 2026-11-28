library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

# Traduzir a library DT
options(DT.options = list(
  language = list(
    decimal = ",",
    thousands = ".",
    lengthMenu = "Mostrar _MENU_ registros por página",
    zeroRecords = "Nenhum registro encontrado",
    info = "Mostrando de _START_ até _END_ de _TOTAL_ registros",
    infoEmpty = "Mostrando 0 até 0 de 0 registros",
    infoFiltered = "(filtrado de _MAX_ registros no total)",
    search = "Buscar:",
    paginate = list(
      first = "Primeiro",
      last = "Último",
      `next` = "Próximo",
      previous = "Anterior"
    )
  )
))

# Dados diretos do GitHub: TidyTuesday 2026-19
food_beverages <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/food_beverages.csv",
  show_col_types = FALSE
)

# Variáveis nas colunas do csv, usadas para renomear
variaveis_prod <- c(
  "Açúcar", "Glicose", "Substituto_café", "Oleo_de_sementes",
  "Etanol_1", "Etanol_2", "Cerveja"
)

# Paleta de cores
paleta_produtos <- c(
  "Açúcar"           = "#1b9e77",
  "Glicose"          = "#d95f02",
  "Substituto_café"  = "#7570b3",
  "Oleo_de_sementes" = "#e7298a",
  "Etanol_1"         = "#66a61e",
  "Etanol_2"         = "#e6ab02",
  "Cerveja"          = "#1f78b4"
)

# Tradução das colunas e conversão de unidades:
# Glicose, Substituto_café e Oleo_de_sementes: quintal -> toneladas (x / 10)
comidas_bebidas <- food_beverages |>
  rename(
    Ano = Year,
    Açúcar = Sugar,
    Glicose = Glucose,
    Substituto_café = Coffee_substitute,
    Oleo_de_sementes = Seed_oil,
    Etanol_1 = Ethyl_alcohol_1,
    Etanol_2 = Ethyl_alcohol_2,
    Cerveja = Beer
  ) |>
  mutate(
    across(c(Glicose, Substituto_café, Oleo_de_sementes), ~ .x / 10)
  )

# Transformar em formato longo
comidas_bebidas_longer <- comidas_bebidas |>
  pivot_longer(
    cols = all_of(variaveis_prod),
    names_to = "Variavel",
    values_to = "Valor"
  ) |>
  mutate(
    Decada = paste0(floor(Ano / 10) * 10, "s")
  )

# Estatística descritiva
cols_estat <- c("soma", "media", "dp", "min", "q1", "mediana", "q3", "max")

tabela_descritiva <- comidas_bebidas_longer |>
  group_by(Decada, Variavel) |>
  summarise(
    n_obs   = sum(!is.na(Valor)),
    n_na    = sum(is.na(Valor)),
    soma    = sum(Valor, na.rm = TRUE),
    media   = mean(Valor, na.rm = TRUE),
    dp      = sd(Valor, na.rm = TRUE),
    min     = suppressWarnings(min(Valor, na.rm = TRUE)),
    q1      = quantile(Valor, probs = 0.25, na.rm = TRUE, names = FALSE),
    mediana = median(Valor, na.rm = TRUE),
    q3      = quantile(Valor, probs = 0.75, na.rm = TRUE, names = FALSE),
    max     = suppressWarnings(max(Valor, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(
    across(
      all_of(cols_estat),
      ~ if_else(is.nan(.x) | is.infinite(.x) | is.na(.x), NA_real_, .x)
    )
  ) |>
  arrange(Decada, Variavel)

# Funções e auxiliares
anos_min <- min(comidas_bebidas_longer$Ano, na.rm = TRUE)
anos_max <- max(comidas_bebidas_longer$Ano, na.rm = TRUE)
decadas  <- sort(unique(na.omit(comidas_bebidas_longer$Decada)))

fmt_br <- function(x) {
  ifelse(
    is.na(x) | !is.finite(x),
    "NA",
    formatC(x, format = "f", digits = 2, big.mark = ".", decimal.mark = ",")
  )
}

fmt_int_br <- function(x) {
  ifelse(
    is.na(x) | !is.finite(x),
    "NA",
    formatC(round(x), format = "f", digits = 0, big.mark = ".", decimal.mark = ",")
  )
}

# observações de dados por década
dados_decada <- comidas_bebidas_longer |>
  filter(!is.na(Decada), !is.na(Variavel), !is.na(Valor)) |>
  summarise(
    valor_decada = mean(Valor, na.rm = TRUE),
    dp_decada    = sd(Valor, na.rm = TRUE),
    n_obs        = n(),
    .by = c(Decada, Variavel)
  ) |>
  mutate(
    decada_num = as.numeric(sub("s$", "", Decada)),
    dp_decada  = coalesce(dp_decada, 0), # década com 1 observação: DP = 0
    inferior   = valor_decada - dp_decada,
    superior   = valor_decada + dp_decada
  ) |>
  arrange(Variavel, decada_num)
