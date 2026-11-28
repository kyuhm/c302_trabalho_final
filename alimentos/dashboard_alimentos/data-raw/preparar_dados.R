# Roda UMA vez, localmente, com o diretório de trabalho na raiz do projeto.
# Baixa os dados do TidyTuesday, trata e salva data/dados_prontos.rds

library(tidyverse)

source("R/constantes.R")  # variaveis_prod e cols_estat

# Dados diretos do GitHub: TidyTuesday 2026-19
food_beverages <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/food_beverages.csv",
  show_col_types = FALSE
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

# Formato longo
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

# Observações de dados por década
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

dir.create("data", showWarnings = FALSE)

saveRDS(
  list(
    comidas_bebidas        = comidas_bebidas,
    comidas_bebidas_longer = comidas_bebidas_longer,
    tabela_descritiva      = tabela_descritiva,
    dados_decada           = dados_decada
  ),
  "data/dados_prontos.rds"
)
