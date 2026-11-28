# =====================================================================
# R/01_dados.R — ler os dados, nomes em português, formatar números
# (veio do grafico_test.R)
# =====================================================================

food_beverages <- read_csv("dados/food_beverages.csv", show_col_types = FALSE)
textiles       <- read_csv("dados/textiles.csv",       show_col_types = FALSE)
transport      <- read_csv("dados/transport.csv",      show_col_types = FALSE)

food_longo <- food_beverages |>
  pivot_longer(-Year, names_to = "produto", values_to = "producao") |>
  mutate(setor = "Alimentos")

textiles_longo <- textiles |>
  pivot_longer(-Year, names_to = "produto", values_to = "producao") |>
  mutate(setor = "Têxteis")

transport_longo <- transport |>
  pivot_longer(-Year, names_to = "produto", values_to = "producao") |>
  mutate(setor = "Transporte")

dados <- bind_rows(food_longo, textiles_longo, transport_longo) |>
  filter(!is.na(producao)) |>
  filter(produto != "Total_yarn", produto != "Total_textiles")

# nomes dos produtos em português (o banco original está em inglês)
nomes_pt <- c(
  Sugar = "Açúcar", Glucose = "Glicose", Coffee_substitute = "Sucedâneo de café",
  Seed_oil = "Óleo de sementes", Ethyl_alcohol_1 = "Álcool etílico (1ª cat.)",
  Ethyl_alcohol_2 = "Álcool etílico (2ª cat.)", Beer = "Cerveja",
  Cotton_yarn = "Fio de algodão", Flock_yarn = "Fio de algodão em flocos",
  Other_yarn = "Fio de outras fibras", Cotton_textiles = "Tecido de algodão",
  Flock_textiles = "Tecido de algodão em flocos", Other_textiles = "Tecido de outras fibras",
  Raw_silk = "Seda crua",
  Ships_launched = "Navios lançados", Ships_weight = "Tonelagem dos navios",
  Steam_and_electric_engine = "Locomotivas a vapor e elétricas",
  Rail_cars_and_electric_locomotives = "Automotrizes e locomotivas elétricas",
  Carriages_and_trailers = "Carruagens e reboques",
  Mail_luggage_vans_and_carriages = "Vagões de correio e bagagem",
  Passenger_cars = "Automóveis de passeio", Other = "Outros veículos motorizados"
)
nome_pt <- function(x) {
  x <- as.character(x)
  saida <- gsub("_", " ", x)
  i <- x %in% names(nomes_pt)
  saida[i] <- unname(nomes_pt[x[i]])
  saida
}



# números em português: 8,5 mi / 120 mil
fmt_num <- function(x, d = 1) format(round(x, d), big.mark = ".", decimal.mark = ",", trim = TRUE)
curto <- function(x) vapply(x, function(v) {
  if (is.na(v)) return("—")
  a <- abs(v)
  if (a >= 1e9) paste0(fmt_num(v / 1e9, 1), " bi")
  else if (a >= 1e6) paste0(fmt_num(v / 1e6, 1), " mi")
  else if (a >= 1e3) paste0(fmt_num(v / 1e3, 1), " mil")
  else fmt_num(v, 0)
}, character(1))

# todos os setores e o período completo
todos_setores <- c("Alimentos", "Têxteis", "Transporte")
periodo_total <- c(1861, 1985)

# resumo: uma linha por produto (usado no Panorama e na Análise)
resumo_produtos <- dados |>
  group_by(setor, produto) |>
  summarise(
    inicio = min(Year),
    fim = max(Year),
    primeiro_valor = first(producao),
    ultimo_valor = last(producao),
    .groups = "drop"
  ) |>
  mutate(crescimento = ultimo_valor / primeiro_valor)

# ---------------------------------------------------------------------
# TONELADAS: conversão aproximada (usada no Panorama)
# ---------------------------------------------------------------------
# Unidades originais do banco (ISTAT) e conversão aproximada para TONELADAS:
#   Sugar, fios, tecidos, seda e tonelagem de navios: já em toneladas (x1)
#   Glicose, sucedâneo de café, óleo de sementes: quintais (1 q = 100 kg -> x0,1)
#   Cerveja: hectolitros (1 hl ~ 100 kg -> x0,1)
#   Álcool etílico: unidades de "100 litros de álcool puro" (~79 kg -> x0,0789)
# Itens de transporte em "número de unidades" (vagões, carros...) NÃO viram tonelada.
fator_t <- c(Sugar = 1, Glucose = 0.1, Coffee_substitute = 0.1, Seed_oil = 0.1,
             Ethyl_alcohol_1 = 0.0789, Ethyl_alcohol_2 = 0.0789, Beer = 0.1,
             Cotton_yarn = 1, Flock_yarn = 1, Other_yarn = 1,
             Cotton_textiles = 1, Flock_textiles = 1, Other_textiles = 1, Raw_silk = 1,
             Ships_weight = 1)

unidade_nativa <- c(Sugar = "t", Glucose = "q", Coffee_substitute = "q", Seed_oil = "q",
                    Ethyl_alcohol_1 = "×100 L", Ethyl_alcohol_2 = "×100 L", Beer = "hl",
                    Cotton_yarn = "t", Flock_yarn = "t", Other_yarn = "t",
                    Cotton_textiles = "t", Flock_textiles = "t", Other_textiles = "t",
                    Raw_silk = "t", Ships_weight = "t", Ships_launched = "navios")

# volume por setor e ano, em toneladas aproximadas
volume_setor <- dados |>
  filter(produto %in% names(fator_t)) |>
  mutate(t = producao * fator_t[produto]) |>
  group_by(setor, Year) |>
  summarise(t = sum(t), .groups = "drop")

# ---------------------------------------------------------------------
# AJUDANTE: escolhe um número "redondo" (usado no Panorama)
# ---------------------------------------------------------------------

# escolhe uma unidade "redonda" (1, 2, 5, 10, 20, 50...) para ~alvo pontos no pico
unidade_bonita <- function(x, alvo = 24) {
  bruto <- x / alvo
  p <- 10^floor(log10(bruto))
  p * c(1, 2, 5, 10)[which(c(1, 2, 5, 10) * p >= bruto)[1]]
}
