# global.R — executado uma vez, antes de R/*.R e app.R
library(shiny)
library(bslib)
library(tidyverse)   # dplyr, tidyr, ggplot2, readr
library(plotly)
library(DT)

# ---- Leitura dos dados (com cache local em dados/) -------------------------
carregar_transport <- function() {
  local <- file.path("dados", "transport.csv")
  url <- paste0(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
    "main/data/2026/2026-05-05/transport.csv"
  )
  if (file.exists(local)) {
    return(readr::read_csv(local, na = "NA", show_col_types = FALSE))
  }
  d <- readr::read_csv(url, na = "NA", show_col_types = FALSE)
  dir.create("dados", showWarnings = FALSE)
  readr::write_csv(d, local)
  d
}

transport <- carregar_transport()

# ---- Navios ----------------------------------------------------------------
navios <- transport |>
  select(Year, Ships_launched, Ships_weight) |>
  mutate(
    peso_medio = Ships_weight / Ships_launched,   # tonelagem bruta por navio
    decada = Year %/% 10 * 10
  )

# ---- Veículos (formato longo) ----------------------------------------------
tipos_veiculos <- tribble(
  ~var,                                 ~tipo,                                  ~grupo,
  "Steam_and_electric_engine",          "Locomotivas a vapor e elétricas",      "Ferroviário",
  "Rail_cars_and_electric_locomotives", "Automotrizes e locomotivas elétricas", "Ferroviário",
  "Carriages_and_trailers",             "Carruagens e reboques",                "Ferroviário",
  "Mail_luggage_vans_and_carriages",    "Vagões de correio e bagagem",          "Ferroviário",
  "Passenger_cars",                     "Carros de passeio",                    "Motorizado",
  "Other",                              "Outros veículos motorizados",          "Motorizado"
)

veiculos <- transport |>
  select(Year, all_of(tipos_veiculos$var)) |>
  pivot_longer(-Year, names_to = "var", values_to = "qtd") |>
  left_join(tipos_veiculos, by = "var") |>
  mutate(tipo = factor(tipo, levels = tipos_veiculos$tipo))
