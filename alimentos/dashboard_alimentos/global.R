library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

# Constantes e funções (R/) são carregadas depois do global.R,
# então carregamos explicitamente aqui para usá-las abaixo, se precisar.
source("R/constantes.R", local = FALSE)
source("R/funcoes.R", local = FALSE)

# Dados já tratados por data-raw/preparar_dados.R
dados <- readRDS("data/dados_prontos.rds")

comidas_bebidas        <- dados$comidas_bebidas
comidas_bebidas_longer <- dados$comidas_bebidas_longer
tabela_descritiva      <- dados$tabela_descritiva
dados_decada           <- dados$dados_decada

# Auxiliares
anos_min <- min(comidas_bebidas_longer$Ano, na.rm = TRUE)
anos_max <- max(comidas_bebidas_longer$Ano, na.rm = TRUE)
decadas  <- sort(unique(na.omit(comidas_bebidas_longer$Decada)))
