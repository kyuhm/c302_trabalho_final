# =====================================================================
# R/02_tema.R — cores e tema dos gráficos (ggplot)
# (veio do grafico_test.R)
# =====================================================================

#  verde-sálvia #1D5C63   terracota #F7B538   creme #E3EFF4   bege #CFE2EA   grafite #0F3A40

# ---------------------------------------------------------------------
# TEMA DOS GRÁFICOS
# O CSS não entra dentro do gráfico (o ggplot vira uma imagem PNG).
# Por isso criamos um "tema" do ggplot com as MESMAS cores do CSS,
# e usamos  + tema_mix  em todos os gráficos.
# ---------------------------------------------------------------------
tema_mix <- theme_minimal(base_size = 14) +
  theme(
    text               = element_text(color = "#0F3A40"),
    axis.text          = element_text(color = "#5F8087", size = 11),
    axis.title         = element_text(color = "#5F8087", size = 12),
    panel.grid.minor   = element_blank(),                       # tira a grade fininha
    panel.grid.major   = element_line(color = "#CFE2EA"),       # grade cor menta
    strip.text         = element_text(face = "bold", size = 13, # títulos dos painéis
                                      hjust = 0, color = "#0F3A40"),
    legend.position    = "bottom",
    legend.title       = element_blank(),
    legend.text        = element_text(size = 12),
    plot.background    = element_rect(fill = "white", color = NA),
    plot.margin        = margin(10, 16, 10, 10)
  )

# Números curtos nos eixos: 10000 -> 10K, 2000000 -> 2M
numero_curto <- function(x) curto(x)      # 10.000 -> "10 mil", 2.000.000 -> "2 mi"

cores <- c("Alimentos"  = "#1D5C63",   # verde-sálvia
           "Têxteis"    = "#F7B538",   # vermelho-terracota
           "Transporte" = "#4FA6B0")   # cinza-pedra
