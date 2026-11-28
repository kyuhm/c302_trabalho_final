# =====================================================================
# PÁGINA ANÁLISE
# =====================================================================
# Responsável: eduardo
# =====================================================================

# ---- TELA ----
ui_analise <- function() {
  pagina("analise", "Análise", "Resumo de cada produto em todo o período (1861–1985)",
         tableOutput("tabela_resumo"))
}

# ---- SERVER ----
server_analise <- function(input, output, session) {
  output$tabela_resumo <- renderTable({
    resumo_produtos |>
      arrange(desc(crescimento)) |>
      transmute(
        Setor                 = setor,
        Produto               = nome_pt(produto),
        `Primeiro ano`        = as.integer(inicio),
        `Último ano`          = as.integer(fim),
        `Cresceu (vezes)`     = round(crescimento, 2)
      )
  })
}
