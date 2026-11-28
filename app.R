# =====================================================================
# app.R — DASHBOARD PRINCIPAL (Indústria italiana, 1861–1985)
#
# Este arquivo só JUNTA as páginas. Cada página está no seu arquivo,
# dentro da pasta R/ (o Shiny carrega a pasta R/ sozinho):
#
#   BASE (todo mundo usa — mexer só combinando com o grupo)
#   R/00_pacotes.R          pacotes
#   R/01_dados.R            dados, nome_pt(), curto(), resumo_produtos, volume_setor
#   R/02_tema.R             cores, tema_mix
#   R/03_layout.R           menu, pagina(), layout_template()
#   R/04_css.R              visual GERAL (CSS)
#
#   PÁGINAS (uma pessoa por arquivo)
#   R/10_panorama.R         Panorama (Dashboard 1 + Dashboard 2 + CSS dele)
#   R/20_alimentos.R        Alimentos
#   R/21_textil.R           Têxtil
#   R/22_transporte.R       Transporte
#   R/30_analise.R          Análise
#   R/40_integrantes.R      Integrantes
#
# Cada arquivo de página tem 2 funções:  ui_xxx()  e  server_xxx()
# Para rodar: botão "Run App" (ou shiny::runApp()). NÃO use source("app.R").
# =====================================================================


# ---------------------------------------------------------------------
# TELA: o molde + uma página por botão do menu
# (a ordem aqui tem que ser a mesma do `menu`, em R/03_layout.R)
# ---------------------------------------------------------------------
ui <- layout_template(
  titulo = "Indústria italiana",
  menu   = menu,

  ui_panorama(),      # R/10_panorama.R  (Dashboard 1 + Dashboard 2)
  ui_alimentos(),     # R/20_alimentos.R
  ui_textil(),        # R/21_textil.R
  ui_transporte(),    # R/22_transporte.R
  ui_analise(),       # R/30_analise.R
  ui_integrantes()    # R/40_integrantes.R
)


# ---------------------------------------------------------------------
# SERVER: chama o server de cada página
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  # troca de página quando clica num ícone do menu
  observeEvent(input$tab_clicada, {
    updateTabsetPanel(session, "tabs", selected = input$tab_clicada)
  })

  server_panorama(input, output, session)   # Dashboard 1 + Dashboard 2
  server_alimentos(input, output, session)
  server_textil(input, output, session)
  server_transporte(input, output, session)
  server_analise(input, output, session)
  server_integrantes(input, output, session)
}


shinyApp(ui, server)
