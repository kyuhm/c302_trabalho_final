# app.R — Transportes na Itália (1861-1985)
# global.R e R/*.R são carregados automaticamente pelo Shiny.
# Para rodar: shiny::runApp()  (na pasta do projeto)


if (!exists("veiculos")) {
  if (!file.exists("global.R")) {
    stop("Não encontrei o arquivo 'global.R' ao lado do app.R. ",
         "Confira se ele está na pasta app_transportes (e não em R/) ",
         "e se o nome é exatamente 'global.R'.")
  }
  source("global.R")
}

ui <- page_navbar(
  title    = "Transportes na Itália, 1861–1985",
  theme    = tema_app,
  fillable = FALSE,
  header   = tagList(
    tags$head(tags$style(HTML(css_app))),
    div(class = "faixa-tricolor")
  ),

  nav_panel("Construção naval",       mod_navios_ui("navios")),
  nav_panel("Ferrovia e motorização", mod_veiculos_ui("veic"))


)

server <- function(input, output, session) {
  mod_navios_server("navios", navios)
  mod_veiculos_server("veic", veiculos)
  # mod_xxx_server("xxx", ...)
}

shinyApp(ui, server)
