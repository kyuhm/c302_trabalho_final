# =====================================================================
# R/03_layout.R — menu, cabeçalho das páginas e o MOLDE da tela
# (veio do grafico_test.R)
# =====================================================================

# O "id" PRECISA ser igual ao nome da página (pagina("id", ...))
menu <- list(
  list(id = "home",        icon = "chart-column", label = "Visão Geral"),
  list(id = "alimentos",   icon = "utensils",     label = "Alimentos"),
  list(id = "textil",      icon = "shirt",        label = "Têxtil"),
  list(id = "carros",      icon = "car",          label = "Transporte"),
  list(id = "analise",     icon = "chart-pie",    label = "Análise"),
  list(id = "integrantes", icon = "users",        label = "Integrantes")
)

sidebar_btn <- function(item, active = FALSE) {
  actionLink(
    inputId    = paste0("nav_", item$id),
    label      = icon(item$icon),
    class      = paste("sb-btn", if (active) "active"),
    title      = item$label,
    `data-tab` = item$id
  )
}


# bandeira da Itália (verde, branco, vermelho) em SVG
bandeira_it <- function() {
  tag("svg", list(class = "bandeira-it", viewBox = "0 0 36 24", width = 48, height = 32,
                  role = "img", `aria-label` = "Bandeira da Itália",
                  tag("rect", list(x = 0,  y = 0, width = 12, height = 24, fill = "#009246")),
                  tag("rect", list(x = 12, y = 0, width = 12, height = 24, fill = "#FFFFFF")),
                  tag("rect", list(x = 24, y = 0, width = 12, height = 24, fill = "#CE2B37")),
                  tag("rect", list(x = 0.5, y = 0.5, width = 35, height = 23, rx = 2.5, fill = "none",
                                   stroke = "#CFE2EA", `stroke-width` = 1))))
}

pagina <- function(id, titulo, subtitulo, ...) {
  tabPanelBody(id,
               div(class = "titulo-linha",
                   bandeira_it(),
                   div(h2(titulo), p(class = "subtitulo", subtitulo))),
               div(class = "flag-gap"),
               ...
  )
}



# ---------------------------------------------------------------------
# MOLDE DA TELA (template): [menu de ícones] [páginas] (+ filtros)
#   titulo  = nome na aba do navegador
#   menu    = lista de botões (id, icon, label)
#   ...     = as páginas, feitas com pagina()
#   filtros = NULL (sem filtros) ou inputs, que vão para a direita
# ---------------------------------------------------------------------
layout_template <- function(titulo, menu, ..., filtros = NULL) {
  fluidPage(
    title = titulo,
    lang  = "pt-BR",
    theme = bs_theme(version = 5, primary = "#1D5C63"),
    tags$head(
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"),
      tags$style(HTML(css))   # CSS geral (R/04_css.R)
    ),
    div(class = "layout",
        # 1) menu de ícones
        div(class = "sidebar-icons",
            lapply(seq_along(menu), function(i) sidebar_btn(menu[[i]], active = (i == 1)))),
        # 2) conteúdo: uma página por botão
        div(class = "content",
            tabsetPanel(id = "tabs", type = "hidden", ...)),
        # 3) filtros (só se pedir)
        if (!is.null(filtros)) div(class = "filtros", h4("Filtros"), filtros)
    ),
    # clicou num ícone: marca como ativo e avisa o server
    tags$script(HTML("
      $(document).on('click', '.sb-btn', function() {
        $('.sb-btn').removeClass('active');
        $(this).addClass('active');
        Shiny.setInputValue('tab_clicada', $(this).data('tab'), {priority: 'event'});
      });
    "))
  )
}
