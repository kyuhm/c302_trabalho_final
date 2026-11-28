# =====================================================================
# PÁGINA TRANSPORTE
# =====================================================================
# Responsável: emanuelly
# =====================================================================

# =====================================================================
# PÁGINA TRANSPORTE  —  VERSÃO CONVERTIDA (para comparar)
# =====================================================================
# Este arquivo junta o app_transportes/ (global.R + R/estilo.R +
# R/mod_navios.R + R/mod_veiculos.R) no molde do projeto.
# Para usar: copie TUDO isto para R/22_transporte.R (substituindo).
# (DT e plotly já estão em R/00_pacotes.R — não precisa adicionar nada.)
#
# O QUE FOI TIRADO / MUDADO do código original:
#   - library(...)               -> já estão em R/00_pacotes.R
#   - carregar_transport()       -> `transport` já vem de R/01_dados.R
#   - page_navbar + tema_app     -> o menu e o tema do projeto já existem
#   - css_app (navbar, faixa)    -> não precisa; só ficou a classe .fonte
#   - shinyApp(ui, server)       -> o app.R já faz isso
#   - cores  -> cores_transporte   (!!! `cores` JÁ EXISTE no projeto,
#                                   R/02_tema.R; com o mesmo nome ele
#                                   apagaria as cores das outras páginas)
#   - todos os outros nomes ganharam _transporte no fim
#
# Os MÓDULOS (mod_..._ui / mod_..._server) continuam iguais por dentro:
# o NS(id) já separa os inputs/outputs, então não há choque de IDs.
# =====================================================================

setor_transporte <- "Transporte"


# =====================================================================
# PARTE 1 — DADOS, ESTILO E AJUDANTES
# (veio de global.R e R/estilo.R)
# =====================================================================

# ---- Navios  (transport já existe: vem de R/01_dados.R) ----
navios_transporte <- transport |>
  select(Year, Ships_launched, Ships_weight) |>
  mutate(
    peso_medio = Ships_weight / Ships_launched,   # tonelagem bruta por navio
    decada = Year %/% 10 * 10
  )

# ---- Veículos (formato longo) ----
tipos_veiculos_transporte <- tribble(
  ~var,                                 ~tipo,                                  ~grupo,
  "Steam_and_electric_engine",          "Locomotivas a vapor e elétricas",      "Ferroviário",
  "Rail_cars_and_electric_locomotives", "Automotrizes e locomotivas elétricas", "Ferroviário",
  "Carriages_and_trailers",             "Carruagens e reboques",                "Ferroviário",
  "Mail_luggage_vans_and_carriages",    "Vagões de correio e bagagem",          "Ferroviário",
  "Passenger_cars",                     "Carros de passeio",                    "Motorizado",
  "Other",                              "Outros veículos motorizados",          "Motorizado"
)

veiculos_transporte <- transport |>
  select(Year, all_of(tipos_veiculos_transporte$var)) |>
  pivot_longer(-Year, names_to = "var", values_to = "qtd") |>
  left_join(tipos_veiculos_transporte, by = "var") |>
  mutate(tipo = factor(tipo, levels = tipos_veiculos_transporte$tipo))

# ---- Paleta (era `cores` — RENOMEADA) ----
cores_transporte <- c(
  verde        = "#008C45",
  verde_escuro = "#006B35",
  branco       = "#F4F5F0",
  vermelho     = "#CD212A",
  grafite      = "#1F2A24",
  cinza        = "#5C6B62"
)

cores_tipos_transporte <- c(
  "Locomotivas a vapor e elétricas"      = "#006B35",
  "Automotrizes e locomotivas elétricas" = "#4FB07D",
  "Carruagens e reboques"                = "#9AA9A0",
  "Vagões de correio e bagagem"          = "#2B3A32",
  "Carros de passeio"                    = "#CD212A",
  "Outros veículos motorizados"          = "#E8878C"
)

# ---- Tema ggplot (era theme_app) ----
tema_transporte <- function() {
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      axis.title         = element_text(colour = cores_transporte[["cinza"]]),
      plot.background    = element_rect(fill = NA, colour = NA)
    )
}

# ---- Formatação pt-BR (era fmt e rotulo_num) ----
fmt_transporte <- function(x, d = 0) {
  format(round(x, d), big.mark = ".", decimal.mark = ",",
         nsmall = d, scientific = FALSE, trim = TRUE)
}

rotulo_num_transporte <- scales::label_number(big.mark = ".", decimal.mark = ",")

# ---- Tabelas DT em português (era dt_opcoes e tabela_dt) ----
dt_opcoes_transporte <- list(
  pageLength = 8, dom = "tip", scrollX = TRUE,
  language = list(
    info = "_START_ a _END_ de _TOTAL_", infoEmpty = "Sem registros",
    emptyTable = "Sem dados para os filtros escolhidos",
    paginate = list(`next` = "Próximo", previous = "Anterior")
  )
)

tabela_dt_transporte <- function(df, colunas_num = character(0), digitos = 0) {
  t <- DT::datatable(df, rownames = FALSE, class = "compact stripe",
                     options = dt_opcoes_transporte)
  if (length(colunas_num) > 0) {
    t <- DT::formatRound(t, colunas_num, digits = digitos,
                         mark = ".", dec.mark = ",")
  }
  t
}

# ---- Média móvel centrada (era media_movel) ----
media_movel_transporte <- function(x, k) {
  if (k <= 1) return(x)
  as.numeric(stats::filter(x, rep(1 / k, k), sides = 2))
}

# ---- Faixas das guerras mundiais (era guerras e camada_guerras) ----
guerras_transporte <- data.frame(
  nome   = c("1ª Guerra Mundial", "2ª Guerra Mundial"),
  inicio = c(1914, 1939),
  fim    = c(1918, 1945)
)

camada_guerras_transporte <- function(periodo) {
  g <- guerras_transporte[guerras_transporte$fim >= periodo[1] &
                          guerras_transporte$inicio <= periodo[2], ]
  if (nrow(g) == 0) return(NULL)
  geom_rect(
    data = g,
    aes(xmin = inicio, xmax = fim, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE, fill = cores_transporte[["vermelho"]], alpha = 0.08
  )
}


# =====================================================================
# MÓDULO 1 — CONSTRUÇÃO NAVAL   (veio de R/mod_navios.R)
# =====================================================================
mod_navios_ui_transporte <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 300,
      sliderInput(ns("periodo"), "Período",
                  min = 1861, max = 1985, value = c(1861, 1985),
                  step = 1, sep = ""),
      radioButtons(ns("metrica"), "O que mostrar",
                   choices = c("Navios lançados"            = "n",
                               "Tonelagem bruta (t)"        = "peso",
                               "Peso médio por navio (t)"   = "medio")),
      selectInput(ns("janela"), "Média móvel",
                  choices = c("Sem suavização" = 1, "3 anos" = 3,
                              "5 anos" = 5, "10 anos" = 10),
                  selected = 5),
      checkboxInput(ns("guerras"), "Destacar as guerras mundiais", TRUE),
      radioButtons(ns("tabela"), "Tabela",
                   choices = c("Ano a ano" = "ano", "Resumo por década" = "decada"))
    ),

    layout_columns(
      col_widths = c(4, 4, 4), fill = FALSE,
      value_box("Ano de pico",      textOutput(ns("v_ano")),   theme = "primary"),
      value_box("Valor no pico",    textOutput(ns("v_pico")),  theme = "primary"),
      value_box("Média no período", textOutput(ns("v_media")), theme = "primary")
    ),

    card(
      full_screen = TRUE,
      card_header(textOutput(ns("titulo"))),
      plotlyOutput(ns("grafico"), height = "380px"),
      card_footer(class = "fonte",
        "A linha clara mostra o valor anual; a linha escura, a média móvel. ",
        "Tonelagem bruta: toneladas de arqueação bruta dos navios lançados.")
    ),

    card(
      card_header("Dados"),
      DTOutput(ns("tabela_dt"))
    )
  )
}

mod_navios_server_transporte <- function(id, navios) {
  moduleServer(id, function(input, output, session) {

    rotulos <- c(
      n     = "Navios lançados por ano",
      peso  = "Tonelagem bruta lançada (t)",
      medio = "Peso médio por navio (t)"
    )
    digitos <- reactive(if (input$metrica == "medio") 1 else 0)

    dados_nav <- reactive({
      req(input$periodo)
      k   <- as.integer(input$janela)
      col <- switch(input$metrica,
                    n = "Ships_launched", peso = "Ships_weight",
                    medio = "peso_medio")
      navios |>
        filter(Year >= input$periodo[1], Year <= input$periodo[2]) |>
        mutate(valor = .data[[col]], suave = media_movel_transporte(valor, k))
    })

    output$titulo <- renderText({
      paste0(rotulos[[input$metrica]], ", ",
             input$periodo[1], "–", input$periodo[2])
    })

    # ---- Cartões-resumo ----
    resumo <- reactive({
      d <- dados_nav() |> filter(!is.na(valor))
      validate(need(nrow(d) > 0, "Sem dados no período escolhido."))
      pico <- d[which.max(d$valor), ]
      list(ano = pico$Year, valor = pico$valor, media = mean(d$valor))
    })
    output$v_ano   <- renderText(resumo()$ano)
    output$v_pico  <- renderText(fmt_transporte(resumo()$valor, digitos()))
    output$v_media <- renderText(fmt_transporte(resumo()$media, digitos()))

    # ---- Gráfico ----
    output$grafico <- renderPlotly({
      d <- dados_nav()
      validate(need(sum(!is.na(d$valor)) > 1, "Sem dados no período escolhido."))
      k      <- as.integer(input$janela)
      rotulo <- rotulos[[input$metrica]]
      dg     <- digitos()

      p <- ggplot(d, aes(Year, valor))
      if (isTRUE(input$guerras)) p <- p + camada_guerras_transporte(input$periodo)

      p <- p +
        geom_line(
          aes(text = paste0("Ano: ", Year, "<br>", rotulo, ": ", fmt_transporte(valor, dg)),
              group = 1),
          colour    = if (k > 1) "#9DB8A8" else cores_transporte[["verde"]],
          linewidth = if (k > 1) 0.6 else 1
        )

      if (k > 1) {
        p <- p + geom_line(
          aes(y = suave,
              text = paste0("Ano: ", Year, "<br>Média móvel (", k, " anos): ",
                            fmt_transporte(suave, dg)),
              group = 1),
          colour = cores_transporte[["verde_escuro"]], linewidth = 1.2
        )
      }

      p <- p +
        scale_y_continuous(labels = rotulo_num_transporte) +
        scale_x_continuous(breaks = scales::pretty_breaks(8)) +
        coord_cartesian(xlim = input$periodo) +
        labs(x = NULL, y = rotulo) +
        tema_transporte()

      ggplotly(p, tooltip = "text") |>
        config(displaylogo = FALSE)
    })

    # ---- Tabela ----
    output$tabela_dt <- renderDT({
      d <- dados_nav()
      if (input$tabela == "ano") {
        t <- d |>
          transmute(Ano = Year,
                    `Navios lançados`      = Ships_launched,
                    `Tonelagem bruta (t)`  = Ships_weight,
                    `Peso médio (t/navio)` = peso_medio)
      } else {
        t <- d |>
          group_by(`Década` = paste0(decada, "s")) |>
          summarise(
            `Navios lançados`      = sum(Ships_launched),
            `Tonelagem bruta (t)`  = sum(Ships_weight, na.rm = TRUE),
            `Peso médio (t/navio)` =
              sum(Ships_weight, na.rm = TRUE) /
              sum(Ships_launched[!is.na(Ships_weight)]),
            `Ano de pico (navios)` = Year[which.max(Ships_launched)],
            .groups = "drop"
          )
      }
      tabela_dt_transporte(t, c("Navios lançados", "Tonelagem bruta (t)"), 0) |>
        DT::formatRound("Peso médio (t/navio)", 1, mark = ".", dec.mark = ",")
    })
  })
}


# =====================================================================
# MÓDULO 2 — FERROVIA × MOTORIZAÇÃO   (veio de R/mod_veiculos.R)
# =====================================================================
mod_veiculos_ui_transporte <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 320,
      checkboxGroupInput(ns("tipos"), "Tipos de veículo",
                         choices  = levels(veiculos_transporte$tipo),
                         selected = levels(veiculos_transporte$tipo)),
      sliderInput(ns("periodo"), "Período",
                  min = 1905, max = 1985, value = c(1905, 1985),
                  step = 1, sep = ""),
      radioButtons(ns("modo"), "Como comparar",
                   choices = c("Valor absoluto"                         = "abs",
                               "Índice (100 = média do período)"        = "idx",
                               "Participação entre os selecionados (%)" = "pct"),
                   selected = "idx"),
      conditionalPanel(
        condition = sprintf("input['%s'] != 'pct'", ns("modo")),
        checkboxInput(ns("log"), "Escala logarítmica", FALSE)
      ),
      checkboxInput(ns("guerras"), "Destacar as guerras mundiais", TRUE),
      radioButtons(ns("tabela"), "Tabela",
                   choices = c("Série por ano" = "ano", "Resumo por tipo" = "tipo"))
    ),

    card(
      full_screen = TRUE,
      card_header(textOutput(ns("titulo"))),
      uiOutput(ns("aviso")),
      plotlyOutput(ns("grafico"), height = "420px"),
      card_footer(class = "fonte",
        "Contagens anuais conforme o conjunto de dados. Carros de passeio e ",
        "outros veículos motorizados só aparecem a partir de 1925.")
    ),

    card(
      card_header("Dados"),
      DTOutput(ns("tabela_dt"))
    )
  )
}

mod_veiculos_server_transporte <- function(id, veiculos) {
  moduleServer(id, function(input, output, session) {

    rotulo_y <- reactive({
      switch(input$modo,
             abs = "Quantidade",
             idx = "Índice (100 = média do período)",
             pct = "% do total dos tipos selecionados")
    })
    digitos <- reactive(if (input$modo == "abs") 0 else 1)

    # Dados brutos filtrados (sem transformação)
    filtrado <- reactive({
      req(input$tipos, input$periodo)
      veiculos |>
        filter(tipo %in% input$tipos,
               Year >= input$periodo[1], Year <= input$periodo[2])
    })

    # Dados transformados conforme o modo escolhido
    dados_veic <- reactive({
      d <- filtrado()
      if (input$modo == "abs") {
        d |> mutate(valor = qtd)
      } else if (input$modo == "idx") {
        d |>
          group_by(tipo) |>
          mutate(valor = qtd / mean(qtd, na.rm = TRUE) * 100) |>
          ungroup() |>
          mutate(valor = ifelse(is.nan(valor), NA_real_, valor))
      } else {
        # Participação: anos sem registro contam como 0 e não entram no total
        d |>
          mutate(qtd0 = replace_na(qtd, 0)) |>
          group_by(Year) |>
          mutate(valor = qtd0 / sum(qtd0) * 100) |>
          ungroup() |>
          filter(is.finite(valor))
      }
    })

    output$titulo <- renderText({
      paste0(rotulo_y(), ", ", input$periodo[1], "–", input$periodo[2])
    })

    output$aviso <- renderUI({
      ini <- veiculos |>
        filter(tipo %in% input$tipos, !is.na(qtd)) |>
        group_by(tipo) |>
        summarise(inicio = min(Year), .groups = "drop") |>
        filter(inicio > input$periodo[1])
      if (nrow(ini) == 0) return(NULL)
      div(class = "fonte px-3 pt-2",
          "Séries que começam depois do início do período: ",
          paste0(ini$tipo, " (", ini$inicio, ")", collapse = "; "),
          ". Antes disso não há registro, o que não significa valor zero.")
    })

    # ---- Gráfico ----
    output$grafico <- renderPlotly({
      d <- dados_veic()
      validate(need(sum(!is.na(d$valor)) > 1,
                    "Sem dados para os filtros escolhidos."))
      dg  <- digitos()
      txt <- aes(text = paste0(tipo, "<br>Ano: ", Year, "<br>",
                               fmt_transporte(valor, dg),
                               if (input$modo == "pct") "%" else ""))

      if (input$modo == "pct") {
        p <- ggplot(d, aes(Year, valor, fill = tipo, group = tipo))
        if (isTRUE(input$guerras)) p <- p + camada_guerras_transporte(input$periodo)
        p <- p +
          geom_area(txt, colour = "white", linewidth = 0.2) +
          scale_fill_manual(values = cores_tipos_transporte) +
          scale_y_continuous(labels = function(x) paste0(x, "%"))
      } else {
        p <- ggplot(d, aes(Year, valor, colour = tipo, group = tipo))
        if (isTRUE(input$guerras)) p <- p + camada_guerras_transporte(input$periodo)
        p <- p +
          geom_line(txt, linewidth = 1) +
          scale_colour_manual(values = cores_tipos_transporte)
        p <- if (isTRUE(input$log)) {
          p + scale_y_log10(labels = rotulo_num_transporte)
        } else {
          p + scale_y_continuous(labels = rotulo_num_transporte)
        }
      }

      p <- p +
        scale_x_continuous(breaks = scales::pretty_breaks(8)) +
        coord_cartesian(xlim = input$periodo) +
        labs(x = NULL, y = rotulo_y()) +
        tema_transporte()

      ggplotly(p, tooltip = "text") |>
        layout(legend = list(orientation = "h", y = -0.15)) |>
        config(displaylogo = FALSE)
    })

    # ---- Tabela ----
    output$tabela_dt <- renderDT({
      if (input$tabela == "ano") {
        t <- dados_veic() |>
          select(Ano = Year, tipo, valor) |>
          pivot_wider(names_from = tipo, values_from = valor) |>
          arrange(Ano)
        tabela_dt_transporte(t, setdiff(names(t), "Ano"), digitos())
      } else {
        t <- filtrado() |>
          filter(!is.na(qtd)) |>
          group_by(Tipo = tipo, Grupo = grupo) |>
          summarise(
            `Primeiro ano com dado` = min(Year),
            `Ano de pico`           = Year[which.max(qtd)],
            `Valor no pico`         = max(qtd),
            `Média anual`           = mean(qtd),
            `Total no período`      = sum(qtd),
            .groups = "drop"
          )
        tabela_dt_transporte(t, c("Valor no pico", "Média anual", "Total no período"), 0)
      }
    })
  })
}


# =====================================================================
# PARTE 2 — TELA (UI)   (veio do  ui <- page_navbar(...)  do app.R)
#   page_navbar + nav_panel  ->  pagina() + navset_tab + nav_panel
# =====================================================================
ui_transporte <- function() {
  pagina("carros", "Transporte", "Navios, trens, carros e outros veículos",

    # única parte do css_app que ainda é usada (rodapé dos cards)
    tags$style(HTML(".fonte { color: #5C6B62; font-size: 0.85rem; }")),

    navset_tab(
      nav_panel("Construção naval",       mod_navios_ui_transporte("navios_transporte")),
      nav_panel("Ferrovia e motorização", mod_veiculos_ui_transporte("veic_transporte"))
    )
  )
}


# =====================================================================
# PARTE 3 — SERVER   (veio do  server <- function(...)  do app.R)
# =====================================================================
server_transporte <- function(input, output, session) {
  mod_navios_server_transporte("navios_transporte", navios_transporte)
  mod_veiculos_server_transporte("veic_transporte", veiculos_transporte)
}
