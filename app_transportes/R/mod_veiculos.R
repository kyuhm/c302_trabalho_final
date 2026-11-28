# R/mod_veiculos.R — Aba 2: Material ferroviário × motorização

mod_veiculos_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 320,
      checkboxGroupInput(ns("tipos"), "Tipos de veículo",
                         choices  = levels(veiculos$tipo),
                         selected = levels(veiculos$tipo)),
      sliderInput(ns("periodo"), "Período",
                  min = 1905, max = 1985, value = c(1905, 1985),
                  step = 1, sep = ""),
      radioButtons(ns("modo"), "Como comparar",
                   choices = c("Valor absoluto"                      = "abs",
                               "Índice (100 = média do período)"     = "idx",
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

mod_veiculos_server <- function(id, veiculos) {
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
    dados <- reactive({
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
      d <- dados()
      validate(need(sum(!is.na(d$valor)) > 1,
                    "Sem dados para os filtros escolhidos."))
      dg <- digitos()
      txt <- aes(text = paste0(tipo, "<br>Ano: ", Year, "<br>",
                               fmt(valor, dg), if (input$modo == "pct") "%" else ""))

      if (input$modo == "pct") {
        p <- ggplot(d, aes(Year, valor, fill = tipo, group = tipo))
        if (isTRUE(input$guerras)) p <- p + camada_guerras(input$periodo)
        p <- p +
          geom_area(txt, colour = "white", linewidth = 0.2) +
          scale_fill_manual(values = cores_tipos) +
          scale_y_continuous(labels = function(x) paste0(x, "%"))
      } else {
        p <- ggplot(d, aes(Year, valor, colour = tipo, group = tipo))
        if (isTRUE(input$guerras)) p <- p + camada_guerras(input$periodo)
        p <- p +
          geom_line(txt, linewidth = 1) +
          scale_colour_manual(values = cores_tipos)
        p <- if (isTRUE(input$log)) {
          p + scale_y_log10(labels = rotulo_num)
        } else {
          p + scale_y_continuous(labels = rotulo_num)
        }
      }

      p <- p +
        scale_x_continuous(breaks = scales::pretty_breaks(8)) +
        coord_cartesian(xlim = input$periodo) +
        labs(x = NULL, y = rotulo_y()) +
        theme_app()

      ggplotly(p, tooltip = "text") |>
        layout(legend = list(orientation = "h", y = -0.15)) |>
        config(displaylogo = FALSE)
    })

    # ---- Tabela ----
    output$tabela_dt <- renderDT({
      if (input$tabela == "ano") {
        t <- dados() |>
          select(Ano = Year, tipo, valor) |>
          pivot_wider(names_from = tipo, values_from = valor) |>
          arrange(Ano)
        tabela_dt(t, setdiff(names(t), "Ano"), digitos())
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
        tabela_dt(t, c("Valor no pico", "Média anual", "Total no período"), 0)
      }
    })
  })
}
