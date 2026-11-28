# R/mod_navios.R — Aba 1: Construção naval italiana

mod_navios_ui <- function(id) {
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

mod_navios_server <- function(id, navios) {
  moduleServer(id, function(input, output, session) {

    rotulos <- c(
      n     = "Navios lançados por ano",
      peso  = "Tonelagem bruta lançada (t)",
      medio = "Peso médio por navio (t)"
    )
    digitos <- reactive(if (input$metrica == "medio") 1 else 0)

    dados <- reactive({
      req(input$periodo)
      k   <- as.integer(input$janela)
      col <- switch(input$metrica,
                    n = "Ships_launched", peso = "Ships_weight",
                    medio = "peso_medio")
      navios |>
        filter(Year >= input$periodo[1], Year <= input$periodo[2]) |>
        mutate(valor = .data[[col]], suave = media_movel(valor, k))
    })

    output$titulo <- renderText({
      paste0(rotulos[[input$metrica]], ", ",
             input$periodo[1], "–", input$periodo[2])
    })

    # ---- Cartões-resumo ----
    resumo <- reactive({
      d <- dados() |> filter(!is.na(valor))
      validate(need(nrow(d) > 0, "Sem dados no período escolhido."))
      pico <- d[which.max(d$valor), ]
      list(ano = pico$Year, valor = pico$valor, media = mean(d$valor))
    })
    output$v_ano   <- renderText(resumo()$ano)
    output$v_pico  <- renderText(fmt(resumo()$valor, digitos()))
    output$v_media <- renderText(fmt(resumo()$media, digitos()))

    # ---- Gráfico ----
    output$grafico <- renderPlotly({
      d <- dados()
      validate(need(sum(!is.na(d$valor)) > 1, "Sem dados no período escolhido."))
      k      <- as.integer(input$janela)
      rotulo <- rotulos[[input$metrica]]
      dg     <- digitos()

      p <- ggplot(d, aes(Year, valor))
      if (isTRUE(input$guerras)) p <- p + camada_guerras(input$periodo)

      p <- p +
        geom_line(
          aes(text = paste0("Ano: ", Year, "<br>", rotulo, ": ", fmt(valor, dg)),
              group = 1),
          colour    = if (k > 1) "#9DB8A8" else cores[["verde"]],
          linewidth = if (k > 1) 0.6 else 1
        )

      if (k > 1) {
        p <- p + geom_line(
          aes(y = suave,
              text = paste0("Ano: ", Year, "<br>Média móvel (", k, " anos): ",
                            fmt(suave, dg)),
              group = 1),
          colour = cores[["verde_escuro"]], linewidth = 1.2
        )
      }

      p <- p +
        scale_y_continuous(labels = rotulo_num) +
        scale_x_continuous(breaks = scales::pretty_breaks(8)) +
        coord_cartesian(xlim = input$periodo) +
        labs(x = NULL, y = rotulo) +
        theme_app()

      ggplotly(p, tooltip = "text") |>
        config(displaylogo = FALSE)
    })

    # ---- Tabela ----
    output$tabela_dt <- renderDT({
      d <- dados()
      if (input$tabela == "ano") {
        t <- d |>
          transmute(Ano = Year,
                    `Navios lançados`     = Ships_launched,
                    `Tonelagem bruta (t)` = Ships_weight,
                    `Peso médio (t/navio)` = peso_medio)
        tabela_dt(t, c("Navios lançados", "Tonelagem bruta (t)"), 0) |>
          DT::formatRound("Peso médio (t/navio)", 1, mark = ".", dec.mark = ",")
      } else {
        t <- d |>
          group_by(`Década` = paste0(decada, "s")) |>
          summarise(
            `Navios lançados`     = sum(Ships_launched),
            `Tonelagem bruta (t)` = sum(Ships_weight, na.rm = TRUE),
            `Peso médio (t/navio)` =
              sum(Ships_weight, na.rm = TRUE) /
              sum(Ships_launched[!is.na(Ships_weight)]),
            `Ano de pico (navios)` = Year[which.max(Ships_launched)],
            .groups = "drop"
          )
        tabela_dt(t, c("Navios lançados", "Tonelagem bruta (t)"), 0) |>
          DT::formatRound("Peso médio (t/navio)", 1, mark = ".", dec.mark = ",")
      }
    })
  })
}
