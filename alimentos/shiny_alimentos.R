library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

# ===================== Shiny ========================

ui <- dashboardPage(
  dashboardHeader(title = "Produção Italiana"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Visão Geral e KPIs",
        tabName = "aba1_visao_geral",
        icon = icon("chart-line")
      ),
      menuItem(
        text = "História, Guerras e Ranking",
        tabName = "aba2_his_war_beer",
        icon = icon("ranking-star")
      )
    )
  ),
  dashboardBody(
    tags$style(HTML("
      .btn-download-espaco {
        margin-bottom: 12px;
      }
    ")),
    tabItems(
      # ---------------- ABA 1 ----------------
      tabItem(
        tabName = "aba1_visao_geral",
        fluidRow(
          valueBoxOutput("kpi_periodo", width = 3),
          valueBoxOutput("kpi_total_ton", width = 3),
          valueBoxOutput("kpi_total_hl", width = 3),
          valueBoxOutput("kpi_maior_total_geral", width = 3)
        ),
        fluidRow(
          box(
            title = "Comparativo entre produtos: como cada produto evoluiu de década em década?",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(outputId = "grafico_heat", height = "520px"),
            helpText(
              "Cor = log1p da média. Em cada célula: média e desvio padrão (±).",
              "Passe o mouse para ver N, NAs, média, DP, mín, Q1, mediana, Q3 e máx.",
              "Células em branco = década sem dados para a variável."
            )
          )
        ),
        fluidRow(
          box(
            title = "Tabela da base de dados traduzida e convertido em toneladas e hectolitros",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            div(
              class = "btn-download-espaco",
              downloadButton("baixar_comidas_bebidas", "Baixar comidas_bebidas (.csv)", class = "btn-success")
            ),
            DTOutput("tabela_dados_brutos")
          )
        )
      ),
      # ---------------- ABA 2 ----------------
      tabItem(
        tabName = "aba2_his_war_beer",
        fluidRow(
          box(
            title = "Série Temporal das variáveis por década: O que mais oscilou nas guerras? (1ª Guerra Mundial: 1914–1918 e 2ª Guerra Mundial: 1939–1945)",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(outputId = "grafico_mm_guerras", height = "900px")
          )
        ),
        fluidRow(
          box(
            title = "Ranking por Década: Qual produto liderou cada década?",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            selectInput(
              inputId = "filtro_periodo_ranking",
              label = "Selecione o período:",
              choices = c(
                "Período todo" = "todo",
                "1ª Guerra (1914–1918)" = "ww1",
                "2ª Guerra (1939–1945)" = "ww2",
                "Pós-2ª Guerra (1946+)" = "pos_ww2"
              ),
              selected = "todo"
            ),
            plotlyOutput(outputId = "grafico_ranking_prod", height = "460px")
          )
        ),
        fluidRow(
          box(
            title = "Tabela de estatísticas descritivas por década e produto",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            div(
              class = "btn-download-espaco",
              downloadButton("baixar_descritiva", "Baixar estatística descritiva (.csv)", class = "btn-success")
            ),
            DTOutput("tabela_DT")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  # =============== primeira aba ===================
  output$kpi_periodo <- renderValueBox({
    valueBox(
      value = paste0(anos_min, "-", anos_max),
      subtitle = paste0("Faixa de anos (", length(decadas), " décadas)"),
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  totais_produtos_aba1 <- reactive({
    comidas_bebidas_longer |>
      filter(!is.na(Valor)) |>
      mutate(
        unidade = ifelse(
          str_detect(str_to_lower(as.character(Variavel)), "cerveja|etanol"),
          "hL",
          "t"
        )
      ) |>
      group_by(Variavel, unidade) |>
      summarise(
        total_periodo = sum(Valor, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$kpi_total_ton <- renderValueBox({
    df_ton <- totais_produtos_aba1() |>
      filter(unidade == "t") |>
      arrange(desc(total_periodo))
    
    if (nrow(df_ton) == 0) {
      return(
        valueBox(
          value = "0,00 t",
          subtitle = "Toneladas totais (sem produtos em t)",
          icon = icon("weight-hanging"),
          color = "yellow"
        )
      )
    }
    
    soma_ton <- sum(df_ton$total_periodo, na.rm = TRUE)
    lider_ton <- df_ton |> slice(1)
    
    valueBox(
      value = paste0(fmt_br(soma_ton), " t"),
      subtitle = paste0("Toneladas totais (Líder: ", lider_ton$Variavel, ")"),
      icon = icon("weight-hanging"),
      color = "yellow"
    )
  })
  
  output$kpi_total_hl <- renderValueBox({
    df_hl <- totais_produtos_aba1() |>
      filter(unidade == "hL") |>
      arrange(desc(total_periodo))
    
    if (nrow(df_hl) == 0) {
      return(
        valueBox(
          value = "0,00 hL",
          subtitle = "Volume total (sem produtos em hL)",
          icon = icon("bottle-droplet"),
          color = "aqua"
        )
      )
    }
    
    soma_hl <- sum(df_hl$total_periodo, na.rm = TRUE)
    lider_hl <- df_hl |> slice(1)
    
    valueBox(
      value = paste0(fmt_br(soma_hl), " hL"),
      subtitle = paste0("Volume Total (Líder: ", lider_hl$Variavel, ")"),
      icon = icon("bottle-droplet"),
      color = "aqua"
    )
  })
  
  output$kpi_maior_total_geral <- renderValueBox({
    df_tot <- totais_produtos_aba1() |>
      arrange(desc(total_periodo))
    
    req(nrow(df_tot) > 0)
    top_tot <- df_tot |> slice(1)
    
    valueBox(
      value = as.character(top_tot$Variavel),
      subtitle = paste0("Maior produção total: ", fmt_br(top_tot$total_periodo), " ", top_tot$unidade),
      icon = icon("arrow-trend-up"),
      color = "olive"
    )
  })
  
  output$grafico_heat <- renderPlotly({
    f <- function(x) ifelse(is.na(x), "NA", fmt_br(x))
    
    d <- tabela_descritiva |>
      filter(!is.na(Decada)) |>
      mutate(
        Decada   = factor(Decada, levels = decadas),
        Variavel = factor(Variavel, levels = variaveis_prod),
        z        = log1p(media),
        hover = paste0(
          "<b>Década:</b> ", Decada, "<br>",
          "<b>Variável:</b> ", Variavel, "<br>",
          "<b>N:</b> ", n_obs, " | <b>NAs:</b> ", n_na, "<br>",
          "<b>Soma:</b> ", f(soma), "<br>",
          "<b>Média:</b> ", f(media), "<br>",
          "<b>DP:</b> ", f(dp), "<br>",
          "<b>Mín:</b> ", f(min), "<br>",
          "<b>Q1:</b> ", f(q1), "<br>",
          "<b>Mediana (Q2):</b> ", f(mediana), "<br>",
          "<b>Q3:</b> ", f(q3), "<br>",
          "<b>Máx:</b> ", f(max)
        ),
        rotulo = ifelse(
          is.na(media), "",
          paste0(f(media), "<br>±", f(dp))
        )
      )
    
    z_meio <- mean(range(d$z, na.rm = TRUE))
    d <- d |>
      mutate(cor_txt = ifelse(!is.na(z) & z > z_meio, "white", "black"))
    
    anotacoes <- lapply(seq_len(nrow(d)), function(i) {
      list(
        x = as.character(d$Decada[i]),
        y = as.character(d$Variavel[i]),
        text = d$rotulo[i],
        showarrow = FALSE,
        font = list(size = 10, color = d$cor_txt[i])
      )
    })
    
    plot_ly(
      data = d,
      x = ~Decada,
      y = ~Variavel,
      z = ~z,
      type = "heatmap",
      text = ~hover,
      hoverinfo = "text",
      colorscale = list(c(0, "#ffffcc"), c(0.5, "#fd8d3c"), c(1, "#800026")),
      colorbar = list(title = "log1p(média)")
    ) |>
      layout(
        xaxis = list(title = "Década", categoryorder = "array", categoryarray = decadas),
        yaxis = list(title = "Variável", autorange = "reversed"),
        annotations = anotacoes,
        hoverlabel = list(align = "left")
      )
  })
  
  output$tabela_dados_brutos <- renderDT({
    cols_com_na <- c("Ano", variaveis_prod)
    
    render_na_bruto_js <- JS(
      "function(data, type, row, meta) {",
      "  if (type !== 'display') return data;",
      "  if (data === null || data === undefined || data === '') {",
      "    return 'NA';",
      "  }",
      "  if (typeof data === 'number' && Number.isNaN(data)) {",
      "    return 'NA';",
      "  }",
      "  return data;",
      "}"
    )
    
    targets_idx <- which(names(comidas_bebidas) %in% cols_com_na) - 1
    
    datatable(
      comidas_bebidas,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = targets_idx, render = render_na_bruto_js)
        )
      )
    )
  })
  
  output$baixar_comidas_bebidas <- downloadHandler(
    filename = function() {
      paste0("comidas_bebidas_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(comidas_bebidas, file)
    }
  )
  
  # =============== segunda aba ===================
  
  output$grafico_ranking_prod <- renderPlotly({
    d <- comidas_bebidas_longer |>
      filter(!is.na(Valor), !is.na(Decada))
    
    if (input$filtro_periodo_ranking == "ww1") {
      d <- d |> filter(!is.na(Ano), Ano >= 1914, Ano <= 1918)
    } else if (input$filtro_periodo_ranking == "ww2") {
      d <- d |> filter(!is.na(Ano), Ano >= 1939, Ano <= 1945)
    } else if (input$filtro_periodo_ranking == "pos_ww2") {
      d <- d |> filter(!is.na(Ano), Ano >= 1946)
    }
    
    validate(need(nrow(d) > 0, "Sem dados disponíveis para o período selecionado."))
    
    rk <- d |>
      mutate(
        unidade = ifelse(
          str_detect(str_to_lower(as.character(Variavel)), "cerveja|etanol"),
          "hL",
          "t"
        )
      ) |>
      group_by(Decada, Variavel, unidade) |>
      summarise(
        total_decada = sum(Valor, na.rm = TRUE),
        .groups = "drop"
      ) |>
      group_by(Decada) |>
      mutate(
        posicao = rank(desc(total_decada), ties.method = "min")
      ) |>
      ungroup()
    
    niveis_decada <- intersect(decadas, unique(rk$Decada))
    n_prod <- length(variaveis_prod)
    
    rk <- rk |>
      mutate(
        Decada = factor(Decada, levels = niveis_decada),
        Variavel = factor(Variavel, levels = variaveis_prod),
        texto_hover = paste0(
          "<b>", Variavel, "</b> (", Decada, ")<br>",
          "<b>Posição:</b> #", posicao, "<br>",
          "<b>Total na década:</b> ", fmt_br(total_decada), " ", unidade
        )
      )
    
    p <- ggplot(rk, aes(x = Decada, y = posicao, color = Variavel, group = Variavel)) +
      geom_line(linewidth = 1.1, alpha = 0.9) +
      geom_point(aes(text = texto_hover), size = 3.2, alpha = 0.95) +
      scale_y_reverse(
        breaks = seq_len(n_prod),
        labels = paste0("#", seq_len(n_prod))
      ) +
      scale_color_manual(values = paleta_produtos, name = "Produto", drop = FALSE) +
      labs(
        x = "Década",
        y = "Posição no ranking"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
      )
    
    ggplotly(p, tooltip = "text") |>
      layout(hoverlabel = list(align = "left"))
  })
  
  limites_y_mm <- reactive({
    dados_decada |>
      summarise(
        ymin = min(valor_decada, na.rm = TRUE),
        ymax = max(valor_decada, na.rm = TRUE),
        .by = Variavel
      ) |>
      mutate(
        folga = (ymax - ymin) * 0.05,
        ymin  = ymin - folga,
        ymax  = ymax + folga
      ) |>
      select(Variavel, ymin, ymax)
  })
  
  periodos_guerra <- reactive({
    tibble(
      xmin   = c(1910, 1940) - 5,
      xmax   = c(1910, 1940) + 5,
      guerra = c("1910s (1914–1918)", "1940s (1939–1945)")
    ) |>
      crossing(limites_y_mm())
  })
  
  output$grafico_mm_guerras <- renderPlotly({
    eixo_x <- sort(unique(dados_decada$decada_num))
    
    dd <- dados_decada |>
      mutate(
        Variavel = factor(Variavel, levels = variaveis_prod),
        texto_hover = paste0(
          "<b>Década:</b> ", Decada, "<br>",
          "<b>Média:</b> ", fmt_br(valor_decada), "<br>",
          "<b>DP:</b> ", fmt_br(dp_decada), "<br>",
          "<b>Média ± DP:</b> ", fmt_br(inferior), " – ", fmt_br(superior), "<br>",
          "<b>N:</b> ", n_obs
        )
      )
    
    p <- ggplot() +
      geom_rect(
        data = periodos_guerra(),
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = guerra),
        alpha = 0.35,
        inherit.aes = FALSE
      ) +
      geom_errorbar(
        data = dd,
        aes(x = decada_num, ymin = inferior, ymax = superior, color = Variavel),
        width = 2, linewidth = 0.6, alpha = 0.85
      ) +
      geom_line(
        data = dd,
        aes(x = decada_num, y = valor_decada, color = Variavel, group = Variavel),
        linewidth = 1.0, alpha = 0.95
      ) +
      geom_point(
        data = dd,
        aes(x = decada_num, y = valor_decada, color = Variavel, text = texto_hover),
        size = 2.2, alpha = 0.95
      ) +
      facet_wrap(~ Variavel, scales = "free_y", ncol = 2) +
      scale_color_manual(values = paleta_produtos, guide = "none", drop = FALSE) +
      scale_fill_manual(
        values = c("1910s (1914–1918)" = "#f39c12", "1940s (1939–1945)" = "#e74c3c"),
        name = "Período destacado"
      ) +
      scale_x_continuous(breaks = eixo_x, labels = paste0(eixo_x, "s")) +
      scale_y_continuous(
        labels = scales::label_number(big.mark = ".", decimal.mark = ",")
      ) +
      labs(
        x = "Década", y = "Média na década"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = "text") |>
      layout(hoverlabel = list(align = "left"))
  })
  
  output$tabela_DT <- renderDT({
    idx_estat <- which(names(tabela_descritiva) %in% cols_estat) - 1
    
    render_2dec_ou_na <- JS(
      "function(data, type, row, meta) {",
      "  if (type !== 'display') {",
      "    return (data === null || data === undefined || data === '' || Number.isNaN(Number(data))) ? -Infinity : Number(data);",
      "  }",
      "  if (data === null || data === undefined || data === '') return 'NA';",
      "  var num = Number(data);",
      "  if (Number.isNaN(num) || !Number.isFinite(num)) return 'NA';",
      "  return num.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });",
      "}"
    )
    
    datatable(
      tabela_descritiva,
      rownames = FALSE,
      colnames = c(
        "Década", "Variável", "N", "NAs", "Soma", "Média", "DP",
        "Mín", "Q1", "Mediana", "Q3", "Máx"
      ),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = idx_estat, render = render_2dec_ou_na)
        )
      )
    )
  })
  
  output$baixar_descritiva <- downloadHandler(
    filename = function() {
      paste0("estatistica_descritiva_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(tabela_descritiva, file)
    }
  )
}

shinyApp(ui = ui, server = server)
