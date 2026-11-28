# =====================================================================
# PÁGINA TÊXTIL  —  VERSÃO CONVERTIDA (para comparar)
# =====================================================================
# Este arquivo é o têxtil.R adaptado ao molde do projeto.
# Para usar: copie TUDO isto para R/21_textil.R (substituindo).
# (DT e plotly já estão em R/00_pacotes.R — não precisa adicionar nada.)
#
# O QUE FOI TIRADO / MUDADO do código original:
#   - library(shinydashboard), library(fresh)  -> não usa mais
#   - download.file + read_csv               -> `textiles` já vem de R/01_dados.R
#   - dashboardPage / menuItem / box         -> pagina() + navset_tab + card
#   - valueBox / valueBoxOutput              -> value_box + textOutput
#   - dashboardSidebar com os filtros        -> layout_sidebar (filtros à esquerda,
#                                               valem para as 2 abas, como antes)
#   - shinyApp(ui, server)                   -> o app.R já faz isso
#   - guerras  -> guerras_textil  (a página Transporte também tem guerras)
#   - todos os IDs e nomes ganharam _textil no fim
#
# Os gráficos (plot_ly) estão IGUAIS ao original.
# =====================================================================

setor_textil <- "Têxteis"


# =====================================================================
# PARTE 1 — DADOS E AJUDANTES
# =====================================================================

# (o download foi APAGADO: textiles já existe, vem de R/01_dados.R)
tex_textil <- textiles |> arrange(Year)

# períodos de guerra
guerras_textil <- tribble(
  ~nome,           ~ini, ~fim, ~cor,
  "Independência", 1866, 1866, "#8e7cc3",
  "Ítalo-Turca",   1911, 1912, "#e6a23c",
  "1ª Guerra",     1915, 1918, "#c0392b",
  "Etiópia",       1935, 1936, "#d4a017",
  "2ª Guerra",     1940, 1945, "#7f1d1d"
)

# coloca as faixas das guerras atrás de um gráfico plotly
# (CORRIGIDO: no original cada layout() apagava a faixa anterior e só
#  aparecia a última guerra; agora junta todas e aplica de uma vez)
bg_guerra_textil <- function(p, ini, fim, show) {
  if (!isTRUE(show)) return(p)
  g <- guerras_textil %>% filter(fim >= !!ini, ini <= !!fim)
  if (nrow(g) == 0) return(p)
  faixas <- lapply(seq_len(nrow(g)), function(i) list(
    type = "rect", xref = "x",
    x0 = max(g$ini[i], ini) - 0.5, x1 = min(g$fim[i], fim) + 0.5,
    yref = "paper", y0 = 0, y1 = 1,
    fillcolor = grDevices::adjustcolor(g$cor[i], alpha.f = 0.1),
    line = list(width = 0), layer = "below"
  ))
  p %>% layout(shapes = faixas)
}

# variação anual do fio (usada no cartão "maior queda" e no gráfico de barras)
variacao_textil <- function(x) {
  x %>% filter(!is.na(Total_yarn)) %>% arrange(Year) %>%
    mutate(v = (Total_yarn / lag(Total_yarn) - 1) * 100,
           cons = Year - lag(Year) == 1) %>%
    filter(cons)
}


# =====================================================================
# PARTE 2 — TELA (UI)
# =====================================================================
ui_textil <- function() {
  pagina("textil", "Têxtil", "Fios, tecidos e seda na Itália",
         
         card(
           layout_sidebar(
             fillable = FALSE,
             
             # ---- filtros (eram do dashboardSidebar) ----
             sidebar = sidebar(
               width = 260,
               sliderInput("anos_textil", "Período:",
                           min = min(tex_textil$Year), max = max(tex_textil$Year),
                           value = c(1890, max(tex_textil$Year)),
                           step = 1, sep = "", ticks = FALSE),
               checkboxInput("guerras_textil", "Destacar guerras", TRUE),
               radioButtons("metrica_textil", "Métrica:",
                            choices = c("Fio e Tecido" = "ambos", "Fio" = "fio",
                                        "Tecido" = "tecido", "Seda" = "seda"),
                            selected = "ambos"),
               p(style = "color: #888; font-size: 11px; margin-top: 20px;",
                 "Fonte: ISTAT / TidyTuesday")
             ),
             
             navset_tab(
               
               # ------------ ABA 1 (era tabName = "industria") ------------
               nav_panel("Indústria & Guerras",
                         layout_columns(
                           value_box(textOutput("vb_prod_fim_txt_textil"),  textOutput("vb_prod_fim_textil"),
                                     showcase = icon("chart-bar"),  theme = "primary"),
                           value_box(textOutput("vb_queda_txt_textil"),     textOutput("vb_queda_textil"),
                                     showcase = icon("arrow-down"), theme = "danger"),
                           value_box("Correlação fio × tecido",            textOutput("vb_correlacao_textil"),
                                     showcase = icon("link"),       theme = "success")
                         ),
                         layout_columns(
                           col_widths = c(8, 4),
                           card(card_header("Evolução da Produção"),
                                plotlyOutput("p_producao_textil", height = "350px")),
                           card(card_header("Variação Anual"),
                                plotlyOutput("p_variacao_textil", height = "350px"))
                         ),
                         layout_columns(
                           card(card_header("Eficiência (Tecido/Fio)"),
                                plotlyOutput("p_eficiencia_textil", height = "330px")),
                           card(card_header("Queda e Recuperação"),
                                plotlyOutput("p_indice_guerra_textil", height = "330px"))
                         )
               ),
               
               # ------------ ABA 2 (era tabName = "fibras") ------------
               nav_panel("Fibras & Substituição",
                         layout_columns(
                           value_box(textOutput("vb_algodao_txt_textil"), textOutput("vb_algodao_textil"),
                                     showcase = icon("leaf"),   theme = "success"),
                           value_box(textOutput("vb_seda_txt_textil"),    textOutput("vb_seda_textil"),
                                     showcase = icon("scroll"), theme = "purple")
                         ),
                         layout_columns(
                           card(card_header("Composição do Fio"),
                                plotlyOutput("p_composicao_textil", height = "400px")),
                           card(card_header("Algodão x Seda"),
                                plotlyOutput("p_seda_algodao_textil", height = "400px"))
                         )
               )
             )
           )
         )
  )
}


# =====================================================================
# PARTE 3 — SERVER
# =====================================================================
server_textil <- function(input, output, session) {
  
  d <- reactive({
    req(input$anos_textil)
    tex_textil %>% filter(Year >= input$anos_textil[1], Year <= input$anos_textil[2])
  })
  
  # atalho para as faixas de guerra (mesmos 3 argumentos em todo gráfico)
  guerra <- function(p) bg_guerra_textil(p, input$anos_textil[1], input$anos_textil[2],
                                         input$guerras_textil)
  
  # ---------------- cartões (eram valueBox) ----------------
  ult_fio <- reactive(d() %>% filter(!is.na(Total_yarn)) %>% arrange(Year) %>% tail(1))
  output$vb_prod_fim_textil     <- renderText(paste0(round(ult_fio()$Total_yarn / 1000), " mil t"))
  output$vb_prod_fim_txt_textil <- renderText(paste0("Fio total em ", ult_fio()$Year))
  
  pior <- reactive({
    x <- variacao_textil(d())
    validate(need(nrow(x) > 0, "—"))
    x[which.min(x$v), ]
  })
  output$vb_queda_textil     <- renderText(paste0(round(pior()$v), "%"))
  output$vb_queda_txt_textil <- renderText(paste0("Maior queda (", pior()$Year, ")"))
  
  output$vb_correlacao_textil <- renderText({
    x <- d() %>% filter(!is.na(Total_yarn), !is.na(Total_textiles))
    round(cor(x$Total_yarn, x$Total_textiles, use = "complete.obs"), 2)
  })
  
  pico_alg <- reactive({
    x <- d() %>% filter(!is.na(Cotton_yarn), !is.na(Total_yarn))
    x[which.max(x$Cotton_yarn / x$Total_yarn), ]
  })
  output$vb_algodao_textil     <- renderText(paste0(round(pico_alg()$Cotton_yarn / pico_alg()$Total_yarn * 100), "%"))
  output$vb_algodao_txt_textil <- renderText(paste0("Pico do algodão (", pico_alg()$Year, ")"))
  
  pico_seda <- reactive({
    x <- d() %>% filter(!is.na(Raw_silk))
    x[which.max(x$Raw_silk), ]
  })
  output$vb_seda_textil     <- renderText(paste0(round(pico_seda()$Raw_silk), " t"))
  output$vb_seda_txt_textil <- renderText(paste0("Pico da seda (", pico_seda()$Year, ")"))
  
  # ---------------- gráficos (iguais ao original) ----------------
  output$p_producao_textil <- renderPlotly({
    x <- d()
    p <- plot_ly(x, x = ~Year)
    if (input$metrica_textil %in% c("ambos", "fio"))
      p <- p %>% add_lines(y = ~Total_yarn / 1000, name = "Fio", line = list(color = "#2C3E50"))
    if (input$metrica_textil %in% c("ambos", "tecido"))
      p <- p %>% add_lines(y = ~Total_textiles / 1000, name = "Tecido", line = list(color = "#18BC9C"))
    if (input$metrica_textil == "seda")
      p <- p %>% add_lines(y = ~Raw_silk, name = "Seda", line = list(color = "#7B5EA7"))
    
    p %>% layout(xaxis = list(title = ""), yaxis = list(title = "Volume"),
                 legend = list(orientation = "h", y = -0.2)) %>%
      guerra() %>%
      config(displayModeBar = FALSE)
  })
  
  output$p_variacao_textil <- renderPlotly({
    x <- variacao_textil(d())
    cor_barra <- ifelse(x$v < 0, "#E74C3C", "#2ECC71")
    plot_ly(x, x = ~Year, y = ~v, type = "bar", marker = list(color = cor_barra)) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "%", range = c(-100, 100))) %>%
      guerra() %>%
      config(displayModeBar = FALSE)
  })
  
  output$p_eficiencia_textil <- renderPlotly({
    x <- d() %>% filter(!is.na(Total_yarn), Total_yarn > 0) %>%
      mutate(ef = Total_textiles / Total_yarn)
    plot_ly(x, x = ~Year, y = ~ef, type = "scatter", mode = "lines",
            line = list(color = "#E67E22")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Tecido / Fio", range = c(0, 1.8))) %>%
      guerra() %>%
      config(displayModeBar = FALSE)
  })
  
  output$p_indice_guerra_textil <- renderPlotly({
    base <- function(ano) tex_textil$Total_yarn[tex_textil$Year == ano]
    x1 <- tex_textil %>% filter(Year >= 1914, Year <= 1925) %>%
      mutate(idx = Total_yarn / base(1914) * 100, g = "1ª Guerra")
    x2 <- tex_textil %>% filter(Year >= 1939, Year <= 1950) %>%
      mutate(idx = Total_yarn / base(1939) * 100, g = "2ª Guerra")
    x <- bind_rows(x1, x2) %>% filter(!is.na(idx))
    
    plot_ly(x, x = ~Year, y = ~idx, color = ~g, type = "scatter", mode = "lines+markers",
            colors = c("1ª Guerra" = "#c0392b", "2ª Guerra" = "#2980b9")) %>%
      layout(xaxis = list(title = "", range = c(1914, 1950)),
             yaxis = list(title = "Índice (Base 100)"),
             legend = list(orientation = "h", y = -0.2)) %>%
      guerra() %>%
      config(displayModeBar = FALSE)
  })
  
  output$p_composicao_textil <- renderPlotly({
    x <- d() %>% filter(!is.na(Total_yarn), Total_yarn > 0) %>%
      transmute(Year,
                Algodão = Cotton_yarn / Total_yarn * 100,
                Flock   = Flock_yarn  / Total_yarn * 100,
                Outras  = Other_yarn  / Total_yarn * 100) %>%
      pivot_longer(-Year, names_to = "Fibra", values_to = "Pct")
    
    plot_ly(x, x = ~Year, y = ~Pct, color = ~Fibra, type = "scatter", mode = "lines",
            stackgroup = "one", groupnorm = "percent",
            colors = c("Algodão" = "#2C3E50", "Flock" = "#E08E0B", "Outras" = "#A23B72")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "%", ticksuffix = "%"),
             legend = list(orientation = "h", y = -0.2)) %>%
      guerra() %>%
      config(displayModeBar = FALSE)
  })
  
  output$p_seda_algodao_textil <- renderPlotly({
    x <- d() %>% filter(!is.na(Cotton_yarn), !is.na(Raw_silk))
    plot_ly(x, x = ~Year) %>%
      add_lines(y = ~Cotton_yarn / 1000, name = "Algodão",
                line = list(color = "#18BC9C"), yaxis = "y") %>%
      add_lines(y = ~Raw_silk, name = "Seda",
                line = list(color = "#7B5EA7", dash = "dot"), yaxis = "y2") %>%
      layout(xaxis  = list(title = ""),
             yaxis  = list(title = "Algodão (mil t)"),
             yaxis2 = list(title = "Seda (t) log", overlaying = "y", side = "right", type = "log"),
             legend = list(orientation = "h", y = -0.2)) %>%
      guerra() %>%
      config(displayModeBar = FALSE)
  })
}
