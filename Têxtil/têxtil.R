library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(tidyr)
library(readr)

# Puxando os dados
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/textiles.csv"
if (!file.exists("tex.csv")) download.file(url, "tex.csv", mode = "wb", quiet = TRUE)
dados <- read_csv("tex.csv", show_col_types = FALSE) %>% arrange(Year)

# Tabela de guerras
g_hist <- data.frame(
  n = c("Independência", "Ítalo-Turca", "1ª Guerra Mundial", "Ítalo-Etíope", "2ª Guerra Mundial"),
  i = c(1866, 1911, 1915, 1935, 1940),
  f = c(1866, 1912, 1918, 1936, 1945),
  col = c("#8e7cc3", "#e6a23c", "#c0392b", "#d4a017", "#7f1d1d")
)

marca_guerra <- function(p, a1, a2, marcar) {
  if (!marcar) return(p)
  
  formas <- list()
  
  for(k in 1:nrow(g_hist)) {
    g <- g_hist[k, ]
    if(g$f < a1 || g$i > a2) next
    
    # retangulo de fundo
    formas[[length(formas) + 1]] <- list(
      type = "rect", xref = "x", x0 = max(g$i, a1) - 0.5, x1 = min(g$f, a2) + 0.5,
      yref = "paper", y0 = 0, y1 = 1, fillcolor = adjustcolor(g$col, alpha.f = 0.1),
      line = list(width = 0), layer = "below"
    )
    
    # area pro mouse
    p <- p %>% add_trace(
      x = c(max(g$i, a1) - 0.5, min(g$f, a2) + 0.5, min(g$f, a2) + 0.5, max(g$i, a1) - 0.5),
      y = c(0.93, 0.93, 1, 1),
      yaxis = "y3", inherit = FALSE, type = "scatter", mode = "lines",
      fill = "toself", fillcolor = g$col, line = list(width = 0),
      hoveron = "fills", text = g$n, hoverinfo = "text",
      hoverlabel = list(bgcolor = g$col, font = list(color = "white")),
      showlegend = FALSE
    )
  }
  
  p %>% layout(
    shapes = formas,
    yaxis3 = list(overlaying = "y", range = c(0, 1), visible = FALSE, fixedrange = TRUE)
  )
}

# --- Interface ---
ui <- dashboardPage(
  skin = "black",
  header = dashboardHeader(title = "Indústria Têxtil Italiana", titleWidth = 240),
  sidebar = dashboardSidebar(
    width = 240,
    sidebarMenu(
      id = "menu",
      menuItem("Indústria & Guerras", tabName = "t1", icon = icon("industry")),
      menuItem("Fibras & Substituição", tabName = "t2", icon = icon("leaf"))
    ),
    div(style = "padding: 20px 15px;",
        sliderInput("ano", "Filtro de Ano:", min = min(dados$Year), max = max(dados$Year),
                    value = c(1890, max(dados$Year)), step = 1, sep = "", ticks = FALSE),
        checkboxInput("cx_guerra", "Mostrar Guerras", TRUE),
        radioButtons("tipo", "Ver:",
                     choices = c("Ambos" = "ambos", "Fio" = "fio",
                                 "Tecido" = "tecido", "Seda" = "seda"),
                     selected = "ambos"),
        p(style = "color: #888; font-size: 11px; margin-top: 20px;", "Fonte: ISTAT / TidyTuesday")
    )
  ),
  body = dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background: #f4f6f9; } 
      .box { border-radius: 6px; }
      .rolagem { max-height: 420px; overflow: auto; }
      .rolagem table { font-size: 12px; white-space: nowrap; }
      .rolagem th { position: sticky; top: 0; background: #fff; z-index: 1; }
    "))),
    tabItems(
      tabItem(tabName = "t1",
        fluidRow(
          valueBoxOutput("caixa1", width = 4),
          valueBoxOutput("caixa2", width = 4),
          valueBoxOutput("caixa3", width = 4)
        ),
        fluidRow(
          box(title = "Produção anual de fio e tecido (mil t)", width = 12, status = "primary",
              plotlyOutput("plot1", height = "420px"))
        ),
        fluidRow(
          box(title = "Tabela Completa", width = 12, status = "primary",
              div(class = "rolagem", tableOutput("tabela1")))
        )
      ),
      tabItem(tabName = "t2",
        fluidRow(
          valueBoxOutput("caixa4", width = 6),
          valueBoxOutput("caixa5", width = 6)
        ),
        fluidRow(
          box(title = "Composição do fio por tipo de fibra (%)", width = 6, status = "info",
              plotlyOutput("plot2", height = "400px")),
          box(title = "Fio de algodão e seda crua (seda em escala log)", width = 6, status = "primary",
              plotlyOutput("plot3", height = "400px"))
        ),
        fluidRow(
          box(title = "Tabela Completa", width = 12, status = "primary",
              div(class = "rolagem", tableOutput("tabela2")))
        )
      )
    )
  )
)

# --- Servidor ---
server <- function(input, output, session) {

  # Filtro reativo
  filtro <- reactive({
    dados %>% filter(Year >= input$ano[1], Year <= input$ano[2])
  })

  # Tabelas
  output$tabela1 <- renderTable({
    filtro() %>% mutate(Year = as.character(Year))
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s",
     digits = 0, na = "-", width = "100%",
     format.args = list(big.mark = ".", decimal.mark = ","))
  
  output$tabela2 <- renderTable({
    filtro() %>% mutate(Year = as.character(Year))
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s",
     digits = 0, na = "-", width = "100%",
     format.args = list(big.mark = ".", decimal.mark = ","))

  # Caixas
  output$caixa1 <- renderValueBox({
    temp <- filtro() %>% filter(!is.na(Total_yarn)) %>% arrange(Year)
    valueBox(paste0(round(tail(temp$Total_yarn, 1)/1000), " mil t"),
             paste0("Fio em ", tail(temp$Year, 1)), icon = icon("chart-bar"), color = "blue")
  })

  output$caixa2 <- renderValueBox({
    temp <- filtro() %>% filter(!is.na(Total_yarn)) %>% arrange(Year) %>%
      mutate(v = (Total_yarn/lag(Total_yarn)-1)*100, valido = Year-lag(Year)==1) %>% filter(valido)
    pior <- temp[which.min(temp$v), ]
    valueBox(paste0(round(pior$v), "%"), paste0("Maior queda (", pior$Year, ")"),
             icon = icon("arrow-down"), color = "red")
  })

  output$caixa3 <- renderValueBox({
    temp <- filtro() %>% filter(!is.na(Total_yarn), !is.na(Total_textiles))
    valueBox(round(cor(temp$Total_yarn, temp$Total_textiles, use = "complete.obs"), 2),
             "Correlação", icon = icon("link"), color = "green")
  })

  output$caixa4 <- renderValueBox({
    temp <- filtro() %>% filter(!is.na(Cotton_yarn), !is.na(Total_yarn))
    p <- temp[which.max(temp$Cotton_yarn/temp$Total_yarn), ]
    valueBox(paste0(round(p$Cotton_yarn/p$Total_yarn*100), "%"),
             paste0("Pico do algodão (", p$Year, ")"), icon = icon("leaf"), color = "green")
  })

  output$caixa5 <- renderValueBox({
    temp <- filtro() %>% filter(!is.na(Raw_silk))
    p <- temp[which.max(temp$Raw_silk), ]
    valueBox(paste0(round(p$Raw_silk), " t"), paste0("Pico da seda (", p$Year, ")"),
             icon = icon("scroll"), color = "purple")
  })

  # Graficos
  output$plot1 <- renderPlotly({
    temp <- filtro()
    p <- plot_ly(temp, x = ~Year)
    
    if (input$tipo %in% c("ambos", "fio"))
      p <- p %>% add_lines(y = ~Total_yarn/1000, name = "Fio", line = list(color = "#2C3E50"))
    if (input$tipo %in% c("ambos", "tecido"))
      p <- p %>% add_lines(y = ~Total_textiles/1000, name = "Tecido", line = list(color = "#18BC9C"))
    if (input$tipo == "seda")
      p <- p %>% add_lines(y = ~Raw_silk, name = "Seda", line = list(color = "#7B5EA7"))

    p %>% layout(xaxis = list(title = ""), yaxis = list(title = "Volume"),
                 legend = list(orientation = "h", y = -0.2)) %>%
      marca_guerra(input$ano[1], input$ano[2], input$cx_guerra) %>%
      config(displayModeBar = FALSE)
  })

  output$plot2 <- renderPlotly({
    temp <- filtro() %>% filter(!is.na(Total_yarn), Total_yarn > 0) %>%
      transmute(Year,
                Algodão = Cotton_yarn/Total_yarn*100,
                Flock = Flock_yarn/Total_yarn*100,
                Outras = Other_yarn/Total_yarn*100) %>%
      pivot_longer(-Year, names_to = "Fibra", values_to = "Pct")

    plot_ly(temp, x = ~Year, y = ~Pct, color = ~Fibra, type = "scatter", mode = "lines",
            stackgroup = "one", groupnorm = "percent",
            colors = c("Algodão" = "#2C3E50", "Flock" = "#E08E0B", "Outras" = "#A23B72")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "%", ticksuffix = "%"),
             legend = list(orientation = "h", y = -0.2)) %>%
      marca_guerra(input$ano[1], input$ano[2], input$cx_guerra) %>%
      config(displayModeBar = FALSE)
  })

  output$plot3 <- renderPlotly({
    temp <- filtro() %>% filter(!is.na(Cotton_yarn), !is.na(Raw_silk))
    
    plot_ly(temp, x = ~Year) %>%
      add_lines(y = ~Cotton_yarn/1000, name = "Algodão", line = list(color = "#18BC9C"), yaxis = "y") %>%
      add_lines(y = ~Raw_silk, name = "Seda", line = list(color = "#7B5EA7", dash = "dot"), yaxis = "y2") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Algodão (mil t)"),
             yaxis2 = list(title = "Seda (t) log", overlaying = "y", side = "right", type = "log"),
             legend = list(orientation = "h", y = -0.2)) %>%
      marca_guerra(input$ano[1], input$ano[2], input$cx_guerra) %>%
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)