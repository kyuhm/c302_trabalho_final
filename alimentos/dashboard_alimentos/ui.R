dashboardPage(
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
