# =====================================================================
# PÁGINA PANORAMA — Dashboard 1 (Visão geral) + Dashboard 2 (Perguntas)
# =====================================================================
# Responsável: ____________
#
# Organizado igual ao grafico_test.R:
#   PARTE 1 — DADOS DO PANORAMA        tabelas prontas que os gráficos usam
#   PARTE 2 — GRÁFICOS DO DASHBOARD 1  uma função por gráfico
#   PARTE 3 — GRÁFICOS DO DASHBOARD 2  uma função por gráfico
#   PARTE 4 — HTML                     cartões de números e top 3 (div)
#   PARTE 5 — TELA (UI)                onde cada gráfico aparece
#   PARTE 6 — SERVER                   liga a tela aos gráficos (1 linha cada)
#   PARTE 7 — CSS                      o visual, entre aspas
#
# COMO CRIAR UM GRÁFICO NOVO (3 passos):
#   1. PARTE 2 ou 3: escreva  grafico_xxx <- function(ano) { ggplot(...) }
#      (teste no console:  grafico_xxx(1950)  — o gráfico aparece no Plots)
#   2. PARTE 5: coloque  plotOutput("xxx")  no card onde ele deve aparecer
#   3. PARTE 6: ligue    output$xxx <- renderPlot(grafico_xxx(input$ano_home))
# =====================================================================


# =====================================================================
# PARTE 1 — DADOS DO PANORAMA (tabelas prontas)
# =====================================================================
# Os dados base (dados, volume_setor, resumo_produtos) vêm de R/01_dados.R.
# Aqui ficam só as tabelas que o Panorama usa.
# O app não tem filtros: usamos sempre os 3 setores e o período inteiro.

# ---------------------------------------------------------------------
# Volume acumulado de cada setor até um ano (usado na rosca)
# ---------------------------------------------------------------------
volume_acum_ate <- function(ano) {
  volume_setor |>
    filter(Year <= ano, setor %in% todos_setores) |>
    group_by(setor) |>
    summarise(t = sum(t), .groups = "drop") |>
    mutate(pct = t / sum(t))
}

# ---------------------------------------------------------------------
# Índice base 100 de cada setor (usado no gráfico 1 do Dashboard 2)
# ---------------------------------------------------------------------
# Índice base 100 ENCADEADO por setor.
# Cada produto tem uma unidade diferente, então olhamos só a VARIAÇÃO anual (em log)
# de cada produto; o setor anda pela MEDIANA dessas variações (a mediana evita que
# um produto que "nasce" com valores minúsculos domine o setor).
# O índice começa em 100 no primeiro ano com dado de cada setor.
indice_encadeado <- function(df) {
  df |>
    filter(producao > 0) |>
    arrange(produto, Year) |>
    group_by(produto) |>
    mutate(dlog = log(producao) - log(lag(producao))) |>
    ungroup() |>
    filter(!is.na(dlog)) |>
    group_by(setor, Year) |>
    summarise(passo = median(pmax(pmin(dlog, 0.7), -0.7)), .groups = "drop") |>
    group_by(setor) |>
    group_modify(~ {
      base <- tibble(Year = min(.x$Year) - 1, passo = 0)      # ano-base = 100
      bind_rows(base, .x) |>
        complete(Year = full_seq(Year, 1), fill = list(passo = 0))
    }) |>
    arrange(setor, Year) |>
    mutate(indice = 100 * exp(cumsum(passo))) |>
    ungroup()
}

# a tabela pronta (3 setores, período inteiro)
indice_setores <- dados |>
  filter(setor %in% todos_setores, Year >= periodo_total[1], Year <= periodo_total[2]) |>
  indice_encadeado()

# ---------------------------------------------------------------------
# Correlação entre os produtos (usado no mapa de calor do Dashboard 2)
# ---------------------------------------------------------------------
# Para cada produto: quanto ele variou de um ano para o outro (em log).
# Depois comparamos as variações de cada par de produtos (correlação).
calcular_correlacao <- function() {
  w <- dados |>
    filter(setor %in% todos_setores, producao > 0,
           Year >= periodo_total[1], Year <= periodo_total[2]) |>
    arrange(produto, Year) |>
    group_by(produto) |>
    mutate(d = log(producao) - log(lag(producao))) |>
    ungroup() |>
    filter(!is.na(d)) |>
    select(Year, produto, d) |>
    pivot_wider(names_from = produto, values_from = d)
  ok <- names(w)[-1][colSums(!is.na(w[-1])) >= 12]
  x <- as.matrix(w[ok])
  m <- suppressWarnings(cor(x, use = "pairwise.complete.obs"))
  m[crossprod(!is.na(x)) < 10] <- NA                       # poucos anos em comum
  m0 <- m; m0[is.na(m0)] <- 0
  ordem <- hclust(as.dist(1 - m0))$order                   # agrupa produtos parecidos
  list(m = m, ordem = ordem)
}

# a tabela pronta + qual setor é cada produto
correlacao       <- calcular_correlacao()
setor_do_produto <- dados |> distinct(produto, setor)


# =====================================================================
# PARTE 2 — GRÁFICOS DO DASHBOARD 1 (Visão geral)
# =====================================================================
# Cada gráfico é uma FUNÇÃO. Teste no console, ex.: grafico_rosca(1950)

# ---------------------------------------------------------------------
# STREAMGRAPH: produção de cada setor ao longo do tempo
# ---------------------------------------------------------------------
# Largura da faixa = produção do setor naquele ano (toneladas aprox.).
# As faixas ficam empilhadas ao redor de um eixo central (por isso o formato de "rio").
# modo "pct": largura = parte de cada setor no total do ano (sempre soma 100%).
grafico_stream <- function(setores_sel, anos, ano_sel, modo = "ton") {
  d <- volume_setor |>
    filter(setor %in% setores_sel, Year >= anos[1], Year <= anos[2])
  validate(need(nrow(d) > 5, "Período curto demais para o streamgraph."))
  
  # suaviza (média de 5 anos) e interpola em passos de 1/4 de ano, para curvas macias
  suav <- d |>
    group_by(setor) |>
    filter(n() >= 6) |>
    group_modify(~ {
      y  <- as.numeric(stats::filter(.x$t, rep(1 / 5, 5), sides = 2))
      y[is.na(y)] <- .x$t[is.na(y)]
      f  <- stats::splinefun(.x$Year, y, method = "monoH.FC")
      xs <- seq(min(.x$Year), max(.x$Year), by = 0.25)
      tibble(Year = xs, t = pmax(f(xs), 0))
    }) |>
    ungroup()
  validate(need(nrow(suav) > 0, "Poucos dados neste período."))
  
  # setores que não existem em certos anos ficam com largura zero
  grade <- seq(min(suav$Year), max(suav$Year), by = 0.25)
  suav <- suav |> complete(setor, Year = grade, fill = list(t = 0))
  
  # ordem de empilhamento: o maior setor fica no meio do rio
  ordem <- c("Transporte", "Alimentos", "Têxteis")
  ordem <- ordem[ordem %in% unique(suav$setor)]
  suav <- suav |>
    mutate(setor = factor(setor, ordem)) |>
    arrange(Year, setor) |>
    group_by(Year) |>
    mutate(total = sum(t),
           larg = if (modo == "pct") ifelse(total > 0, t / total * 100, 0) else t,
           topo = cumsum(larg),
           tot2 = sum(larg),
           ymax = if (modo == "pct") topo else topo - tot2 / 2,
           ymin = ymax - larg) |>
    ungroup()
  
  # rótulos dentro das faixas, no ano em que cada setor é mais largo
  esp <- max(suav$ymax) - min(suav$ymin)
  x_ini <- min(suav$Year); x_fim <- max(suav$Year); esp_x <- x_fim - x_ini
  rot <- suav |>
    filter(Year > x_ini + 0.12 * esp_x, Year < x_fim - 0.12 * esp_x) |>   # longe das bordas, para o nome caber
    group_by(setor) |>
    slice_max(larg, n = 1, with_ties = FALSE) |>
    ungroup() |>
    filter(larg > 0.045 * esp) |>
    mutate(y = (ymin + ymax) / 2,
           cor_txt = ifelse(setor == "Alimentos", "#FFFFFF", "#0F3A40"))
  
  topo_y <- max(suav$ymax)
  p <- ggplot(suav, aes(Year)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = setor), color = "white", linewidth = 0.35) +
    geom_vline(xintercept = ano_sel, color = "#0F3A40", linewidth = 0.6) +
    geom_label(data = tibble(x = ano_sel, y = topo_y), aes(x, y, label = ano_sel),
               inherit.aes = FALSE, vjust = -0.2, size = 3.4, fontface = "bold",
               color = "white", fill = "#0F3A40", label.size = 0, label.r = unit(3, "pt")) +
    geom_text(data = rot, aes(Year, y, label = setor, color = cor_txt),
              fontface = "bold", size = 3.8, inherit.aes = FALSE) +
    scale_fill_manual(values = cores, guide = "none") +
    scale_color_identity() +
    scale_x_continuous(breaks = seq(1860, 1990, 20), expand = expansion(mult = c(0.01, 0.03))) +
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.14))) +
    theme_void() +
    theme(axis.text.x = element_text(size = 10, color = "#5F8087", margin = margin(t = 4)),
          plot.margin = margin(6, 10, 6, 10))
  
  # barra de escala (só no modo toneladas): quanto vale uma certa espessura
  if (modo == "ton") {
    b <- unidade_bonita(max(suav$tot2), alvo = 4)
    x0 <- min(suav$Year) + 0.012 * diff(range(suav$Year))
    p <- p +
      annotate("segment", x = x0, xend = x0, y = -b / 2, yend = b / 2,
               color = "#0F3A40", linewidth = 0.9) +
      annotate("text", x = x0, y = b / 2, label = paste0(curto(b), " t"),
               hjust = -0.15, vjust = -0.3, size = 3.3, color = "#0F3A40")
  }
  p
}

# texto embaixo do streamgraph
legenda_stream <- function(modo) {
  if (modo == "pct") {
    "Largura de cada faixa = parte do setor no total do ano  ·  linha vertical = ano do slider acima"
  } else {
    "Largura de cada faixa = produção do setor no ano (toneladas aproximadas, suavizada em 5 anos)  ·  linha vertical = ano do slider  ·  transporte = só tonelagem dos navios"
  }
}

# ---------------------------------------------------------------------
# ROSCA: quanto cada setor já produziu (acumulado)
# ---------------------------------------------------------------------
# Anel de fora = acumulado até o ano escolhido.
# Anel de dentro (mais claro) = acumulado até 10 anos antes.
grafico_rosca <- function(ano) {
  agora <- volume_acum_ate(ano) |> mutate(anel = "agora", x = 3)
  validate(need(nrow(agora) > 0, "Ainda sem dados neste período."))
  antes <- volume_acum_ate(ano - 10) |> mutate(anel = "antes", x = 2)
  d <- bind_rows(antes, agora) |>
    mutate(setor = factor(setor, c("Alimentos", "Têxteis", "Transporte")),
           cor_txt = ifelse(setor == "Alimentos", "#FFFFFF", "#0F3A40"),
           alfa = ifelse(anel == "antes", 0.55, 1),
           rot = ifelse(pct >= 0.06, scales::percent(pct, accuracy = 1), ""))
  ggplot(d, aes(x = x, y = t, fill = setor, group = interaction(setor, anel))) +
    geom_col(aes(alpha = alfa), width = 0.95, color = "white", linewidth = 1.2) +
    geom_text(aes(label = rot, color = cor_txt, size = anel), position = position_stack(vjust = 0.5),
              fontface = "bold") +
    annotate("text", x = 0.4, y = 0,
             label = paste0("1861 - ", ano, "\n", curto(sum(agora$t)), " t"),
             size = 5.6, fontface = "bold", color = "#0F3A40", lineheight = 1) +
    coord_polar(theta = "y") +
    scale_x_continuous(limits = c(0.4, 3.6)) +
    scale_fill_manual(values = cores) +
    scale_alpha_identity() + scale_color_identity() +
    scale_size_manual(values = c(antes = 3.2, agora = 4.4), guide = "none") +
    theme_void(base_size = 14) +
    theme(legend.position = "bottom", legend.title = element_blank(),
          legend.text = element_text(size = 12, color = "#0F3A40"))
}

# texto embaixo da rosca
legenda_rosca <- function(ano) {
  agora <- volume_acum_ate(ano); antes <- volume_acum_ate(ano - 10)
  if (nrow(agora) == 0) return("")
  cres <- agora |> left_join(select(antes, setor, t_antes = t), by = "setor") |>
    mutate(t_antes = coalesce(t_antes, 0), ganho = t - t_antes)
  paste0("Anel de fora: acumulado até ", ano, "  ·  anel de dentro: até ", ano - 10, ".  Nos últimos 10 anos: ",
         paste0(cres$setor, " +", curto(cres$ganho), " t", collapse = "   ·   "), ".")
}

# ---------------------------------------------------------------------
# LINHA DO TEMPO: desde quando cada produto é registrado?
# ---------------------------------------------------------------------
# Cada barra vai do primeiro ao último ano com dado daquele produto.
grafico_linha_tempo <- function() {
  ggplot(resumo_produtos, aes(y = reorder(nome_pt(produto), -inicio), color = setor)) +
    geom_segment(aes(x = inicio, xend = fim, yend = nome_pt(produto)),
                 linewidth = 4, lineend = "round") +  # pontas arredondadas
    scale_color_manual(values = cores) +
    labs(x = NULL, y = NULL) +
    tema_mix +
    theme(panel.grid.major.y = element_blank(),     # sem linhas horizontais
          axis.text.y = element_text(size = 10))
}

# ---------------------------------------------------------------------
# CRESCIMENTO: quais produtos mais cresceram?
# ---------------------------------------------------------------------
# Barra = último valor ÷ primeiro valor. Linha tracejada em 1× = não mudou.
grafico_crescimento <- function() {
  ggplot(resumo_produtos, aes(x = crescimento, y = reorder(nome_pt(produto), crescimento), fill = setor)) +
    geom_vline(xintercept = 1, color = "#5F8087", linetype = "dashed") +
    geom_col(width = 0.7) +
    scale_x_log10(labels = function(x) paste0(prettyNum(x, big.mark = "."), "×")) +
    scale_fill_manual(values = cores) +
    labs(x = "Quantas vezes cresceu (escala log)", y = NULL) +
    tema_mix +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 10))
}


# =====================================================================
# PARTE 3 — GRÁFICOS DO DASHBOARD 2 (Perguntas)
# =====================================================================
# Cada gráfico é uma FUNÇÃO. Teste no console, ex.: grafico_sankey(fluxo_ano(1950))

# ---------------------------------------------------------------------
# FLUXO (Sankey): para onde foi a produção num ano
# ---------------------------------------------------------------------
# fluxo setor -> produtos em um ano: top 3 produtos de cada setor + "Outros" (toneladas aprox.)
fluxo_ano <- function(ano) {
  d <- dados |>
    filter(Year == ano, produto %in% names(fator_t)) |>
    mutate(t = producao * fator_t[produto]) |>
    group_by(setor) |>
    arrange(desc(t), .by_group = TRUE) |>
    mutate(rank = row_number(),
           grupo = ifelse(rank <= 3, produto, "Outros")) |>
    group_by(setor, grupo) |>
    summarise(t = sum(t), n = n(), .groups = "drop") |>
    mutate(outros = grupo == "Outros",
           rotulo = ifelse(outros, paste0("Outros (", n, ")"), nome_pt(grupo)))
  ordem_setor <- d |> group_by(setor) |> summarise(tt = sum(t)) |> arrange(desc(tt)) |> pull(setor)
  d |> mutate(setor = factor(setor, ordem_setor)) |>
    arrange(setor, outros, desc(t))
}

# diagrama de fluxo (Sankey) feito à mão com ggplot
grafico_sankey <- function(fl) {
  tot  <- sum(fl$t)
  nR   <- nrow(fl)
  gapR <- 0.03 * tot
  H    <- tot + gapR * (nR - 1)
  
  # nós da direita (produtos), de cima para baixo
  R <- fl |>
    mutate(h = t, hi = H - (cumsum(h) - h) - gapR * (row_number() - 1), lo = hi - h)
  
  # nós da esquerda (setores), mesma altura total
  L <- fl |> group_by(setor) |> summarise(h = sum(t), .groups = "drop")
  gapL <- if (nrow(L) > 1) (H - tot) / (nrow(L) - 1) else 0
  L <- L |> mutate(hi = H - (cumsum(h) - h) - gapL * (row_number() - 1), lo = hi - h)
  
  # fitas: segmento na esquerda (dentro do setor) -> nó da direita
  fitas <- R |>
    group_by(setor) |>
    mutate(off = cumsum(h) - h) |>
    ungroup() |>
    left_join(select(L, setor, Lhi = hi), by = "setor") |>
    mutate(l_hi = Lhi - off, l_lo = l_hi - h, r_hi = hi, r_lo = lo, id = row_number())
  
  tt  <- seq(0, 1, length.out = 40)
  suave <- (1 - cos(pi * tt)) / 2
  poli <- fitas |>
    rowwise() |>
    reframe(setor = setor, id = id,
            x = c(tt, rev(tt)),
            y = c(l_hi + (r_hi - l_hi) * suave, rev(l_lo + (r_lo - l_lo) * suave))) |>
    ungroup()
  
  # "partículas" brancas dentro das fitas (posições fixas, não tremem ao mudar o ano)
  set.seed(7)
  part <- fitas |>
    rowwise() |>
    reframe(setor = setor,
            t_ = runif(max(2, round(h / tot * 90))),
            u_ = runif(max(2, round(h / tot * 90))),
            l_hi = l_hi, l_lo = l_lo, r_hi = r_hi, r_lo = r_lo) |>
    mutate(s_ = (1 - cos(pi * t_)) / 2,
           top = l_hi + (r_hi - l_hi) * s_, bot = l_lo + (r_lo - l_lo) * s_,
           x = t_, y = bot + u_ * (top - bot))
  
  ggplot() +
    geom_polygon(data = poli, aes(x, y, group = id, fill = setor), alpha = 0.42) +
    geom_point(data = part, aes(x, y), color = "white", alpha = 0.75, size = 0.55) +
    geom_rect(data = L, aes(xmin = -0.025, xmax = 0, ymin = lo, ymax = hi, fill = setor)) +
    geom_rect(data = R, aes(xmin = 1, xmax = 1.025, ymin = lo, ymax = hi, fill = setor)) +
    geom_text(data = L, aes(x = -0.04, y = (lo + hi) / 2,
                            label = paste0(setor, "\n", curto(h), " t")),
              hjust = 1, color = "#0F3A40", size = 4, fontface = "bold", lineheight = 0.95) +
    geom_text(data = R, aes(x = 1.04, y = (lo + hi) / 2,
                            label = paste0(rotulo, "   ", curto(t), " t")),
              hjust = 0, color = "#5F8087", size = 3.5) +
    scale_fill_manual(values = cores) +
    scale_x_continuous(limits = c(-0.42, 1.68)) +
    theme_void() +
    theme(legend.position = "none", plot.margin = margin(4, 4, 4, 4))
}

# ---------------------------------------------------------------------
# TOP 3: os 3 produtos que mais produziram em cada setor num ano
# ---------------------------------------------------------------------
# top 3 produtos de cada setor num ano (ranking por volume; barra = valor / maior valor da história)
picos_tab <- dados |> group_by(produto) |> summarise(pico = max(producao), ano_pico = Year[which.max(producao)], .groups = "drop")
top_produtos <- function(ano) {
  picos <- picos_tab
  dados |>
    filter(Year == ano) |>
    filter(!(setor == "Transporte" & produto == "Ships_weight")) |>   # tonelagem não se compara com "nº de unidades"
    mutate(rank_val = ifelse(produto %in% names(fator_t), producao * fator_t[produto], producao)) |>
    group_by(setor) |>
    slice_max(rank_val, n = 3, with_ties = FALSE) |>
    ungroup() |>
    left_join(picos, by = "produto") |>
    mutate(pct = producao / pico,
           unidade = ifelse(produto %in% names(unidade_nativa), unidade_nativa[produto], "un."))
}

# ---------------------------------------------------------------------
# 1. QUE SETOR CRESCEU MAIS RÁPIDO? (índice base 100)
# ---------------------------------------------------------------------
# Linhas = índice de cada setor (todos começam em 100). Pontilhado = ano escolhido.
grafico_indice <- function(ano) {
  ic  <- indice_setores
  fim <- ic |> group_by(setor) |> slice_max(Year, n = 1, with_ties = FALSE) |> ungroup()
  ggplot(ic, aes(Year, indice, color = setor)) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "#8FB3BB") +
    geom_vline(xintercept = ano, linetype = "dotted", color = "#0F3A40", linewidth = 0.7) +
    geom_line(linewidth = 1.3) +
    geom_text(data = fim, aes(label = paste0("×", fmt_num(indice / 100, 1))),
              hjust = -0.2, fontface = "bold", size = 4.4, show.legend = FALSE) +
    scale_y_log10(labels = numero_curto) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.10))) +
    scale_color_manual(values = cores) +
    labs(x = NULL, y = "Índice (base 100, escala log)") +
    tema_mix
}

# frase embaixo do gráfico
texto_indice <- function() {
  r <- indice_setores |>
    group_by(setor) |>
    summarise(anos = max(Year) - min(Year), mult = last(indice) / 100, .groups = "drop") |>
    filter(anos > 0) |>
    mutate(cagr = mult^(1 / anos) - 1) |>
    arrange(desc(cagr))
  paste0("O setor que mais cresceu foi ", r$setor[1], ": ficou ", fmt_num(r$mult[1], 1),
         " vezes maior em ", r$anos[1], " anos.")
}

# ---------------------------------------------------------------------
# 2. QUAIS PRODUTOS SOBEM E DESCEM JUNTOS? (mapa de calor)
# ---------------------------------------------------------------------
# Verde escuro = sobem e descem juntos. Amarelo = quando um sobe, o outro desce.
grafico_calor <- function() {
  m <- correlacao$m; ord <- rownames(m)[correlacao$ordem]
  long <- as_tibble(m, rownames = "a") |>
    pivot_longer(-a, names_to = "b", values_to = "r") |>
    mutate(a = factor(a, ord), b = factor(b, ord))
  cor_eixo <- cores[setor_do_produto$setor[match(ord, setor_do_produto$produto)]]
  ggplot(long, aes(a, b, fill = r)) +
    geom_tile(color = "#FFFFFF", linewidth = 0.5) +
    scale_fill_gradient2(low = "#F7B538", mid = "#F3F8FA", high = "#1D5C63",
                         midpoint = 0, limits = c(-1, 1), na.value = "#E8F0F3",
                         name = "Correlação") +
    scale_x_discrete(labels = nome_pt) +
    scale_y_discrete(labels = nome_pt) +
    coord_fixed() +
    labs(x = NULL, y = NULL) +
    tema_mix +
    theme(panel.grid = element_blank(),
          legend.position = "right",
          legend.title = element_text(size = 11),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = cor_eixo),
          axis.text.y = element_text(size = 9, color = cor_eixo))
}

# frase embaixo do gráfico
texto_calor <- function() {
  par <- as_tibble(correlacao$m, rownames = "a") |>
    pivot_longer(-a, names_to = "b", values_to = "r") |>
    filter(a < b, !is.na(r))
  forte <- par |> slice_max(r, n = 1, with_ties = FALSE)
  paste0("Os produtos que mais andam juntos são ", nome_pt(forte$a), " e ", nome_pt(forte$b), ".")
}


# =====================================================================
# PARTE 4 — HTML DO PANORAMA (as caixas div)
# =====================================================================
# Aqui só se MONTA a estrutura (div, span). O visual de cada class
# está no CSS da PARTE 7.   Ex.: class = "bud-bar"  ->  .bud-bar { }

# ---------------------------------------------------------------------
# CARTÕES DE NÚMEROS (topo do Dashboard 1 e do Dashboard 2)
# ---------------------------------------------------------------------
# mini gráfico de linha (sparkline) em SVG para os cartões
spark_svg <- function(v, cor) {
  v <- v[!is.na(v)]
  if (length(v) < 2) return(NULL)
  w <- 120; h <- 36
  x <- seq(2, w - 2, length.out = length(v))
  r <- range(v)
  y <- if (diff(r) == 0) rep(h / 2, length(v)) else (h - 4) - (v - r[1]) / diff(r) * (h - 10)
  linha <- paste0(ifelse(seq_along(v) == 1, "M", "L"), round(x, 1), " ", round(y, 1), collapse = " ")
  area  <- paste0(linha, " L", round(tail(x, 1), 1), " ", h, " L", round(x[1], 1), " ", h, " Z")
  tag("svg", list(viewBox = paste(0, 0, w, h), width = w, height = h,
                  tag("path", list(d = area, fill = cor, `fill-opacity` = 0.16)),
                  tag("path", list(d = linha, fill = "none", stroke = cor, `stroke-width` = 2,
                                   `stroke-linejoin` = "round", `stroke-linecap` = "round")),
                  tag("circle", list(cx = round(tail(x, 1), 1), cy = round(tail(y, 1), 1), r = 3.4,
                                     fill = "#FFFFFF", stroke = cor, `stroke-width` = 2))))
}

todos_kpi <- bind_rows(
  volume_setor |> group_by(Year) |> summarise(t = sum(t), .groups = "drop") |> mutate(setor = "Total"),
  volume_setor)
montar_kpis <- function(ano, d1 = FALSE) {
  todos <- todos_kpi
  mk <- function(nome, titulo, cor, sub) {
    d <- todos |> filter(setor == nome) |> arrange(Year)
    atual <- d$t[d$Year == ano]; ant <- d$t[d$Year == ano - 1]
    hist  <- d |> filter(Year <= ano, Year > ano - 25) |> pull(t)
    chip <- NULL
    if (length(atual) && length(ant) && ant > 0) {
      v <- atual / ant - 1
      chip <- span(class = paste("chip", if (v >= 0) "chip-up" else "chip-down"),
                   paste0(if (v >= 0) "▲ " else "▼ ", fmt_num(abs(v) * 100, 1), "%"))
    }
    div(class = "kpi",
        div(class = "kpi-top", span(class = "kpi-dot", style = paste0("background:", cor)), paste0(titulo, " · ", ano)),
        div(class = "kpi-val", if (length(atual)) paste0(curto(atual), " t") else "—"),
        div(class = "kpi-row",
            tags$small(sub),
            spark_svg(hist, cor)))
  }
  np <- function(st) length(unique(dados$produto[dados$setor == st]))
  div(class = "d2-kpis",
      mk("Total",      "Volume total",  "#0F3A40",             paste(np("Alimentos") + np("Têxteis") + np("Transporte"), "produtos")),
      mk("Alimentos",  "Alimentos",     cores[["Alimentos"]],  paste(np("Alimentos"),  "produtos")),
      mk("Têxteis",    "Têxteis",       cores[["Têxteis"]],    paste(np("Têxteis"),    "produtos")),
      mk("Transporte", "Transporte",    cores[["Transporte"]], paste(np("Transporte"), "produtos")))
}

# ---------------------------------------------------------------------
# TOP 3 de cada setor (Dashboard 2). tp = tabela do top_produtos(ano)
# ---------------------------------------------------------------------
html_top3 <- function(tp) {
  if (nrow(tp) == 0) return(p(class = "por-que", "Sem dados neste ano."))
  blocos <- lapply(c("Alimentos", "Têxteis", "Transporte"), function(st) {
    d <- tp |> filter(setor == st)
    if (nrow(d) == 0) return(NULL)
    cor <- cores[[st]]
    tagList(
      div(class = "bud-sec", span(class = "kpi-dot", style = paste0("background:", cor)), st),
      lapply(seq_len(nrow(d)), function(i) {
        r <- d[i, ]
        record <- r$pct >= 0.97
        fundo <- if (record) {
          paste0("repeating-linear-gradient(135deg,", cor, ",", cor, " 6px, rgba(255,255,255,.28) 6px, rgba(255,255,255,.28) 10px)")
        } else cor
        div(class = "bud-row",
            div(class = "bud-line",
                span(class = "bud-name", nome_pt(r$produto),
                     if (record) span(class = "bud-badge", "RECORDE")),
                span(class = "bud-val",
                     paste0(curto(r$producao), " de ", curto(r$pico), " ", r$unidade))),
            div(class = "bud-bar",
                div(class = "bud-fill", style = paste0("width:", round(min(r$pct, 1) * 100), "%;background:", fundo))))
      }))
  })
  div(blocos)
}


# =====================================================================
# PARTE 5 — TELA (UI): onde cada gráfico aparece
# =====================================================================
# plotOutput("xxx") = espaço para o gráfico que o server manda em output$xxx

# ---------------------------------------------------------------------
# Página Panorama: os 2 botões + o Dashboard 1
# ---------------------------------------------------------------------
ui_panorama <- function() {
  pagina("home", "Panorama", "Indústria italiana, 1861–1985",
         tags$style(HTML(css_panorama)),   # <- o CSS da PARTE 7 (fim do arquivo)
         div(class = "sub-nav",
             radioButtons("sub_home", NULL, inline = TRUE, selected = "d1",
                          choices = c("Dashboard 1 · Visão geral" = "d1",
                                      "Dashboard 2 · Perguntas" = "d2"))),
         conditionalPanel("input.sub_home == 'd1'",
                          uiOutput("d1_kpis"),
                          layout_columns(
                            col_widths = c(8, 4),
                            card(card_header("Produção de cada setor ao longo do tempo (streamgraph)"),
                                 radioButtons("stream_modo", NULL, inline = TRUE, selected = "ton",
                                              choices = c("Toneladas" = "ton", "Participação de cada setor (%)" = "pct")),
                                 plotOutput("stream_home", height = "400px"),
                                 textOutput("leg_stream"),
                                 sliderInput("ano_home", NULL, min = 1871, max = 1985, value = 1950,
                                             step = 1, sep = "", width = "100%")),
                            card(card_header("Quanto cada setor já produziu (acumulado)"),
                                 plotOutput("pizza_home", height = "400px"),
                                 textOutput("leg_pizza"))
                          ),
                          layout_columns(
                            card(card_header("Desde quando cada produto é registrado?"),
                                 plotOutput("grafico_linha_tempo", height = "440px")),
                            card(card_header("Quais produtos mais cresceram? (último ÷ primeiro ano)"),
                                 plotOutput("grafico_crescimento", height = "440px"))
                          )
         ),
         conditionalPanel("input.sub_home == 'd2'", ui_dash2())   # <- logo abaixo
  )
}

# ---------------------------------------------------------------------
# Dashboard 2 (aparece quando clica no botão Dashboard 2)
# ---------------------------------------------------------------------
ui_dash2 <- function() {
  por_que <- function(...) p(class = "por-que", ...)
  div(class = "d2",
      # ---- topo: título + linha do tempo (slider com play) ----
      div(class = "d2-top",
          div(h3("Fluxo da produção"),
              p("Indústria italiana · por setor e produto · arraste o ano")),
          div(class = "d2-slider",
              sliderInput("ano_d2", NULL, min = 1862, max = 1985, value = 1950, step = 1,
                          sep = "", width = "100%"))),
      # ---- cartões: total + 3 setores ----
      uiOutput("d2_kpis"),
      # ---- fluxo (Sankey) + top 3 de cada setor ----
      layout_columns(
        col_widths = c(8, 4),
        card(card_header("Para onde foi a produção"),
             por_que("De onde veio a produção no ano escolhido. Cada setor se divide nos seus produtos principais; quanto mais larga a faixa, mais foi produzido."),
             plotOutput("d2_sankey", height = "430px")),
        card(card_header("Top 3 de cada setor"),
             por_que("Os 3 produtos que mais produziram em cada setor. A barra mostra quanto falta para o recorde do próprio produto."),
             uiOutput("d2_top"))
      ),
      # ---- perguntas ----
      card(card_header("1. Que setor cresceu mais rápido?"),
           por_que("Todos os setores começam em 100, para poder comparar. Quanto mais alta a linha, mais o setor cresceu. A linha pontilhada é o ano escolhido."),
           plotOutput("d2_crescimento", height = "360px"),
           textOutput("d2_crescimento_txt")),
      card(card_header("2. Quais produtos sobem e descem juntos?"),
           por_que("Cada quadradinho compara dois produtos. Verde escuro: sobem e descem juntos. Amarelo: quando um sobe, o outro desce."),
           plotOutput("d2_calor", height = "680px"),
           textOutput("d2_calor_txt"))
  )
}


# =====================================================================
# PARTE 6 — SERVER: liga a tela aos gráficos
# =====================================================================
# Só CHAMA as funções das PARTES 2, 3 e 4. Cada output = 1 gráfico.
# bindCache(...) = guarda o gráfico pronto; se o ano repetir, não desenha de novo.
server_panorama <- function(input, output, session) {

  # ---- Dashboard 1 (usa o slider input$ano_home) ----
  output$d1_kpis             <- renderUI(montar_kpis(input$ano_home))

  output$stream_home         <- renderPlot(bg = "transparent",
                                  grafico_stream(todos_setores, periodo_total, input$ano_home, input$stream_modo)
                                ) |> bindCache(input$ano_home, input$stream_modo)
  output$leg_stream          <- renderText(legenda_stream(input$stream_modo))

  output$pizza_home          <- renderPlot(bg = "transparent", grafico_rosca(input$ano_home)) |> bindCache(input$ano_home)
  output$leg_pizza           <- renderText(legenda_rosca(input$ano_home))

  output$grafico_linha_tempo <- renderPlot(grafico_linha_tempo()) |> bindCache("v1")
  output$grafico_crescimento <- renderPlot(grafico_crescimento()) |> bindCache("v1")

  # ---- Dashboard 2 (usa o slider input$ano_d2) ----
  output$d2_kpis             <- renderUI(montar_kpis(input$ano_d2))

  output$d2_sankey           <- renderPlot(bg = "transparent", grafico_sankey(fluxo_ano(input$ano_d2))) |> bindCache(input$ano_d2)
  output$d2_top              <- renderUI(html_top3(top_produtos(input$ano_d2)))

  output$d2_crescimento      <- renderPlot(bg = "transparent", grafico_indice(input$ano_d2)) |> bindCache(input$ano_d2)
  output$d2_crescimento_txt  <- renderText(texto_indice())

  output$d2_calor            <- renderPlot(bg = "transparent", grafico_calor()) |> bindCache("v1")
  output$d2_calor_txt        <- renderText(texto_calor())
}


# =====================================================================
# PARTE 7 — CSS DO PANORAMA (o visual)
# =====================================================================
# Tudo o que está ENTRE AS ASPAS abaixo é CSS, não é R.
# Ligação R <-> CSS pelo nome da classe:
#   no R (lá em cima):  div(class = "kpi-val", ...)
#   no CSS (aqui):      .kpi-val { ... }
# O ui_panorama() coloca este texto na página com tags$style(HTML(css_panorama)).
# ---------------------------------------------------------------------
css_panorama <- "
  /* botão superior do Panorama (Dashboard 1 / Dashboard 2) */
  .sub-nav { margin: -6px 0 22px 0; }
  .sub-nav .shiny-options-group { display: flex; gap: 8px; flex-wrap: wrap; }
  .sub-nav .form-check, .sub-nav .radio-inline { padding: 0; margin: 0; }
  .sub-nav input { position: absolute; opacity: 0; pointer-events: none; }
  .sub-nav label { margin: 0; cursor: pointer; }
  .sub-nav label span {
    display: inline-block; padding: 8px 18px; border-radius: 999px;
    border: 1px solid #CBDDE5; background: #fff; color: #7FB2BD;
    font-size: 13px; font-weight: 600; transition: all .2s;
  }
  .sub-nav label:hover span { background: #D6E8EE; }
  .sub-nav input:checked + span {
    background: #0F3A40; border-color: #0F3A40; color: #fff;
    box-shadow: 0 6px 14px rgba(15, 58, 64, .18);
  }
  .por-que { color: #5F8087; font-size: 13px; margin: 0 0 10px 0; }


  /* ---------- DASHBOARD 2: mesmo visual claro do Dashboard 1 ---------- */
  .d2-top { display: flex; justify-content: space-between; align-items: center;
            gap: 20px; flex-wrap: wrap; margin-bottom: 14px; }
  .d2-top h3 { margin: 0; font-size: 24px; font-weight: 700; color: #0F3A40; }
  .d2-top p  { margin: 2px 0 0 0; color: #5F8087; font-size: 13px; }
  .d2-slider { flex: 0 1 470px; min-width: 280px; }
  .d2 .form-group { margin-bottom: 0; }
  .d2 .irs--shiny .irs-bar { background: #F7B538; border-color: #F7B538; }
  .d2 .irs--shiny .irs-handle { background: #fff; border: 3px solid #F7B538; }
  .d2 .irs--shiny .irs-single, .d2 .irs--shiny .irs-from, .d2 .irs--shiny .irs-to { background: #F7B538; color: #0F3A40; font-weight: 700; }

  /* cartões de valor com mini gráfico */
  .d2-kpis { display: grid; grid-template-columns: repeat(4, 1fr); gap: 14px; margin-bottom: 16px; }
  .kpi { background: #fff; border: 1px solid #CFE2EA; border-radius: 20px; padding: 16px 18px;
         box-shadow: 0 10px 40px rgba(29, 92, 99, .10); }
  .kpi-top { display: flex; align-items: center; gap: 8px; color: #5F8087; font-size: 13px; font-weight: 600; }
  .kpi-dot { width: 10px; height: 10px; border-radius: 50%; }
  .kpi-val { font-size: 28px; font-weight: 700; margin: 8px 0 6px; color: #0F3A40; letter-spacing: -.5px; }
  .kpi-row { display: flex; justify-content: space-between; align-items: flex-end; gap: 8px; }
  .kpi small { display: block; color: #5F8087; font-size: 12px; margin-top: 4px; }
  .chip { display: inline-block; border-radius: 8px; padding: 2px 8px; font-size: 12px; font-weight: 600; }
  .chip-up   { background: rgba(29,92,99,.12); color: #1D5C63; }
  .chip-down { background: rgba(214,69,65,.12); color: #C0443F; }
  @media (max-width: 900px) { .d2-kpis { grid-template-columns: repeat(2, 1fr); } }

  /* Budgets: top 3 de cada setor */
  .bud-sec { display: flex; align-items: center; gap: 8px; margin: 14px 0 8px 0;
             font-weight: 700; font-size: 13px; color: #0F3A40; }
  .bud-sec:first-child { margin-top: 2px; }
  .bud-row { margin-bottom: 11px; }
  .bud-line { display: flex; justify-content: space-between; align-items: baseline; gap: 8px; font-size: 13px; }
  .bud-name { font-weight: 600; color: #0F3A40; }
  .bud-val  { color: #5F8087; font-size: 12px; white-space: nowrap; }
  .bud-bar  { height: 8px; border-radius: 99px; background: #E3EFF4; overflow: hidden; margin-top: 5px; }
  .bud-fill { height: 100%; border-radius: 99px; }
  .bud-badge { font-size: 10px; font-weight: 700; margin-left: 6px; padding: 1px 6px;
               border-radius: 6px; background: rgba(247,181,56,.28); color: #8A5A00; }
"
