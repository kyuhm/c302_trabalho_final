# =====================================================================
# R/04_css.R — o visual (CSS) GERAL do app (o do Panorama está em R/10_panorama.R)
# (veio do grafico_test.R)
# =====================================================================

css <- r"---(
  html, body { height: 100%; margin: 0; }

  body {
    background:
      radial-gradient(circle at 5% 0%,    #CFE5EC 0%, transparent 40%),
      radial-gradient(circle at 100% 100%, #FBE7BC 0%, transparent 36%),
      #E3EFF4;
    font-family: 'Inter', 'Segoe UI', system-ui, sans-serif;
    color: #0F3A40;
  }
  .container-fluid { padding: 0; }

  /* LAYOUT: [menu de ícones] [conteúdo] — lado a lado */
  .layout {
    display: flex;
    align-items: center;
    gap: 14px;
    height: calc(100vh - 48px);
    margin: 24px;
  }

  /* 1) MENU DE ÍCONES: pílula branca flutuante */
  .sidebar-icons {
    flex: 0 0 58px;
    width: 58px;
    padding: 12px 0;
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 8px;
    background: #fff;
    border: 1px solid #CBDDE5;
    border-radius: 30px;
    box-shadow: 0 10px 30px rgba(29, 92, 99, .12);
  }
  .sb-btn {
    width: 42px; height: 42px;
    display: flex; align-items: center; justify-content: center;
    border-radius: 50%;
    color: #6B8F96 !important;
    font-size: 16px;
    text-decoration: none !important;
    transition: background .2s, color .2s, transform .15s;
  }
  .sb-btn:hover { background: #D6E8EE; transform: scale(1.07); }

  /* item ativo: centro branco + anel metade verde, metade vermelho */
  .sb-btn.active {
    color: #1D5C63 !important;
    background:
      radial-gradient(circle closest-side, #fff calc(100% - 3px), transparent calc(100% - 3px)),
      conic-gradient(from 45deg, #1D5C63 0 50%, #F7B538 50% 100%);
    box-shadow: 0 6px 14px rgba(15, 58, 64, .18);
  }


  /* 3) CARTÃO DE CONTEÚDO */
  .content {
    flex: 1;
    min-width: 0;                 /* deixa os gráficos encolherem */
    height: 100%;
    box-sizing: border-box;
    background: #fff;
    border-radius: 26px;
    box-shadow: 0 10px 40px rgba(29, 92, 99, .10);
    padding: 32px 40px;
    overflow-y: auto;
  }
  .content h2 {
    margin: 0;
    font-weight: 700;
    font-size: 26px;
    letter-spacing: -.3px;
  }
  .subtitulo { margin: 4px 0 0 0; color: #5F8087; font-size: 14px; }

  /* título com a bandeira da Itália ao lado */
  .titulo-linha { display: flex; align-items: center; gap: 16px; }
  .bandeira-it { flex: 0 0 auto; border-radius: 4px; box-shadow: 0 4px 12px rgba(15, 58, 64, .18); }
  .flag-gap { height: 22px; }

  /* ---------- CARDS DOS GRÁFICOS ---------- */
  .card {
    border: 1px solid #CFE2EA;
    border-radius: 20px;
    box-shadow: 0 4px 14px rgba(29, 92, 99, .06);
    margin-bottom: 20px;
    overflow: hidden;                       /* gráfico respeita os cantos redondos */
    transition: box-shadow .2s, transform .2s;
  }
  /* passar o mouse: o card de gráfico ganha mais sombra */
  .card:not(.bslib-value-box):hover {
    box-shadow: 0 12px 30px rgba(29, 92, 99, .16);
    transform: translateY(-2px);
  }

  /* título do card com um tracinho verde/vermelho na frente */
  .card-header {
    background: #fff;
    border-bottom: 1px solid #CFE2EA;
    font-weight: 600;
    font-size: 15px;
    padding: 16px 22px;
  }
  .card-header::before {
    content: "";
    display: inline-block;
    width: 4px;
    height: 16px;
    margin-right: 10px;
    vertical-align: -2px;
    border-radius: 2px;
    background: linear-gradient(#1D5C63 50%, #F7B538 50%);
  }
  .card-body { padding: 14px 18px; }

  /* imagem do gráfico com cantos arredondados */
  .shiny-plot-output img { border-radius: 12px; }

  /* enquanto o gráfico recalcula (mexeu no filtro): fica clarinho, suave */
  .recalculating { opacity: .45; transition: opacity .3s; }

  /* caixas de valor (números) */
  .bslib-value-box { border: none; transition: transform .15s; }
  .bslib-value-box:hover { transform: translateY(-3px); }
  .value-box-title {
    font-size: 12px !important;
    text-transform: uppercase;
    letter-spacing: .08em;
    opacity: .85;
  }
  .value-box-value { font-size: 2.2rem !important; font-weight: 700; }
  .bslib-value-box .shiny-text-output:not(.value-box-value) {
    font-size: 12px; opacity: .9; overflow-wrap: anywhere;
  }

  .card-body .shiny-text-output { color: #5F8087; font-size: 12px; margin-top: 6px; }



  /* tabela da página Análise */
  .content table { width: 100%; font-size: 14px; }
  .content th { color: #5F8087; text-transform: uppercase; font-size: 12px; }

  /* tela média (ex.: painel Viewer do RStudio): layout mais compacto */
  @media (max-width: 1100px) {
    .layout { margin: 12px; gap: 10px; height: calc(100vh - 24px); }
    .content { padding: 24px; }
  }

  /* só no celular (bem estreito): um embaixo do outro */
  @media (max-width: 600px) {
    .layout { flex-direction: column; height: auto; }
    .sidebar-icons { flex-direction: row; width: auto; padding: 0 12px; }
    .content { width: 100%; height: auto; }
  }
)---"

# coluna de filtros à direita (só aparece se usar filtros = ... no layout_template)
css <- paste(css, "
  .filtros { flex: 0 0 230px; align-self: stretch; box-sizing: border-box;
             background: #fff; border-radius: 26px; padding: 28px 22px;
             box-shadow: 0 10px 40px rgba(15, 58, 64, .08); overflow-y: auto; }
  .filtros h4 { margin: 0 0 18px 0; font-size: 15px; font-weight: 700; }
")
