# =====================================================================
# PÁGINA INTEGRANTES
# =====================================================================
#
# COMO COLOCAR A FOTO DE ALGUÉM:
#   1. Salve a foto na pasta  www/fotos/  (crie a pasta se não existir)
#   2. Use o nome que está na coluna `foto` abaixo  (ex.: www/fotos/shirley.jpg)
#   3. Rode o app de novo. Enquanto não houver foto, aparece um círculo
#      com a inicial do nome.
#   Dica: foto quadrada (ex.: 400 x 400 px) fica melhor no círculo.
# =====================================================================

integrantes <- tribble(
  ~nome,        ~curso,                    ~foto,
  "Shirley",    "Estudante de Estatística", "fotos/shirley.jpg",
  "Emmanuelly", "Estudante de Estatística", "fotos/emmanuelly.jpg",
  "Emily",      "Estudante de Estatística", "fotos/emily.jpg",
  "Kelly",      "Estudante de Estatística", "fotos/kelly.jpg",
  "Eduardo",    "Estudante de Estatística", "fotos/eduardo.jpg"
)

# um cartão por pessoa: foto (ou inicial) + nome + curso
cartao_integrante <- function(nome, curso, foto) {
  tem_foto <- file.exists(file.path("www", foto))

  card(
    class = "cartao-integrante",
    if (tem_foto) {
      img(src = foto, alt = paste("Foto de", nome), class = "foto-integrante")
    } else {
      div(class = "foto-integrante sem-foto", substr(nome, 1, 1))
    },
    h4(nome),
    p(class = "curso-integrante", curso)
  )
}

css_integrantes <- "
  .cartao-integrante { text-align: center; padding: 24px 12px; }
  .foto-integrante {
    width: 120px; height: 120px; margin: 0 auto 14px auto;
    border-radius: 50%; object-fit: cover;
    border: 4px solid #CFE2EA;
    box-shadow: 0 6px 16px rgba(15, 58, 64, .15);
  }
  .sem-foto {
    display: flex; align-items: center; justify-content: center;
    background: #1D5C63; color: #fff; font-size: 48px; font-weight: 700;
  }
  .cartao-integrante h4 { margin: 0; font-weight: 700; color: #0F3A40; }
  .curso-integrante { margin: 4px 0 0 0; color: #5F8087; font-size: 14px; }
"

# ---- TELA ----
ui_integrantes <- function() {
  pagina("integrantes", "Integrantes", "Quem fez este projeto",
    tags$style(HTML(css_integrantes)),
    layout_column_wrap(
      width = "200px",
      !!!pmap(integrantes, cartao_integrante)
    )
  )
}

# ---- SERVER ----
server_integrantes <- function(input, output, session) {
  # (não tem nada para calcular)
}
