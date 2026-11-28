# Funções auxiliares de formatação (padrão brasileiro)

fmt_br <- function(x) {
  ifelse(
    is.na(x) | !is.finite(x),
    "NA",
    formatC(x, format = "f", digits = 2, big.mark = ".", decimal.mark = ",")
  )
}

fmt_int_br <- function(x) {
  ifelse(
    is.na(x) | !is.finite(x),
    "NA",
    formatC(round(x), format = "f", digits = 0, big.mark = ".", decimal.mark = ",")
  )
}
