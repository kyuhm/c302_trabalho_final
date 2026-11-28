# Constantes usadas no app e no script de preparação dos dados

# Variáveis nas colunas do csv, usadas para renomear
variaveis_prod <- c(
  "Açúcar", "Glicose", "Substituto_café", "Oleo_de_sementes",
  "Etanol_1", "Etanol_2", "Cerveja"
)

# Colunas de estatísticas descritivas
cols_estat <- c("soma", "media", "dp", "min", "q1", "mediana", "q3", "max")

# Paleta de cores
paleta_produtos <- c(
  "Açúcar"           = "#1b9e77",
  "Glicose"          = "#d95f02",
  "Substituto_café"  = "#7570b3",
  "Oleo_de_sementes" = "#e7298a",
  "Etanol_1"         = "#66a61e",
  "Etanol_2"         = "#e6ab02",
  "Cerveja"          = "#1f78b4"
)

# Traduzir a library DT
options(DT.options = list(
  language = list(
    decimal = ",",
    thousands = ".",
    lengthMenu = "Mostrar _MENU_ registros por página",
    zeroRecords = "Nenhum registro encontrado",
    info = "Mostrando de _START_ até _END_ de _TOTAL_ registros",
    infoEmpty = "Mostrando 0 até 0 de 0 registros",
    infoFiltered = "(filtrado de _MAX_ registros no total)",
    search = "Buscar:",
    paginate = list(
      first = "Primeiro",
      last = "Último",
      `next` = "Próximo",
      previous = "Anterior"
    )
  )
))
