
# Función para extraer el número
#' Title
#'
#' @param nombre nombre de familia
#'
#' @return numero de familia
#' @noRd
#'
#'
extraer_numero <- function(nombre) {
  numero <- unlist(regmatches(nombre, gregexpr("\\|(\\d+)\\|", nombre)))
  return(numero)
}
