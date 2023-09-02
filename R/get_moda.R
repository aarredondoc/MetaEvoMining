#' @title Encontrar la Moda de una Serie de Números
#'
#' @description Esta función lee una serie de números en forma de vector y
#' encuentra el elemento que mas se repite, es decir la moda.
#'
#' @param v Es una serie de números en forma de  un vector simple de r.
#'
#' @details si tu vector de entrada puede ser interpretado alternando números y
#' letras escritas entre comillas "". Si un vector esta vacío, dará como
#' resultado un NULL.
#' @return El carácter con mas frecuencia de el vector de entrada.
#' @noRd
#'
#' @examples
#' serie_números <- c(1, 2, 2, 2, 2, 3, 3, 4, 4, 4)
#' resultado <- getmode(serie_números)
#' print(resultado)


# encontrar la moda de una columna----------------------------------------####
getmoda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
