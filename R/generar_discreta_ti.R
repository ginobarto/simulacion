#' Generar variable aleatoria discreta
#' 
#' Esta función genera una variable aleatoria discreta X
#' tabulada usando el método de la transformada inversa.
#'
#' @param x Valores posibles de X.
#' @param px Función de probabilidad p(x) en el mismo orden que `x`.
#'
#' @returns Número aleatorio de la distribución especificada.
#'
#' @examples
#' generar_discreta_ti(x = 1:4, px = c(0.50,0.30,0.15,0.05))
#' 
#' @export
generar_discreta_ti = function(x, px) {
  stopifnot(length(x) == length(unique(x)))
  stopifnot(length(x) == length(px))
  stopifnot(sum(px) == 1)
  # algoritmo de la transformada inversa
  # X = xi / F(xi-1) < U < F(xi)
  Fx = cumsum(px)
  u = runif(1)
  for (i in 1:length(x)) {
    if (u < Fx[i]) return(x[i])
  }
}