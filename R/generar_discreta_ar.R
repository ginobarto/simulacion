#' Generar variable aleatoria discreta
#' 
#' Esta función genera una variable aleatoria discreta X 
#' tabulada usando el método de aceptación/rechazo.
#'
#' @param x Valores posibles de X.
#' @param px Función de probabilidad p(x) en el mismo orden que `x`.
#'
#' @returns Número aleatorio de la distribución especificada.
#' 
#' @details Esta función no requiere especificar la ley
#' de probabilidad q(y) de la variable auxiliar Y porque 
#' siempre utiliza la distribución uniforme discreta 
#' para generar posibles valores de X.
#'
#' @examples
#' generar_discreta_ar(x = 1:4, px = c(0.50,0.30,0.15,0.05))
#' 
#' @export
generar_discreta_ar = function(x, px) {
  stopifnot(length(x) == length(unique(x)))
  stopifnot(length(x) == length(px))
  stopifnot(sum(px) == 1)
  
  # calcular constantes
  l = length(x)
  c = l * max(px) # c = max(pi/qi), con qi = 1/l
  # algoritmo de aceptacion/rechazo
  while (TRUE) {
    # 1. generar uniforme discreta en [1,l]
    y = generar_uniforme_discreta(1,l)
    # 2. generar uniforme en (0,1)
    u = runif(1)
    # 3. aceptar valor o rechazarlo: u < p(y)/(c * q(y))
    if (u < l * px[y] / c) return(x[y])
  }
}