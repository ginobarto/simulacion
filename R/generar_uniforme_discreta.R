#' Generar variable aleatoria uniforme discreta
#' 
#' Esta función genera una variable aleatoria
#' uniforme discreta con igual probabilidad 
#' en el intervalo \[a,b\].
#'
#' @param a Límite inferior.
#' @param b Límite superior.
#'
#' @returns Número aleatorio entero entre `a` y `b`.
#'
#' @examples
#' # tirar un dado legal:
#' generar_uniforme_discreta(1,6)
#' 
#' @export
generar_uniforme_discreta = function(a, b) {
  if (a %% 1 != 0) stop("(a) debe ser un entero.")
  if (b %% 1 != 0) stop("(b) debe ser un entero.")
  if (b <= a) stop("(a) debe ser menor que (b).")
  u = runif(1)
  x = floor((b-a+1) * u) + a
  return(x)
}