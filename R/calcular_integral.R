#' Calcular integral definida en el \[0,1\] por Monte Carlo
#' 
#' Esta funcion calcula la integral definida
#' en el rango \[0,1\] de una funcion `g`
#' simulando `n` variables aleatorias U(0,1).
#'
#' @param g Funcion definida en el \[0,1\].
#' @param n Numero de simulaciones.
#'
#' @returns Una lista con el valor `estimado` de la integral y su `error` asociado.
#'
#' @examples
#' calcular_integral(g = \(x) exp(x))
#' 
#' @export
calcular_integral = function(g, n = 1e3) {
  u = runif(n)
  gu = g(u)
  estimado = mean(gu)
  ee = sqrt(var(gu)/n)
  return(list("estimado" = estimado, "error" = ee))
}