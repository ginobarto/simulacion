#' Calcular integral definida en el \[0,1\]
#' 
#' Esta funcion utiliza un metodo de simulacion
#' por Monte Carlo para calcular la integral definida
#' en el rango \[0,1\] de una funcion `g`
#' generando `n` variables aleatorias uniformes.
#'
#' @param g funcion definida en el \[0,1\]
#' @param n numero de uniformes generadas
#'
#' @returns una lista con el valor estimado de la integral y su error asociado.
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