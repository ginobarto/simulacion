#' Generar numeros aleatorios
#' 
#' Esta funcion genera una secuencia rectangular de numeros aleatorios
#' a partir de un generador congruencial lineal:
#' \deqn{x_n = (a \cdot x_{n-1} + c) \text{ mod } m}
#'
#' @param n Largo de la secuencia.
#' @param x0 Valor inicial o semilla.
#' @param a Multiplicador.
#' @param c Constante aditiva.
#' @param m Modulo.
#'
#' @returns Secuencia de numeros aleatorios en el intervalo \[0,1\] de largo `n`.
#'
#' @examples
#' generar_numeros_aleatorios(n = 10, x0 = 5, a = 3, c = 0, m = 150)
#' 
#' @export
generar_numeros_aleatorios = function(n, x0, a, c, m) {
  x = numeric(n)
  for (i in seq_len(n)) {
    x[i] = (a * x0 + c) %% m
    x0 = x[i]
  }
  u = x / m
  return(u)
}