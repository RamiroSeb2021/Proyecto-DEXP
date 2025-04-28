# -------------------------------
# PARTE 1: ASIGNACIÓN SIN COSTOS
# -------------------------------

#' Asignación proporcional de réplicas sin considerar costos ni tamaño de muestra
#'
#' Esta función calcula un esquema de asignación proporcional de réplicas por tratamiento, 
#' basado en las desviaciones estándar estimadas de cada tratamiento. 
#' El objetivo es redistribuir un número total fijo de observaciones iniciales, sin considerar costos individuales
#' ni un tamaño de muestra objetivo distinto al dado por \code{r0} y \code{a}.
#'
#' @param a Número de tratamientos.
#' @param r0 Número de réplicas iniciales por tratamiento (antes de ajustar proporcionalmente).
#' @param sigmas Vector de desviaciones estándar estimadas para cada tratamiento. Su longitud debe ser igual a \code{a}.
#'
#' @return
#' Un vector numérico de longitud \code{a} que indica el número de réplicas ajustadas proporcionalmente para cada tratamiento,
#' redondeadas al número entero más cercano.
#'
#' @details
#' Se calcula el número total de observaciones como \code{n = r0 * a}.
#' Luego, se asigna a cada tratamiento una cantidad de réplicas proporcional a su desviación estándar:
#' \deqn{r_i = \mathrm{round}\left(\frac{n \times \sigma_i}{\sum \sigma_i}\right)}
#' donde \eqn{\sigma_i} es la desviación estándar del tratamiento \eqn{i}.
#'
#' @examples
#' sigmas <- c(6.27, 9.57, 12, 3.32)
#' proporcionalidad_sin_costo_ni_tamaño_de_muestra(a = 4, r0 = 5, sigmas = sigmas)
#'
#' @export
proporcionalidad_sin_costo_ni_tamaño_de_muestra <- function(a, r0, sigmas) {
  if (length(sigmas) != a) {
    stop("La cantidad de tratamientos no coincide con la longitud del vector de desviaciones.")
  }
  n <- r0 * a
  r_prop <- round((n * sigmas) / sum(sigmas))
  return(r_prop)
}

#' Asignación proporcional de réplicas considerando costos y sin un tamaño de muestra fijo
#'
#' Esta función calcula un esquema de asignación proporcional de réplicas por tratamiento,
#' considerando las desviaciones estándar estimadas de los tratamientos y los costos unitarios
#' de observación de cada tratamiento. Se ajusta a un presupuesto total \code{costo_total} disponible.
#'
#' @param a Número de tratamientos.
#' @param sigmas Vector de desviaciones estándar estimadas para cada tratamiento.
#' @param costos Vector de costos unitarios por tratamiento (de la misma longitud que \code{sigmas}).
#' @param costo_total Presupuesto total disponible para realizar todas las observaciones.
#'
#' @return
#' Un vector numérico de longitud \code{a} que indica el número de réplicas ajustadas proporcionalmente para cada tratamiento,
#' redondeadas al número entero más cercano.
#'
#' @details
#' Se verifica que el número de tratamientos coincida con las longitudes de los vectores \code{sigmas} y \code{costos}.
#' El cálculo sigue la siguiente estructura:
#' \itemize{
#'   \item Se define \eqn{\lambda = 1/a}.
#'   \item Se calcula \eqn{\phi} como:
#'     \deqn{\phi = \left( \sum \lambda \sigma_i \sqrt{c_i} \right)^2 / (costo\_total)^2}
#'   \item El número de réplicas para cada tratamiento se estima como:
#'     \deqn{r_i = \mathrm{round}\left( \frac{\lambda \sigma_i}{\sqrt{\phi c_i}} \right)}
#' }
#'
#' @examples
#' costos <- c(1000, 200, 700, 1100)
#' sigmas <- c(6.27, 9.57, 12, 3.32)
#' costo_total <- 50000
#' proporcionalidad_con_costo_ni_tamaño_de_muestra(a = 4, sigmas = sigmas, costos = costos, costo_total = costo_total)
#'
#' @export
proporcionalidad_con_costo_ni_tamaño_de_muestra <- function(a, sigmas, costos, costo_total) {
  if (length(sigmas) != a) {
    stop("La cantidad de tratamientos no coincide con la longitud del vector de desviaciones.")
  }
  if (length(costos) != a) {
    stop("La cantidad de tratamientos no coincide con la longitud del vector de costos.")
  }
  
  lambda <- 1 / a
  phi <- (sum(lambda * sigmas * sqrt(costos))^2) / (costo_total^2)
  r_prop <- round(lambda * sigmas / sqrt(phi * costos))
  
  return(r_prop)
}

# ----------------------------------------
# PARTE 2: ASIGNACIÓN CON COSTOS (ÓPTIMA)
# ----------------------------------------

#' Cálculo del número de tratamientos y réplicas bajo modelo de efectos aleatorios
#'
#' Esta función estima el número óptimo de tratamientos y el número de réplicas por tratamiento
#' en un diseño experimental considerando un modelo de efectos aleatorios. Se toma en cuenta
#' el costo de cada tratamiento, el costo por unidad experimental, la varianza del error, la proporción
#' de la varianza atribuible a los efectos aleatorios (\code{rho}) y un valor máximo permitido de varianza relativa (\code{v_max}).
#'
#' @param costo_tratamiento Costo unitario de cada tratamiento (\code{C1}).
#' @param costo_ue Costo unitario por cada unidad experimental (\code{C2}).
#' @param sigma_cuadrado Varianza de los errores (\code{\sigma^2}).
#' @param rho Proporción de la varianza total atribuida a los efectos aleatorios, es decir,
#' \code{rho = \sigma^2_{\tau} / \sigma^2}.
#' @param v_max Valor máximo permitido para la varianza relativa de las medias de los tratamientos.
#'
#' @return
#' Una lista con dos elementos:
#' \describe{
#'   \item{num_de_tratamientos}{Número estimado de tratamientos a incluir en el experimento (redondeado al entero más cercano).}
#'   \item{num_de_replicas}{Número estimado de réplicas por tratamiento (redondeado al entero más cercano).}
#' }
#'
#' @details
#' El procedimiento de cálculo sigue las siguientes fórmulas:
#' \itemize{
#'   \item Se estima primero la varianza de los efectos aleatorios como \eqn{\sigma^2_A = \rho \sigma^2}.
#'   \item El número de tratamientos se calcula como:
#'     \deqn{a = \left( \frac{1}{v_{\max}} \right) \left( \sigma^2_A + \sqrt{ \frac{\sigma^2_A \sigma^2 C2}{C1} } \right)}
#'   \item El número de réplicas por tratamiento se calcula como:
#'     \deqn{r = \sqrt{ \frac{\sigma^2 C1}{\sigma^2_A C2} }}
#' }
#'
#' @examples
#' resultado <- numero_de_tratamientos_y_replicas_con_efectos_aleatorios(
#'   costo_tratamiento = 150000,
#'   costo_ue = 50000,
#'   sigma_cuadrado = 416.21,
#'   rho = 0.3796,
#'   v_max = 43.49
#' )
#'
#' @export
numero_de_tratamientos_y_replicas_con_efectos_aleatorios <- function(costo_tratamiento, costo_ue, sigma_cuadrado, rho, v_max) {
  sigma_A2 <- rho * sigma_cuadrado
  
  num_de_tratamientos <- round((1 / v_max) * (sigma_A2 + sqrt((sigma_A2 * sigma_cuadrado * costo_ue) / costo_tratamiento)))
  num_de_replicas <- round(sqrt((sigma_cuadrado * costo_tratamiento) / (sigma_A2 * costo_ue)))
  
  return(list(num_de_tratamientos = num_de_tratamientos, num_de_replicas = num_de_replicas))
}
