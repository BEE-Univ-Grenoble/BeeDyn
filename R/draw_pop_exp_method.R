#' @import ggplot2
NULL

#' Fonction graphique suivant l'instance de l'objet
#' Trace l'evolution des populations en fonction du temps
#'
#' @param object Un object du package BeeDyn
#' @param ylog
#'
#' @return
#' @export
#'
#' @examples
draw_pop_expansion <- function(object, ylog=FALSE) {
   UseMethod("draw_pop_expansion", object)
}


#' Plot l'évolution d'une espece et de ses populations
#'
#' @param e Une instance d'espece
#' @param ylog Axe y en log
#'
#' @return Un graph ggplot
#' @export
#'
#' @examples
draw_pop_expansion.Espece <- function(objet, ylog = FALSE) {
  gg_espece <- ggplot(data = as_tibble(objet),
                      aes(x = temps, y = valeur)) +
    geom_point(aes(col = population, shape = espece)) +
    geom_line(aes(col = population, lty = espece))
  if (ylog) {
    gg_espece <- gg_espece + scale_y_log10()
  }
  gg_espece
}


#' Représenation graphique de l'évolution des espèces
#'
#' @param pop
#'
#' @return
#' le graphique de la population selectionné
#' @export
#'
#' @examples
#' plot_pop(pop1)
draw_pop_expansion.Population <- function(objet, ylog = FALSE) {
  gg_pop <- as_tibble(objet) %>%
    ggplot(aes(x = temps, y = valeur)) +
    geom_point(aes(col = population))

  if(ylog){gg_pop <- gg_pop + scale_y_log10()}

  gg_pop

}
