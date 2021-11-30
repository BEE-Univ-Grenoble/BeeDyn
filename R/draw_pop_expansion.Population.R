#' @import ggplot2
NULL






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
plot_pop <- function(m, ylog = F){
  df_pop <- as_tibble(m) %>%
    ggplot(aes(x = temps, y = valeur)) +
    geom_point(aes(col = population)) +
    facet_grid( ~ population)

  if(ylog){df_pop <- df_pop + scale_y_log10()}

  df_pop

}
