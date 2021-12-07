#' Simulation des tailles de population
#'
#' @param milieu objet de type "Milieu"
#' @param temps numeric
#' @param competition objet de type "Competition"
#' @param modele
#'
#' @return Pour chaque population leur taille aux différents temps
#' @export
#'
#' @examples
#' m <- Milieu("environnement")
#' e1 <- Espece("e1")
#' e2 <- Espece("e2")
#' insert(m, e1, e2)
#' p1 <- Population("p1", 10, 0.8, 1000)
#' p2 <- Population("p2", 10, 0.8, 500)
#' p3 <- Population("p3", 5, 0.5, 1000)
#' insert(e1, p1, p2)
#' insert(e2, p3)
#' comp <- Competition(m)
#' mod_exp <- function(milieu,competition) {
#'    sizes <- latest_pop_size(milieu)
#'    rates <- get_growth_rate(milieu)
#'    delta <- sizes * rates
#'    delta
#' }
#' simulate_pop(mod_exp,m,temps=20,competition=comp)
simulate_pop <- function(modele,milieu,temps,competition=NULL) {
  reset(milieu)

  for (t in 1:temps) {
    push_new_size(milieu,
                  latest_pop_size(milieu) + modele(milieu,competition))
  }
}
