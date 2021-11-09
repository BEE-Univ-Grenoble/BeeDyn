#' Title
#'
#' @param identifiant
#'
#' @return
#' @export
#'
#' @examples
Milieu <- function(identifiant) {
  structure(list(
    identifiant = identifiant,
    especes = list()
  ),
  class = "Milieu"
  )
}

Ajout_Espece <- function(milieu, ...) {
  especes <- as.list(...)
  milieu$especes <- c(milieu$especes,especes)
}

