
#' Title
#'
#' @param identifiant
#' Valeur numérique identifiant l'espèce
#' @param taille_ini
#' Taille initiale de la population en question
#' @param taux_rep
#' Taux de reproduction maximum de la population
#' @param capacite
#' Capacite biotique du milieu
#' @return
#' Une liste structuré comprenant les paramètres de la population mentionnés ci-dessus
#' @export
#'
#' @examples
population <- function(identifiant, taille_ini, taux_rep, capacite){
  structure(list(identifiant = identifiant,
                 capacite = capacite,
                 taux = taux_rep,
                 taille = c(taille_ini)
                 ))
}
