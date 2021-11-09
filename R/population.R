
#' Population structure
#' A structured object to describe population
#'
#'
#'
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
#' Population(34, 50, 0.8, 5000)
Population <- function(identifiant, taille_ini, taux_rep, capacite){
  if(abs(taux_rep) > 1){stop("Error : Reproduction rate > 1")}
  structure(list(identifiant = identifiant,
                 capacite = capacite,
                 taux = taux_rep,
                 taille = c(taille_ini)
                 ))
}
