
#' @import tidyverse
NULL



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

  if (abs(taux_rep) > 1) {
    stop("Error : Reproduction rate > 1")
  }

  pop <- new.env()
  class(pop) <- "Population"
  pop$identifiant <- identifiant
  pop$capacite <- capacite
  pop$taux <- taux_rep
  pop$taille <- taille_ini[1]

  pop
}

#' Print population
#'
#' @param x
#' Un environnement associé à la population
#'
#' @return
#' Taille de la population par identifiant pour chaque identifiant
#' @export
#'
#' @examples
#' print.Population(pop)
#'
print.Population <- function(x) {
  cat("Population : ", x$identifiant, "avec une taille initiale de", x$taille, "\n",
      ",", "une capacité biotique de", x$capacite, "\n",
      "et un taux de reproduction de", x$taux, "\n")
}

#' Classe de la population
#'
#' @param x : environnement associé à la population
#'
#' @return
#' Retourne un booléen indiquant s'il s'agit d'une population ou non
#' @export
#'
#' @examples
#' is_Population(pop)
is_Population <- function(x) {
  "Population" %in% class(x)
}

#' Nombre de simulation
#'
#' @param x : environnement
#'
#' @return
#' le nombre de simulation (temps)
#' @export
#'
#' @examples
#' length.Population(pop)
length.Population <- function(x) {
  length(x$taille)
}


#' Taille de la population
#'
#' @param x : environnement
#'
#' @return
#' la taille de la population
#' @export
#'
#' @examples
#' pop_size(pop)
get_pop_size <- function(x){
  x$taille
}


#' tibble de la population
#'
#' @param x : population
#'
#' @return
#' un tibble avec des na dans le milieu, des na dans les especes, le nombre d'individus dans les populations et le temps
#' @export
#'
#' @examples
#' astibblepop(pop)
as_tibble.Population <- function(e, milieu_id = NA,species_id = NA){
 tibble(milieu = milieu_id,
            espece = species_id,
            population = id(e),
            temps = seq_along(e$taille),
            valeur = e$taille
        )
}
