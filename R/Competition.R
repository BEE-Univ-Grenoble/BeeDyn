#' Matrice des coefficients de competition
#'
#' @param milieu objet de type "Milieu"
#'
#' @return la matrice des coefficients de competition
#' @export
#'
#' @examples
#' m <- Milieu("environnement")
#' e1 <- Espece("e1")
#' e2 <- Espece("e2")
#' insert(m, e1, e2)
#' p1 <- Population("p1", 10, 0.8, 1000)
#' p2 <- Population("p2", 10, 0.8, 500)
#' p3 <- Population("p3", 10, 0.8, 1000)
#' insert(e1, p1, p2)
#' insert(e2, p3)
#' Competition(m)
Competition <- function(milieu) {
  if (!is_Milieu(milieu)) {
    stop("milieu must be of class Milieu")
  }

  nb_species <- length(milieu)
  # Calcul du nombre de population
  npop <- 0
  for (e in get_species(milieu)) {
    npop <- npop + length(e)
  }

  npop

  # # Création de la matrice en fonction du nombre de population
  competition_matrice <- matrix(0, ncol = npop, nrow = npop)

  # affectation de 1/K dans la diagonale, récupération des noms de population
  num_colonne <- 1
  nom_population <- c()
  for (e in get_species(milieu)) {
    for (p in get_populations(e)) {
      competition_matrice[num_colonne, num_colonne] <- 1 / p$capacite
      nom_population <- c(nom_population, paste(id(e), id(p), sep = "."))
      num_colonne <- num_colonne + 1
    }
  }

  # renommer les colonnes et les lignes
  colnames(competition_matrice) <- nom_population
  rownames(competition_matrice) <- nom_population

  class(competition_matrice) <- c("Competition", class(competition_matrice))
  competition_matrice
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' m <- Milieu("environnement")
#' e1 <- Espece("e1")
#' e2 <- Espece("e2")
#' insert(m, e1, e2)
#' p1 <- Population("p1", 10, 0.8, 1000)
#' p2 <- Population("p2", 10, 0.8, 500)
#' p3 <- Population("p3", 10, 0.8, 1000)
#' insert(e1, p1, p2)
#' insert(e2, p3)
#' comp <- Competition(m)
#' is_competition(comp)
#' is_competition(m)
is_Competition <- function(x) {
  "Competition" %in% class(x)
}
