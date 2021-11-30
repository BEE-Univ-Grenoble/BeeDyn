#' Nombre de population
#'
#' @param milieu objet de type "Milieu"
#'
#' @return le nombre de population
#' @export
#'
#' @examples
#' #' m <- Milieu("environnement")
#' e1 <- Espece("e1")
#' e2 <- Espece("e2")
#' insert(m, e1, e2)
#' p1 <- Population("p1", 10, 0.8, 1000)
#' p2 <- Population("p2", 10, 0.8, 500)
#' p3 <- Population("p3", 10, 0.8, 1000)
#' insert(e1, p1, p2)
#' insert(e2, p3)
#' npops(m)
npops <- function(milieu) {
  if (!is_Milieu(milieu)) {
    stop("milieu must be of class Milieu")
  }

  # Calcul du nombre de population
  n <- 0
  for (e in get_species(milieu)) {
    n <- n + length(e)
  }

  n
}

#' Simulation des tailles de population
#'
#' @param milieu objet de type "Milieu"
#' @param temps numeric
#' @param competition objet de type "Competition"
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
#' simulate_pop(m,temps=20,competition=comp)
simulate_pop <- function(milieu,temps,competition=NULL) {

  npop <- npops(milieu)

  popsizes <- matrix(0,nrow=temps,ncol=npop)
  rates <- numeric(npop)
  i <- 0
  n <- character(npop)

  #initialisation de la matrice de taille, récupération des taux de reproduction et des noms des populations
  for (e in get_species(milieu))
    for (p in get_populations(e)) {
      i<-i+1
      popsizes[1,i] <- get_pop_size(p)[1]
      rates[i]<-get_growth_rate(p)
      n[i]<- paste(id(e), id(p), sep = ".")
    }

  colnames(popsizes) <- n

  # Calcule si besoin de la matrice de competition
  if (is.null(competition))
    competition <- Competition(milieu)

  # remplissage de la matrice de taille
  for (t in 2:temps) {
    delta <- rates * (1 - popsizes[t-1,] %*% competition) * popsizes[t-1,]
    popsizes[t,] <- popsizes[t-1,] + delta
  }

  i <- 0

  # incrémentation des tailles aux niveaux des objets populations
  for (e in get_species(milieu))
    for (p in get_populations(e)) {
      i<-i+1
      p$taille <- popsizes[,i]
    }
}
