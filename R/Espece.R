#' @import tidyverse
NULL


#' Crée une nouvelle instance de la classe `Espece`
#'
#' @param identifiant une chaine de caractères precisant un code pour chaque espèce
#'
#' @return une instance de la classe espèce
#' @export
#'
#' @md
#' @examples
#'   e <- Espece("Gallus gallus")
#'   print(e)
Espece <- function(identifiant) {
  e <- new.env()
  class(e) <- "Espece"
  e$identifiant = identifiant
  e$populations = list()
  e
}


#' Calcule le nombre de populations pour une espèce
#'
#' On appellera longueur d'une espèce son nombre de populations associées
#'
#' @param x identifiant d'une espèce
#'
#' @return le nombre de population
#' @export
#'
#' @examples
#'  nb_p <- length.Espece(Gallus varius)
#'  print(nb_p)
length.Espece <- function(x) {
  length(x$populations)
}

#' Affiche un objet espèce
#'
#' Affiche l'identifiant et le nombre de populations d'une espèce
#'
#' @param x un objet instance de Espece
#' @param ...
#'
#' @export
#'
#' @examples
#'  poule <- Espece("Gallus lafayetti")
#'  print(poule)
print.Espece <- function(x, ...) {
  cat("Espece : ", x$identifiant, "avec ",length(x)," populations\n")
}


#' Ajoute une population dans une Espece
#'
#' @param espece un objet instance de Espece
#' @param ... une ou des instance.s de Population
#'
#' @export
#'
#' @examples
#'  poule <- Espece("Gallus sonneratii")
#'  vercors <- Population("Vercors", 150, 0.7, 1000)
#'  insert(poule, vercors)
#'  print(poule)
insert.Espece <- function(espece,...) {
  pops <- list(...)
  for (p in pops)
    espece$populations[[id(p)]] <- p
}


#' Verifier la classe d'une Espece
#'
#' @param x
#'
#' @return une valeur logique, TRUE si la classe est Espece
#' @export
#'
#' @examples
#'  poule <- Espece("Gallus murghi")
#'  is_Espece(poule)
#'  # TRUE
#'  dindon <- Population("Belledonne", 600, 0.1, 2000)
#'  is_Espece(dindon)
#'  # FALSE
is_Espece <- function(x) {
  "Espece" %in% class(x)
}


#' Recupère les populations d'une instance Espece
#'
#' @param espece Une instance Espece
#'
#' @return une liste de populations
#' @export
#'
#' @examples
#'  poule <- Espece("Gallus bankiva")
#'  get_populations(poule)
get_populations <- function(espece) {
  espece$populations
}


#' Convertir une instance Espece en Tibble
#'
#' @param x Une instance Espece
#'
#' @return
#' @export
#'
#' @examples
as.tibble.Espece <- function(x) {
  e <- tibble()
  e$Milieu = rep(NA, length(x$opulations))
  e$Espece = rep(x$identifiant, length(x$populations))
  for (i in 1:length(x$populations)) {
    e$Population[i] = x$populations[i,]$identifiant
    e$Temps[i] = 1
    e$Valeur[i] = x$population[i,]$taille_ini
  }
  }
