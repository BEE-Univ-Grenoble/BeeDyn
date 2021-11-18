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
#' @param x identifiant de l'espèce
#' @param ...
#'
#' @return Renvoie l'identifiant et le nombre de populations d'une espèce
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
#' @param espece
#' @param population
#'
#' @return
#' @export
#'
#' @examples
#'  poule <- Espece("Gallus sonneratii")
#'  vercors <- Population("Vercors", 150, 0.7, 1000)
#'  add_population(poule, vercors)
#'  print(poule)
add_population <- function(espece,...) {
  pops <- list(...)
  for (p in pops)
    espece$populations[[id(p)]] <- p
}


#' Verifier la classe d'une Espece
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#'  poule <- Espece("Gallus gallus")
#'  is_Espece(poule)
#'  # TRUE
#'  dindon <- Population("Belledonne", 600, 0.1, 2000)
#'  is_Espece(dindon)
#'  # FALSE
is_Espece <- function(x) {
  "Espece" %in% class(x)
}
