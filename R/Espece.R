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
#'  nb_p <- length.Espece(Gallus gallus)
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
#'  poule <- Espece("Gallus gallus")
#'  print(poule)
print.Espece <- function(x, ...) {
  cat("Espece : ", x$identifiant, "avec ",length(x)," populations\n")
}


populations <- function(espece) {
  espece$populations
}

`add_population<-` <- function(espece,value) {
  espece$populations <- c(espece$populations, list(value))
  espece
}

