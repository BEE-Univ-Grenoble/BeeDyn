#' Structure Milieu
#'
#' @param identifiant
#'
#' @return un objet de classe milieu avec un identifiant
#' @export
#'
#' @examples
#' m <- Milieu("Prairie")
#' print(m)
Milieu <- function(identifiant) {
  m <- new.env()
  class(m) <- "Milieu"
  m$identifiant <- identifiant
  m$especes = list()
  m
}

#' Verification si l'objet est de la classe milieu
#'
#' @param x
#'
#' @return True/false en fonction de si l'objet est de classe Milieu ou non
#' @export
#'
#' @examples
#' m <- Milieu("Prairie")
#' is_Milieu(m)
is_Milieu <- function(x) {
  "Milieu" %in% class(x)
}

#' Ajout des Especes dans un Milieu
#'
#' @param milieu
#' @param ...
#'
#' @return Un objet de classe Milieu contenant ... espèces
#' @export
#'
#' @examples
insert.Milieu <- function(milieu, ...) {
  especes <- list(...)
  for (e in especes)
    milieu$especes[[id(e)]] <- e
}

#' print.milieu
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.Milieu <- function(x, ...) {
  cat("Milieu", x$identifiant, ":nombre d'espèces=", length(x$especes), "\n")
  cat("Liste d'espèces : \n")
  for (e in x$especes) {
    cat(" - ", print(e),"\n")
  }
}



#' Length.Milieu
#'
#' @param x
#'
#' @return une valeur numérique
#' @export
#'
#' @examples
length.Milieu <- function(x) {
  length(x$especes)
}

#' Title
#'
#' @param milieu
#'
#' @return
#' @export
#'
#' @examples
species <- function(milieu) {
  milieu$especes
}
