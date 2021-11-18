#' Structure Milieu
#'
#' @param identifiant
#'
#' @return
#' @export
#'
#' @examples
Milieu <- function(identifiant) {
  m <- new.env()
  class(m) <- "Millieu"
  m$identifiant <- identifiant
  m
}

#' Ajout_Espece in Milieu
#'
#' @param milieu
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
Ajout_Espece <- function(milieu, ...) {
  especes <- as.list(...)
  milieu$especes <- c(milieu$especes, especes)
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
print.milieu <- function(x, ...) {
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
#' @return
#' @export
#'
#' @examples
length.Milieu <- function(x) {
  length(x$especes)
}
