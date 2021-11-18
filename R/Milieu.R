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
  class(m) <- "Milieu"
  m$identifiant <- identifiant
  m$especes = list()
  m
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is_Milieu <- function(x) {
  "Milieu" %in% class(x)
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
#' @return
#' @export
#'
#' @examples
length.Milieu <- function(x) {
  length(x$especes)
}
