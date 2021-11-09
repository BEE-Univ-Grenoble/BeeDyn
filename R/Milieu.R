#' Title
#'
#' @param identifiant
#'
#' @return
#' @export
#'
#' @examples
Milieu <- function(identifiant) {
  structure(list(
    identifiant = identifiant,
    especes = list()
  ),
  class = "Milieu"
  )
}

Ajout_Espece <- function(milieu, ...) {
  especes <- as.list(...)
  milieu$especes <- c(milieu$especes, especes)
}

#' Title
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
  for (e in x$especes) {
    print(e)
  }
}



#' Title
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
