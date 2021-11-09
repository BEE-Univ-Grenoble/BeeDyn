#' Crée une nouvelle instance de la classe `Espece`
#'
#' @param identifiant une chaine de caractères precisant un code pour chaque espèce
#'
#' @return une instance de la classe espèce.
#' @export
#'
#' @md
#' @examples
#'   e <- Espece("Gallus gallus")
#'   print(e)
Espece <- function(identifiant) {

  structure(list(identifiant = identifiant,
                 populations = list()),
            class = "Espece"
            )
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
length.Espece <- function(x) {
  length(x$populations)
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
print.Espece <- function(x, ...) {

  cat("Espece : ", x$identifiant, "avec ",length(x)," populations\n")
}

ajoute_population <- function(espece,population){

}
