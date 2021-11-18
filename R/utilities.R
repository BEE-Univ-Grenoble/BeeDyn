#' Returns the id of a BeeDyn object
#'
#' @param object
#'
#' @return a character value containing to object id
#' @export
#'
#' @examples
id <- function(object) {
  object$identifiant
}


#' Add elements to a DeeDyn object
#'
#' @param object the DeeDyn object to modify
#' @param ... every objects to be added to the object parameter
#'
#' @export
#'
insert <- function(object,...) {
  UseMethod("insert",object)
}
