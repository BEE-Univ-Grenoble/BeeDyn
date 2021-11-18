#' Returns the id of a BeeDyn object
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
id <- function(object) {
  object$identifiant
}


#' Add elements to a DeeDyn object
#'
#' @param object
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
insert <- function(object,...) {
  UseMethod("insert",object)
}
