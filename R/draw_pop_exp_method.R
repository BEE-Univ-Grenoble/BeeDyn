#' Title
#'
#' @param object
#' @param ylog
#'
#' @return
#' @export
#'
#' @examples
draw_pop_expansion <- function(object, ylog=FALSE) {
   UseMethod("draw_pop_expansion", object)
}
