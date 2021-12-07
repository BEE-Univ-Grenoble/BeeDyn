#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
latest_pop_size <- function(object) {
  UseMethod("latest_pop_size",object)
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
latest_pop_size.Population <- function(object) {
  oid <- id(object)
  latest <- object$taille[length(object$taille)]
  names(latest) <- oid
  latest
}


#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
latest_pop_size.Espece <- function(object) {
  latest <- lapply(object$populations,latest_pop_size)
  vlatest <- do.call(c, latest)
  names(vlatest) <- paste(id(object),names(latest),sep = ".")
  vlatest
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
latest_pop_size.Milieu <- function(object) {
  latest <- lapply(object$especes,latest_pop_size)
  n <- do.call(c,lapply(latest,names))
  vlatest <- do.call(c, latest)
  names(vlatest) <- n
  vlatest
}


#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
push_new_size <- function(object,size) {
  UseMethod("push_new_size",object)
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
push_new_size.Population <- function(object,size) {
  object$taille <- c(object$taille,size)
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
push_new_size.Espece <- function(object,size) {
  invisible(mapply(push_new_size,object$populations, size))
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
push_new_size.Milieu <- function(object,size) {
  i <- 1
  for (e in object$especes) {
    l <- length(e)
    push_new_size(e,size[i:(i+l-1)])
    i <- i + l
  }
}


#' Capacité biotique
#'
#' @param x : environnement
#'
#' @return
#' la capacité biotique
#' @export
#'
#' @examples
#' capacity(pop)
get_capacity <- function(object) {
  UseMethod("get_capacity",object)
}


#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
get_capacity.Population <- function(object) {
  oid <- id(object)
  capacity <- object$capacite
  names(capacity) <- oid
  capacity
}


#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
get_capacity.Espece <- function(object) {
  capacity <- lapply(object$populations,get_capacity)
  vcapacity <- do.call(c, capacity)
  names(vcapacity) <- paste(id(object),names(capacity),sep = ".")
  vcapacity
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
get_capacity.Milieu <- function(object) {
  capacity <- lapply(object$especes,get_capacity)
  n <- do.call(c,lapply(capacity,names))
  vcapacity <- do.call(c, capacity)
  names(vcapacity) <- n
  vcapacity
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
get_growth_rate <- function(object) {
  UseMethod("get_growth_rate",object)
}


#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
get_growth_rate.Population <- function(object) {
  oid <- id(object)
  growth_rate <- object$taux
  names(growth_rate) <- oid
  growth_rate
}


#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
get_growth_rate.Espece <- function(object) {
  growth_rate <- lapply(object$populations,get_growth_rate)
  vgrowth_rate <- do.call(c, growth_rate)
  names(vgrowth_rate) <- paste(id(object),names(growth_rate),sep = ".")
  vgrowth_rate
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
get_growth_rate.Milieu <- function(object) {
  growth_rate <- lapply(object$especes,get_growth_rate)
  n <- do.call(c,lapply(growth_rate,names))
  vgrowth_rate <- do.call(c, growth_rate)
  names(vgrowth_rate) <- n
  vgrowth_rate
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
reset <- function(object) {
  UseMethod("reset",object)
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
reset.Population <- function(object) {
  object$taille <- object$taille[1]
}


#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
reset.Espece <- function(object) {
  for (p in object$populations) {
    reset(p)
  }
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
reset.Milieu <- function(object) {
  for (p in object$especes) {
    reset(p)
  }
}

