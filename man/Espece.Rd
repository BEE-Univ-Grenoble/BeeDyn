% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Espece.R
\name{Espece}
\alias{Espece}
\title{Crée une nouvelle instance de la classe \code{Espece}}
\usage{
Espece(identifiant)
}
\arguments{
\item{identifiant}{une chaine de caractères precisant un code pour chaque espèce}
}
\value{
une instance de la classe espèce
}
\description{
Crée une nouvelle instance de la classe \code{Espece}
}
\examples{
  e <- Espece("Gallus gallus")
  print(e)
}
