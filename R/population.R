
population <- function(identifiant, taille_ini, taux_rep, capacite){
  structure(list(identifiant = identifiant,
                 capacite = capacite,
                 taux = taux_rep,
                 taille = c(taille_ini)
                 ))
}
