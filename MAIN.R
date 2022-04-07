
## Librairies 

require(zoo)
require(tseries)
#install.pacakges("tseries")
require(fUnitRoots)
#install.packages("fUnitRoots")

## Importation des données 

datafile <- "data/valeurs_mensuelles.csv"
data <- read.csv(datafile, sep=";", skip = 3, col.names =c("mois", "valeur", "A"))
data <- data[,-3] # Colonne inutile

#summary(data)
# Les dates sont déjà des caractères 

dates_char <- data$mois
dates_char[1]; dates_char[length(dates_char)] # Janvier 1990 à Février 2022
dates <- as.yearmon(seq(from = 1990+1/12, to = 2022+2/12, by = 1/12))

indice <- zoo(data$valeur, order.by = dates) # C'est bien la liste précédente qui est utile
dindice <- diff(indice, 1) # Différence première

plot(cbind(indice, dindice))

reg <- lm(formula = indice ~ dates)
summary(reg)

## Partie 1 - Données

# Q1 - Pas dans R

# Q2 - Transformer la série pour la rendre stationnaire

# Q3 - Représenter la série avant et après transformation

## Partie 2 - Modèles ARMA 

# Q4 - Choisir un ARMA(p,q) pour la série corrigée. Estimer les paramètres
# et vérifier la validité

# Q5 - Exprimer le modèle ARIMA(p,d,q)

## Partie 3 - Prévision