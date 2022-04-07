
## Librairies 

require(tidyverse)
require(zoo)
require(tseries)

## Importation des données 

datafile <- "data/valeurs_mensuelles.csv"
data <- read.csv(datafile, sep=";", skip = 3, col.names =c("mois", "valeur", "A"))
data <- data[,-3]

#summary(data)
# Les dates sont déjà des caractères 

dates_char <- data$mois
dates_char[1]; dates_char[length(dates_char)] # Janvier 1990 à Février 2022
dates <- as.yearmon(seq(from = 1990+1/12, to = 2022+2/12, by = 1/12))

indice <- zoo(data$valeur, order.by = dates) # C'est bien la liste précédente qui est utile
dindice <- diff(indice, 1) # Différence première

plot(cbind(indice, dindice))



# Q1 - 

# Q2 - 