# Note: ce code est �galement disponible en annexe du rapport

### Librairies

require(zoo)
require(tseries)
require(fUnitRoots)
require(forecast)

## Importation des données 

datafile <- "data/valeurs_mensuelles.csv"
data <- read.csv(datafile, sep=";", skip = 3, col.names =c("mois", "valeur", "A"))
data <- data[,-3] # Suppression de la colonne inutile

dates_char <- data$mois
dates_char[1]; dates_char[length(dates_char)] # Janvier 1990 à Février 2022
dates <- as.yearmon(seq(from = 1990+0/12, to = 2022+1/12, by = 1/12))


## Premiers graphiques

indice <- zoo(data$valeur, order.by = dates) # C'est bien la liste précédente qui est utile
dindice <- diff(indice, 1) # Différence première

plot(cbind(indice, dindice))

# Notre série paraît très persistent, sans tendance très visible
# On a peut-être une stochastique trend ou une racine unitaire ? 
# La sériée différenciée une fois parait stationnaire autour de 0 


## Vérification de la présence d'une tendance linéaire déterministe dans les données

reg <- lm(formula = indice ~ dates)
summary(reg)

# Comme attendu vu les graphiques, il ne semble pas y avoir de telle tendance 
# (coeff de dates par significatif)


## Fonctions pour les tests de racine unitaire

# Fonction effectuant le test d'autocorrélation des résidus 
Qtests= function(series, k, fitdf=0) {
  # series = série à analyser
  # k = nombre maximal de lags à vérifier
  # fitdf = nombre de coefficients estimés, à retirer des ddl pour le test
  pvals = apply(matrix(1:k), 1, FUN=function(l) {
    pval = if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}

# Fonction effectuant des tests ADF jusqu'à ce que l'un d'eux soit valide
# Cad jusqu'à ce que les résidus ne soient plus corrélés
adfTest_valid <- function(series, kmax, type) {
  k <- 0
  noautocorr <- 0
  while (noautocorr == 0) {
    cat(paste0("ADF with ", k," lags: residuals OK? "))
    adf <- adfTest(series, lags =k, type = type)
    pvals <- Qtests(adf@test$lm$residuals, 24, fitdf = length(adf@test$lm$coefficients))[,2]
    if(sum(pvals<0.05, na.rm =T) == 0){
      noautocorr <- 1; cat("OK \n")}
    else cat("nope \n")
    k <- k +1
  }
  return(adf)
}

## Test de racine unitaire sur la série non-différenciée

# On teste sur la série non-différenciée pour la présence d'une racine unitaire
# On utilise "nc" puisque d'après la régression effectuée, notre série ne contient
# ni tendance temporelle déterministe ni ordonnée à l'origine
# On teste jusqu'à 24 retards
adf <- adfTest_valid(indice, 24, "nc")
adf
# Il a fallu ajouter 5 retards pour que le test soit valide
# pval = 0.38 > 0.05, AHO, on a donc une racine unitaire dans la série non-différenciée


## Test de racine unitaire sur la série différenciée 

regd <- lm(formula = dindice ~ dates[-1])
summary(regd)

# Les coefficients de la régression sur le temps ne sont pas significatifs dans 
# le cas de la série différenciée

adfd <- adfTest_valid(dindice, 24, type ="nc")
# Il a fallu ajouter 4 lags pour que le test soit valide
adfd
# pval < 0.01 donc <0.05, RHO, pas de racine unitaire 
# Donc notre série différenciée semble être stationnaire
# La série de base est donc I(1)


### Recherche d'un modèle ARMA

## Valeurs maximales de p & q

x = dindice # Le x sur lequel on travaille est la série différenciée

par(mfrow=c(1,2))
acf(x); pacf(x)
# Vu l'ACF, on prend qmax = 2 (en ignorant le pic significatif lointain, 
# il est normal qu'il y en ait quelques uns à 5%, et on veut un modèle avec peu 
# de paramètres)
# Vu le PACF, on choisit pmax = 5 (en ignorant de la même manière les pics
# lointains)
pmax = 5
qmax = 2

## Utilisation de critères d'information pour déterminer les meilleurs modèles candidats

mat <- matrix(NA, nrow = pmax+1, ncol = qmax+1)
rownames(mat) <- paste0("p= ", 0:pmax)
colnames(mat) <-paste0("q = ", 0:qmax)
AICs <- mat
BICs <- mat
pqs <- expand.grid(0:pmax, 0:qmax) # toutes les combinaisons de p et q

for (row in 1:dim(pqs)[1]){ #On boucle sur chaque pq
  p <- pqs[row, 1] # On récupère p 
  q <- pqs[row, 2] # On récupère q
  estim <- try(arima(x, c(p,0,q), include.mean= F)) # Tenter d'estimer l'ARIMA
  AICs[p+1, q+1] <- if(class(estim) == "try-error") NA else estim$aic #Assigne l'AIC
  BICs[p+1, q+1] <- if(class(estim) == "try-error") NA else BIC(estim) # Assigne le BIC
}

AICs
AICs == min(AICs)
# Le modèle (5,0) minimise l'AIC, donc on le conserve:
arima510 <- arima(indice, c(5,1,0), include.mean = F)

BICs
BICs == min(BICs)
# Le modèle (1,1) minimise le BIC, donc on le conserve:
arima111 <- arima(indice, c(1,1,1), include.mean = F)


## Significativité des coefficients des modèles sélectionnés

arima510
0.1449/0.05353
# Le modèle est bien ajusté puisque ar5/se(ar5) > 2 en valeur absolue

arima111
0.4/0.1
0.71/0.0778
# Les 2 coefficients sont bien ajustés (rapport > 2 en valeur absolue)


## Autocorrélation des résidus

Qtests(arima510$residuals, 24, fitdf = 5)
# Pour chaque valeur de lag envisagée, on ne rejette pas l'hypothèse de non-corrélation
# des résidus, l'ARIMA 510 est donc valide

Qtests(arima111$residuals, 24, fitdf = 2)
# Même conclusion pour l'ARIMA 111

# On dispose donc à ce stade de 2 modèles semblant être valides

### Prévision pour 2 périodes supplémentaires
# On peut utiliser FORECAST mais aussi PREDICT 

#6 - Equation vérifiée par la région de confiance sur 2 étapes


#7 - Hypothèses utilisées


#8 - Représentation graphique de la région et commentaire

predict(arima510, n.ahead = 2)

# Au moins les 2 commandes suivantes donnent des résultats cohérents

forecast(arima510, h = 2, level = 95)
autoplot(forecast(arima510, h =2, level = 95))

# Intervalle de confiance plus large en T+2
