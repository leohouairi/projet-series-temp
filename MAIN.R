
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

# Notre série paraît très persistent, sans tendance très visible
# On a peut-être une stochastique trend ou une racine unitaire ? 
# La sériée différenciée une fois parait stationnaire (autour d'une moyenne)

reg <- lm(formula = indice ~ dates)
summary(reg)
# Pas de trend déterministe linéaire 


adf <- adfTest(indice, lag = 0, type = "nc")

#AH0 donc si on a compris on a une racine unitaire 


# On doit vérifier l'autocorrélation des résidus: fitdf = 0 parce qu'on a pas de tendance
Qtests= function(series, k, fitdf=0) {
  pvals = apply(matrix(1:k), 1, FUN=function(l) {
    pval = if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}

Qtests(adf@test$lm$residuals, 24, length(adf@test$lm$coefficients))
 
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

adf <- adfTest_valid(indice, 24, "nc")
adf
# Ceci est le résultat valide. pval < 0.05 AH0 on a une racine unitaire 

# Maintenant on vérifie la série différenciée

regd <- lm(formula = dindice ~ dates[-1])
summary(regd)
# Coefficients pas significatifs 

adfd <- adfTest_valid(dindice, 24, type ="nc")
adfd
# RH0, pas de racine unitaire donc a priori la série différenciée est sationnaire 

par(mfrow=c(1,2))

acf(dindice, 50); pacf(dindice, 50)

# On a quand même l'impression qu'il y a un problème de saisonnalité
# sur nos données sans sainnoalité. Très content
