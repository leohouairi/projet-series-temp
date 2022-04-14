
## Librairies 


install.packages("tseries")
install.packages("fUnitRoots")
install.packages("forecast")

require(zoo)
require(tseries)
require(fUnitRoots)
require(forecast)

## Importation des données 

datafile <- "projet-series-temp/data/valeurs_mensuelles.csv"
data <- read.csv(datafile, sep=";", skip = 3, col.names =c("mois", "valeur", "A"))
data <- data[,-3] # Colonne inutile

#summary(data)
# Les dates sont déjà des caractères 

dates_char <- data$mois
dates_char[1]; dates_char[length(dates_char)] # Janvier 1990 à Février 2022
dates <- as.yearmon(seq(from = 1990+0/12, to = 2022+1/12, by = 1/12))

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




indice
# On prévoit une ARMA (p,q) avec q<=2 et p <=5

paramGrid=expand.grid(p=seq(0,5),q=seq(0,2))
str(paramGrid)
paramGrid=paramGrid[-c(1),]

tableModeles=data.frame("p"=paramGrid$p,"q"=paramGrid$q)
str(tableModeles)


### VERIFIER 1 ou 2 !!!!

x=dindice[2:length(dindice)] 

for (i in (1:nrow(paramGrid))){
  #tableModeles$p[i]=paramGrid$p[i]
  #tableModeles$q[i]=paramGrid$q[i]
  modTemp=try(arima(x,order=c(tableModeles$p[i],0,tableModeles$q[i]),include.mean = F))
  tableModeles$AIC[i]=if (class(modTemp)=="try-error") NA else modTemp$aic
  tableModeles$BIC[i]=if (class(modTemp)=="try-error") NA else BIC(modTemp)
}

tableModeles


minAIC=which.min(tableModeles$AIC)
tableModeles[minAIC,]
modelAIC=arima(x,order=c(tableModeles$p[minAIC],0,tableModeles$q[minAIC]),include.mean=F)
modelAIC
AIC(modelAIC)

minBIC=which.min(tableModeles$BIC)
tableModeles[minBIC,]
modelBIC=arima(dindice,order=c(tableModeles$p[minBIC],0,tableModeles$q[minBIC]),include.mean = F)
modelBIC
BIC(modelBIC)



significativite=function(fittedModel){
  t=fittedModel$coef/sqrt(diag(fittedModel$var.coef))
  pval=(1-pnorm(abs(t)))*2
  return(rbind(coef=fittedModel$coef,se=sqrt(diag(fittedModel$var.coef)),t,pval))
}

testModel=function(p,q,data=x){
  #estimation Mod?le
  model=try(arima(data,order=c(p,0,q),optim.control=list(maxit=20000),include.mean=F))
  if (class(model)=="try-error") return(FALSE)
  #test significativit? derniers coefs
  significatifModel=significativite(model)
  testSignificativiteAR=if (p==0) TRUE else significatifModel[4,p]<=0.05
  testSignificativiteMA=if (q==0) TRUE else significatifModel[4,p+q]<=0.05
  testSignificativite=(testSignificativiteAR)&(testSignificativiteMA)
  cat(paste0("r?sultat des tests de significativit? : ","\n"))
  print(significatifModel)
  #test autocorr?lation r?sidu
  bpTest=lapply(seq(p+q+1,24),Box.test,x=model$residuals,type="Box-Pierce",fitdf=p+q)
  resultBpTest=lapply(bpTest,function(y){y$p.value>0.05})
  validModel=prod(as.numeric(resultBpTest))==1
  cat(paste0("R?sultat du test de Box-Pierce : ",validModel,"\n"))
  
  #Resultat final
  return(testSignificativite & validModel)
}


modelAIC
testModel(2,2,dindice)

modelBIC
testModel(1,1,dindice)


Qtests(modelBIC$residuals,24,length(modelBIC$coef))
dindice
forecast(modelBIC)
