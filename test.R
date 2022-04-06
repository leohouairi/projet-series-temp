# Fichier de test 
df <- read.csv("valeurs_mensuelles.csv",sep=';',skip = 3)
df

install.packages("zoo")
require(zoo)

dates_char <- as.character(df$Période)

dates <- as.yearmon(seq(from=1990+0/12,to=2022+1/12,by=1/12))

x<-zoo(df$X,order.by=dates)


dx<-diff(x,1)

plot(cbind(x,dx))

plot(decompose(x))
