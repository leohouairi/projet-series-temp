# projet-series-temp

Projet dans le cadre du cours "Séries Temporelles Linéaires"

Données issues de: 
https://www.insee.fr/fr/statistiques/serie/010537749#Telechargement

Indice CVS-CJO de la production industrielle (base 100 en 2015) - Fabrication 
de machines agricoles et forestières (NAF rév. 2, niveau groupe, poste 28.3)

Il s'agit d'un indice de production industrielle, corrigé des variations saisonnières 
et du nombre de jours ouvrables

## Sous-période : "le Courant" 

```

data<-data[181:nrow(data),]
dates <- as.yearmon(seq(from = 2005+0/12, to = 2022+1/12, by = 1/12))
