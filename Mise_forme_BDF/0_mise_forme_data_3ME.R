
# Mise en forme des hypothèses et trajectoires de ThreeME
# Lecture depuis fichier excel, converti en Rdata format long
# Output : ThreeME.RData
# Préparation de la matrice des sauts de consommation par rénovation énergétique
# Output : Mat_gain_ener_2025.RData


# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(readxl)

# DATA --------------------------------------------------------------------

setwd("D:/CIRED/Projet_Ademe")


# AMS ---------------------------------------------------------------------
if(scenario=="AMS"){
  S="scen AMS"
}
if(scenario=="AME"){
  S="scen AME"
}
if(scenario=="ssTCO"){
  S="scen AMS ss TCO"
}
if(scenario=="ssRES"){
  S="scen AMS ss residentiel"
}
if(scenario=="ssVE"){
  S="scen AMS ss VE"
}

# suppressWarnings(scen<-read_excel(path="Donnees_brutes/Sorties ThreeMe/2019-09-23_ThreeME/Sorties_Three-ME_201900903.xlsx",sheet=S))

suppressWarnings(scen<-read_excel(path="D:/CIRED/Projet_Ademe/IMACLIM/Sorties Three-ME.xlsx",sheet=S))

#Retirer première colonne avec variable expliquée
scen<-scen[,2:47]
colnames(scen)<-c("Var",seq(2006,2050))
# Conversion format long
scen<- scen %>% gather(key=year, value=value, -c(1))

ThreeME<-scen

save(ThreeME, file="Donnees_brutes/Sorties ThreeMe/ThreeME.RData")


# SUCCESS -----------------------------------------------------------------

print("0_mise_forme_data_3ME : SUCCESS")

