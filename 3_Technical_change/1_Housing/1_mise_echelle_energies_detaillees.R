
# OBJECTIF ----------------------------------------------------------------

# A partir du fichier appmen_depensesactiv_2010, mise à l'échelle des dépenses détaillées énergies des ménages des valeurs source_usage (eg "Elec_ECS") par ménage


# Vocabulaire 
# source = source d'énergie => Elec", "Gaz", "GPL", "Fuel", "Solides", "Urbain"

# usage : usages=c("ECS","chauff","clim","Cuisson","ecl","ElecSpe")

# activité : sommeil, trav_etud, trajet, etc



# Libraries ---------------------------------------------------------------

library(plyr)
library(reshape2)
library(tidyverse)


# DONNEES -----------------------------------------------------------------
setwd("D:/CIRED/Projet_Ademe")
source("Code_global_Ademe/compute_share.R")

load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/menage_echelle_0.RData",sep=""))
load("2010/dep_ener_2010.RData")
load("Technical_change/TC_renovation_DPE/list_source_usage.RData")
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))

## Source ##
sources=c("Elec", "Gaz", "GPL", "Fuel", "Solides", "Urbain")



# MISE A L'ECHELLE --------------------------------------------------------


dep_ener<-dep_ener_2010[c("ident_men",list_source_usage,sources)]
# 45 valeurs négatives sur les 390982 (0.01%) que compte la base dep_ener_2010 (Non corrigées)
table(dep_ener<0)



# Verif préliminaire
for (source in sources){
  print(source)
  print(table(abs(dep_ener[source]-rowSums(dep_ener %>% select(ident_men, list_source_usage) %>% select(contains(source))))<10^(-6))
)
}



# A02 : Electricité
dep_ener[colnames(dep_ener %>% select(contains("Elec")))]<-
  dep_ener[colnames(dep_ener %>% select(contains("Elec")))]*
  (1+menage_echelle$elast_prix_A02*(FC$A02/menage_echelle$IP_stone-1)) * (1+menage_echelle$elast_rev_A02*menage_echelle$TC_RDB_reel)*FC$A02

# A03 : Gaz
dep_ener[colnames(dep_ener %>% select(contains("Gaz")))]<-
  dep_ener[colnames(dep_ener %>% select(contains("Gaz")))]*(1+menage_echelle$elast_prix_A03*(-1+FC$A03/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A03*menage_echelle$TC_RDB_reel)*FC$A03

# A04 : Autres energies domestiques

dep_ener[colnames(dep_ener %>% select(contains("GPL")))]<-
  dep_ener[colnames(dep_ener %>% select(contains("GPL")))]*(1+menage_echelle$elast_prix_A04*(-1+FC$A04/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*FC$A04

dep_ener[colnames(dep_ener %>% select(contains("Fuel")))]<-
  dep_ener[colnames(dep_ener %>% select(contains("Fuel")))]*(1+menage_echelle$elast_prix_A04*(-1+FC$A04/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*FC$A04

dep_ener[colnames(dep_ener %>% select(contains("Solides")))]<-
  dep_ener[colnames(dep_ener %>% select(contains("Solides")))]*(1+menage_echelle$elast_prix_A04*(-1+FC$A04/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*FC$A04

dep_ener[colnames(dep_ener %>% select(contains("Urbain")))]<-
  dep_ener[colnames(dep_ener %>% select(contains("Urbain")))]*(1+menage_echelle$elast_prix_A04*(-1+FC$A04/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*FC$A04

# Valeurs négatives dep_ener
table(dep_ener<0)
# 0.001342773 #0.13% des valeurs sont négative (525 valeurs, 12 fois plus que dans dep_ener_2010) Les ménages ayant des RDB négatifs (revenus salariaux négatifs), des taux d'impositions négatif rajoutent des valeurs négatives par la mise à l'échelle des dépenses énergétiques. 
# Non corrigé.

# SAVE --------------------------------------------------------------------

menage_echelle<-
  menage_echelle %>%
  left_join(dep_ener,by="ident_men")

source("Technical_change/TC_renovation_DPE/calc_energie_kWh_m2.R")

energie_dom_surf(menage_echelle)

menage_echelle<- 
  menage_echelle %>%
  left_join(dep_source_usage,by="ident_men")


# Dep_logement
sources=c("Elec","Gaz","Fuel","Solides","Urbain","GPL")
dep_sources=paste("dep",sources,sep="_")
menage_echelle$dep_energie=rowSums(menage_echelle[dep_sources])
menage_echelle$dep_energie_logement=rowSums(menage_echelle[
  c("Elec_ECS","Gaz_ECS","GPL_ECS","Fuel_ECS","Solides_ECS","Urbain_ECS","Elec_chauff","Gaz_chauff",
    "GPL_chauff","Fuel_chauff","Solides_chauff","Urbain_chauff","Elec_clim")])



save(menage_echelle,file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/menage_echelle_1.RData",sep=""))



# VERIF -------------------------------------------------------------------
# 

# Verif Finale
for (source in sources){
  print(source)
  print("Accord interne dep_ener")
  print(table(abs(dep_ener[source]-rowSums(dep_ener %>% select(ident_men, list_source_usage) %>% select(contains(source))))<10^(-6)))
  # print("Changements depuis dep_ener_2010")
  # print(table(abs(dep_ener[source]-dep_ener_2010[source])<10^-(1)))      
  
}



for (source in sources){
  dep_source=paste("dep",source,sep="_")
  # print(source)
  # print(table(dep_ener[source]-menage_echelle[dep_source]<10^(-6)))
}


table(abs(menage_echelle$Elec-menage_echelle$dep_Elec)<10^(-6))
table(abs(menage_echelle$Gaz-menage_echelle$dep_Gaz)<10^(-6))
table(abs(menage_echelle$GPL-menage_echelle$dep_GPL)<10^(-6))
table(abs(menage_echelle$Fuel-menage_echelle$dep_Fuel)<10^(-6))
table(abs(menage_echelle$Solides-menage_echelle$dep_Solides)<10^(-6))

# dep<-dep_ener %>% mutate(diff=dep_ener$Elec-menage_echelle$dep_Elec) %>% filter(diff>10^(-12))
# View(dep)

# Croissance depuis 2010 --------------------------------------------------
# 
# source("Code_global_Ademe/mutate_when.R")
# dep_ener$TC_ener_tot<-rowSums(dep_ener[sources])/rowSums(dep_ener_2010[sources])
# dep_ener<-dep_ener %>% mutate_when(is.na(TC_ener_tot),list(TC_ener_tot=0))
# mean(dep_ener$TC_ener_tot)




# SUCCESS -----------------------------------------------------------------

print("1_mise_echelle_energies_detaillees : SUCCESS")

