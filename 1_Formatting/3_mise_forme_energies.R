# OBJECTIF ----------------------------------------------------------------

# A partir du fichier appmen_depensesactiv_2010, recomposition des valeurs source_usage (eg "Elec_ECS") par ménage


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

load("Donnees_brutes/IMACLIM_Simona/Base/appmen_depensesactiv_ener.RData")
# NB : dans appmen_intensities : dépenses détaillées par source_activite et par usage_activite. Variables souhaitée : source_usage. 

# load("2010/appmen_depensesactiv_2010.RData")

load("2010/menage_forme_2010_rehab.RData")
# load("2010/menage_forme_2010_rehab.RData")

# Selection des ménages en commun dans appmen_intensities et menage_forme_2010_rehab
appmen_intensites<-appmen_intensites[which(appmen_intensites$ident_men %in% intersect(menage_forme_2010_rehab$ident_men,appmen_intensites$ident_men)),]




# SOURCES & USAGES & ACTIVITES --------------------------------------------


##  Usages ##
usages=c("ECS","chauff","clim","Cuisson","ecl","ElecSpe")


## Activités ##
activites=c("sommeil", "physio", "repas_dom", "repas_hors", "trav_etu", "achats", "travdom_alim", "travdom_vetem", "travdom_maison","assistance","loisirs_lecture","loisirs_tele","pleinair_sport","trajets_domtrav","trajets_autres")


## Source ##
sources=c("Elec", "Gaz", "GPL", "Fuel", "Solides", "Urbain")
reventil_sources=paste("reventil",sources,sep="_")
dep_sources=paste("dep",sources,sep="_")



# TRAITEMENT PRELIMINAIRE -------------------------------------------------

# Suite à la reventilation des dépenses 2010 de rehabilitation énergétique, les totaux de dépenses ener ont été changés pour environ 200 ménages. 
# Update des dépenses dans appmen_intensities également. 

# on crée dans appmen_intensities une nouvelle variable égale à l'ancienne multipliée par la progression due à la reventilation qu'on trouve dans menage_calibr par rapport à menage_forme_rehab. On applique le % de progression (Avant-Après)/Après
# appmen_intensites_bis[sources]<-
#   appmen_intensites*
#   (1+(menage_forme_2010_rehab[dep_sources] - menage_forme_rehab_2010[dep_sources])/menage_forme_rehab_2010[dep_sources])
# 
# for (x in reventil_sources){appmen_intensites[which(is.na(appmen_intensites[x])),x]<-0}


# Vérif -------------------------------------------------------------------

# Pour le ménage 
# i=1549
# menage_forme_2010_rehab %>% filter(ident_men==i) %>% select(dep_sources)
# menage_forme_2010_rehab%>% filter(ident_men==i) %>% select(dep_sources)

# > menage_forme_2010_rehab %>% filter(ident_men==i) %>% select(dep_sources)
# dep_Elec dep_Gaz dep_GPL dep_Fuel dep_Solides dep_Urbain
# 1 721.6682       0      52        0         189          0

# > menage_forme_2010_rehab%>% filter(ident_men==i) %>% select(dep_sources)
# dep_Elec dep_Gaz  dep_GPL dep_Fuel dep_Solides dep_Urbain
# 1 840.6782       0 20.45068        0    74.33037          0

# (840-721)/721
# 721.668*(1+((840-721)/721))
# (20.45-52)/52
# 52*(1+(20.45-52)/52)


# table(dep_source_usage$Elec-menage_forme_2010_rehab$dep_Elec<10^(-4))
# View(cbind(menage_forme_2010_rehab[which(dep_source_usage$Elec-menage_forme_2010_rehab$dep_Elec<10^(-4)),c("ident_men","dep_Elec")],dep_source_usage[which(dep_source_usage$Elec-menage_forme_2010_rehab$dep_Elec<10^(-4)),c("ident_men","Elec")]))

# SOMMER DEPENSES par ACTIVITE --------------------------------------------

# Selection des colonnes "ident_men" et de la forme "usages_activite"
dep_usage_activite<-appmen_intensites[c(1,187:257)] 

# Selection des colonnes "ident_men" et de la forme "source_activite"
dep_source_activite <- appmen_intensites[c(1,258:347)]


# Pour chacun des listes source et usage, sommer toutes les activites en dépenses énergétiques. Les usages ne sont pas exhaustifs, pas d'égalité entre les différentes sommes. 
for (act in activites){
  dep_usage_activite[act]<- dep_usage_activite %>% select(contains(act)) %>% rowSums()
  dep_source_activite[act]<- dep_source_activite %>% select(contains(act)) %>% rowSums()
  print(act)
  # print(table(dep_usage_activite[act]==dep_source_activite[act]))
  print(table(dep_usage_activite[act]-dep_source_activite[act]>10^(-10)))
}



# VERIFICATION n°1 --------------------------------------------------------

# CONCLUSION
# dep_usage_activite[act]==dep_source_activite[act] dans tous les cas, à l'erreur numérique près

# commun<-left_join(dep_usage_activite,dep_source_activite,by="ident_men")
# act<-"loisirs_tele"
# for (act in activites){
# commun[paste(act,"diff",sep="_")]<-commun[paste(act,"x",sep=".")]-commun[paste(act,"y",sep=".")]
# print(table(commun[paste(act,"diff",sep="_")]>10^(-11)))} #ALL FALSE
# View(commun[which(!commun[paste(act,"x",sep=".")]-commun[paste(act,"y",sep=".")]<10^-13),c(paste(act,"x",sep="."),paste(act,"y",sep="."),paste(act,"diff",sep="_"))])






# SOMMER DEPENSES par SOURCE   --------------------------------------------


# Sommer pour chaque ménage les dépenses pour chaque source d'énergie
for (source in sources){
  print(source)
  dep_source_activite[source]<- dep_source_activite %>% select(contains(source)) %>% rowSums()  
}



# SOMMER DEPENSES par USAGE -----------------------------------------------




# Sommer pour chaque ménage les dépenses pour chaque usage d'énergie (non exhaustif)
for (usage in usages){
  print(usage)
  dep_usage_activite[usage]<- dep_usage_activite %>% select(starts_with(usage)) %>% rowSums()
}



# VERIFICATION n°2 --------------------------------------------------------

# Cohérence avec menage_forme_2010_rehab (dep_elec, dep_gaz, etc) => OUI
table(dep_source_activite$Elec==menage_forme_2010_rehab$dep_Elec)
table(dep_source_activite$Elec==menage_forme_2010_rehab$dep_Elec)
table(dep_source_activite$Gaz==menage_forme_2010_rehab$dep_Gaz)
table(dep_source_activite$GPL==menage_forme_2010_rehab$dep_GPL)
table(dep_source_activite$Fuel==menage_forme_2010_rehab$dep_Fuel)
table(dep_source_activite$Solides==menage_forme_2010_rehab$dep_Solides)
# table(dep_source_activite$Solides-menage_forme_2010_rehab$dep_bois-menage_forme_2010_rehab$dep_charbon<10^(-12.8)) # les 217 erreurs <e-13
table(dep_source_activite$Urbain==menage_forme_2010_rehab$dep_Urbain)


# Cohérence avec menage_forme_2010_rehab (dep_elec, dep_gaz, etc) 
table(dep_source_activite$Elec==menage_forme_2010_rehab$dep_Elec)
table(dep_source_activite$Gaz==menage_forme_2010_rehab$dep_Gaz)
table(dep_source_activite$GPL==menage_forme_2010_rehab$dep_GPL)
table(dep_source_activite$Fuel==menage_forme_2010_rehab$dep_Fuel)
table(dep_source_activite$Solides==menage_forme_2010_rehab$dep_Solides)
# table(dep_source_activite$Solides-menage_forme_2010_rehab$dep_bois-menage_forme_2010_rehab$dep_charbon<10^(-12.8)) # les 217 erreurs <e-13
table(dep_source_activite$Urbain==menage_forme_2010_rehab$dep_Urbain)

# Somme des activités par les sources
sum(dep_source_activite[activites])
# [1] 13932919

# Somme des activités par les usages
sum(dep_usage_activite[activites])  
# [1] 13932919

# Somme des dépenses par les usages
sum(rowSums(dep_usage_activite[usages])) # en €
# [1] 13 932 919

# Somme des dépenses par les sources
sum(rowSums(dep_source_activite[sources])) # en €
# [1] 13 932 919




# DEP_ENER ----------------------------------------------------------------


#On a bien vérifié que la somme sur les activités étaient les mêmes dont on excluent les act de dep_usages_activites, 
dep_usage_activite2<-dep_usage_activite%>% select(-activites)

dep_ener<-
  dep_source_activite %>% left_join(dep_usage_activite2,by = c("ident_men"))







# VENTILATION -------------------------------------------------------------

# PRINCIPE : on multiplie les dépenses liées à un usage et une activité
# (le chaffage pour le sommeil) qu'on multiplie par le pourcentage lié à une source d'énergie (gaz et sommeil) : on somme cette quantité pour toutes les activités. La seule exception est ElecSpé et Clim puisque ventiler des dépenses d'ElecSpé sur de l'Urbain ou du Fuel n'a pas de sens


# la quantité d'ElecSpé est retirée de la consommation électrique des ménages
dep_ener$Elec<-dep_source_activite$Elec-dep_usage_activite$ElecSpe-dep_usage_activite$clim-dep_usage_activite$ecl
dep_ener$Elec_ElecSpe<-dep_usage_activite$ElecSpe
dep_ener$Elec_clim<-dep_usage_activite$clim
dep_ener$Elec_ecl<-dep_usage_activite$ecl
usages_bis<-c("ECS","chauff","Cuisson") #exclusion ElecSpe


# pour sommer nécessité d'enlever les "NA" et de les remplacer par des 0
dep_ener[is.na(dep_ener)]<-0
table(is.na(dep_ener))

#fonction inverse qui renvoie 0 si le dénominateur est nul
inverse<-function(x) ifelse(x==0,0,1/x)

# On crée des séries artificielles à zéro pour s'assurer de la bonne exécution des boucles
for (act in activites){
  for (usage in usages){
    usage_act=paste(usage,act,sep="_")
    if (!usage_act %in% colnames(dep_ener)){
      dep_ener[usage_act]<-0
    }
  }
}


for (act in activites){
  source_act_list=c()
  for (source in sources){
    source_act=paste(source,act,sep="_")
    source_act_list=c(source_act_list,source_act)
  }
  dep_ener[act]<-
    rowSums(dep_ener[source_act_list])-
    dep_ener[paste("ElecSpe",act,sep="_")]-
    dep_ener[paste("clim",act,sep="_")]-
    dep_ener[paste("ecl",act,sep="_")]
  
  dep_ener[paste(act,"bis",sep="_")]<-apply(dep_ener[act],1,inverse) #on inverse la quantité pour le calcul du ratio source_act/act
}

# dep_ener$ecl_sommeil<-0
# dep_ener$ecl_repas_hors<-0
# dep_ener$clim_repas_hors<-0



list_source_usage=c("Elec_ElecSpe","Elec_clim","Elec_ecl")
for (usage in usages_bis){
  for (source in sources){
    
    source_usage=paste(source,usage,sep="_")
    
    Table<-dep_ener
    list_table=c()
    
    for (act in activites){
      
      usage_act=paste(usage,act,sep="_")
      source_usage_act=paste(source,usage,act,sep="_")
      source_act=paste(source,act,sep="_")
      
      if(source=="Elec"){
        X<-(Table[source_act]-
              Table[paste("ElecSpe",act,sep="_")]-
              Table[paste("clim",act,sep="_")]-
              Table[paste("ecl",act,sep="_")])*
          Table[paste(act,"bis",sep="_")]
      }
      else{
        X<-Table[source_act]*Table[paste(act,"bis",sep="_")]  
      }
      
      Table[paste(source,act,"ratio",sep="_")]<-X
      
      ifelse(usage_act %in% colnames(Table),
             Table[source_usage_act]<-Table[usage_act]*Table[paste(source,act,"ratio",sep="_")],
             Table[source_usage_act]<-0
      )
      list_table=c(list_table,source_usage_act)
    }
    
    print(source_usage)
    list_source_usage=c(list_source_usage,source_usage)
    dep_ener[source_usage]<-rowSums(Table[list_table])
    
    rm(Table)
    rm(list_table)
    
  }
}


save(list_source_usage,file="Technical_change/TC_renovation_DPE/list_source_usage.RData")

# on rajoute de nouveau ElecSpe au total de l'elec
dep_ener$Elec<-
  dep_ener$Elec+
  dep_usage_activite$ElecSpe+
  +dep_usage_activite$clim+
  +dep_usage_activite$ecl
# on rajoute de nouveau ElecSpe aux totaux d'activites
for (act in activites){
  source_act_list=c()
  for (source in sources){
    source_act=paste(source,act,sep="_")
    source_act_list=c(source_act_list,source_act)
  }
  dep_ener[act]<-rowSums(dep_ener[source_act_list])
}

head(rowSums(dep_ener %>% select(list_source_usage) %>% select(starts_with("Elec"))))
head(dep_ener$Elec)



# VERIFICATION n°3 --------------------------------------------------------


sum(rowSums(dep_ener[list_source_usage]))
sum(rowSums(dep_ener[usages]))
sum(rowSums(dep_ener[sources]))
sum(rowSums(dep_ener[activites]))

# Somme ok : 13932919
for (source in sources){
  print(source)
  print(sum(rowSums(dep_ener[list_source_usage] %>% select(starts_with(source))))==sum(dep_ener[source]))
}
sum(rowSums(dep_ener[list_source_usage] %>% select(starts_with("Elec"))))
# [1] 7028660
sum(dep_ener$Elec)
# [1] 7028660
sum(rowSums(dep_ener[list_source_usage] %>% select(starts_with("Gaz"))))
# [1] 7827271
sum(dep_ener$Gaz)
# [1] 7028660




for (source in sources){
  print(source)
  print(sum(rowSums(dep_ener[source])))
}

 
# > sum(rowSums(dep_ener[list_source_usage]))
# [1] 13932919
# > sum(rowSums(dep_ener[usages]))
# [1] 13932919
# > sum(rowSums(dep_ener[sources]))
# [1] 13932919
# > sum(rowSums(dep_ener[activites]))
# [1] 13932919
# 
# [1] "Elec"
# [1] 7030068
# [1] "Gaz"
# [1] 3584273
# [1] "GPL"
# [1] 643398
# [1] "Fuel"
# [1] 2132795
# [1] "Solides"
# [1] 536834
# [1] "Urbain"
# [1] 8587





# Verif -------------------------------------------------------------------

dep_ener_verif1<-dep_ener %>% select(ident_men, list_source_usage)
for (source in sources){
  dep_ener_verif1[source]<-rowSums(dep_ener_verif1 %>% select(contains(source)))
  print(source)
  print(table(dep_ener_verif1[source]-dep_ener[source]<10^(-5)))
  
  dep_source=paste("dep",source,sep="_")
  print(table(dep_ener[source]-menage_forme_2010_rehab[dep_source]<10^(-9)))
}




# Save --------------------------------------------------------------------

dep_ener_2010<-dep_ener
save(dep_ener_2010,file="2010/dep_ener_2010.RData")

# [old] Màj après renventilation, -----------------------------------------
######"
# Désormais fait dans le fichier 2_1_update_dep_ener_2010.R
######'


# # Mise à jour -------------------------------------------------------------
# 
# dep_ener_reventil <-dep_ener %>% select(ident_men, list_source_usage,sources)
# 
# for (source in sources){
#   dep_source=paste("dep",source,sep="_")
#   factor<-(1+(menage_forme_2010_rehab[dep_source] - menage_forme_2010_rehab[dep_source])/menage_forme_2010_rehab[dep_source])
#   for(col in colnames(dep_ener_reventil %>% select(contains(source)))){
#     print(col)
#     dep_ener_reventil[col]<-
#       dep_ener_reventil[col] * factor
#   }
#  }
# 
# for (x in c(list_source_usage,sources)){dep_ener_reventil[which(is.na(dep_ener_reventil[x])),x]<-0}
# 


# VERIF -------------------------------------------------------------------

# 
# i=1549
# # View(rbind(dep_ener %>% filter(ident_men==i) %>% select(ident_men,list_source_usage,sources),
# #            dep_ener_reventil%>% filter(ident_men==i) %>% select(ident_men,list_source_usage,sources)))
# 
# 
# # Verif 
# # Vérifions que la somme des ener_usage est bien égale aux éner
# # puis que ener de dep_ener_reventil vaut bien menage_forme_2010_rehab dep_source
# 
# dep_ener_reventil_verif1<-dep_ener_reventil %>% select(ident_men, list_source_usage)
# for (source in sources){
#   dep_ener_reventil_verif1[source]<-rowSums(dep_ener_reventil_verif1 %>% select(contains(source)))
#   print(source)
#   print(table(dep_ener_reventil_verif1[source]-dep_ener_reventil[source]<10^(-5)))
#   
#   dep_source=paste("dep",source,sep="_")
#   # print(table(dep_ener_reventil[source]-menage_forme_2010_rehab[dep_source]<10^(-6)))
# }
# 
# for (source in sources){
#   dep_ener_reventil_verif1[source]<-rowSums(dep_ener_reventil_verif1 %>% select(contains(source)))
#   print(source)
#   # print(table(dep_ener_reventil_verif1[source]-dep_ener_reventil[source]<10^(-5)))
#   
#   dep_source=paste("dep",source,sep="_")
#   print(table(dep_ener_reventil[source]-menage_forme_2010_rehab[dep_source]<10^(-6)))
# }
# 
# 
# # Cohérence avec menage_forme_2010_rehab (dep_elec, dep_gaz, etc) => OUI
# table(dep_ener_reventil$Elec-menage_forme_2010_rehab$dep_Elec<10^(-6))
# table(dep_ener_reventil$Gaz-menage_forme_2010_rehab$dep_Gaz<10^(-6))
# table(dep_ener_reventil$GPL-menage_forme_2010_rehab$dep_GPL<10^(-6))
# table(dep_ener_reventil$Fuel==menage_forme_2010_rehab$dep_Fuel)
# table(dep_ener_reventil$Solides==menage_forme_2010_rehab$dep_Solides)
# table(dep_ener_reventil$Urbain==menage_forme_2010_rehab$dep_Urbain)
# 
# 
# # SAVE FILE ---------------------------------------------------------------
# 
# dep_ener_2010<-dep_ener_reventil %>% filter(ident_men %in% menage_forme_2010_rehab$ident_men)
# 
# save(dep_ener_2010,file="2010/dep_ener_2010.RData")
