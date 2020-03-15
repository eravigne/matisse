
# OBJECTIF ----------------------------------------------------------------

# Suite à la reventilation des dépenses 2010 de rehabilitation énergétique, les totaux de dépenses ener ont été changés pour environ 200 ménages. 
# Update des dépenses dans dep_ener également. 

# on crée dans dep_ener une nouvelle variable égale à l'ancienne multipliée par la progression due à la reventilation qu'on trouve dans menage_calibr par rapport à menage_forme_rehab. On applique le % de progression (Avant-Après)/Après
# appmen_intensites_bis[sources]<-
#   appmen_intensites*
#   (1+(menage_calibr_2010[dep_sources] - menage_forme_rehab_2010[dep_sources])/menage_forme_rehab_2010[dep_sources])
# 
# for (x in reventil_sources){appmen_intensites[which(is.na(appmen_intensites[x])),x]<-0}


# LIBRARY -----------------------------------------------------------------

library(tidyverse)

# DATA --------------------------------------------------------------------
setwd("D:/CIRED/Projet_Ademe/")

# Avant reventilation des dépenses de rehab 2010
load("2010/dep_ener_2010.RData")
load("2010/menage_forme_2010_rehab.RData")

# Après reventilation des dépenses de rehab 2010
load("2010/menage_forme_2010.RData")

source("Code_global_Ademe/mutate_when.R")

## Source ##
sources=c("Elec", "Gaz", "GPL", "Fuel", "Solides", "Urbain")
dep_sources=paste("dep",sources,sep="_")


# UPDATE ------------------------------------------------------------------

dep_ener_2010_update<-
  dep_ener_2010 %>%
  mutate_at(.,colnames(dep_ener_2010%>% select(starts_with("Elec"))),function(x) x*ifelse(menage_forme_2010_rehab$dep_Elec==0,0,menage_forme_2010$dep_Elec/menage_forme_2010_rehab$dep_Elec))

dep_ener_2010_update<-
  dep_ener_2010_update %>%
  mutate_at(.,colnames(dep_ener_2010%>% select(starts_with("Gaz"))),function(x) x*ifelse(menage_forme_2010_rehab$dep_Gaz==0,0,menage_forme_2010$dep_Gaz/menage_forme_2010_rehab$dep_Gaz))

dep_ener_2010_update<-
  dep_ener_2010_update %>%
  mutate_at(.,colnames(dep_ener_2010%>% select(starts_with("GPL"))),function(x) x*ifelse(menage_forme_2010_rehab$dep_GPL==0,0,menage_forme_2010$dep_GPL/menage_forme_2010_rehab$dep_GPL))

dep_ener_2010_update<-
  dep_ener_2010_update %>%
  mutate_at(.,colnames(dep_ener_2010%>% select(starts_with("Fuel"))),function(x) x*ifelse(menage_forme_2010_rehab$dep_Fuel==0,0,menage_forme_2010$dep_Fuel/menage_forme_2010_rehab$dep_Fuel))

dep_ener_2010_update<-
  dep_ener_2010_update %>%
  mutate_at(.,colnames(dep_ener_2010%>% select(starts_with("Solides"))),function(x) x*ifelse(menage_forme_2010_rehab$dep_Solides==0,0,menage_forme_2010$dep_Solides/menage_forme_2010_rehab$dep_Solides))

dep_ener_2010_update<-
  dep_ener_2010_update %>%
  mutate_at(.,colnames(dep_ener_2010%>% select(starts_with("Urbain"))),function(x) x*ifelse(menage_forme_2010_rehab$dep_Urbain==0,0,menage_forme_2010$dep_Urbain/menage_forme_2010_rehab$dep_Urbain))




# Verification ------------------------------------------------------------

for (source in sources){
  print(source)
print(table((dep_ener_2010_update[source]-rowSums(dep_ener_2010_update %>% select(starts_with(paste(source,"_",sep="")))))<10^(-11)))  
}
# Conclusion : bonne sommation des dépenses désagrégées



# A l'origine on a égalité de dep_ener et de ménage_forme_2010_rehab
for (source in sources){
  print(source)
  dep_source=paste("dep",source,sep="_")
  print(table((dep_ener_2010[source]-menage_forme_2010_rehab[dep_source])<10^(-9)))  
}


# Après update on a égalité de dep_ener et de menage_forme_2010 après reventilation
for (source in sources){
  print(source)
  dep_source=paste("dep",source,sep="_")
  print(table((dep_ener_2010_update[source]-menage_forme_2010[dep_source])<10^(-12)))  
}



# SAVE --------------------------------------------------------------------

dep_ener_2010<-dep_ener_2010_update

save(dep_ener_2010,file="2010/dep_ener_2010.RData")


