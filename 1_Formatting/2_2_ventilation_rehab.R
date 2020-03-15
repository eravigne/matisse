
# MAJ 30/09/2019 : ce code n'est plus utilisé, le montant des rénovations 2010 que l'on retire des budgets n'est plus reventilé mais amputé de la consommation, il vient augmenter le flux d'épargne en 2010. 


# LIBRARIES ---------------------------------------------------------------
library(tidyverse)


# FUNCTION ----------------------------------------------------------------

#solde représente le solde budgétaire, positif ou négatif, issu d'un réarrangement 
# du budget pour intégrer le TC
# Vecteur colonne, nombre de ligne correspondant au nombre de ménage de 2025 : ident_men et colonne




Ventil_solde <- function(solde,menage_forme){

  

# DATA --------------------------------------------------------------------
# Import Data
menage_forme_bis<-menage_forme
# Join SOLDE
menage_forme_bis <- menage_forme_bis %>% left_join(solde,by="ident_men")

#Equivalent tu TC de croissance du revenu : ici on regarde le % du RDB qu'on 
# redistribue comme si c'était un % de croissance de ce même RDB
# menage_forme_bis$solde est négatif lorsqu'on améliore l'efficacité énergétique 
# des logements. Un solde négatif veut dire qu'on redistribue une certaine quantité 
# égale à -solde. Si on perd en efficacité énergétique alors le solde est positif 
# (on dépense plus en énergie) et donc on a un effet revenu négatif. On considère donc -solde sur RDB


#-solde*0.5 ; signe changé pour le solde pour raison algorithmique, la moitié seulement est reventilée pour respecter le ratio de financement par la dette (Rcons diminue, et l'épargne négative (ie la dette) augmente d'autant (+0.5*solde))


menage_forme_bis <- menage_forme_bis %>% mutate(new_solde=-solde*0.5)
# menage_forme_bis <- menage_forme_bis %>% mutate(solde=-solde)

# ELASTICITES -------------------------------------------------------------

 # Importer Elasticité
load("Donnees_brutes/Econometrie_demande/elasticite_demande.RData") #Elast

Elast_rev<-
  Elast %>% 
  filter(typ_elast=="rev") %>% 
  select(CODADEME,Typo, Decile, elast) %>%
  spread(key=CODADEME,value=elast)

Elast_rev$Decile <- as.numeric(Elast_rev$Decile)
menage_forme_bis$decucn <- as.numeric(menage_forme_bis$decucn)


# On va rajouter les élasticité sous le nom A01, A02, etc. 
# Il s'agit de supprimer les anciennes références à ces colonnes
list_A=c()
for (i in 1:9){list_A=c(list_A,paste("A0",i,sep=""))}
for (i in 10:14){list_A=c(list_A,paste("A",i,sep=""))}
# list_A
list_elast<-paste("elast_rev",list_A,sep="_")
colnames(Elast_rev)<-c("Typo","Decile",list_elast) 

menage_forme_bis <- 
  menage_forme_bis %>% 
  left_join(Elast_rev,by=c("typo2010f"="Typo","decucn"="Decile"))


list_dep_autres_ener=c("dep_GPL","dep_Fuel","dep_Urbain", "dep_Solides")

list_cat=c("agriculture",
           "elec",
           "gaz_ville",
           "autres_energies_dom",
           "BTP",
           "prod_veh",
           "carb_lubr",
           "transp_rail_air",
           "transp_routes_eau",
           "loisirs_com",
           "autres_services",
           "autres",
           "loyers",
           "veh_occasion",
           "Hors_budget")

#le taux de croissance virtuel du revenu est de -solde/2, on a déjà appliqué cette transformation donc ici c'est juste le "nouveau" solde. Solde est en effet négatif (on économise de l'argent qu'on reventile, par convention cela correspond à un solde négatif). Seuls 50% de la rénovation ont été auto-financé, donc soustrait aux autres postes, c'est ce montant égal à 50% du solde que l'on reventile. 
# A02
menage_forme_bis$dep_Elec<- menage_forme_bis$dep_Elec*(1+ menage_forme_bis$elast_rev_A02*ifelse(menage_forme_bis$RDB==0,0,menage_forme_bis$new_solde/menage_forme_bis$RDB))
# A03
menage_forme_bis$dep_Gaz<- menage_forme_bis$dep_Gaz*(1+ menage_forme_bis$elast_rev_A03*ifelse(menage_forme_bis$RDB==0,0,menage_forme_bis$new_solde/menage_forme_bis$RDB))
# A04
menage_forme_bis[list_dep_autres_ener]<- menage_forme_bis[list_dep_autres_ener] *(1+ menage_forme_bis$elast_rev_A04*ifelse(menage_forme_bis$RDB==0,0,menage_forme_bis$new_solde/menage_forme_bis$RDB))

# Dep_logement
menage_forme_bis$dep_energie_logement<-rowSums(menage_forme_bis[c("dep_Elec","dep_Gaz",list_dep_autres_ener)])

for (i in c(1,5:9)){
  A0I<-paste("elast_rev_A0",i,sep="")
  menage_forme_bis[list_cat[i]]<-  menage_forme_bis[list_cat[i]]*(1+menage_forme_bis[A0I]*ifelse(menage_forme_bis$RDB==0,0,menage_forme_bis$new_solde/menage_forme_bis$RDB))
}

for (i in 10:14){
  AI<-paste("elast_rev_A",i,sep="")
  menage_forme_bis[list_cat[i]]<-  menage_forme_bis[list_cat[i]]*(1+ menage_forme_bis[AI]*ifelse(menage_forme_bis$RDB==0,0,menage_forme_bis$new_solde/menage_forme_bis$RDB))
}



list_dep=c("agriculture",
           "dep_Elec",
           "dep_Gaz",
           "dep_GPL",
           "dep_Fuel",
           "dep_Urbain",
           "dep_Solides",
           "BTP",
           "prod_veh",
           "carb_lubr",
           "transp_rail_air",
           "transp_routes_eau",
           "loisirs_com",
           "autres_services",
           "autres",
           "loyers",
           "veh_occasion",
           "Hors_budget")
menage_forme_bis$Rcons <- rowSums(menage_forme_bis[list_dep])

menage_forme_bis$Delta <- menage_forme_bis$Rcons - (menage_forme$Rcons-menage_forme_bis$new_solde)
# équivaut à Rcons'-Rcons = - new_solde <0 = solde/2 <0
# Rcons diminue de la moitié du montant du solde BTP.
# Un Delta positif indique que les dépenses de consommations après reventilation sont plus
# importantes qu'à l'origine, cet excès doit être retiré du secteur A13, de tous les secteurs 
# au pro rata de leur part budgétaire sinon.
# Ici Rcons doit diminuer avec la reventilation pour faire diminuer la dette de 50% du montant de la rénovation qui est annulée ici. (solde correspond à 100% du montant de la rénovation)

#test
menage_forme_bis %>% filter(ident_men==1549) %>% select(autres,Delta,Rcons,solde,new_solde)
menage_forme %>% filter(ident_men==1549) %>% select(autres,Rcons)


menage_forme_bis$autres<-as.numeric(menage_forme_bis$autres)


menage_forme_bis <-
  menage_forme_bis %>%
  mutate(Beta=0) %>%
  mutate_when(
    Delta<=autres,
    list(autres=autres-Delta),
    Delta>autres,
    list(autres=0,
         Beta=Delta-autres))

#test
menage_forme_bis %>% filter(ident_men==1549) %>% select(autres,Delta,Beta,Rcons,solde,new_solde)
menage_forme %>% filter(ident_men==1549) %>% select(autres,Rcons)

menage_forme_ter<-menage_forme_bis %>% filter(ident_men==1549)

for (dep in list_dep[1:17]){
  menage_forme_bis[paste(dep,"solde",sep="_")] <- menage_forme_bis[dep]-menage_forme_bis[dep]/rowSums(menage_forme_bis[list_dep[1:17]])*menage_forme_bis$Beta
}
# Ne pas changer dans la boucle précédent les dep pour ne pas faire varier le ratio des dépenses en cours de route
for (dep in list_dep[1:17]){
menage_forme_bis[dep] <-menage_forme_bis[paste(dep,"solde",sep="_")] 
}


menage_forme_bis$Rcons <- rowSums(menage_forme_bis[list_dep])
#test
menage_forme_bis %>% filter(ident_men==1549) %>% select(autres,Delta,Beta,Rcons,solde,new_solde)
menage_forme %>% filter(ident_men==1549) %>% select(autres,Rcons)



# Nouvelles parts budgétaires ---------------------------------------------

for (k in list_dep){
  menage_forme_bis[paste("part",k,sep="_")]<-menage_forme_bis[k]/menage_forme_bis$Rcons
}


table(menage_forme_bis$RDB -  menage_forme$RDB)
table(menage_forme_bis$Rcons -  menage_forme$Rcons)

table(menage_forme$RDB-menage_forme$Rcons==menage_forme$epargne)


# Epargne
menage_forme_bis$epargne <- 
  menage_forme_bis$RDB - 
  menage_forme_bis$Rcons + 
  menage_forme_bis$rev_exceptionnel

# taux épargne
menage_forme_bis$taux_epargne <- 
  ifelse(menage_forme_bis$RDB==0,0,menage_forme_bis$epargne/menage_forme_bis$RDB)

table(menage_forme_bis$epargne-menage_forme$epargne)

menage_forme_bis$ratio_S <-  menage_forme_bis$epargne / menage_forme_bis$Rcons  # probleme pour RDB = 0 (ratio_S = -1)


menage_forme_bis<-
  menage_forme_bis %>% 
  select(colnames(menage_forme))

# View(rbind(menage_forme %>% filter(ident_men==1549),menage_forme_bis%>% filter(ident_men==1549) %>% select(colnames(menage_forme))))


menage_forme_bis<<-menage_forme_bis
# return(menage_forme_ter)
rm(menage_forme_bis,Elast,Elast_rev)

}

# SUCCESS -----------------------------------------------------------------

print("1_2_ventilation_rehab : SUCCESS")

