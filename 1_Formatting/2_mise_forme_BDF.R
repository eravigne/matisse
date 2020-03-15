# 
# ########## CALCUL AGGREGATS RESSOURCES ET DEPENSES PAR CLASSE DE MENAGES POUR REPONDERATION #########
# 
# Objectif du code : adapter la base BDF et la nomenclature au projet ADEME et THREE_ME, creer des variables calculees


# LIBRARY -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(car)
library(plyr)
library(reshape2)



# Load --------------------------------------------------------------------

setwd("D:/CIRED/Projet_Ademe")

#Donnees Brutes BDF 2010
menage<-as.data.frame(read_excel("Donnees_brutes/BDF_2010/menage.xlsx"),stringsAsFactors=F)
individu<-read_excel("Donnees_brutes/BDF_2010/individu.xlsx")
c05<-read_csv2("Donnees_brutes/BDF_2010/c05.csv")


#BDFE S. De Lauretis
load("Donnees_brutes/IMACLIM_SIMONA/Base/appmen_depensesactiv_2010.RData")
appmen_depensesactiv_2010<-appmen_depensesactiv
rm(appmen_depensesactiv)

load("2010/menage_DPE.RData")


decucn_2010<-read_excel("Donnees_brutes/Econometrie_demande/decucn_2010.xlsx")
decucn_2010 <- 
  decucn_2010 %>% 
  mutate(ident_men=as.integer(ident_men))



# Exclusion DOM -----------------------------------------------------

menage <-
  menage %>% 
  filter(zeat>0)




# Selection ménages -------------------------------------------------------

menage$ident_men <- as.numeric(menage$ident_men)

#Selection des ménages présents dans la base appmen de Simona pour bénéficier des données énergies EDF
menage <- menage[which(menage$ident_men %in% menage_DPE$ident_men),]
appmen_depensesactiv_2010 <- appmen_depensesactiv_2010[which(appmen_depensesactiv_2010$ident_men %in% menage$ident_men),]

# Selection des ménages dans base c05 
c05$ident_men <-  as.numeric(c05$ident_men)
c05 <- c05[which(c05$ident_men %in% menage$ident_men),]

# Selection des ménages dans base individus
individu$ident_men <-  as.numeric(individu$ident_men)
individu <- individu[which(individu$ident_men %in% menage$ident_men),]








# RECODER VARIABLES ---------------------------------------------------------------
# creation variables type menage corrige, quintile UC et maison individuelle pour recreer classes

# TYPMEN
menage <- within(menage, {
  typmen_corr <- ifelse(typmen5 == 1 & agepr <= 65, 1,         
                        # celib <= 65
                        ifelse(typmen5 == 1 & agepr > 65, 2,   
                               # celib > 65
                               ifelse(typmen5 == 2, 5,                
                                      # famille monoparentale
                                      ifelse(typmen5 == 3 & agepr <= 65, 3,  
                                             # couple sans enfants, <=65
                                             ifelse(typmen5 == 3 & agepr > 65, 4,   
                                                    # couple sans enfants, > 65
                                                    ifelse(typmen5 == 4, 6,                
                                                           # couple avec enfants
                                                           ifelse(typmen5 == 5 & nenfants > 0, 6, 
                                                                  # mÃ©nages complexes : 6,3 ou 4
                                                                  ifelse(typmen5 == 5 & nenfants == 0 & agepr <= 65, 3,
                                                                         ifelse(typmen5 == 5 & nenfants == 0 & agepr > 65, 4,
                                                                                NA
                                                                         )))))))))
})


# QUINTILE UC

menage<-menage %>% left_join(decucn_2010,by="ident_men")
menage$quintileuc <- car::recode(menage$decucn," 1:2 = 1 ; 3:4 = 2 ; 5:6 = 3 ; 7:8 = 4 ; 9:10 = 5 ")


# TYPLOG / MI
menage$MI_corr <- car::recode(menage$typlog, "1:2 = 1 ; 3:6 = 0")










# AGREGER VARIABLES INDIVIDUS ---------------------------------------------

#A partir de la table individus, calcul du nombre de retraités, de chômeurs => l'objectif est de corriger les donnes de la table menage dont les variables nactifs et nactoccup ne sont pas fiables (nombre negatif de chomeurs en les sommant)

# CHOMEURS
nbchomeurs <- 
  ddply(.data = individu, .variables = ~ ident_men, .fun = function(x) sum(x$situa == 4)) ; names(nbchomeurs) <- c("ident_men","nbchomeurs")


#ACTIFS OCCUPES
nbactoccup <- ddply(.data = individu, .variables = ~ ident_men, .fun = function(x) sum(x$situa == 1 | x$situa == 2)) ; names(nbactoccup) <- c("ident_men","nbactoccup")


# ACTIFS
nbactifs <- 
  nbchomeurs %>% 
  left_join(nbactoccup,by="ident_men") %>%
  mutate(nbactifs=nbchomeurs + nbactoccup) %>%
  select(-nbactoccup,-nbchomeurs)


# RETRAITES
nbretraites <- ddply(.data = individu, .variables = ~ ident_men, .fun = function(x) sum(x$situa == 5)) ; names(nbretraites) <- c("ident_men","nbretraites")







# CREATION MENAGE_FORME ---------------------------------------------------
# menage_forme regroupe l'essentiel des variables utilisées dans la suite du code

menage_forme <-
  menage %>% 
  select(ident_men, pondmen, quintileuc,typmen_corr,MI_corr,npers,coeffuc,chomage,retraites,decucn) %>%
  left_join(nbchomeurs, by="ident_men") %>%
  left_join(nbactoccup, by="ident_men") %>%
  left_join(nbactifs, by="ident_men") %>%
  left_join(nbretraites, by="ident_men") %>%
  left_join(appmen_depensesactiv_2010 %>% select(ident_men,surfhab_d,tuu), by="ident_men")


# calcul nombre inactifs
menage_forme$nbinact <- 
  menage_forme$npers - 
  menage_forme$nbretraites - 
  menage_forme$nbactifs

#rajout du nombre de chômeurs et de retaités dans menage
menage <- menage %>% left_join(.,nbchomeurs,by="ident_men") %>%
  left_join(nbactoccup,by="ident_men") %>% 
  left_join(nbretraites,by="ident_men")


# Rajout nombre de véhicule 
auto<-read_excel("Donnees_brutes/BDF_2010/AUTOMOBILE.xlsx",sheet="AUTO_METROPOLE")
menage_forme <-
  menage_forme %>%
  left_join(auto %>% select(ident_men, nbvehic) %>% distinct(),by="ident_men")
rm(auto)







# # CORRECTIONS PRERETRAITES -----------------------------------------------------------------
# 
# #Rectification des revenus retraites et chômages => on veut compter les pré-retraites dans la retraite et pas dans le chômage (voir rapport Ademe + notes S. de Lauretis 26/08/16))
# 
# menage_forme[which(menage$nbchomeurs == 0 & menage$nbretraites > 0), "retraites"] <-
#   menage[which(menage$nbchomeurs == 0 & menage$nbretraites > 0), "chomage"] +
#   menage[which(menage$nbchomeurs == 0 & menage$nbretraites > 0), "retraites"]
# 
# 
# menage_forme[which(menage$nbchomeurs > 0 & menage$nbretraites == 0), c("chomage","retraites")] <-
#   menage[which(menage$nbchomeurs > 0 & menage$nbretraites == 0), c("chomage","retraites")]
# 
# 
# menage_forme[which(menage$nbchomeurs == 0 & menage$nbretraites == 0), c("chomage","retraites")] <-
#   menage[which(menage$nbchomeurs == 0 & menage$nbretraites == 0), c("chomage","retraites")] # chÃ´meurs ayant repris l'activitÃ© dans l'annÃ©e !?
# 
# 
# menage_forme[which(menage$nbchomeurs > 0 & menage$nbretraites > 0), c("chomage","retraites")] <-
#   menage[which(menage$nbchomeurs > 0 & menage$nbretraites > 0), c("chomage","retraites")]
# 
# # cas particulier
# menage_forme[which(menage$nbchomeurs > 0 & menage$nbretraites > 0 &
#                      menage$retraites == 0 & menage$chomage > 0), "chomage"] <-
#   with(menage[which(menage$nbchomeurs > 0 & menage$nbretraites > 0 &
#                       menage$retraites == 0 & menage$chomage > 0), ],
#        chomage * nbchomeurs/(nbchomeurs + nbretraites)
#   )
# 
# menage_forme[which(menage$nbchomeurs > 0 & menage$nbretraites > 0 &
#                      menage$retraites == 0 & menage$chomage > 0), "retraites"] <-
#   with(menage[which(menage$nbchomeurs > 0 & menage$nbretraites > 0 &
#                       menage$retraites == 0 & menage$chomage > 0), ],
#        chomage * nbretraites/(nbchomeurs + nbretraites)
#   )
# 
# 





# REVENUS -----------------------------------------------------------------

#categories de revenus definies dans excel
def_rev<- 
  read_excel("Donnees_brutes/Nomenclature_CIRED_ADEME/Definition_revenus.xlsx")
# View(def_rev)


# Revenus de l'activité salariale et/ou independante + revenus de l'étranger
menage_forme$rev_activites <- 
  rowSums(menage[intersect(names(menage),def_rev[which(def_rev$REV_CAT=="REVACT"),]$rev)])

# Revenus de l'activité salariale et/ou independante SANS revenus de l'étranger 
menage_forme$rev_activites_sans_etranger <- 
  rowSums(menage[c("salaires","revindep")])


# Revenus de l'étranger (inclus dans revact)
menage_forme$rev_etranger <- 
  rowSums(menage[intersect(names(menage),def_rev[which(def_rev$REV_CAT=="REVETRANGER"),]$rev)])

# Revenus exceptionnels
menage_forme$rev_exceptionnel <- rowSums(menage[intersect(names(menage),def_rev[which(def_rev$REV_CAT=="REVEXC"),]$rev)])

# Revenus du patrimoine
menage_forme$rev_patrimoine <- rowSums(menage[intersect(names(menage),def_rev[which(def_rev$REV_CAT=="REVPAT"),]$rev)])

# Revenus sociaux (tous)
menage_forme$rev_sociaux <- rowSums(menage[intersect(names(menage),def_rev[which(def_rev$REV_CAT=="REVSOC"),]$rev)])

# Revenus sociaux (excl chômage pour calage)
menage_forme$rev_sociaux_autres <- rowSums(menage[intersect(names(menage),def_rev[which(def_rev$REV_CAT=="REVSOC_AUTRES"),]$rev)])

# Revenus pour le RDB
menage_forme$rev700<-menage$rev700
menage_forme$rev701<-menage$rev701
menage_forme$rev999<-menage$rev999
# Revenus pour le RDB_macro (RDB_new)
menage_forme$rev801<-menage$rev801


menage_forme$rev_TCO<-0

# #Revenus totaux 
# # (revact + revsoc + revpat + rev700 +rev 701 + rev 999)
menage_forme$RDBAI <- rowSums(menage_forme[c("rev_activites","rev_patrimoine","rev_sociaux","rev700","rev701","rev999")])







# MISE EN FORME DEPENSES ENERGIE --------------------------------------------------------

#les depenses energetiques ne proviennent pas de BDF mais de l'ENL 2013. 
# On les importe donc de appmen_depensesactiv_2010 : base appairée BDFE (de Lauretis, 2017)

#A02
menage_forme$dep_Elec <- rowSums(appmen_depensesactiv_2010[grep("Elec_", names(appmen_depensesactiv_2010))])
#A03
menage_forme$dep_Gaz <- rowSums(appmen_depensesactiv_2010[grep("Gaz_", names(appmen_depensesactiv_2010))])
#A04
menage_forme$dep_GPL <- rowSums(appmen_depensesactiv_2010[grep("GPL_", names(appmen_depensesactiv_2010))])
menage_forme$dep_Fuel <-rowSums(appmen_depensesactiv_2010[grep("Fuel_", names(appmen_depensesactiv_2010))])
menage_forme$dep_Urbain <- rowSums(appmen_depensesactiv_2010[grep("Urbain_", names(appmen_depensesactiv_2010))])
menage_forme$dep_Solides <- rowSums(appmen_depensesactiv_2010[grep("Solides_", names(appmen_depensesactiv_2010))]) 


sources=c("Elec","Gaz","Urbain","Solides","GPL","Fuel")
dep_sources=paste("dep",sources,sep="_")
# Variable calculée
menage_forme$dep_energie_logement <- rowSums(menage_forme[dep_sources])





# MISE FORME DEPENSES - Nomenclature ADEME ---------------------------------------------------------

Nomenclature_ADEME_COICOP <-
  read_excel("Donnees_brutes/Nomenclature_CIRED_ADEME/Nomenclature_coicop_threeme.xlsx")



#A01
menage_forme$agriculture <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A01"),]$COICOP_2011)])

# Pas d'intérêt à sommer les dépenses BDF d'énergie, on utilise les données ENL
# menage_forme$elec <- 
#   rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A02"),]$COICOP_2011)])
# 
# menage_forme$gaz_ville <- 
#   rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A03"),]$COICOP_2011)])

# menage_forme$autres_energies_dom <-
#   rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A04"),]$COICOP_2011)])


#A05
menage_forme$BTP <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A05"),]$COICOP_2011)])

#A06
menage_forme$prod_veh <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A06"),]$COICOP_2011)])

#A07
menage_forme$carb_lubr <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A07"),]$COICOP_2011)])

#A08
menage_forme$transp_rail_air <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A08"),]$COICOP_2011)])

#A09
menage_forme$transp_routes_eau <- rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A09"),]$COICOP_2011)])

#A10
menage_forme$loisirs_com <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A10"),]$COICOP_2011)])

#A11
menage_forme$autres_services <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A11"),]$COICOP_2011)])

#A12
menage_forme$autres <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A12"),]$COICOP_2011)])

#A13
menage_forme$loyers <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A13"),]$COICOP_2011)])

#A14
menage_forme$veh_occasion <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A14"),]$COICOP_2011)])


#Hors postes dépenses
menage_forme$Hors_budget <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="Hors_budget"),]$COICOP_2011)])

menage_forme<-
  menage_forme%>%
  left_join(c05%>%select(c13711,ident_men),by="ident_men")

# Dans le Hors Budget on trouve 
# *Stupéfiants
# *Autres dépenses occasionnées par une cérémonie
# *Dépenses SAI des personnes vivant hors du domicile au moins un jour par semaine
# *Remboursements de prêts pour la résidence principale (yc garage et dépendance)
# *Remboursements des autres prêts immobiliers (résidence secondaire et autre logement yc dépendance)
# *Aides et dons (occasionnels ou réguliers) en argent offerts par le ménage et pensions alimentaires
# *Remboursements de crédits à la consommation (voiture, gros travaux, biens durables)
# *Prélèvements de l'employeur
# *Achats de logements, garages, parkings, box et terrains
# *Epargne salariale : achat d’actions de l’entreprise








# VARIABLES CALCULEES MENAGES ---------------------------------------------------------------

#IR
menage_forme$impot_revenu <- 
  c05$c13141

# Autres impôts directs (AID)
menage_forme$AID <- rowSums(c05[c("c13111","c13121","c13151","c13161")])

# Revenu Brut Disponible (RDB) 
menage_forme$RDB <- menage_forme$RDBAI - rowSums(menage_forme[c("impot_revenu","AID")])

# Taux d'imposition
menage_forme$taux_IR<-ifelse(menage_forme$RDB==0,0,
  menage_forme$impot_revenu/menage_forme$RDBAI)
menage_forme$taux_AID<-ifelse(menage_forme$RDB==0,0,
  menage_forme$AID/menage_forme$RDBAI)
# (menage_forme$RDB==0,0,  menage_forme$AID/menage_forme$RDBAI)
# 




# TYPOLOGIE VULNERABILITE -------------------------------------------------
# Rajout des typologies de vulnérabilité dans la base menage_forme

typo_vuln<-read_excel("Donnees_brutes/CIRED/datacp_typovuln_bdf_2011.xlsx",sheet="identmen")

typo_vuln <- typo_vuln %>% 
  separate(col="IDENTMEN", into=c("year","ident_men"),sep="2011")
typo_vuln$ident_men<-as.numeric(typo_vuln$ident_men)

# SELECTION MENAGES
typo_vuln_bis<- typo_vuln %>% filter(ident_men %in% menage_forme$ident_men)
# rajout typo
menage_forme<-menage_forme%>% left_join(.,typo_vuln_bis[c("ident_men","typo2010f")],by="ident_men")
rm(typo_vuln,typo_vuln_bis)





# Suppression des bases superflues ----------------------------------------

rm(nbactoccup,nbchomeurs,nbretraites,Nomenclature_ADEME_COICOP,def_rev,individu)


# PARTS BUDGETAIRES excl Hors Budget -------------------------------------------------------


list_dep=c("agriculture",
           "dep_Elec",
           "dep_Gaz",
           "dep_autres_energies",
           "BTP",
           "prod_veh",
           "carb_lubr",
           "transp_rail_air",
           "transp_routes_eau",
           "loisirs_com",
           "autres_services",
           "autres",
           "loyers",
           "veh_occasion"
           )
menage_forme_bis<- menage_forme %>% mutate(dep_autres_energies=dep_Fuel+dep_GPL+dep_Solides+dep_Urbain)
menage_forme_bis$Rcons <- rowSums(menage_forme_bis[list_dep])
menage_forme_bis$Rcons_bis <- rowSums(menage_forme_bis[list_dep])+menage_forme_bis$rev801




for (i in 1:14){
  k=list_dep[i]
  print(k)
  if(i<10){
  menage_forme[paste("share_A0",i,sep="")]<-menage_forme_bis[k]/menage_forme_bis$Rcons_bis}
  else{ menage_forme[paste("share_A",i,sep="")]<-menage_forme_bis[k]/menage_forme_bis$Rcons_bis}
}
menage_forme$share_A13=(menage_forme_bis$loyers+menage_forme_bis$rev801)/menage_forme_bis$Rcons_bis
rm(menage_forme_bis)


# PARTS BUDGETAIRES incl Hors Budget -------------------------------------------------------


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
menage_forme$Rcons <- rowSums(menage_forme[list_dep])


for (k in list_dep){
  menage_forme[paste("part",k,sep="_")]<-menage_forme[k]/menage_forme$Rcons
}


# Epargne
menage_forme$epargne <- 
  menage_forme$RDB - 
  menage_forme$Rcons + 
  menage_forme$rev_exceptionnel

# Taux Epargne
menage_forme$taux_epargne <- ifelse(menage_forme$RDB==0,0,
  menage_forme$epargne/menage_forme$RDB)

#Ratio_S
menage_forme$ratio_S <- ifelse(menage_forme$Rcons==0,0,
  menage_forme$epargne/menage_forme$Rcons)  # probleme pour RDB = 0 (ratio_S = -1)


menage_forme <- menage_forme[which(menage_forme$ratio_S < -1.01 | menage_forme$ratio_S > -0.99),]
menage_forme <- menage_forme[which(menage_forme$RDB < -5 | menage_forme$RDB > 5),] # on elimine les ménages ayant RDB = 0 (ou proche de 0)

test1<-menage_forme
menage_forme_2010_rehab<-menage_forme
save(menage_forme_2010_rehab,file="2010/menage_forme_2010_rehab.RData")


source("Mise_forme_BDF/3_mise_forme_energies.R")


# DESAGREGER GROS TRAVAUX -------------------------------------------------

source("Mise_forme_BDF/2_1_desagregation_reno_rehab.R")


solde<-
  Gros_travaux_2010 %>% 
  select(ident_men) %>%
  mutate(solde=-Gros_travaux_2010$GT_REHAB) #solde négatif car Gros_travaux_2010$GT_REHAB>0 et on veut que le solde se comporte comme une économie

rm(Gros_travaux_2010)

menage_forme$BTP <- 
  menage_forme$BTP + 
  solde$solde

save(menage_forme,file="2010/menage_forme_essai.RData")
save(solde,file="2010/solde.RData")

menage_forme$decucn<-as.numeric(as.character(menage_forme$decucn))


#Les gros travaux sont directement retirés de la consommation et viennent augmenter le flux d'épargne. 
# Le fichier 2_2_ventilation est obsolète. 

##
##
## /!\ à vérifier tout de même que les agrégats tombent justes pour certains ménages typiques. 
##
##
##
##


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
menage_forme$Rcons <- rowSums(menage_forme[list_dep])


#pour test 
menage_forme_bis<-menage_forme

for (k in list_dep){
  menage_forme[paste("part",k,sep="_")]<-menage_forme[k]/menage_forme$Rcons
}


# Epargne
menage_forme$epargne <- 
  menage_forme$RDB - 
  menage_forme$Rcons + 
  menage_forme$rev_exceptionnel

# taux épargne
menage_forme$taux_epargne <- 
  ifelse(menage_forme$RDB==0,0,menage_forme$epargne/menage_forme$RDB)


menage_forme$ratio_S <-  menage_forme$epargne / menage_forme$Rcons  # probleme pour RDB = 0 (ratio_S = -1)



# 
# source("Mise_forme_BDF/2_2_ventilation_rehab.R")
# 
# Ventil_solde(solde,menage_forme)
# # renvoie menage_forme_bis
# test2<-menage_forme_bis
# 
# # # menage 1221 : on annule completement ses dépenses de gros travaux. Reventilation de plus de 8000€.
# # # menage 1549 : partage entre REHAB et RENO (81% rehab, 19% reno), reventilation de 35000€. 
# # # menage 7792 : Delta de 45373, Beta de 44208
# #  # menage 12815 : Beta de 96027
# i=12815
# # ccl : aucune dépense ne devient négative. Principe ok. 
# 
# 
# ### TEST 
# # décommenter
# # View(cbind(c("init","final","final - init"),
#   # rbind(test1 %>% filter(ident_men==i),test2 %>% filter(ident_men==i),test2%>% filter(ident_men==i)-test1%>% filter(ident_men==i))))


# Ajout DPE ---------------------------------------------------------------

menage_forme_bis <-
  menage_forme_bis %>%
  left_join(menage_DPE,by="ident_men")


# Taux effort financier ---------------------------------------------------

c05_forme_2010 <- c05[which(c05$ident_men %in% menage_forme$ident_men),]


menage_forme_bis <- 
  menage_forme_bis %>% mutate(taux_effort_financier=(c05_forme_2010$c13211+c05_forme_2010$c13221+c05_forme_2010$c13511+c05_forme_2010$c12611)/RDB) %>% mutate_when(taux_effort_financier<0,list(taux_effort_financier=0))


# Dépenses contraintes ----------------------------------------------------

#on laisse les c05 des dépenses contraintes séparées pour pouvoir imputer l'augmentation de la part des dépenses contraintes à une catégorie en particulier

dep_preeng_code<-read_excel("D:/CIRED/Projet_Ademe/Donnees_brutes/Nomenclature_CIRED_ADEME/nomenclature_dep_preeng_coicop5.xlsx",sheet="nomenclature_code",col_names = T)



c05_dep_preeng<-c05_forme_2010 %>% select(ident_men,dep_preeng_code$COICOP)


menage_forme_bis <-
  menage_forme_bis %>%
  left_join(c05_dep_preeng,by="ident_men")
 
 
# SAVE FILE ---------------------------------------------------------------

# Création menage_forme_2010
menage_forme_2010 <-menage_forme_bis
appmen_depensesactiv_2010 <- appmen_depensesactiv_2010[which(appmen_depensesactiv_2010$ident_men %in% menage_forme$ident_men),]


# SAVE
save(menage_forme_2010,file="2010/menage_forme_2010.RData")
save(appmen_depensesactiv_2010,file="2010/appmen_depensesactiv_2010.RData")
save(c05_forme_2010,file="2010/c05_forme_2010.RData")

source("Mise_forme_BDF/3_1_update_dep_ener_2010.R")


# Parts_budgétaires -------------------------------------------------------

#NB : hors budget exclus, parts budgétaires sur les 14 postes de la nomenclature, 
source("Code_Global_Ademe/compute_share.R")
share_2010<-compute_share(menage_forme_2010)
share_2010



# SUCCESS -----------------------------------------------------------------

print("2_mise_forme_BDF : SUCCESS")

