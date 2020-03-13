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
menage<-as.data.frame(read_excel("Donnees_brutes/BDF_2010/menage.xlsx"))
individu<-read_excel("Donnees_brutes/BDF_2010/individu.xlsx")
c05<-read_csv2("Donnees_brutes/BDF_2010/c05.csv")


#BDFE S. De Lauretis
load("Donnees_brutes/IMACLIM_SIMONA/Base/appmen_depensesactiv_2010.RData")
appmen_depensesactiv_2010<-appmen_depensesactiv
rm(appmen_depensesactiv)



# Exclusion DOM -----------------------------------------------------

menage<-menage %>% filter(zeat>0)
menage$ident_men <- as.numeric(menage$ident_men)
menage <- menage[which(menage$ident_men %in% appmen_depensesactiv_2010$ident_men),]


appmen_depensesactiv_2010 <- appmen_depensesactiv_2010[which(appmen_depensesactiv_2010$ident_men %in% menage$ident_men),]

c05$ident_men <-  as.numeric(c05$ident_men)
c05 <- c05[which(c05$ident_men %in% appmen_depensesactiv_2010$ident_men),]

individu$ident_men <-  as.numeric(individu$ident_men)
individu <- individu[which(individu$ident_men %in% appmen_depensesactiv_2010$ident_men),]





# Variables ---------------------------------------------------------------

# creation variables type menage corrige, quintile UC et maison individuelle pour recreer classes

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

menage$quintileuc <- recode(menage$decuc1," 1:2 = 1 ; 3:4 = 2 ; 5:6 = 3 ; 7:8 = 4 ; 9:10 = 5 ")

menage$MI_corr <- recode(menage$typlog, "1:2 = 1 ; 3:6 = 0")

#Dans appmen_depensesactiv (déjà fait)
# appmen_depensesactiv_2010$typmen_corr <- recode(appmen_depensesactiv_2010$typmen_corr_old, "2=1 ; 7=2 ; 3=3 ; 6=4 ; 4=5 ; 5=6")


#A partir de la table individus, calcul du nombre de retraités, de chômeurs => l'objectif est de corriger les donnes de la table menage dont les variables nactifs et nactoccup ne sont pas fiables (nombre negatif de chomeurs en les sommant)

nbchomeurs <- ddply(.data = individu, .variables = ~ ident_men, .fun = function(x) sum(x$situa == 4)) ; names(nbchomeurs) <- c("ident_men","nbchomeurs")
nbactoccup <- ddply(.data = individu, .variables = ~ ident_men, .fun = function(x) sum(x$situa == 1 | x$situa == 2)) ; names(nbactoccup) <- c("ident_men","nbactoccup")
nbactifs <- nbchomeurs$nbchomeurs + nbactoccup$nbactoccup
nbretraites <- ddply(.data = individu, .variables = ~ ident_men, .fun = function(x) sum(x$situa == 5)) ; names(nbretraites) <- c("ident_men","nbretraites")



# menage_forme -----------------------------------------------------------

menage_forme <- cbind(
    menage[c("ident_men","pondmen","quintileuc","typmen_corr","MI_corr","npers","coeffuc")],
    nbchomeurs["nbchomeurs"],
    nbactoccup["nbactoccup"],
    nbactifs,
    nbretraites["nbretraites"],
    appmen_depensesactiv_2010[c("surfhab_d","decuc1")])

menage_forme$nbinact <- menage_forme$npers - 
  menage_forme$nbretraites - 
  menage_forme$nbactifs

#rajout du nombre de chômeurs et de retaités dans menage
menage <- menage %>% left_join(.,nbchomeurs) %>%
  left_join(nbactoccup) %>% 
  left_join(nbretraites)


# REVENUS -----------------------------------------------------------------

#Rectification des revenus retraites et chômages => on veut compter les pré-retraites dans la retraite et pas dans le chômage (voir notes S. de Lauretis 26/08/16))

menage_forme[which(menage$nbchomeurs == 0 & menage$nbretraites > 0), "retraites"] <- 
  menage[which(menage$nbchomeurs == 0 & menage$nbretraites > 0), "chomage"] +
  menage[which(menage$nbchomeurs == 0 & menage$nbretraites > 0), "retraites"]


menage_forme[which(menage$nbchomeurs > 0 & menage$nbretraites == 0), c("chomage","retraites")] <- 
  menage[which(menage$nbchomeurs > 0 & menage$nbretraites == 0), c("chomage","retraites")]


menage_forme[which(menage$nbchomeurs == 0 & menage$nbretraites == 0), c("chomage","retraites")] <- 
  menage[which(menage$nbchomeurs == 0 & menage$nbretraites == 0), c("chomage","retraites")] # chÃ´meurs ayant repris l'activitÃ© dans l'annÃ©e !?


menage_forme[which(menage$nbchomeurs > 0 & menage$nbretraites > 0), c("chomage","retraites")] <- 
  menage[which(menage$nbchomeurs > 0 & menage$nbretraites > 0), c("chomage","retraites")] 

# cas particulier
menage_forme[which(menage$nbchomeurs > 0 & menage$nbretraites > 0 & 
                      menage$retraites == 0 & menage$chomage > 0), "chomage"] <- 
  with(menage[which(menage$nbchomeurs > 0 & menage$nbretraites > 0 & 
                             menage$retraites == 0 & menage$chomage > 0), ],
       chomage * nbchomeurs/(nbchomeurs + nbretraites)
  )

menage_forme[which(menage$nbchomeurs > 0 & menage$nbretraites > 0 & 
                      menage$retraites == 0 & menage$chomage > 0), "retraites"] <- 
  with(menage[which(menage$nbchomeurs > 0 & menage$nbretraites > 0 & 
                             menage$retraites == 0 & menage$chomage > 0), ],
       chomage * nbretraites/(nbchomeurs + nbretraites)
  )


#categories de revenus definies dans excel
def_rev<-read_excel("Donnees_brutes/Definition_revenus.xlsx")

#sommer les revenus de chaque catégorie
menage_forme$rev_activites <- rowSums(menage[intersect(names(menage),def_rev[which(def_rev$REV_CAT=="REVACT"),]$rev)])

menage_forme$rev_indep <- rowSums(menage[intersect(names(menage),def_rev[which(def_rev$REV_CAT=="REVINDEP"),]$rev)])

menage_forme$rev_exceptionnel <- rowSums(menage[intersect(names(menage),def_rev[which(def_rev$REV_CAT=="REVEXC"),]$rev)])

menage_forme$rev_patrimoine <- rowSums(menage[intersect(names(menage),def_rev[which(def_rev$REV_CAT=="REVPAT"),]$rev)])

menage_forme$rev_sociaux <- rowSums(menage[intersect(names(menage),def_rev[which(def_rev$REV_CAT=="REVSOC"),]$rev)])

menage_forme$rev_totaux <- rowSums(menage[intersect(names(menage),def_rev[which(def_rev$REV_CAT=="REVTOT"),]$rev)])


### SELECTIONNER MENAGES PRESENTS DANS BASE APPARIEE ------
menage_forme$ident_men <- as.numeric(menage_forme$ident_men)
menage_forme <- menage_forme[which(menage_forme$ident_men %in% appmen_depensesactiv_2010$ident_men),]

c05$ident_men <-  as.numeric(c05$ident_men)
c05 <- c05[which(c05$ident_men %in% appmen_depensesactiv_2010$ident_men),]

menage$ident_men <- as.numeric(menage$ident_men)
menage <- menage[which(menage$ident_men %in% appmen_depensesactiv_2010$ident_men),]



# DEPENSES ENERGIE --------------------------------------------------------

#les depenses energetiques ne proviennent pas de BDF mais de l'ENL 2013. On les importe donc de appmen_depensesactiv_2010 : base appairée BDFE


menage_forme$dep_Elec <- rowSums(appmen_depensesactiv_2010[grep("Elec_", names(appmen_depensesactiv_2010))])
menage_forme$dep_Gaz <- rowSums(appmen_depensesactiv_2010[grep("Gaz_", names(appmen_depensesactiv_2010))])
menage_forme$dep_GPL <- rowSums(appmen_depensesactiv_2010[grep("GPL_", names(appmen_depensesactiv_2010))])
menage_forme$dep_Fuel <-rowSums(appmen_depensesactiv_2010[grep("Fuel_", names(appmen_depensesactiv_2010))])
menage_forme$dep_Urbain <- rowSums(appmen_depensesactiv_2010[grep("Urbain_", names(appmen_depensesactiv_2010))])
menage_forme$dep_Solides <- rowSums(appmen_depensesactiv_2010[grep("Solides_", names(appmen_depensesactiv_2010))]) 

sources=c("Elec","Gaz","Urbain","Solides","GPL","Fuel")
dep_sources=paste("dep",sources,sep="_")


menage_forme$dep_energie_logement <- rowSums(menage_forme[dep_sources])



# menage_forme$dep_elec <- rowSums(appmen_depensesactiv_2010[grep("Elec_", names(appmen_depensesactiv_2010))])
# menage_forme$dep_gaz <- rowSums(appmen_depensesactiv_2010[grep("Gaz_", names(appmen_depensesactiv_2010))])
# menage_forme$dep_GPL_fioul <- rowSums(appmen_depensesactiv_2010[grep("GPL_", names(appmen_depensesactiv_2010))]) +
#   rowSums(appmen_depensesactiv_2010[grep("Fuel_", names(appmen_depensesactiv_2010))])
# menage_forme$dep_chaleur <- rowSums(appmen_depensesactiv_2010[grep("Urbain_", names(appmen_depensesactiv_2010))])
# # (NB dans fichier INSEE conso mÃ©nages par produit, en 2010, Produits sylvicoles ccm 691 Mâ¬, AgglomÃ©rÃ©s et briquettes de houille 58 Meuros)
# menage_forme$dep_bois <- 691/(691+58)*rowSums(appmen_depensesactiv_2010[grep("Solides_", names(appmen_depensesactiv_2010))]) #on va le mettre dans le composite
# menage_forme$dep_charbon <- 58/(691+58)*rowSums(appmen_depensesactiv_2010[grep("Solides_", names(appmen_depensesactiv_2010))])

# menage_forme$dep_energie_logement <- rowSums(menage_forme[c("dep_elec","dep_gaz","dep_GPL_fioul","dep_chaleur","dep_charbon","dep_bois")])
# 
# menage_forme$dep_carburants <- c05$c07221 # ceci inclut Ã©lectricitÃ©, huiles et lubrifiants


# Note méthodo : pour utiliser les élasticité, il faudra se rapporter aux catégories, A02, A03 et A04 pour les variations de parts budgétaires en agrégeant les dépenses non gaz et élec. Grosses différences entre les données BDF et ENL, les dépenses en autres_energies_domestiques sont quasi toujours nulles alors que l'ENL trouve des valeurs non négatives. Pour les analyses on privilégiera les valeurs commençant par "dep_". 


# Nomenclature ADEME ---------------------------------------------------------

Nom <-read_excel("Donnees_brutes/Nomenclature_coicop_threeme.xlsx")

# Supression des doublons dans la nomenclature
doublons <- which(duplicated(Nom$COICOP_2011)) 
Nomenclature_ADEME_COICOP<-as.data.frame(Nom[-doublons,]) 


#dans la nomenclature on a attribué c04500 à A03 or il s'agit des factures indissociables des factures de gaz et élec, mais pour se rapprocher le plus possibles de valeurs EDF et CEREN on l'attribue au gaz (vieux contrats ou le chauffage se fait principalement au gaz)

#sommer les revenus de chaque catégorie
menage_forme$agriculture <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A01"),]$COICOP_2011)])

# menage_forme$elec <- 
#   rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A02"),]$COICOP_2011)])
# 
# menage_forme$gaz_ville <- 
#   rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A03"),]$COICOP_2011)])

# menage_forme$autres_energies_dom <-
#   rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A04"),]$COICOP_2011)])

menage_forme$BTP <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A05"),]$COICOP_2011)])

menage_forme$prod_veh <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A06"),]$COICOP_2011)])

menage_forme$carb_lubr <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A07"),]$COICOP_2011)])

menage_forme$transp_rail_air <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A08"),]$COICOP_2011)])

menage_forme$autres_services <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A09"),]$COICOP_2011)])

menage_forme$transp_routes_eau <- rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A10"),]$COICOP_2011)])

menage_forme$loisirs_com <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A11"),]$COICOP_2011)])

menage_forme$autres_conso <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A12"),]$COICOP_2011)])

menage_forme$autres <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A13"),]$COICOP_2011)])

menage_forme$loyers <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A14"),]$COICOP_2011)])

menage_forme$veh_occasion <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A15"),]$COICOP_2011)])



# Vérifications - valeurs négatives ---------------------------------------

# menage_forme_verif<-menage_forme
# 
# 
# list_cat=c("agriculture",
#            "BTP",
#            "prod_veh",
#            "carb_lubr",
#            "transp_rail_air",
#            "autres_services",
#            "transp_routes_eau",
#            "loisirs_com",
#            "autres_conso",
#            "autres",
#            "loyers",
#            "veh_occasion")
# 
# for (cat in list_cat){
#   menage_forme_verif[paste(cat,"verif",sep="_")]<-menage_forme_verif[cat]<0
# }
# 
# 
# menage_forme_verif$verif<-rowSums(menage_forme_verif[paste(list_cat,"verif",sep="_")])
# table(menage_forme_verif$verif)
# # 73 ménages ont au moins une catégorie de dépenses négative
# #isolons ces ménages
# menage_neg<-menage_forme_verif %>% filter(verif==1)
#   # 72 ont dep négatives en A13 : autres
#   # 1 a dep négative en loyer
# 
# menage_neg_autres<-menage_neg %>% filter(autres_verif==TRUE)
# # -9  -880  -115 -2265   -42  -403   -60 -3646  -360  -514    -9  -190 -1636  -959  -817 -5368  -502  -104  -114 -1811  -133 -2426
# # -82  -444 -3560  -131 -5837   -86 -5236 -1048  -199  -718  -661   -51 -1415 -2568  -115 -3541  -352  -275  -138   -10   -18 -2271
# # -110  -287 -3580 -5317 -3347 -2753   -69 -1734  -507  -370  -230  -139   -39 -2427  -417 -8231   -39   -20 -3211  -754  -366 -1116
# # -1194 -9650  -435   -56   -75 -1308
# #CONCLUSION : suppression de ces ménages
#   # SUPRESSION
#   menage_forme<-menage_forme %>% filter(autres>=0)
# menage_neg_loyer<-menage_neg %>% filter(loyers_verif==TRUE)
# #loyers = -12
# #CONCLUSION : correction à 0
# menage_forme$loyers[which(menage_forme$loyers<0)]<-0
# 
# rm(menage_neg)
# rm(menage_forme_verif)
# rm(menage)
# rm(menage_neg_autres)
# rm(menage_neg_loyer)


# Mise à niveau nombre ménages --------------------------------------------
# 
c05 <- c05[which(c05$ident_men %in% menage_forme$ident_men),]
appmen_depensesactiv_2010 <- appmen_depensesactiv_2010[which(appmen_depensesactiv_2010$ident_men %in% menage_forme$ident_men),]



# Vérification différences énergies BDF - ENL - EDF -----------------------

# menage_ener<-menage_forme
# 
# menage_ener$diff_elec<-(menage_ener$elec-menage_ener$dep_elec)/menage_ener$elec*100
# menage_ener$diff_gaz<-(menage_ener$gaz_ville-menage_ener$dep_gaz)/menage_ener$gaz_ville*100 
# #pour afficher sans écriture scientifique => sprintf("%.5f", .)
# menage_ener$diff_ener_dom<-(menage_ener$autres_energies_dom-(menage_forme$dep_gaz+
# menage_forme$dep_GPL_fioul +
# menage_forme$dep_chaleur +
# menage_forme$dep_bois+
# menage_forme$dep_charbon))/menage_ener$autres_energies_dom*100
# 
# menage_ener$dep_agreg<-menage_forme$dep_gaz+
#   menage_forme$dep_GPL_fioul +
#   menage_forme$dep_chaleur +
#   menage_forme$dep_bois+
#   menage_forme$dep_charbon
# 
# # menage_ener[c("autres_energies_dom","dep_agreg")]
# 
# rm(menage_ener)


# VARIABLES des MENAGES ---------------------------------------------------------------
menage_forme$impot_revenu <- c05$c13141
menage_forme$AID <- rowSums(c05[c("c13111","c13121","c13151","c13161")]) # autres impôts directs
menage_forme$FBCF <- rowSums(c05[c("c13411","c13421","c13711")]) # gros travaux et épargne immobilière
# menage_forme$rembours_prets : rembours. credit immobiliers + credits conso calculé dans ressources => à garder ? ligne 138 du code de Simona

# A VALIDER
# # # # # # 
# menage_forme$rev_sociaux_aut <- menage_forme$rev_sociaux-menage_forme$retraites-menage_forme$chomage
menage_forme$RDBAI <- rowSums(menage_forme[c("rev_activites","rev_sociaux","rev_patrimoine","rev_indep")])



menage_forme$RDB <- menage_forme$RDBAI - rowSums(menage_forme[c("impot_revenu","AID")])


menage_forme$taux_IR<-menage_forme$impot_revenu/menage_forme$RDB
menage_forme$taux_AID<-menage_forme$AID/menage_forme$RDB



# TYPOLOGIE VULNERABILITE -------------------------------------------------

typo_vuln<-read_excel("Donnees_brutes/CIRED/datacp_typovuln_bdf_2011.xlsx",sheet="identmen")

typo_vuln <- typo_vuln %>% 
  separate(col="IDENTMEN", into=c("year","ident_men"),sep="2011")
typo_vuln$ident_men<-as.numeric(typo_vuln$ident_men)

# SELECTION MENAGES
typo_vuln_bis<- typo_vuln %>% filter(ident_men %in% menage_forme$ident_men)
# rajout typo
menage_forme<-menage_forme%>% left_join(.,typo_vuln_bis[c("ident_men","typo2010f")],by="ident_men")
rm(typo_vuln,typo_vuln_bis)

# SAVE FILES --------------------------------------------------------------

# save(menage_forme,file="2010/menage_forme_2010.RData")
# save(c05,file="2010/c05_2010.RData")
# save(appmen_depensesactiv_2010,file="2010/appmen_depensesactiv_2010.RData")


# Suppression des bases superflues ----------------------------------------

rm(nbactoccup,nbchomeurs,nbretraites,Nom,Nomenclature_ADEME_COICOP,def_rev,individu,depmen)

# load("2010/menage_forme_2010.RData")

# PARTS BUDGETAIRES -------------------------------------------------------
 

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
           "autres_services",
           "transp_routes_eau",
           "loisirs_com",
           "autres_conso",
           "autres",
           "loyers",
           "veh_occasion")
menage_forme$Rcons <- rowSums(menage_forme[list_dep])


for (k in list_dep){
  menage_forme[paste("part",k,sep="_")]<-menage_forme[k]/menage_forme$Rcons
}



menage_forme$epargne <- menage_forme$RDB - menage_forme$Rcons + menage_forme$rev_exceptionnel
menage_forme$taux_epargne<- menage_forme$epargne/menage_forme$RDB
menage_forme$ratio_S <- menage_forme$epargne/menage_forme$Rcons  # probleme pour RDB = 0 (ratio_S = -1)

menage_forme <- menage_forme[which(menage_forme$ratio_S < -1.01 | menage_forme$ratio_S > -0.99),]
menage_forme <- menage_forme[which(menage_forme$RDB < -5 | menage_forme$RDB > 5),] # on elimine les ménages ayant RDB = 0 (ou proche de 0)







# SAVE FILE ---------------------------------------------------------------

menage_forme_2010<-menage_forme
appmen_depensesactiv_2010 <- appmen_depensesactiv_2010[which(appmen_depensesactiv_2010$ident_men %in% menage_forme$ident_men),]
c05_forme_2010 <- c05[which(c05$ident_men %in% menage_forme$ident_men),]


save(menage_forme_2010,file="2010/menage_forme_2010.RData")
save(appmen_depensesactiv_2010,file="2010/appmen_depensesactiv_2010.RData")
save(c05_forme_2010,file="2010/c05_forme_2010.RData")

# ANNEXES -----------------------------------------------------------------


################ Calcul part intérêts

# # calcul part d'intérêts sur les prêts à partir de données TEE 2010 et comptes financiers des ménages 2010
# # TEE 2010 intérêts payés par les ménages : 204 G€ ; comptes financiers 2010 remb crédits ménages : 549 G€
# rembours_prets <- rowSums(c05[c("c13211","c13221","c13511")])
# 
# part_interets <- 204/(204+549)

###########" Rebmourssements des intérêts et du capital

# menage_forme$remb_prets_interets <- part_interets*rembours_prets
# menage_forme$remb_prets_capital <- (1-part_interets)*rembours_prets

############ Autres revenus
# menage_forme$rev_patr_autres_NET <- menage$revpat - rowSums(menage[c("rev504","rev505","rev506")]) - menage_forme$remb_prets_interets  
# #revpat moins loyers moins intérêts payés

