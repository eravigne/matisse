
# Objectifs  --------------------------------------------------------------

# Préparer le calage sur marge sur données socio-économiques INSEE


### Contraintes population INSEE calage sur marges ---
# horizon=2025



# Library -----------------------------------------------------------------


setwd("D:/CIRED/Projet_Ademe")
library(tidyverse)
library(readxl)
library(car)
library(plyr)
library(base)





# Donnees -----------------------------------------------------------------

# Charger données de BDFE 2010, mises en forme auparavant
load("2010/menage_forme_2010.RData")


# Création menage_calibr_2010
menage_calibr_2010<-menage_forme_2010


###
### CONTRAINTES MICRO #####
###



# PREPARATION DATA --------------------------------------------------------

## Type ménage ###################
 # hypothèse : les types des ménages ne varient pas de 2010 à 2025, ce sont les pondérations qui évolueront.
  
#création matrice indicatrices du type de ménage (1 à 6)
menage_calibr_2010$typmen_corr <- 
  as.factor(menage_calibr_2010$typmen_corr)

dummies_typmen <- 
  model.matrix(~ typmen_corr, 
               data = menage_calibr_2010,
               contrasts.arg = list(typmen_corr = 
                        contrasts(menage_calibr_2010$typmen_corr, contrasts = F))
               )[,-1]

#rajout des colonnes d'indicatrices à la matrice 2025
menage_calibr_2010 <- 
  cbind(
    menage_calibr_2010,
    dummies_typmen
    )

rm(dummies_typmen)




## Vague d'enquête (6 vagues) #################

load("2010/depmen.RData")  
depmen$ident_men <- as.numeric(depmen$ident_men)

# Selection des ménages de depmen présents dans menage_calibr_2010
depmen_bis <- depmen[which(depmen$ident_men %in% menage_calibr_2010$ident_men),]

#création d'indicatrices de vague des ménages
depmen_bis["vague_1"] <- ifelse(depmen_bis$vag == "1", 1, 0)
depmen_bis["vague_2"] <- ifelse(depmen_bis$vag == "2", 1, 0)
depmen_bis["vague_3"] <- ifelse(depmen_bis$vag == "3", 1, 0)
depmen_bis["vague_4"] <- ifelse(depmen_bis$vag == "4", 1, 0)
depmen_bis["vague_5"] <- ifelse(depmen_bis$vag == "5", 1, 0)

# rajout des indicatrices de vague dans menage_calibr_2010
menage_calibr_2010[c("vague_1","vague_2","vague_3","vague_4","vague_5")] <- depmen_bis[c("vague_1","vague_2","vague_3","vague_4","vague_5")]
rm(depmen,depmen_bis)






## Age PR (5 Catégories) ######################

load("2010/appmen_depensesactiv_2010.RData")

# Selection des ménages de appmen_depensesactiv_2010 présents dans menage_calibr_2010
agepr <- 
  appmen_depensesactiv_2010 %>%  
  filter(ident_men %in% menage_calibr_2010$ident_men) %>%
  select(agepr)

# Rajout colonne "Âge du PR" dans menage_calibr_2010
menage_calibr_2010 <- 
  cbind(
    menage_calibr_2010, 
    agepr
    )

# Création des indicatrices d'âge
menage_calibr_2010["agePR_0_29"] <- 
  ifelse(
    menage_calibr_2010$agepr <= 29, 
    1,
    0)
menage_calibr_2010["agePR_30_44"] <- 
  ifelse(
    menage_calibr_2010$agepr >= 30 & menage_calibr_2010$agepr <= 44, 
    1, 
    0)
menage_calibr_2010["agePR_45_59"] <- 
  ifelse(
    menage_calibr_2010$agepr >= 45 & menage_calibr_2010$agepr <= 59,
    1,
    0)
menage_calibr_2010["agePR_60_74"] <- 
  ifelse(
    menage_calibr_2010$agepr >= 60 & menage_calibr_2010$agepr <= 74,
    1,
    0)
rm(agepr)






## ZEAT  Zones d'Etudes et d'Amenagement du Territoire ##############"

# Méthodo : ZEAT regroupée (8 modalités : 7 France métropole) : région parisienne + bassin parisien, Nord, Est, Ouest, Sud-Ouest, Centre-Est, Méditerranée

# Lire données BDF
suppressWarnings(menage<-read_excel("Donnees_brutes/BDF_2010/menage.Xlsx"))

# Selection des ménages présents dans menage_calibr_2010
menage$ident_men <- as.numeric(menage$ident_men)
menage_bis <- menage[which(menage$ident_men %in% menage_calibr_2010$ident_men),]

# Création des indicatrices
menage_bis["ZEAT_Paris"] <- ifelse(menage_bis$zeat == "1" | menage_bis$zeat == "2", 1, 0)
menage_bis["ZEAT_Nord"] <- ifelse(menage_bis$zeat == "3", 1, 0)
menage_bis["ZEAT_Est"] <- ifelse(menage_bis$zeat == "4", 1, 0)
menage_bis["ZEAT_Ouest"] <- ifelse(menage_bis$zeat == "5", 1, 0)
menage_bis["ZEAT_SudOuest"] <- ifelse(menage_bis$zeat == "7", 1, 0)
menage_bis["ZEAT_CentreEst"] <- ifelse(menage_bis$zeat == "8", 1, 0)
# menage_bis["ZEAT_Mediterranee"] <- ifelse(menage_bis$zeat == "9", 1, 0) solde

#
menage_calibr_2010[c("ZEAT_Paris","ZEAT_Nord","ZEAT_Est","ZEAT_Ouest","ZEAT_SudOuest","ZEAT_CentreEst")] <- 
  menage_bis[c("ZEAT_Paris","ZEAT_Nord","ZEAT_Est","ZEAT_Ouest","ZEAT_SudOuest","ZEAT_CentreEst")]

rm(menage,menage_bis)




## Taille d'Unité Urbaine (TUU) regroupées#############"

# Def : TUU regroupée (0-1-2-3 = 21, 4-5-6 = 22, 7 = 23, 8 = 24) 
#rajout d'une colonne tuu dans menage_calibr_2015
menage_calibr_2010["tuu"] <- merge(menage_calibr_2010["ident_men"], appmen_depensesactiv_2010[c("ident_men","tuu")], by = c("ident_men"))["tuu"]

#Recodage regroupant les TUU : 21,22,23,24
menage_calibr_2010["tuu_corr"] <- car::recode(menage_calibr_2010$tuu, "0:3 = 21 ; 4:6 = 22 ; 7 = 23 ; 8 = 24")
# Matrice d'indicatrices
dummies_tuu <- model.matrix(~ tuu_corr, data = menage_calibr_2010, 
                                 contrasts.arg = list(tuu_corr = contrasts(menage_calibr_2010$tuu_corr, contrasts = F)))[,-1]
# Rajout des indicatrices
menage_calibr_2010 <- cbind(menage_calibr_2010,dummies_tuu)






## Age x Sexe (12 modalités) ################""

# Croisement âge/sexe individus en 12 modalités (voir fichier Insee_Pop dans données brutes, issues projections 2013-2070)
# 0-14 ans,	15-24 ans, 25-39 ans, 40-59 ans, 60-74 ans, 75 ans et +

individu<-read_excel("Donnees_brutes/BDF_2010/individu.xlsx")
individu$ident_men <- as.numeric(individu$ident_men)

# Création des indicatrices. Excepté celles des Hommes de plus de 75ans, variable de bouclage (à 100% de la pop)
individu_bis <- individu[which(individu$ident_men %in% menage_calibr_2010$ident_men),]
individu_bis["F_0_14"] <- ifelse(individu_bis$age <= 14 & individu_bis$sexe == "2", 1, 0)
individu_bis["M_0_14"] <- ifelse(individu_bis$age <= 14 & individu_bis$sexe == "1", 1, 0)
individu_bis["F_15_24"] <- ifelse(individu_bis$age >= 15 & individu_bis$age <= 24 & individu_bis$sexe == "2", 1, 0)
individu_bis["M_15_24"] <- ifelse(individu_bis$age >= 15 & individu_bis$age <= 24 & individu_bis$sexe == "1", 1, 0)
individu_bis["F_25_39"] <- ifelse(individu_bis$age >= 25 & individu_bis$age <= 39 & individu_bis$sexe == "2", 1, 0)
individu_bis["M_25_39"] <- ifelse(individu_bis$age >= 25 & individu_bis$age <= 39 & individu_bis$sexe == "1", 1, 0)
individu_bis["F_40_59"] <- ifelse(individu_bis$age >= 40 & individu_bis$age <= 59 & individu_bis$sexe == "2", 1, 0)
individu_bis["M_40_59"] <- ifelse(individu_bis$age >= 40 & individu_bis$age <= 59 & individu_bis$sexe == "1", 1, 0)
individu_bis["F_60_74"] <- ifelse(individu_bis$age >= 60 & individu_bis$age <= 74 & individu_bis$sexe == "2", 1, 0)
individu_bis["M_60_74"] <- ifelse(individu_bis$age >= 60 & individu_bis$age <= 74 & individu_bis$sexe == "1", 1, 0)
individu_bis["F_75_plus"] <- ifelse(individu_bis$age >= 75 & individu_bis$sexe == "2", 1, 0)

individu_bis$ident_men<-as.numeric(individu_bis$ident_men)

# rajout des colonnes AGExSEXE dans menage_calibr_2010
# a chaque colonne indicatrices de la matrice individu_bis, on somme pour chaque ménage (variable = ident_men) le nombre de personnes dans chacune des catégories. Colonne ident_men retirée du dataframe obtenu et fusion de menage_calibr des nouvelles colonnes indicatrices
menage_calibr_2010[
  c("F_0_14","M_0_14","F_15_24","M_15_24","F_25_39","M_25_39",
                     "F_40_59","M_40_59","F_60_74","M_60_74","F_75_plus")] <- 0


for (agexsexe in  c("F_0_14","M_0_14","F_15_24","M_15_24","F_25_39","M_25_39",
                      "F_40_59","M_40_59","F_60_74","M_60_74","F_75_plus")){
  ind<-individu_bis %>% group_by(ident_men)%>% summarise(sum(get(agexsexe))) 
  menage_calibr_2010[agexsexe]<-ind$`sum(get(agexsexe))`
}
rm(ind)


# menage_calibr_2010["F_0_14"] <- ifelse(individu_bis$age <= 14 & individu_bis$sexe == "2", 1, 0)
# individu_bis["M_0_14"] <- ifelse(individu_bis$age <= 14 & individu_bis$sexe == "1", 1, 0)
# individu_bis["F_15_24"] <- ifelse(individu_bis$age >= 15 & individu_bis$age <= 24 & individu_bis$sexe == "2", 1, 0)
# individu_bis["M_15_24"] <- ifelse(individu_bis$age >= 15 & individu_bis$age <= 24 & individu_bis$sexe == "1", 1, 0)
# individu_bis["F_25_39"] <- ifelse(individu_bis$age >= 25 & individu_bis$age <= 39 & individu_bis$sexe == "2", 1, 0)
# individu_bis["M_25_39"] <- ifelse(individu_bis$age >= 25 & individu_bis$age <= 39 & individu_bis$sexe == "1", 1, 0)
# individu_bis["F_40_59"] <- ifelse(individu_bis$age >= 40 & individu_bis$age <= 59 & individu_bis$sexe == "2", 1, 0)
# individu_bis["M_40_59"] <- ifelse(individu_bis$age >= 40 & individu_bis$age <= 59 & individu_bis$sexe == "1", 1, 0)
# individu_bis["F_60_74"] <- ifelse(individu_bis$age >= 60 & individu_bis$age <= 74 & individu_bis$sexe == "2", 1, 0)
# individu_bis["M_60_74"] <- ifelse(individu_bis$age >= 60 & individu_bis$age <= 74 & individu_bis$sexe == "1", 1, 0)
# individu_bis["F_75_plus"] <- ifelse(individu_bis$age >= 75 & individu_bis$sexe == "2", 1, 0)
#   # ddply(
#   #   .data=individu_bis[c("ident_men","F_0_14","M_0_14","F_15_24","M_15_24","F_25_39","M_25_39","F_40_59","M_40_59","F_60_74","M_60_74","F_75_plus")],
#   #   .variables=.(ident_men), 
#   #   .fun=function(x) numcolwise(sum)(x[colnames(x) %in% c("F_0_14","M_0_14","F_15_24","M_15_24","F_25_39","M_25_39", "F_40_59","M_40_59","F_60_74","M_60_74","F_75_plus")])
#   #       )[2:12]

rm(individu,individu_bis)


# # ## DPE #############"
# 
# # # Matrice d'indicatrices
# # menage_calibr_2010$DPE<-as.factor(menage_calibr_2010$DPE_pred)
# # dummies_dpe_2010 <- model.matrix(~ DPE, data = menage_calibr_2010,
# #                                  contrasts.arg = list(DPE = contrasts(menage_calibr_2010$DPE, contrasts = F)))[,-1]
# # menage_calibr_2010 <- menage_calibr_2010 %>% select(-DPE)
# # colnames(dummies_dpe_2010)<-paste("DPE_2010",LETTERS[1:7],sep="_")
# # # Rajout des indicatrices
# # menage_calibr_2010 <- cbind(menage_calibr_2010,dummies_dpe_2010)
# # 
# # menage_calibr_2010 <-
# #   menage_calibr_2010 %>%
# #   mutate_at(vars(starts_with("DPE_m2_")),function(x) x*menage_calibr_2010$surfhab_d)
# # head(menage_echelle_2025 %>% select(starts_with("DPE_m2_")))
# 
# # 
# # 
# list_contraintes_menage<-
#   c("vague_1" , "vague_2","vague_3" , "vague_4" , "vague_5" ,
#     "agePR_0_29" , "agePR_30_44", "agePR_45_59" , "agePR_60_74" ,
#     "F_0_14", "M_0_14" , "F_15_24" , "M_15_24", "F_25_39" , "M_25_39" , "F_40_59",
#     "M_40_59" , "F_60_74" , "M_60_74", "F_75_plus",
#     "tuu_corr21" , "tuu_corr22", "tuu_corr23",
#     "MI_corr",
#     "ZEAT_Paris","ZEAT_Nord" , "ZEAT_Est" ,     "ZEAT_Ouest", "ZEAT_SudOuest" , "ZEAT_CentreEst" )
# 
# # 
# # 
# # 
# # # MENAGE_CONTRAINTES ------------------------------------------------------
# # 
# menage_contraintes_2025<-menage_calibr_2010[c("ident_men",list_contraintes_menage)]
# save(menage_contraintes,file="2010/menage_contraintes.RData")
# 




###
### CONTRAINTES MACRO #####
###





# CREATION DU FICHIER CONTRAINTES_2025 ------------------------------------

# A partir d'ici, création des contraintes #MACRO# (en absolu ou relatif en part de la population)


# PARTS des VARIABLES -----------------------------------------------------

#Pour chaque variables de calage, calcul des parts dans la population => on ne cale pas sur les variables absolues mais en pourcentage

# âge PR (5 modalités), vague de collecte (6 vagues), tuu (4 mod), type de menage (6 mod), croisement âge/sexe individ (12 mod.), type de logement (2 mod), région regroupée (7 mod)

# ## Age PR
# 
# part_agepr_0_29 <- sum(menage_calibr_2010$agePR_0_29*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# part_agepr_30_44 <- sum(menage_calibr_2010$agePR_30_44*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# part_agepr_45_59 <- sum(menage_calibr_2010$agePR_45_59*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# part_agepr_60_74 <- sum(menage_calibr_2010$agePR_60_74*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# 
# ## Vagues
# 
# part_men_vague1 <- sum(menage_calibr_2010$vague_1*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# part_men_vague2 <- sum(menage_calibr_2010$vague_2*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# part_men_vague3 <- sum(menage_calibr_2010$vague_3*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# part_men_vague4 <- sum(menage_calibr_2010$vague_4*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# part_men_vague5 <- sum(menage_calibr_2010$vague_5*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# 

# # Import des contraintes INSEE pop 2025
# pop_INSEE <- read_excel("Donnees_brutes/INSEE/INSEE - projection pop.xlsx",sheet="Pop")
# pop_INSEE <- pop_INSEE %>% 
#   gather(key=year, value=part_age_sexe, -c(1:2)) %>% mutate(cat_sexe_age=paste("part",Sexe,Age,sep="_"))
# 
# pop_INSEE_2025<- pop_INSEE %>% filter(year==2025)

# # Création automatique des variables de part (e.g part_F_0_14)
# for (k in pop_INSEE_2025$cat_sexe_age){
#   value<-pop_INSEE_2025 %>% filter(cat_sexe_age==k) %>% select(part_age_sexe)
#   assign(k,value)}
# save(pop_INSEE,file="Donnees_brutes/INSEE/pop_INSEE.RData")
# rm(pop_INSEE,pop_INSEE_2025)

# ## TUU
#     # NB : la TUU est considéée dans le calage seulement pour la France métropolitaine
# 
# part_tuu_corr_21 <- sum(menage_calibr_2010$tuu_corr21*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# part_tuu_corr_22 <- sum(menage_calibr_2010$tuu_corr22*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# part_tuu_corr_23 <- sum(menage_calibr_2010$tuu_corr23*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# 
# ## MI
# 
# part_MI_corr <- sum(menage_calibr_2010$MI_corr*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# 
# ## ZEAT
# 
# part_ZEAT_Paris <- sum(menage_calibr_2010$ZEAT_Paris*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# part_ZEAT_Nord <- sum(menage_calibr_2010$ZEAT_Nord*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# part_ZEAT_Est <- sum(menage_calibr_2010$ZEAT_Est*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# part_ZEAT_Ouest <- sum(menage_calibr_2010$ZEAT_Ouest*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# part_ZEAT_SudOuest <- sum(menage_calibr_2010$ZEAT_SudOuest*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# part_ZEAT_CentreEst <- sum(menage_calibr_2010$ZEAT_CentreEst*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
# 

# 
# 
# 
# # Ecriture des contraintes ------------------------------------------------
# # NB : ce sont uniquement les contraites INSEE, les contraintes macro de ThreeME/IMACLIM sont rajoutées dans le fichier Reponderation/1_contraintes_macro.R
# 
# contraintes <-
#   data.frame(
#     pondmen=as.numeric(
#       menage_calibr_2010 %>% 
#         summarise(sum(pondmen))
#       ),
#     npers=as.numeric(
#       menage_calibr_2010 %>% 
#         summarise(sum(npers*pondmen))
#       )
#   )
# 
# contraintes_2 <- contraintes
# contraintes_2["vague_1"] <- contraintes_2$pondmen*part_men_vague1
# contraintes_2["vague_2"] <- contraintes_2$pondmen*part_men_vague2
# contraintes_2["vague_3"] <- contraintes_2$pondmen*part_men_vague3
# contraintes_2["vague_4"] <- contraintes_2$pondmen*part_men_vague4
# contraintes_2["vague_5"] <- contraintes_2$pondmen*part_men_vague5
# 
# contraintes_2["agePR_0_29"] <- contraintes_2$pondmen*part_agepr_0_29
# contraintes_2["agePR_30_44"] <- contraintes_2$pondmen*part_agepr_30_44
# contraintes_2["agePR_45_59"] <- contraintes_2$pondmen*part_agepr_45_59
# contraintes_2["agePR_60_74"] <- contraintes_2$pondmen*part_agepr_60_74
# 
# contraintes_2["F_0_14"] <- contraintes_2$npers*part_F_0_14
# contraintes_2["M_0_14"] <- contraintes_2$npers*part_M_0_14
# contraintes_2["F_15_24"] <- contraintes_2$npers*part_F_15_24
# contraintes_2["M_15_24"] <- contraintes_2$npers*part_M_15_24
# contraintes_2["F_25_39"] <- contraintes_2$npers*part_F_25_39
# contraintes_2["M_25_39"] <- contraintes_2$npers*part_M_25_39
# contraintes_2["F_40_59"] <- contraintes_2$npers*part_F_40_59
# contraintes_2["M_40_59"] <- contraintes_2$npers*part_M_40_59
# contraintes_2["F_60_74"] <- contraintes_2$npers*part_F_60_74
# contraintes_2["M_60_74"] <- contraintes_2$npers*part_M_60_74
# contraintes_2["F_75_plus"] <- contraintes_2$npers*part_F_75_plus
# 
# contraintes_2["tuu_corr21"] <- contraintes_2$pondmen*part_tuu_corr_21
# contraintes_2["tuu_corr22"] <- contraintes_2$pondmen*part_tuu_corr_22
# contraintes_2["tuu_corr23"] <- contraintes_2$pondmen*part_tuu_corr_23
# 
# contraintes_2["MI_corr"] <- contraintes_2$pondmen*part_MI_corr              
# 
# contraintes_2["ZEAT_Paris"] <- contraintes_2$pondmen*part_ZEAT_Paris
# contraintes_2["ZEAT_Nord"] <- contraintes_2$pondmen*part_ZEAT_Nord
# contraintes_2["ZEAT_Est"] <- contraintes_2$pondmen*part_ZEAT_Est
# contraintes_2["ZEAT_Ouest"] <- contraintes_2$pondmen*part_ZEAT_Ouest
# contraintes_2["ZEAT_SudOuest"] <- contraintes_2$pondmen*part_ZEAT_SudOuest
# contraintes_2["ZEAT_CentreEst"] <- contraintes_2$pondmen*part_ZEAT_CentreEst
# 
# contraintes_2 <- contraintes_2 %>% select(-npers)
# list_data <- colnames(contraintes_2 %>% select(-pondmen))
# 
# 




# SAVE FILES --------------------------------------------------------------

# Création menage_calibr_2025
# menage_calibr_2025<-menage_calibr_2010 %>% select(-starts_with("DPE_2010_"))



# save(menage_calibr_2025,file="2025/menage_calibr_2025.RData")
save(menage_calibr_2010,file="2010/menage_calibr_2010.RData")

# contraintes<-contraintes_2
# save(contraintes,file="2025/contraintes_2025.RData")
# save(contraintes,file="2025/Iteration_1/Input/contraintes_2025.RData")



# SUCCESS -----------------------------------------------------------------

print("4_contraintes_calages : SUCCESS")

