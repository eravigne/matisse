# Il s'agit d'une fonction appelée dans le code global en fonction de l'itération à laquelle nous sommes rendus
### test
# Iter = 


# contraintes_macro <- function(Iter){

# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(car)
library(plyr)
library(base)

# DATA --------------------------------------------------------------------
setwd("D:/CIRED/Projet_Ademe")
source("Code_global_Ademe/mutate_when.R")
# load("2025/contraintes_2025.RData")
load("2010/menage_calibr_2010.RData")

load(paste(scenario,"/",horizon,"/IMACLIM.RData",sep=""))

load("Donnees_brutes/Sorties ThreeMe/ThreeME.RData")


#!!!!!
coeff_ventes_veh_priv<-43.51/100

###!!!!!!

# Préparation Données -----------------------------------------------------


# list_contraintes_menage<-
#   c("vague_1" , "vague_2","vague_3" , "vague_4" , "vague_5" , 
#     "agePR_0_29" , "agePR_30_44", "agePR_45_59" , "agePR_60_74" , 
#     "F_0_14", "M_0_14" , "F_15_24" , "M_15_24", "F_25_39" , "M_25_39" , "F_40_59",
#     "M_40_59" , "F_60_74" , "M_60_74", "F_75_plus",
#     "tuu_corr21" , "tuu_corr22", "tuu_corr23", 
#     "MI_corr",
#     "ZEAT_Paris","ZEAT_Nord" , "ZEAT_Est" ,     "ZEAT_Ouest", "ZEAT_SudOuest" , "ZEAT_CentreEst" )




# MENAGE_CONTRAINTES ------------------------------------------------------

# menage_contraintes_2025<-menage_calibr_2010[c("ident_men",list_contraintes_menage)]
# save(menage_contraintes_2025,file="2025/menage_contraintes_2025.RData")





# Calage Macro ------------------------------------------------------------

Calage <-
  IMACLIM %>%
  filter(Categorie=="CALAGE") %>%
  filter(year==horizon)

Calage_relatif <-
  Calage %>% 
  filter(model=="IMACLIM") 

for (i in 1:nrow(Calage_relatif)){
  var=as.character(Calage_relatif[i,"Variable"])
  if(is.na(Calage_relatif[i,"value"])){
    Calage_relatif[i,"value"]<-
      as.numeric(
        Calage %>% 
        filter(model=="ThreeME") %>% 
        filter(Variable==var) %>% 
        select(value))
        Calage_relatif[i,"model"]<-"ThreeME"
  }
}



# CONTRAINTES -------------------------------------------------------------


Cal<-Calage_relatif %>% mutate(value=as.numeric(value)) %>% select(Variable,value)
contraintes_3ME<-spread(Cal,key=Variable,value=value)

# Pondmen

men_INSEE <- read_excel("Donnees_brutes/INSEE/INSEE - projection men.xlsx",sheet="men")



contraintes <-
  data.frame("npers"=
               contraintes_3ME$cal_pop_tot*
               sum(menage_calibr_2010$npers*menage_calibr_2010$pondmen),
             "pondmen"=contraintes_3ME$cal_pop_tot*
               sum(menage_calibr_2010$npers*menage_calibr_2010$pondmen)/as.numeric(men_INSEE %>% filter(year==horizon) %>% select(nb_pers_men))
  )




# AGREGATS INSEE ----------------------------------------------------------

#part pop SEXE x AGE + pondmen


# Import des contraintes INSEE pop 2025
pop_INSEE <- read_excel("Donnees_brutes/INSEE/INSEE - projection pop.xlsx",sheet="Pop")
pop_INSEE <- pop_INSEE %>% 
  gather(key=year, value=part_age_sexe, -c(1:2)) %>% mutate(cat_sexe_age=paste("part",Sexe,Age,sep="_"))

pop_INSEE_horizon<- pop_INSEE %>% filter(year==horizon)


# Création automatique des variables de part (e.g part_F_0_14)
for (k in pop_INSEE_horizon$cat_sexe_age){
  value<-as.numeric(pop_INSEE_horizon %>% filter(cat_sexe_age==k) %>% select(part_age_sexe))
  assign(k,value)}
save(pop_INSEE,file="Donnees_brutes/INSEE/pop_INSEE.RData")
rm(pop_INSEE,pop_INSEE_horizon)



# AGREGATS constants depuis BDF 2010 --------------------------------------

# PARTS des VARIABLES -----------------------------------------------------

#Pour chaque variables de calage, calcul des parts dans la population => on ne cale pas sur les variables absolues mais en pourcentage

# âge PR (5 modalités), vague de collecte (6 vagues), tuu (4 mod), type de menage (6 mod), croisement âge/sexe individ (12 mod.), type de logement (2 mod), région regroupée (7 mod)

## Age PR

part_agepr_0_29 <- sum(menage_calibr_2010$agePR_0_29*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
part_agepr_30_44 <- sum(menage_calibr_2010$agePR_30_44*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
part_agepr_45_59 <- sum(menage_calibr_2010$agePR_45_59*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
part_agepr_60_74 <- sum(menage_calibr_2010$agePR_60_74*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)

## Vagues

part_men_vague1 <- sum(menage_calibr_2010$vague_1*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
part_men_vague2 <- sum(menage_calibr_2010$vague_2*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
part_men_vague3 <- sum(menage_calibr_2010$vague_3*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
part_men_vague4 <- sum(menage_calibr_2010$vague_4*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
part_men_vague5 <- sum(menage_calibr_2010$vague_5*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)

## TUU
# NB : la TUU est considéée dans le calage seulement pour la France métropolitaine

part_tuu_corr_21 <- sum(menage_calibr_2010$tuu_corr21*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
part_tuu_corr_22 <- sum(menage_calibr_2010$tuu_corr22*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
part_tuu_corr_23 <- sum(menage_calibr_2010$tuu_corr23*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)

## MI

part_MI_corr <- sum(menage_calibr_2010$MI_corr*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)

## ZEAT

part_ZEAT_Paris <- sum(menage_calibr_2010$ZEAT_Paris*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
part_ZEAT_Nord <- sum(menage_calibr_2010$ZEAT_Nord*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
part_ZEAT_Est <- sum(menage_calibr_2010$ZEAT_Est*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
part_ZEAT_Ouest <- sum(menage_calibr_2010$ZEAT_Ouest*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
part_ZEAT_SudOuest <- sum(menage_calibr_2010$ZEAT_SudOuest*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)
part_ZEAT_CentreEst <- sum(menage_calibr_2010$ZEAT_CentreEst*menage_calibr_2010$pondmen)/sum(menage_calibr_2010$pondmen)


# Ajouter Contraintes -----------------------------------------------------



contraintes["vague_1"] <- contraintes$pondmen*part_men_vague1
contraintes["vague_2"] <- contraintes$pondmen*part_men_vague2
contraintes["vague_3"] <- contraintes$pondmen*part_men_vague3
contraintes["vague_4"] <- contraintes$pondmen*part_men_vague4
contraintes["vague_5"] <- contraintes$pondmen*part_men_vague5

contraintes["agePR_0_29"] <- contraintes$pondmen*part_agepr_0_29
contraintes["agePR_30_44"] <- contraintes$pondmen*part_agepr_30_44
contraintes["agePR_45_59"] <- contraintes$pondmen*part_agepr_45_59
contraintes["agePR_60_74"] <- contraintes$pondmen*part_agepr_60_74

contraintes["F_0_14"] <- contraintes$npers*part_F_0_14
contraintes["M_0_14"] <- contraintes$npers*part_M_0_14
contraintes["F_15_24"] <- contraintes$npers*part_F_15_24
contraintes["M_15_24"] <- contraintes$npers*part_M_15_24
contraintes["F_25_39"] <- contraintes$npers*part_F_25_39
contraintes["M_25_39"] <- contraintes$npers*part_M_25_39
contraintes["F_40_59"] <- contraintes$npers*part_F_40_59
contraintes["M_40_59"] <- contraintes$npers*part_M_40_59
contraintes["F_60_74"] <- contraintes$npers*part_F_60_74
contraintes["M_60_74"] <- contraintes$npers*part_M_60_74
contraintes["F_75_plus"] <- contraintes$npers*part_F_75_plus

contraintes["tuu_corr21"] <- contraintes$pondmen*part_tuu_corr_21
contraintes["tuu_corr22"] <- contraintes$pondmen*part_tuu_corr_22
contraintes["tuu_corr23"] <- contraintes$pondmen*part_tuu_corr_23

contraintes["MI_corr"] <- contraintes$pondmen*part_MI_corr              

contraintes["ZEAT_Paris"] <- contraintes$pondmen*part_ZEAT_Paris
contraintes["ZEAT_Nord"] <- contraintes$pondmen*part_ZEAT_Nord
contraintes["ZEAT_Est"] <- contraintes$pondmen*part_ZEAT_Est
contraintes["ZEAT_Ouest"] <- contraintes$pondmen*part_ZEAT_Ouest
contraintes["ZEAT_SudOuest"] <- contraintes$pondmen*part_ZEAT_SudOuest
contraintes["ZEAT_CentreEst"] <- contraintes$pondmen*part_ZEAT_CentreEst




# AGREGATS depuis THREEME ----------------------------------------------------



agreg_best <-
  contraintes %>%
  select(-npers) %>%
  mutate(
  
  "npers"=
    contraintes_3ME$cal_pop_tot*
    sum(menage_calibr_2010$npers*menage_calibr_2010$pondmen),
 
  "nbactoccup"=
    contraintes_3ME$cal_act_occ
  *sum(menage_calibr_2010$nbactoccup*menage_calibr_2010$pondmen,
       na.rm = T),

  "nbchomeurs"=
    contraintes_3ME$cal_chom*
    sum(menage_calibr_2010$nbchomeurs*menage_calibr_2010$pondmen,
        na.rm = T),

  "rev_activites"=
    contraintes_3ME$cal_revact*
    sum(menage_calibr_2010$rev_activites_sans_etranger*menage_calibr_2010$pondmen,
        na.rm = T),

  "rev_patrimoine"=
    contraintes_3ME$cal_revpat*
    sum(menage_calibr_2010$rev_patrimoine*menage_calibr_2010$pondmen,
        na.rm = T),

  "chomage"=
    contraintes_3ME$cal_revchom*
    sum(menage_calibr_2010$chomage*menage_calibr_2010$pondmen,
        na.rm = T),

  "rev_sociaux_autres"=
    contraintes_3ME$cal_revsoc*
    sum(menage_calibr_2010$rev_sociaux_autres*menage_calibr_2010$pondmen,
        na.rm = T),

  "rev_etranger"=
    contraintes_3ME$cal_revetr*
    sum(menage_calibr_2010$rev_etranger*menage_calibr_2010$pondmen,
        na.rm = T),

  # "taux_epargne"=
  #   contraintes_3ME$cal_epar
  # *sum(menage_calibr_2010$taux_epargne*menage_calibr_2010$pondmen*menage_calibr_2010$RDB /
  #        sum(menage_calibr_2010$RDB*menage_calibr_2010$pondmen),
  #      na.rm = T),

  "surfhab_d"=
    contraintes_3ME$cal_m2*
    sum(menage_calibr_2010$surfhab_d*menage_calibr_2010$pondmen,
        na.rm = T),
  "DPE_m2_A"=
    contraintes_3ME$cal_m2_dpe_A*
    as.numeric(menage_calibr_2010 %>% filter(DPE_pred=="A")%>% summarise(sum(surfhab_d*pondmen))),
  "DPE_m2_B"=
    contraintes_3ME$cal_m2_dpe_B*
    as.numeric(menage_calibr_2010 %>% filter(DPE_pred=="B")%>% summarise(sum(surfhab_d*pondmen))),
  "DPE_m2_C"=
  contraintes_3ME$cal_m2_dpe_C*
  as.numeric(menage_calibr_2010 %>% filter(DPE_pred=="C")%>% summarise(sum(surfhab_d*pondmen))),
  "DPE_m2_D"=
    contraintes_3ME$cal_m2_dpe_D*
    as.numeric(menage_calibr_2010 %>% filter(DPE_pred=="D")%>% summarise(sum(surfhab_d*pondmen))),
  "DPE_m2_E"=
    contraintes_3ME$cal_m2_dpe_E*
    as.numeric(menage_calibr_2010 %>% filter(DPE_pred=="E")%>% summarise(sum(surfhab_d*pondmen))),
  "DPE_m2_F"=
    contraintes_3ME$cal_m2_dpe_F*
    as.numeric(menage_calibr_2010 %>% filter(DPE_pred=="F")%>% summarise(sum(surfhab_d*pondmen))),
  "nbvehic" =
    contraintes_3ME$cal_VP *
    sum(menage_calibr_2010$nbvehic*menage_calibr_2010$pondmen,
        na.rm = T),
  
  "nbVE" =
    contraintes_3ME$cal_VE*1000)



# Ventes de véhicules depuis ThreeME

ventes_VE<-
  as.numeric((ThreeME %>% 
     filter(year==horizon)%>%
     filter(Var=="NEWAUTO_ELEC_H01_2")%>%
     select(value))[1,]*coeff_ventes_veh_priv*1000)


ventes_VT<-
  as.numeric(ThreeME %>% 
  filter(year==horizon)%>%
  filter(Var=="NEWAUTO_TH_H01_2")%>%
  select(value)*coeff_ventes_veh_priv*1000)


agreg_best <- 
  agreg_best %>%
  mutate("ventes_VT"=ventes_VT)%>%
  mutate("ventes_VE"=ventes_VE)

# # Pour ne garder que les contraintes macro
# agreg_best <- agreg_best[c(1,(length(contraintes)+1):length(agreg_best))]


save(agreg_best, file=
       paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Input/agreg_best.RData",sep=""))

print("Repondation : 1_contraintes_macro : SUCCESS")

# }

