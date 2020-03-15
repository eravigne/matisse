
# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(plyr)
setwd("D:/CIRED/Projet_Ademe")



# DATA --------------------------------------------------------------------

load("Technical_change/TC_VE/appariement_bdf_entd.RData")

load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_TC_DPE.RData",sep=""))
load("2010/appmen_depensesactiv_2010.RData")
load("Donnees_brutes/Sorties ThreeMe/ThreeME.RData")


load("2010/auto.RData")

source("Code_global_Ademe/mutate_when.R")


# horizon=2025

# PREPARATION DATA --------------------------------------------------------


menage_echelle<-menage_echelle_TC_DPE
# rm(menage_echelle_TC_DPE)

# Rajouter typmen5 pour le cellulage
menage_echelle <-
  menage_echelle %>%
  left_join(appmen_depensesactiv_2010 %>% 
              select(ident_men, typmen5),
            by="ident_men")


# Rename variable
appariement_bdf_entd <- 
  appariement_bdf_entd %>%
  dplyr::rename(percent_pkm_eligible=percent_W_mean_eligible)


  # Rajouter tuu
# menage_echelle <- 
#   menage_echelle %>%
#   # left_join(appmen_depensesactiv_2010 %>% select(ident_men,tuu),by="ident_men") %>%
#   mutate(tuu = as.numeric(tuu)) 
# %>%
#   mutate(tuu=tuu-1) %>% #Pb de definition, tout a été shiffté d'une unité
#   mutate(typmen5 = as.numeric(typmen5))

  rm(appmen_depensesactiv_2010)
# Selection variable : nombre véhicule

auto_elec<- 
  auto %>% 
  select(ident_men,carbu)%>%
  filter(carbu==4) %>%
  filter(ident_men %in% menage_echelle$ident_men) %>%
  left_join(menage_echelle %>% select(ident_men, pondmen),by="ident_men")



# POURCENTAGE PKM ELIGIBLES ------------------------------------------------


# Cellulage des ménages sur trois variables : tuu, quintile UC et si le nombre le permet typmen
for (i in 1:nrow(appariement_bdf_entd)){
  Classe<-appariement_bdf_entd[i,]
  if(is.na(Classe$typmen5)) {
    menage_echelle <-
      menage_echelle %>%
      mutate_when(quintileuc==Classe$niveau_vie_quintile &
                  tuu==Classe$tuu,
                  list(percent_pkm_eligible=Classe$percent_pkm_eligible))
    
    
  } else {
    menage_echelle <-
      menage_echelle %>%
      mutate_when(quintileuc==Classe$niveau_vie_quintile &
                  tuu==Classe$tuu &
                  typmen5==Classe$typmen5,
                  list(percent_pkm_eligible=Classe$percent_pkm_eligible))
    
  }
  i=i+1
}
rm(i,Classe,appariement_bdf_entd)


# CLASSEMENT --------------------------------------------------------------

# Classement des ménages possédant strictement plus d'une voiture et des dépenses en carburant strictement positives.
# on n'exclut pas les ménages achetant une auto en 2010 (prod_veh>0) pour la sélection 2010-2024. En revanche, nouveau classement en 2025 uniquement sur les ménages avec prod_veh>0 (exclus ceux qui ont déjà un VE pour respecter les trajectoires ADEME)
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol


menage_echelle <- 
  menage_echelle %>%
  mutate_when(!is.na(nbvehic)& !is.na(percent_pkm_eligible) & !is.na(carb_lubr) & carb_lubr>0 & nbvehic>0,
              list(
                potentiel_VE=carb_lubr * percent_pkm_eligible/100)) 

menage_echelle <- 
  menage_echelle %>%
  mutate_when(!is.na(nbvehic)& !is.na(carb_lubr) & carb_lubr>0 & nbvehic>0,
              list(VE_rank_opt =row_number(-potentiel_VE)))


menage_echelle <-
  menage_echelle %>% 
  dplyr::mutate(VE_rank_pess =max(menage_echelle %>% filter(!is.na(VE_rank_opt)) %>% select(VE_rank_opt))-VE_rank_opt+1)
                
  menage_echelle<-
  menage_echelle %>% 
  dplyr::mutate(VE_rank_med =VE_rank_pess-VE_rank_opt) %>% 
  mutate(L=max(menage_echelle %>% filter(!is.na(VE_rank_opt)) %>% select(VE_rank_opt))) %>%
  mutate_when(
    VE_rank_med<0 & !is.na(VE_rank_med),
    list(
      VE_rank_med = ifelse(
        is.wholenumber(L/2),
        -VE_rank_med+1,
        -VE_rank_med-1)
    )
  ) %>%
  select(-L)

menage_echelle <-
  menage_echelle %>% 
  mutate_when(!is.na(VE_rank_opt),list(VE_rank_rich=row_number(-RDB/coeffuc))) %>% 
  mutate_when(!is.na(VE_rank_opt),list(VE_rank_poor=max(VE_rank_rich)-VE_rank_rich+1))




# View(menage_echelle%>% select(ident_men, nbvehic,carb_lubr,percent_pkm_eligible,potentiel_VE,VE_rank_pess,VE_rank_opt,VE_rank_med,VE_rank_rich,VE_rank_poor)%>%arrange(potentiel_VE))
# View(menage_echelle %>% select(VE_rank_opt,VE_rank_med,VE_rank_pess,VE_rank_rich,VE_rank_poor)%>% arrange(VE_rank_opt))



# Percent_km_eligible -----------------------------------------------------

menage_echelle <- 
  menage_echelle %>%
  mutate(percent_pkm_eligible = percent_pkm_eligible/100)



# MCREVOI_D ----------------------------------------------------------------

#Pour tous les ménages classés, on indique le montant actuel du remboursement de l'emprunt au cours des 12 derniers mois pour le dernier véhicule acheté par le ménage

#pour chaque ménage on sélectionne le véhicule le plus récent, on indique si oui ou non le ménage a un remboursement en cours


# menage_echelle <- menage_echelle %>% left_join(auto %>% select(ident_men, anvoi, recvoi, mcrevoi_d,crevoi),by="ident_men")

# NB : l'approche avec le filter ci dessous fonctionne mais renvoie toutes les lignes vérifiant la condition recvoi=max(recvoi) or il nous en faut une seule. Cette ligne créer 368 doublons. 
# max_recvoi<- auto %>% group_by(ident_men)%>%filter(recvoi==max(recvoi))%>%select(ident_men,anvoi,recvoi,mcrevoi_d,crevoi)
# Nous préférons donc la solution 
max_recvoi<-ddply(auto %>% select(ident_men,recvoi,mcrevoi_d), .(ident_men), function(x) x[which.max(x$recvoi),])
km_auto_sum<-ddply(auto %>% select(ident_men,km_auto), .(ident_men), function(x) sum(x$km_auto)*12)
colnames(km_auto_sum)<-c("ident_men","km_auto")
  

menage_echelle <- menage_echelle %>% left_join(max_recvoi %>% select(ident_men,mcrevoi_d),by="ident_men") %>% mutate_when(is.na(mcrevoi_d),list(mcrevoi_d=0))%>%left_join(km_auto_sum,by="ident_men")






# KM_AUTO -----------------------------------------------------------------

# KM_tot
# KM_AUTO_TOT<-as.numeric(
#   (ThreeME %>% 
#      filter(Var=="KM_TRAV_AUTO_CD_H01_2+KM_TRAV_AUTO_LD_H01_2") %>%
#      filter(year==2010) %>%
#      select(value))[1,])*1000

KM_AUTO_TOT<-as.numeric(
    ThreeME %>%
       filter(year==2010) %>%
       filter(str_detect(Var, 'KM_AUTO_H01_C'))%>%
      summarise(sum(value))
    )*1000


AUTO_H01_2<-as.numeric(
  (ThreeME %>% 
     filter(Var=="AUTO_H01_2") %>%
     filter(year==2010) %>%
     select(value))[1,])*1000

#vkm moyen
KM_AUTO_2010<-KM_AUTO_TOT/AUTO_H01_2

menage_echelle <- 
  menage_echelle %>%
  mutate_when(is.na(km_auto),list(km_auto=KM_AUTO_2010))


#Hypothèse : on choisit de négliger l'évolution du km moyen par véhicule au cours de la projection. En effet le TC des vkm moyens est de 0.78% en 2035,  0.29% en 2030, 0.074% en 2025. 
# KM_AUTO
KM_AUTO_TOT<-as.numeric(
  ThreeME %>%
    filter(year==horizon) %>%
    filter(str_detect(Var, 'KM_AUTO_H01_C'))%>%
    summarise(sum(value))
)*1000


AUTO_H01_2<-as.numeric(
  (ThreeME %>% 
     filter(Var=="AUTO_H01_2") %>%
     filter(year==horizon) %>%
     select(value))[1,])*1000

#vkm moyen (quelque soit motorisation)
KM_AUTO<-KM_AUTO_TOT/AUTO_H01_2

ratio_TC_KM<-KM_AUTO/KM_AUTO_2010
ratio_TC_KM



# BONUS-MALUS -------------------------------------------------------------

#2010
BM_net_2010 <- 0 #Bonus-malus net pour les Véhicules thermiques
Vol_tot_2010<-0
for (x in LETTERS[1:7]){
  Vol<-as.numeric(ThreeME %>% 
                    filter(year==2010)%>%
                    filter(Var==paste("NEWAUTO_TH_H01_C",x,"_2",sep=""))%>%
                    select(value))*1000
  Vol_tot_2010<-Vol_tot_2010+Vol
  BM<- as.numeric(ThreeME %>% 
                    filter(year==2010)%>%
                    filter(Var==paste("BONUS_TH_H01_C",x,"_2",sep=""))%>%
                    select(value))*1000
  BM_net_2010<-BM_net_2010+Vol*BM
  
}
BM_avg_2010<-BM_net_2010/Vol_tot_2010

TOT_VTH_nv_2010<-as.numeric( #en Milliers
  (ThreeME %>% 
     filter(Var=="NEWAUTO_TH_H01_2") %>%
     filter(year==2010) %>%
     select(value))[1,]
) * 1000

TOT_VTH_euros_2010 <-as.numeric( # en Millions €
  ThreeME %>% 
    filter(Var=="PNEWAUTO_TH_H01_2*NEWAUTO_TH_H01_2") %>%
    filter(year==2010) %>%
    select(value)
) * 10^6


P_VTH_2010 <- TOT_VTH_euros_2010/TOT_VTH_nv_2010

#Bonus-malus relatif au prix d'un VT
BM_rel_2010<-BM_avg_2010/P_VTH_2010


#Horizon
BM_net <- 0 #Bonus-malus net pour les Véhicules thermiques
Vol_tot<-0
for (x in LETTERS[1:7]){
  Vol<-as.numeric(ThreeME %>% 
                    filter(year==horizon)%>%
                    filter(Var==paste("NEWAUTO_TH_H01_C",x,"_2",sep=""))%>%
                    select(value))*1000
  Vol_tot<-Vol_tot+Vol
  BM<- as.numeric(ThreeME %>% 
                    filter(year==horizon)%>%
                    filter(Var==paste("BONUS_TH_H01_C",x,"_2",sep=""))%>%
                    select(value))*1000
  BM_net<-BM_net+Vol*BM
  
}
BM_avg<-BM_net/Vol_tot
# BM_avg
# Coût moyen d'un VT
## PRIX UNITAIRE d'un Veh_thermique
# Données ThreeME de stock (en milliers)
TOT_VTH_nv<-as.numeric( #en Milliers
  (ThreeME %>% 
     filter(Var=="NEWAUTO_TH_H01_2") %>%
     filter(year==horizon) %>%
     select(value))[1,]
) * 1000

TOT_VTH_euros <-as.numeric( # en Millions €
  ThreeME %>% 
    filter(Var=="PNEWAUTO_TH_H01_2*NEWAUTO_TH_H01_2") %>%
    filter(year==horizon) %>%
    select(value)
) * 10^6


P_VTH <- TOT_VTH_euros/TOT_VTH_nv

#Bonus-malus relatif au prix d'un VT
BM_rel<-BM_avg/P_VTH
# BM_rel


# La partie du bonus-malus net de 2010 a été mise à l'échelle via la croissance des revenus et l'évolution des prix, mais elle représente toujours BM_rel_2010 du prix payé par les consommateurs, soit 0.76%. Le pourcentage duquel on augmente le prix payé par les VT est donc de BM_rel-0.76%
BM_rel_net<-BM_rel-BM_rel_2010

Bonus_VE<-as.numeric(ThreeME %>% 
                       filter(year==horizon)%>%
                       filter(Var=="BONUS_ELEC_H01_2")%>%
                       select(value))*1000



# SUCCESS -----------------------------------------------------------------

print("1_VE_classement_2025 : SUCCESS")


