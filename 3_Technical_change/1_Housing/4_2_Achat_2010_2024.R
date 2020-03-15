# Constructions neuves entre 2010 et 2024 :
  # Selection des ménages
  # Mise à jour budgets



# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(dplyr)

# DATA --------------------------------------------------------------------

setwd("D:/CIRED/Projet_Ademe")

# load("2025/Mat_gain_ener_2025.RData")
# load("2025/menage_DPE_neuf_2025.RData")
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_DPE_neuf_",horizon,".RData",sep=""))

load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_41.RData",sep=""))
# load("Technical_change/TC_renovation_DPE/menage_echelle_41.RData")
load("2010/depmen.RData")
load("2010/auto.RData")

# load("2025/menage_ener_dom_2025.RData")
menage_echelle<-menage_echelle_41
# load("2025/c13_2025.RData")
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/ident_accedants.RData",sep=""))
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))


load("Donnees_brutes/Sorties ThreeMe/ThreeME.RData")
source("Code_global_Ademe/mutate_when.R")
source("Code_global_Ademe/compute_share.R")
source("Code_global_Ademe/compute_share_export.R")
source("Code_global_Ademe/compute_savings_rate_export.R")
source("Code_global_Ademe/mutate_when.R")
source("Code_global_Ademe/verif_epargne_taux.R")
source("Code_global_Ademe/maj_dep_preeng.R")
source("Technical_change/Repayment.R")

load("Technical_change/TC_renovation_DPE/list_source_usage.RData")




# DONNEES MANUELLES -------------------------------------------------------

sources=c("Elec","Gaz","Fuel","GPL","Urbain","Solides")
dep_sources=paste("dep",sources,sep="_")
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



###
# CHOIX PESSIMISTE vs OPTIMISTE
###

# scenario="PESSIMISTE"
# scenario="OPTIMISTE"
# scenario="MEDIAN"
# scenario="RICH"
# scenario="POOR"
# print(paste("SCENARIO", scenario,sep=" "))




# DATA ThreeME ------------------------------------------------------------

# VOLUME CONSTRUCTION NEUF ------------------------------------------------
# en m2

NEWBUIL_H01_CA_2<-
  ThreeME %>% 
  filter(year<horizon & year >=2010) %>%
  filter(Var=="NEWBUIL_H01_CA_2")%>%
  select(year,value)

NEWBUIL_H01_CB_2<-
  ThreeME %>% 
  filter(year<horizon & year >=2010) %>%
  filter(Var=="NEWBUIL_H01_CB_2")%>%
  select(year,value)

NEWBUIL_H01_CC_2<-
  ThreeME %>% 
  filter(year<horizon & year >=2010) %>%
  filter(Var=="NEWBUIL_H01_CC_2")%>%
  select(year,value)








# SELECTION MENAGE --------------------------------------------------------


# # Exclusion des ménages accédants en horizon, on veut que les budgets réflètent un achat entre 2011 et 2024 donc sans dep c13711
# ident_accedants <- ident
  
  # c13_horizon %>% filter(c13711>0) %>% select(ident_men,c13711)
# 174 ménages



menage_echelle<-
  menage_echelle %>%
  left_join(depmen %>% select(ident_men, stalog,ancons,prixrp_d,remb,totpre_d,mcred1_d,mcred2_d,mcred3_d,mcred4_d,mcred5_d,mcred6_d,mcred7_d,mcred8_d,mcred9_d),by="ident_men") 
  
  menage_echelle$mcred1_d[which(is.na(menage_echelle$mcred1_d))]<-0
  menage_echelle$mcred2_d[which(is.na(menage_echelle$mcred2_d))]<-0
  menage_echelle$mcred3_d[which(is.na(menage_echelle$mcred3_d))]<-0
  menage_echelle$mcred4_d[which(is.na(menage_echelle$mcred4_d))]<-0
  menage_echelle$mcred5_d[which(is.na(menage_echelle$mcred5_d))]<-0
  menage_echelle$mcred6_d[which(is.na(menage_echelle$mcred6_d))]<-0
  menage_echelle$mcred7_d[which(is.na(menage_echelle$mcred7_d))]<-0
  menage_echelle$mcred8_d[which(is.na(menage_echelle$mcred8_d))]<-0
  menage_echelle$mcred9_d[which(is.na(menage_echelle$mcred9_d))]<-0
  menage_echelle$totpre_d[which(is.na(menage_echelle$totpre_d))]<-0
  menage_echelle<-
    menage_echelle %>% mutate(mcred_tot=(mcred1_d+mcred2_d+mcred3_d+mcred4_d+mcred5_d+mcred6_d+mcred7_d+mcred8_d+mcred9_d)*as.numeric(FC$A12))%>%mutate(totpre_d=totpre_d*as.numeric(FC$A05))

  
  menage_echelle <- menage_echelle %>%
    mutate(solv=ifelse(RDB==0,999,(mcred_tot+totpre_d)/RDB))%>%
    select(-c(totpre_d,mcred1_d,mcred2_d,mcred3_d,mcred4_d,mcred5_d,mcred6_d,mcred7_d,mcred8_d,mcred9_d))

menage_echelle<-
  menage_echelle %>%
  mutate(exclus=FALSE,NEUF=FALSE) %>%
  mutate_when(year_neuf==horizon,list(exclus=TRUE),
              ident_men %in% ident_accedants,list(exclus=TRUE),
              DPE_dep=="A",list(exclus=TRUE),
              stalog>2,list(exclus=TRUE)) %>%
  mutate_when(!year_neuf==horizon, list(classe_arr=DPE_dep))%>%
  mutate(solde_int=0,solde_ener=0,principal_dette=0,solde_princ=0,solde_int_prov=0,solde_int_prov=0)%>%
  mutate_when(solv>0.28,list(exclus=TRUE))%>%
  mutate_when(ident_men==8063,list(exclus=TRUE))%>% #menage trop fragile qui crée des NA
  mutate_when(ident_men==10583,list(exclus=TRUE)) #(ménage qui bug en AME 2025 Pess decile)

menage_echelle <- 
  menage_echelle %>%
  left_join(depmen%>%select(ident_men,totpre_d),by="ident_men")

rm(depmen)


# rm(menage_echelle_41)
rm(menage_DPE_neuf_horizon)
# rm(menage_ener_dom_horizon)
#NEUF va indiquer les ménages sélectionnés pour rénover leur logement : 
# passer de DPE_pred à class_arr



# Classement DPE --------------------------------------------------------------
# Precision d'utiliser mutate de dplyr et pas de plyr

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol


menage_echelle<- menage_echelle %>% mutate_when(is.na(ener_dom),list(ener_dom=0))

menage_echelle<-
  menage_echelle %>% 
  dplyr::mutate(kWh_rank_opt =row_number(-ener_dom))

menage_echelle <-
  menage_echelle %>% 
  dplyr::mutate(kWh_rank_pess =max(kWh_rank_opt,na.rm=T)-kWh_rank_opt+1)

menage_echelle<-
  menage_echelle %>% 
  dplyr::mutate(kWh_rank_med =kWh_rank_pess-kWh_rank_opt) %>% 
  mutate(L=max(kWh_rank_opt,na.rm=T)) %>%
  mutate_when(
    kWh_rank_med<0,
    list(
      kWh_rank_med = ifelse(
        is.wholenumber(L/2),
        -kWh_rank_med+1,
        -kWh_rank_med-1)
    )
  ) %>%
  select(-L)

menage_echelle <-
  menage_echelle %>% 
  dplyr::mutate(kWh_rank_rich=row_number(-RDB/coeffuc))

menage_echelle <-
  menage_echelle %>%
  dplyr::mutate(kWh_rank_poor=max(kWh_rank_rich)-kWh_rank_rich+1)






if(str_detect(scenario_classement,"Pessimiste")){
  menage_echelle <- menage_echelle %>% mutate(kWh_rank=kWh_rank_pess)
}
if(str_detect(scenario_classement,"Optimiste")){
  menage_echelle <- menage_echelle %>% mutate(kWh_rank=kWh_rank_opt)
}
if(scenario_classement=="Median"){
  menage_echelle <- menage_echelle %>% mutate(kWh_rank=kWh_rank_med)
}
if(scenario_classement=="Rich"){
  menage_echelle <- menage_echelle %>% mutate(kWh_rank=kWh_rank_rich)
}
if(scenario_classement=="Poor"){
  menage_echelle <- menage_echelle %>% mutate(kWh_rank=kWh_rank_poor)
}


# SELECTION DES MENAGES ---------------------------------------------------

menage_echelle <- 
  menage_echelle %>%
  mutate_when(exclus,list(kWh_rank=0))



# ANNEE PAR ANNEE
#important pour que les ménages puissent faire plusieurs REHAB

ident_rehab=data.frame("Year"=c(),"list_ident"=c())
A1<-menage_echelle
ident_r<-c()

for (Y in 2011:(horizon-1)){
# for (Y in 2010:2023){
  print(Y)
  ident_r<-c()

  menage_echelle <- menage_echelle %>% mutate(principal_dette=0)
  # Mat_gain_ener -----------------------------------------------------------

  # Extraction de la conso moyenne au m2 en kWH par classe DPE
  conso_moy_dep=data.frame("A"=0, "B"=0, "C"=0, "D"=0, "E"=0, "F"=0, "G"=0)
  for (i in LETTERS[1:7]){
    conso_moy_dep[i]<-
      as.numeric(
        ThreeME %>%
          filter(Var==
                   paste("ENER_BUIL_H01_C",i,"_2*11630/BUIL_H01_C",i,"_2",sep="")
          ) %>%
          filter(year==Y) %>%
          select(value)
      )
  }

  Mat_gain_ener<-data.frame("DPE_before"=sort(rep(LETTERS[1:7],7)),"DPE_after"=rep(LETTERS[1:7],7))

  Mat_gain_ener$value_after<-sapply(Mat_gain_ener$DPE_after,function(x) as.numeric(conso_moy_dep[x]))
  Mat_gain_ener$value_before<-sapply(Mat_gain_ener$DPE_before,function(x) as.numeric(conso_moy_dep[x]))
  Mat_gain_ener$value<-(Mat_gain_ener$value_after-Mat_gain_ener$value_before)/Mat_gain_ener$value_before

  Mat_gain_ener <- Mat_gain_ener %>% select(-c(value_after,value_before))

  # DONNEES THREEME ---------------------------------------------------------
  # travaux de rénovation énergétiques en volume par saut de classe (en M2)
  # Transition de L vers M
  # Dépenses en constructions neuves en valeur (M€ courants)
  PNEWBUIL_H01_2_NEWBUIL_H01_2_Y <-
    as.numeric(
      ThreeME %>%
        filter(Var=="PNEWBUIL_H01_2*NEWBUIL_H01_2") %>%
        filter(year==Y) %>%
        select(value)
    )*10^6

  NEWBUIL_H01_2_Y<-
    as.numeric(
      ThreeME %>%
        filter(Var=="NEWBUIL_H01_2") %>%
        filter(year==Y) %>%
        select(value)
    )


  NEWBUIL_H01_2_2010<-
    as.numeric(
      ThreeME %>%
        filter(Var=="NEWBUIL_H01_2") %>%
        filter(year==2010) %>%
        select(value)
    )
  
  
  # Dépenses en constructions neuves en valeur (M€ courants) 2010
  PNEWBUIL_H01_2_NEWBUIL_H01_2_2010 <-
    as.numeric(
      ThreeME %>%
        filter(Var=="PNEWBUIL_H01_2*NEWBUIL_H01_2") %>%
        filter(year==2010) %>%
        select(value)
    )*10^6
  
  # Prix du m2 de logement neuf en horizon
  PNEWBUIL_H01_2_Y<-
    PNEWBUIL_H01_2_NEWBUIL_H01_2_Y /
    NEWBUIL_H01_2_Y
  
  
  # Prix du m2 de logement neuf en 2010
  PNEWBUIL_H01_2_2010<-
    PNEWBUIL_H01_2_NEWBUIL_H01_2_2010 /
    NEWBUIL_H01_2_2010
  
  #ratio du prix du m2 neuf en 2010 et horizon
  ratio_prix_m2<-PNEWBUIL_H01_2_Y/PNEWBUIL_H01_2_2010

  # taux de remboursement des constructions neuves
  R_RMBS_NEWBUIL_H01_CA<-
    as.numeric(
      ThreeME %>%
        filter(Var=="R_RMBS_NEWBUIL_H01_CA") %>%
        filter(year==Y) %>%
        select(value)
    )


  # Taux d'intérêts des emprunts liés à la construction neuve en %
  R_I_BUIL_H01_CG_2<-
    as.numeric(
      ThreeME %>%
        filter(Var=="R_I_BUIL_H01_CG_2") %>%
        filter(year==Y) %>%
        select(value)
    )

  # BASCULE -----------------------------------------------------------------
  
  for (arr in LETTERS[1:3]){
 
     if(arr=="A"){stock_m2_trans=NEWBUIL_H01_CA_2 %>% filter(year==Y)%>%select(value)}
      if(arr=="B"){
        stock_m2_trans=NEWBUIL_H01_CB_2 %>% filter(year==Y)%>%select(value)
        menage_echelle <-
          menage_echelle %>%
          mutate_when(DPE_dep=="B",list(kWh_rank=0))
      }
    if(arr=="C"){
      stock_m2_trans=NEWBUIL_H01_CC_2 %>% filter(year==Y)%>%select(value)
      menage_echelle <-
        menage_echelle %>%
        mutate_when(DPE_dep=="C",list(kWh_rank=0))
    }
    
        sum=0

        i=1
        while(!i %in% menage_echelle$kWh_rank){i=i+1}

        while(sum<stock_m2_trans){
          sum =
            sum +
            as.numeric(menage_echelle %>% filter(kWh_rank==i) %>% summarise(sum(pondmen*surfhab_d)))

          # identifiant du ménage sélectionné
          im<-as.numeric(menage_echelle %>% filter(kWh_rank==i) %>% select(ident_men))
          # print(im)
          ident_r<-c(ident_r,im)
          # Modification des variables REHAB et class_arr dans la base globale
          menage_echelle<- menage_echelle %>%
            mutate_when(ident_men==im,list(NEUF=TRUE,classe_arr=arr,year_neuf=Y,kWh_rank=0))

          # Itération, le non prise en compte des constructions neuves
          # fait disparaîtres certains rangs du classement
          i=i+1
          while(!i %in% menage_echelle$kWh_rank){i=i+1}

        }
  }

        
        for (dep in LETTERS[1:7]){
          for (arr in LETTERS[1:7]){
        

        rate_gain_ener<-as.numeric(
          Mat_gain_ener %>%
            filter(DPE_before==dep) %>%
            filter(DPE_after==arr) %>%
            select(value))
        # print(rate_gain_ener)

        if(dim(menage_echelle %>% filter(year_neuf==Y & DPE_dep==dep & classe_arr==arr) %>% select(ident_men))[1]>0){
          menage_echelle <-
            menage_echelle %>%
            mutate_when(
              # Condition
              year_neuf==Y &
                DPE_dep==dep &
                classe_arr==arr,
              # Action
              list(
                principal_dette=ifelse(is.na(prixrp_d) || prixrp_d<10^5,PNEWBUIL_H01_2_Y*surfhab_d,prixrp_d*ratio_prix_m2),# par sécurité ne prendre que le surcout en compte, être sûr de ne pas compter deux fois un éventuel surcoût
                #Energie
                Elec_ECS=Elec_ECS*(1+rate_gain_ener),
                Gaz_ECS=Gaz_ECS*(1+rate_gain_ener),
                GPL_ECS=GPL_ECS*(1+rate_gain_ener),
                Fuel_ECS=Fuel_ECS*(1+rate_gain_ener),
                Solides_ECS=Solides_ECS*(1+rate_gain_ener),
                Urbain_ECS=Urbain_ECS*(1+rate_gain_ener),
                Elec_chauff=Elec_chauff*(1+rate_gain_ener),
                Gaz_chauff=Gaz_chauff*(1+rate_gain_ener),
                GPL_chauff=GPL_chauff*(1+rate_gain_ener),
                Fuel_chauff=Fuel_chauff*(1+rate_gain_ener),
                Solides_chauff=Solides_chauff*(1+rate_gain_ener),
                Urbain_chauff=Urbain_chauff*(1+rate_gain_ener),
                Elec_clim=Elec_clim*(1+rate_gain_ener)
                # ,
                # Gaz_clim=Gaz_clim*(1+rate_gain_ener),
                # GPL_clim=GPL_clim*(1+rate_gain_ener),
                # Fuel_clim=Fuel_clim*(1+rate_gain_ener),
                # Solides_clim=Solides_clim*(1+rate_gain_ener),
                # Urbain_clim=Urbain_clim*(1+rate_gain_ener)
              ))



          menage_echelle$solde_int_prov <-  sapply(menage_echelle$principal_dette, function(X) ifelse(X==0,0,as.numeric(int_princ(loan=X,                                                                                                   n=1/R_RMBS_NEWBUIL_H01_CA,
                                                                                                                               year_purchase = Y,
                                                                                                                               horizon=horizon,
                                                                                                                               i=R_I_BUIL_H01_CG_2,
                                                                                                                               pf=1)[1])-2/3*if(is.na(menage_echelle$totpre_d)){0}else{menage_echelle$totpre_d*ratio_prix_m2}))
          menage_echelle$solde_princ_prov<-sapply(menage_echelle$principal_dette, function(X) ifelse(X==0,0,as.numeric(int_princ(loan=X,
                                                                                                                              n=1/R_RMBS_NEWBUIL_H01_CA,
                                                                                                                              year_purchase = Y,
                                                                                                                              horizon=horizon,
                                                                                                                              i=R_I_BUIL_H01_CG_2,
                                                                                                                              pf=1
          )[2])-1/3*if(is.na(menage_echelle$totpre_d)){0} else{menage_echelle$totpre_d*ratio_prix_m2}))

          
          menage_echelle <- 
            menage_echelle %>%
            mutate_when(year_neuf==Y,list(solde_int=solde_int_prov,solde_princ=solde_princ_prov))
          
        }
        
        }
        }
  # print(compute_share_export(menage_echelle))
    }
  
  
rm(i,sum,stock_m2_trans)

menage_echelle <- menage_echelle %>% select(-solde_int_prov,-solde_princ_prov)
  sauv_int<-menage_echelle
# menage_echelle<-sauv_int
  
 
  # SOLDE_ENER --------------------------------------------------------------
  
  # Mise à jour des totaux
  menage_echelle<-
    menage_echelle %>% 
    mutate(
      Elec=rowSums(menage_echelle %>% select(list_source_usage) %>% select(starts_with("Elec"))),
      Gaz=rowSums(menage_echelle %>% select(list_source_usage) %>% select(starts_with("Gaz"))),
      GPL=rowSums(menage_echelle %>% select(list_source_usage) %>% select(starts_with("GPL"))),
      Fuel=rowSums(menage_echelle %>% select(list_source_usage) %>% select(starts_with("Fuel"))),
      Urbain=rowSums(menage_echelle %>% select(list_source_usage) %>% select(starts_with("Urbain"))),
      Solides=rowSums(menage_echelle %>% select(list_source_usage) %>% select(starts_with("Solides")))
    )
  
  # Due à la fusion Sources et Dep_sources sont redondants, la mise à jour de Sources permet de déduire facilement le solde sur tous les sources d'éneergie
  menage_echelle$solde_ener<-
    rowSums(menage_echelle[sources]) -
    rowSums(menage_echelle[dep_sources])
  
  A<-menage_echelle %>% filter(abs(solde_ener)>10^(-9))%>% select(ident_men)
  
  menage_echelle %>% filter(NEUF) %>% filter(!year_neuf==horizon) %>% filter(!ident_men %in% A$ident_men) %>% select(ident_men)
  # 5797
  # View(rbind(menage_echelle))
  
  menage_echelle<-
    menage_echelle %>% 
    mutate(
      dep_Elec=Elec,
      dep_Gaz=Gaz,
      dep_GPL=GPL,
      dep_Fuel=Fuel,
      dep_Solides=Solides,
      dep_Urbain=Urbain)
  
  menage_echelle$dep_energie=rowSums(menage_echelle[dep_sources])
  menage_echelle$dep_energie_logement=rowSums(menage_echelle[
    c("Elec_ECS","Gaz_ECS","GPL_ECS","Fuel_ECS","Solides_ECS","Urbain_ECS","Elec_chauff","Gaz_chauff",
      "GPL_chauff","Fuel_chauff","Solides_chauff","Urbain_chauff","Elec_clim")])
  
  
  # SOLDE_DETTE -------------------------------------------------------------
  solde<-menage_echelle %>% 
    mutate(solde=solde_ener+solde_int
           # +solde_princ
           ) %>%
    select(ident_men,solde)
  
  # 
  # menage_echelle %>%left_join(solde, by="ident_men")%>%filter(solde>RDB_reel) %>%select(ident_men)
  # menage_echelle %>%left_join(solde, by="ident_men")%>%filter(solde>Rcons) %>%select(ident_men)
  # 
  # df<-menage_echelle%>%select(starts_with("elast_rev"))
  # df$max <- apply(df, 1, max)
  # menage_echelle$elast_rev_max<-df$max
  # menage_echelle %>%
  #   left_join(solde, by="ident_men")%>%
  #    filter(solde>(RDB_reel/elast_rev_max)) %>%select(ident_men)
  
  
  
  menage_echelle <- 
    menage_echelle %>%
    mutate(autres_services=autres_services+solde_int,
           solde_int_total=solde_int_total+solde_int,
           solde_princ_total=solde_princ_total+solde_princ,
           Hors_budget=Hors_budget+solde_princ)
  
  A1<-menage_echelle
  # VENTILATION -------------------------------------------------------------
  source("Technical_change/Econometrie_solde_budg_Logement.R")
  # source("Technical_change/Econometrie_solde_budg_bouclage_autres.R")
  
  Ventil_solde(solde,menage_echelle)
  
  
  menage_echelle <- A 
  # %>%
    # mutate(autres=autres+solde_int,Hors_budget=Hors_budget+solde_princ)
  # Recalcul de toutes les variables impactées : Rcons, épargne, ratio_S, RDB
  
  # Rcons
  menage_echelle$Rcons <- 
    rowSums(menage_echelle[list_dep])
  
  # Parts budgétaires
  for (k in list_dep){
    menage_echelle[paste("part",k,sep="_")]<-menage_echelle[k]/menage_echelle$Rcons
  }
  
  # Epargne
  menage_echelle$epargne <- 
    menage_echelle$RDB - 
    menage_echelle$Rcons + 
    menage_echelle$rev_exceptionnel
  
  # Ratio_S
  menage_echelle$ratio_S <-  
    menage_echelle$epargne / 
    menage_echelle$Rcons 
  
  # Taux épargne
  menage_echelle$taux_epargne<- ifelse(menage_echelle$RDB==0,0,
                                            menage_echelle$epargne / 
                                              menage_echelle$RDB)
  
  
  
  source("Technical_change/TC_renovation_DPE/calc_energie_kWh_m2.R")
  
  energie_dom_surf(menage_echelle)
  
  menage_echelle<- 
    menage_echelle %>% 
    select(-ener_dom_surf,-ener_dom) %>%
    left_join(dep_source_usage,by="ident_men")
  
  
  menage_echelle <- menage_echelle %>% mutate_when(year_neuf>0,list(NEUF=TRUE))
  
  A2<-menage_echelle %>% select(-kWh_rank_pess,-kWh_rank_opt,-kWh_rank,-solde_dette,-solde_ener)
  
  
  
  # VERS LA PROCHAINE ETAPE -------------------------------------------------
  
  # ident_rehab=cbind(ident_rehab,c(Y,menage_echelle%>%filter(REHAB)%>%select(ident_men)))





# SAVE --------------------------------------------------------------------

menage_echelle_42<-menage_echelle
  # %>% mutate(DPE_2024=DPE_dep) %>% select(-stalog,-propri,-REHAB,-DPE_dep,-classe_arr ,-kWh_rank_pess,-kWh_rank_opt,-kWh_rank,-REHAB,-classe_arr)
# load("Technical_change/TC_renovation_DPE/menage_echelle_42.RData")

  
  

# Parts Budgétaires -------------------------------------------------------

print(compute_share_export(menage_echelle_42))
print(compute_savings_rate_export(menage_echelle_42))  



# Maj_dep_preeng ----------------------------------------------------------

menage_echelle_42 <- maj_dep_preeng(bdd1= menage_echelle_41,bdd2=menage_echelle_42)





# SAVE --------------------------------------------------------------------


load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_41.RData",sep=""))

#
# inter<-intersect(colnames(menage_echelle_42), colnames(menage_echelle_41))
# not_inter<-setdiff(colnames(menage_echelle_42), colnames(menage_echelle_41))

# menage_echelle_42<-menage_echelle_42 %>% select(inter)



save(menage_echelle_42, file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_42.RData",sep=""))

# 
# 
# 
# 
# 
# 
# 
# # Suppression des bases ---------------------------------------------------
# 
# rm(
#   tot_Constr_neuf_10_24,
#   Constr_neuf_10_24,
#   sum, 
#   i,
#   scenario,
#   dep_sources,
#   len,
#   A,
#   A1,A3,A4,
#   ThreeME,
#   c13_2025,
#   dep_ener_2025,
#   ident_accedants,
#   im,
#   menage_echelle_prop,
#   menage_echelle,
#   solde,
#   rate_gain_ener,
#   list_source_usage,
#   j,arr,dep,
#   Mat_gain_ener_2025
# )
# 
# 
# 
# # VERIF prix au M2 --------------------------------------------------------
# 
# # CCL : impossible de vérifier que les prix de construction au m2 
# # sont cohérents avec les données de THREEME. 
# # La variable prixrp de DEPMEN est trop parcellaire.
# 
# 

# SUCCESS -----------------------------------------------------------------

print("4_2_Achat_2010_2024 : SUCCESS")
# 
# 
# # load("2010/depmen.RData")
# # 
# # IM<-menage_echelle %>% filter(NEUF)%>% select(ident_men)
# # 
# # Prix<-
# #   depmen %>% 
# #   select(ident_men,prixrp_d,surfhab_d) %>% 
# #   filter(ident_men %in% IM$ident_men) %>% 
# #   mutate(prix_2=prixrp_d/surfhab_d) %>%
# #   mutate_when(is.na(prix_2),list(prix_2=0))
# # 
# # dim(Prix)
# # 
# # head(Prix$surfhab_d)
# # head(Prix$prixrp_d)
# # table(is.na(Prix$prixrp_d)) #=> Tous des NA
# # 
# # Prix %>% summarise(mean(prix_2))
# 
# # rm(depmen)
# 

