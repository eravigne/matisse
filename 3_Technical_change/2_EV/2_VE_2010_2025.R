


# LIBRARIES ---------------------------------------------------------------
library(tidyverse)


# DATA --------------------------------------------------------------------
setwd("D:/CIRED/Projet_Ademe")

load("Donnees_brutes/Sorties ThreeMe/ThreeME.RData")

### Paramètres emprunts
# taux d'intérêts des emprunts liés à l'achat d'automobile en %  : R_I_AUTO_H01_CG_2
# taux de remboursement des automobiles : R_RMBS_AUTO_ELEC_H01_CA

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


# SOURCE ------------------------------------------------------------------

source("Technical_change/TC_VE/1_VE_classement_2025.R")
source("Technical_change/Repayment.R")
source("Code_global_Ademe/compute_share.R")
source("Code_global_Ademe/verif_epargne_taux.R")
source("Code_global_Ademe/compute_share_export.R")
source("Code_global_Ademe/compute_savings_rate_export.R")
source("Code_global_Ademe/maj_dep_preeng.R")
###
  # CHOIX PESSIMISTE VS OPTIMSITE -------------------------------------------
###


# scenario="PESSIMISTE"
# scenario="OPTIMISTE"
# print(paste("SCENARIO", scenario,sep=" "))
if(str_detect(scenario_classement,"Pessimiste")){
  menage_echelle <- menage_echelle %>% mutate(VE_rank=VE_rank_pess)
}
if(str_detect(scenario_classement,"Optimiste")){
  menage_echelle <- menage_echelle %>% mutate(VE_rank=VE_rank_opt)
}
if(scenario_classement=="Median"){
  
  menage_echelle <- menage_echelle %>% mutate(VE_rank=VE_rank_med)
}
if(scenario_classement=="Rich"){
  menage_echelle <- menage_echelle %>% mutate(VE_rank=VE_rank_rich)
}
if(scenario_classement=="Poor"){
  menage_echelle <- menage_echelle %>% mutate(VE_rank=VE_rank_poor)
}



# menage_echelle$solv<-menage_echelle$solv+(menage_echelle$solde_int_total+menage_echelle$solde_princ_total)/menage_echelle$RDB
# 
# menage_echelle<-menage_echelle %>% mutate_when(is.na(solv) & !is.na(VE_rank),list(VE_rank=0),
#                                                !is.na(solv) &solv>0.15 & !is.na(VE_rank),list(VE_rank=0))

# VE 2010 --------------------------------------------------------------------

menage_echelle <- 
  menage_echelle %>%
  mutate(VE=FALSE) %>%
  mutate(new_VE=FALSE) %>%
  mutate(year_VE=0)%>%
  mutate_when(
    ident_men %in% auto_elec$ident_men,
    list(VE=TRUE,year_VE=2010)
  ) 


# TESTS -------------------------------------------------------------------

#Differents cas Entre 6 et 13 ans pour les ménages qu'on sélectionne pour le VE => on ne choisit que des ménages n'ayant pas de remboursement en cours sur leur véhicule le plus récent
# test=5850 #bascule en 2014 #seule incidence sur le budget, solde_elec et solde_carb, Rcons au même niveau (reventilation), même épargne
# test=1497 #bascule en 2023 mcrevoi_d=0 # incidence sur le budget, Rcons constant, epargne 
test=14604 #bascule 2024, mcrevoi_d=260

Before<-menage_echelle %>% mutate(
  solde_carb=0,
  solde_elec=0,
  solde_dette=0,
  solde_veh=0,
  solde_rev_capital=0
)

A1 <- Before %>% filter(ident_men==test)


# BASCULE DES MENAGES 2011-2025 -------------------------------------------
# Si j'achète un VE à l'année n, alors le remplacement de celui ci intervient à la 14e année, soit à n+13
# horizon=2025


IM=c()
#Classement par lequel on va commencer
i=1
while(!i %in% menage_echelle$VE_rank & i<max(menage_echelle$VE_rank,na.rm=T) ){i=i+1}

menage_echelle <- 
  menage_echelle %>% 
  mutate(
    solde_carb=0,
    solde_elec=0,
    solde_dette=0,
    solde_veh=0,
    solde_malus=0,
    solde_rev_capital=0,
    solde_int=0,
    solde_princ=0,
  )


# sauv_int<-menage_echelle
for (Y in 2010:horizon){

# for (Y in 2010:2033){
  print(Y)
  
  # print(paste("année",Y,sep=" "))
  menage_echelle <-
    menage_echelle %>%
    mutate(new_VE=FALSE)
  
  TOT_VE_nv <-as.numeric( #en Milliers
    (ThreeME %>% 
      filter(Var=="NEWAUTO_ELEC_H01_2") %>%
      filter(year==Y) %>%
      select(value)
  )[1,]) * 1000
  
  #on renouvelle tout d'abord le stock de VE achetés 13 ans plus tôt
  if(Y>2022){
    menage_echelle <-
    menage_echelle %>%
    mutate_when(year_VE==Y-13,
                list(year_VE=Y,new_VE=TRUE))}
  
  
  sum=menage_echelle %>% filter(year_VE==Y) %>% summarise(sum(pondmen))
  if(is.na(sum)){sum=0}
  
  
  
  # La trajectoire de ThreeME prévoit de plus en plus de ventes de VE, le renouvellement de la flotte ne saurait entièrement occuper les ventes de l'année Y 
     # while(!i %in% menage_echelle$VE_rank & i<max(menage_echelle$VE_rank,na.rm=T) ){i=i+1}
  while (sum<TOT_VE_nv & i<max(menage_echelle$VE_rank,na.rm=T)){
    #Pour les ménages qu'on sélectionne entre 6 et 13 ans, ils doivent avoir fini de rembourser leur éventuel crédit ou reconstitué leur épargne, on enlève donc les ménages où crevoi==1
    if(Y>horizon-13 & Y<=horizon-6){
      while((!i %in% menage_echelle$VE_rank  & i<max(menage_echelle$VE_rank,na.rm=T)) || menage_echelle %>% filter(VE_rank==i) %>%select(mcrevoi_d)<0){i=i+1}
      im<-as.numeric(menage_echelle %>% filter(VE_rank==i)  %>% select(ident_men))
      # im= identifiant du ménage sélectionné 
    }
    if(Y==horizon-13){
      while((!i %in% menage_echelle$VE_rank  & i<max(menage_echelle$VE_rank,na.rm=T)|| !menage_echelle %>% filter(VE_rank==i) %>%select(prod_veh)>5*10^(3) )){i=i+1}
      im<-as.numeric(menage_echelle %>% filter(VE_rank==i)  %>% select(ident_men))
    }
    if(Y==horizon){
      while((!i %in% menage_echelle$VE_rank & i<max(menage_echelle$VE_rank,na.rm=T)) || (menage_echelle %>% filter(VE_rank==i) %>%select(prod_veh)<10^(3)) &i<max(menage_echelle$VE_rank,na.rm=T) ){i=i+1}
      im<-as.numeric(menage_echelle %>% filter(VE_rank==i)  %>% select(ident_men))
    }
    if(Y<horizon-13 || (Y>horizon-6 & Y<horizon)){
      # im= identifiant du ménage sélectionné 
      # while(!i %in% menage_echelle$VE_rank ){i=i+1}
      im<-as.numeric(menage_echelle %>% filter(VE_rank==i) %>% select(ident_men))
      }
    
    
     sum =
      sum +
      as.numeric(menage_echelle %>% filter(ident_men==im) %>% select(pondmen))
    
   
    IM<-c(IM,im)
    # print(im)
    menage_echelle<- 
      menage_echelle %>% 
      mutate_when(ident_men==im,list(VE=TRUE,new_VE=TRUE,year_VE=Y,VE_rank=0))
    
    # Itération, le non prise en compte des ménages ayant déjà un VE
    # fait disparaître certains rangs du classement
    # if(Y==horizon-1){ menage_echelle<- 
    #   menage_echelle %>% 
    #   mutate_when(is.na(prod_veh) || prod_veh<=0, list(VE_rank=0)) } #pour l'horizon on exclut du périmètre les ménages n'ayant pas de dépenses en achat de véhicules.
    # Vérification
    # menage_echelle %>% filter(VE_rank>0) %>% summarise(sum(pondmen))>  as.numeric(
      # (ThreeME %>% 
         # filter(Var=="NEWAUTO_ELEC_H01_2") %>%
         # filter(year==horizon) %>%
         # select(value)
      # )[1,]) * 1000
    # i=i+1
    while(!i %in% menage_echelle$VE_rank & i<max(menage_echelle$VE_rank,na.rm=T)){i=i+1}
  if(i==max(menage_echelle$VE_rank,na.rm=T)+1){i=1}
    while(!i %in% menage_echelle$VE_rank & i<max(menage_echelle$VE_rank,na.rm=T)){i=i+1}
}
  

  
  # BUDGETS -----------------------------------------------------------------
  
  # Coût d'un VE
  cout_VE_tot <-as.numeric( # en M€
    ThreeME %>% 
      filter(Var=="EXP_AUTO_H01_CA_23_2*PEXP_23_H01_2") %>%
      filter(year==Y) %>%
      select(value)
  ) * 10^6
  
  vkm_VE_tot<-as.numeric(
    ThreeME %>% 
      filter(Var=="KM_AUTO_H01_CA_23_2") %>%
      filter(year==Y) %>%
      select(value)
  ) *1000
  
  cout_VE_km<-cout_VE_tot/vkm_VE_tot
  
  
  
  
  
  # # Autofinancement
  # caf<-as.numeric(
  #   ThreeME %>% 
  #     filter(Var=="R_CASH_AUTO_H01_CA") %>%
  #     filter(year==Y) %>%
  #     select(value)
  # )
  
  #Taux d'intérêt
  R_I_AUTO_H01_CG_2<-as.numeric(
    ThreeME%>%
      filter(Var=="R_I_AUTO_H01_CG_2") %>%
      filter(year==Y)%>%
      select(value)
  )
    # Durée emprunt
  R_RMBS_AUTO_ELEC_H01_CA<-as.numeric(
    ThreeME%>%
      filter(Var=="R_RMBS_AUTO_ELEC_H01_CA") %>%
      filter(year==Y)%>%
      select(value)
  )
  # duree_emprunt<-1/taux_emprunt
  
  ## PRIX UNITAIRE d'un VE
  # Données ThreeME de stock (en milliers)
  TOT_VE_nv<-as.numeric(
    (ThreeME %>% 
      filter(Var=="NEWAUTO_ELEC_H01_2") %>%
      filter(year==Y) %>%
      select(value))[1,])*1000
  
  TOT_VE_euros <-as.numeric( # en Millions €
    (ThreeME %>% 
       filter(Var=="PAUTO_ELEC_H01_2*NEWAUTO_ELEC_H01_2") %>%
       filter(year==Y) %>%
       select(value)
    )[1,]) * 10^6
  # plusieurs lignes avec le même nombre de variable "PAUTO_ELEC_H01_2*NEWAUTO_ELEC_H01_2" dans ThreeME
  P_VE <- TOT_VE_euros/TOT_VE_nv
 
  
  ## PRIX UNITAIRE d'un Veh_thermique
  # Données ThreeME de stock (en milliers)
  TOT_VTH_nv<-as.numeric( #en Milliers
    (ThreeME %>% 
      filter(Var=="NEWAUTO_TH_H01_2") %>%
      filter(year==Y) %>%
      select(value))[1,]
  ) * 1000
  
  TOT_VTH_euros <-as.numeric( # en Millions €
    ThreeME %>% 
      filter(Var=="PNEWAUTO_TH_H01_2*NEWAUTO_TH_H01_2") %>%
      filter(year==Y) %>%
      select(value)
  ) * 10^6

  
  P_VTH <- TOT_VTH_euros/TOT_VTH_nv
  
  
  # # KM_AUTO
  # KM_AUTO_TOT<-as.numeric(
  #   ThreeME %>%
  #     filter(year==horizon) %>%
  #     filter(str_detect(Var, 'KM_AUTO_H01_C'))%>%
  #     summarise(sum(value))
  # )*1000
  # 
  # 
  # AUTO_H01_2<-as.numeric(
  #   (ThreeME %>% 
  #      filter(Var=="AUTO_H01_2") %>%
  #      filter(year==horizon) %>%
  #      select(value))[1,])*1000
  # 
  # #vkm moyen (quelque soit motorisation)
  # KM_AUTO<-KM_AUTO_TOT/AUTO_H01_2
  # 
  # ratio_TC_KM<-KM_AUTO/KM_AUTO_2010
  
  
  if(Y>horizon-6){
    
    repayment_VE<-as.numeric(int_princ(loan=P_VE, 
                                       # n=1/as.numeric(R_RMBS_AUTO_ELEC_H01_CA),
                                       n=6,
                                       year_purchase = Y,
                                       horizon=horizon,
                                       i=  R_I_AUTO_H01_CG_2  ,
                                       pf=1))
    
  repayment_VTH<-as.numeric(int_princ(loan=P_VTH, 
                                     # n=1/as.numeric(R_RMBS_AUTO_ELEC_H01_CA),
                                     n=6,
                                     year_purchase = Y,
                                     horizon=horizon,
                                     i=  R_I_AUTO_H01_CG_2  ,
                                     pf=1))

  ratio_int_over_princ<-repayment_VTH[1]/sum(repayment_VTH)}
  
  
 
  
  if(Y>horizon-13 & Y<=horizon-6){
    #Ces ménages n'ont pas de remboursement en cours, selon les hypothèses de Gaël, ces ménages ont déjà fini de rembourser leur épargne ou leur dette
    # les modifications de budgets sont le carburant et l'électricité.
  menage_echelle <- 
    menage_echelle %>% 
    mutate_when(
      # Condition : achat VE neuf pour ménage avec plusieurs véhicules
      new_VE& nbvehic>1,
      # Action
      list(
        solde_carb = - carb_lubr * percent_pkm_eligible,
        solde_elec = cout_VE_km* km_auto* percent_pkm_eligible,
        dep_Elec = dep_Elec + cout_VE_km* km_auto* percent_pkm_eligible,
        Elec=Elec+ cout_VE_km* km_auto* percent_pkm_eligible,
        Elec_ElecSpe = Elec_ElecSpe +  cout_VE_km* km_auto* percent_pkm_eligible
      ),
      new_VE& nbvehic==1,
      list(
        solde_carb = - carb_lubr, #pas de report modal dans le cas d'un unique véhicule électrifié
        solde_elec = cout_VE_km* km_auto* percent_pkm_eligible,
        dep_Elec = dep_Elec + cout_VE_km* km_auto* percent_pkm_eligible,
        Elec=Elec+ cout_VE_km* km_auto* percent_pkm_eligible,
        Elec_ElecSpe = Elec_ElecSpe +  cout_VE_km* km_auto* percent_pkm_eligible
      )
      )
  
  if(Y>horizon-6 & Y<horizon) {
    menage_echelle <- 
      menage_echelle %>% 
      mutate_when(
        # Condition : achat VE neuf pour ménage avec plusieurs véhicules => carburant et électricité
        new_VE & nbvehic>1,
        # Action
        list(
          solde_carb = - carb_lubr * percent_pkm_eligible,
          solde_elec = cout_VE_km* km_auto* percent_pkm_eligible,
          dep_Elec = dep_Elec + cout_VE_km* km_auto* percent_pkm_eligible,
          Elec=Elec+ cout_VE_km* km_auto* percent_pkm_eligible,
          Elec_ElecSpe = Elec_ElecSpe +  cout_VE_km* km_auto* percent_pkm_eligible
        ),
        new_VE& nbvehic==1,
        list(
          solde_carb = - carb_lubr, #pas de report modal dans le cas d'un unique véhicule électrifié
          solde_elec = cout_VE_km* km_auto* percent_pkm_eligible,
          dep_Elec = dep_Elec + cout_VE_km* km_auto* percent_pkm_eligible,
          Elec=Elec+ cout_VE_km* km_auto* percent_pkm_eligible,
          Elec_ElecSpe = Elec_ElecSpe +  cout_VE_km* km_auto* percent_pkm_eligible
        ),
        
      # Pour la dette
      new_VE & mcrevoi_d>0 ,
      list(solde_int=mcrevoi_d*(P_VE-P_VTH)/P_VTH*ratio_int_over_princ,
           solde_princ=mcrevoi_d*(P_VE-P_VTH)/P_VTH*(1-ratio_int_over_princ)
      ),
      new_VE & is.na(mcrevoi_d), #alors ça signifie que le ménage a payé son véhicule par désépargne. Il faut donc augmenter le montant désépargner du 
      list(solde_rev_capital=sum(repayment_VE)) #négatif qui va venir amputer les revenus du capital
      ) 
  }
  # menage_echelle <- 
  #   menage_echelle %>% 
  #   mutate_when(
  #     new_VE & nbvehic>1,
  #     list(carb_lubr = carb_lubr* (1-percent_pkm_eligible)))
  }
  # On reprend à 0 parce qu'à moins de 6 ans de l'horizon on peut sélectionner les ménages ayant un crédit en cours. 
  if(Y==horizon) {
    menage_echelle <- 
      menage_echelle %>% 
      mutate_when(
        # Condition : achat VE neuf pour ménage avec plusieurs véhicules => carburant et électricité
        new_VE & nbvehic>1,
        # Action
        list(
          solde_carb = - carb_lubr * percent_pkm_eligible*1/2,
          solde_elec = cout_VE_km* km_auto* percent_pkm_eligible*1/2,
          dep_Elec = dep_Elec + cout_VE_km* km_auto* percent_pkm_eligible*1/2,
          Elec=Elec+ cout_VE_km* km_auto* percent_pkm_eligible*1/2,
          Elec_ElecSpe = Elec_ElecSpe +  cout_VE_km* km_auto* percent_pkm_eligible*1/2,
          solde_veh=prod_veh *(P_VE/P_VTH-1) # Update budget A06 des ménages basculant : A06×(prix_moyen VE)/(prix moyen VP TH)
          # pour garder la dispersion (petits ou gros veh)
        ),
        new_VE& nbvehic==1,
        list(
          solde_carb = - carb_lubr*1/2, #pas de report modal dans le cas d'un unique véhicule électrifié
          solde_elec = cout_VE_km* km_auto* percent_pkm_eligible*1/2,
          dep_Elec = dep_Elec + cout_VE_km* km_auto* percent_pkm_eligible*1/2,
          Elec=Elec+ cout_VE_km* km_auto* percent_pkm_eligible*1/2,
          Elec_ElecSpe = Elec_ElecSpe +  cout_VE_km* km_auto* percent_pkm_eligible*1/2,
          solde_veh=prod_veh *(P_VE/P_VTH-1)
          # Update budget A06 des ménages basculant : A06×(prix_moyen VE)/(prix moyen VP TH)
          # pour garder la dispersion (petits ou gros veh)
        ),
        
        # Pour la dette
        new_VE & mcrevoi_d>0 ,
        list(solde_int=mcrevoi_d*(P_VE-P_VTH)/P_VTH*ratio_int_over_princ,
             solde_princ=mcrevoi_d*(P_VE-P_VTH)/P_VTH*(1-ratio_int_over_princ)
             ),
        new_VE & is.na(mcrevoi_d), #alors ça signifie que le ménage a payé son véhicule par désépargne. Il faut donc augmenter le montant désépargner du 
        list(solde_rev_capital=sum(repayment_VE)) #négatif qui va venir amputer les revenus du capital
      ) 
  }
  

  
  i=min(menage_echelle%>%filter(VE_rank>0)%>%select(VE_rank))
  print(dim(menage_echelle %>% filter(year_VE==Y)%>%select(ident_men)))
  # print(compute_share(menage_echelle))
}

# print(menage_echelle %>% filter(ident_men==test) %>% select(solde_carb))

sauv_int<-menage_echelle
A2 <- menage_echelle %>% filter(ident_men==test)



# Amélioration de la consommation des veh thermiques ----------------------

# Efficacité des moteurs
conso_fuel_horizon <-as.numeric(
  ThreeME %>%
  filter(year==horizon) %>%
  filter(Var=="EXP_AUTO_H01_CA_22_2/AUTO_H01_CA_22_2") %>%
  select(value)
)
conso_fuel_2010 <-as.numeric(
  ThreeME %>%
    filter(year==2010) %>%
    filter(Var=="EXP_AUTO_H01_CA_22_2/AUTO_H01_CA_22_2") %>%
    select(value)
)

gain_ener <- (conso_fuel_horizon-conso_fuel_2010)/conso_fuel_2010


# Diminution des usages (télétravail et voirie)
forcage_vkm<-read_excel(path="Donnees_brutes/Sorties ThreeME/forcage_vkm_teletravail_voirie.xlsx",sheet="value")
gain_vkm<-as.numeric(forcage_vkm %>% filter(year==horizon)%>%select(gain_vkm))


#Malus pour les VT
# attention malus BM_rel<0 pour indiquer un surcoût, on rajoute un moins
menage_echelle <-
  menage_echelle %>%
  mutate_when((prod_veh>5*10^(3)),
              list(solde_malus=prod_veh*-BM_rel_net))%>% #solde_malus positif => va venir diminuer les autres consommations
  mutate_when(new_VE,
              list(solde_malus=0))
  

Bonus_VE_tot<-Bonus_VE *  menage_echelle %>% filter(year_VE==horizon)%>%summarise(sum(pondmen))
Bonus_malus_tot<-Bonus_VE_tot-menage_echelle %>% summarise(sum(solde_malus*pondmen))
write.csv(c(Bonus_malus_tot,scenario,scenario_classement,horizon, redistribution),file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/BM_net.csv",sep=""))


# Variable New_VT

menage_echelle <-
  menage_echelle %>%
  mutate(new_VT=FALSE)

menage_echelle <-
  menage_echelle %>%
  mutate_when((prod_veh>5*10^(3) & new_VE==FALSE),list(new_VT=TRUE))


# Créer les soldes budgétaires
menage_echelle <-
  menage_echelle %>%
  # mutate(carb_lubr=carb_lubr*(1+gain_ener)) %>%
  mutate(carb_lubr = carb_lubr+solde_carb)

menage_echelle <-
  menage_echelle %>%
  mutate(solde_carb=solde_carb+carb_lubr*gain_ener+carb_lubr*(1+gain_ener)*gain_vkm) %>%
  mutate(
    carb_lubr=carb_lubr+carb_lubr*gain_ener+carb_lubr*(1+gain_ener)*gain_vkm,
    prod_veh=prod_veh+solde_veh+solde_malus,
    autres_services=autres_services+solde_int,
    Hors_budget=Hors_budget+solde_princ,
    solde_int_total=solde_int_total+solde_int,
    solde_princ_total=solde_princ_total+solde_princ,
    rev_patrimoine=rev_patrimoine+solde_rev_capital,
    RDB=RDB+solde_rev_capital)


# Reventilation -----------------------------------------------------------


solde<-menage_echelle %>% 
  mutate(solde=
           solde_elec+
           solde_carb+
           solde_rev_capital+
           solde_int+
           # solde_princ+
           solde_veh+
           solde_malus
           ) %>%
  select(ident_men,solde)



# menage_echelle <-
#   menage_echelle %>%
#   mutate(Rcons=
#            Rcons + 
#            solde_dette)

sauv_int<-menage_echelle

source("Technical_change/Econometrie_solde_budg.R")
# source("Technical_change/Econometrie_solde_budg_bouclage_autres.R")


Ventil_solde(solde,menage_echelle)
# A3<-A %>% filter(ident_men==test)

menage_echelle_VE <-   A




# VERIF -------------------------------------------------------------------
# 
# A4<-menage_echelle_VE %>% filter(ident_men==test)




# dim(A1)
# dim(A3)
# dim(A4)
# setdiff(colnames(A1),colnames(A4))


# Recalcul de toutes les variables impactées : Rcons, épargne, ratio_S, RDB

# Rcons
menage_echelle_VE$Rcons <- 
  rowSums(menage_echelle_VE[list_dep])

# Parts budgétaires
for (k in list_dep){
  menage_echelle_VE[paste("part",k,sep="_")]<-menage_echelle_VE[k]/menage_echelle_VE$Rcons
}

# Epargne
menage_echelle_VE$epargne <- 
  menage_echelle_VE$RDB - 
  menage_echelle_VE$Rcons + 
  menage_echelle_VE$rev_exceptionnel

# Ratio_S
menage_echelle_VE$ratio_S <-  
  menage_echelle_VE$epargne / 
  menage_echelle_VE$Rcons 

# Taux épargne
menage_echelle_VE$taux_epargne<- ifelse(menage_echelle_VE$RDB==0,0,
  menage_echelle_VE$epargne / 
  menage_echelle_VE$RDB)

# # Taux effort financier
# menage_echelle_VE <- menage_echelle_VE %>%
#   mutate(taux_effort_financier=((taux_effort_financier*RDB)+solde_dette)/RDB)


# ener_surf

source("Technical_change/TC_renovation_DPE/calc_energie_kWh_m2.R")

energie_dom_surf(menage_echelle_VE)

menage_echelle_VE<- 
  menage_echelle_VE %>% 
  select(-ener_dom_surf,-ener_dom) %>%
  left_join(dep_source_usage,by="ident_men")


# Dep_logement
menage_echelle$dep_energie=rowSums(menage_echelle[dep_sources])
menage_echelle$dep_energie_logement=rowSums(menage_echelle[
  c("Elec_ECS","Gaz_ECS","GPL_ECS","Fuel_ECS","Solides_ECS","Urbain_ECS","Elec_chauff","Gaz_chauff",
    "GPL_chauff","Fuel_chauff","Solides_chauff","Urbain_chauff","Elec_clim")])



# Je compare A4, situation finale avec la situation avant tout intervention, A1


# A4 <- menage_echelle_VE %>% filter(ident_men==test)
# #solde_veh
# A4$solde_veh
# #solde_elec
# A4$solde_elec
# #solde_dette
# A4$solde_dette
# #solde_carb
# A4$solde_carb
# A4$carb_lubr
# 
# 
# # Rcons
# A4$Rcons
# A1$Rcons
# A4$Rcons-A1$Rcons
# 
# # Epargne
# A4$epargne
# A1$epargne
# A4$epargne-A1$epargne
# 
# 
# 
# 


# View(rbind(A1,A3,A4))



# ETUDE DE IM -------------------------------------------------------------

# # Ménages ayant acheté un VE
# 
# load("2010/menage_calibr_2010.RData")
# menage_calibr_2010 %>% filter(ident_men %in% IM) %>% summarise(sum(pondmen))
# # 1829912
# menage_calibr_2010 %>% filter(ident_men %in% IM) %>% filter(prod_veh>0) %>% summarise(sum(pondmen))
# # 1     601 595.2
# # => 33% des ménages qu'on a sélectionné ont des dépenses non nulles en prod veh. Non négligeable. 

# IM1<-IM
# IM1 : identifiants des ménages VE==TRUE sans contrainte sur prod_veh>0
# IM : identifiants des ménages VE==TRUE avec contraintes sur prod_veh=0

# menage_echelle %>% filter(ident_men %in% IM) %>% summarise(sum(pondmen))
# menage_echelle %>% filter(ident_men %in% intersect(IM,IM1)) %>% summarise(sum(pondmen))
# 
# menage_echelle %>% filter(ident_men %in% intersect(IM,IM1)) %>% summarise(sum(pondmen))/
# menage_echelle %>% filter(ident_men %in% IM) %>% summarise(sum(pondmen))
# 1    0.6707697 # 67% des ménages sélectionnés en commun. Et on est plus près des chiffres de ThreeME sur les ventes de veh thermiques


# IM2<-IM
# IM2 c'est en partant de menage_echelle_41 après les achats de maisons neuves en 2025. 
#IM C'est avec contraintes sur prod_veh=0 et une fois refait les rehab et achats de DPE
# menage_echelle %>% filter(ident_men %in% IM) %>% summarise(sum(pondmen))
# menage_echelle %>% filter(ident_men %in% intersect(IM,IM2)) %>% summarise(sum(pondmen))
# 
# menage_echelle %>% filter(ident_men %in% intersect(IM,IM2)) %>% summarise(sum(pondmen))/
# menage_echelle %>% filter(ident_men %in% IM) %>% summarise(sum(pondmen))
# 1    0.9900978



# Maj_dep_preeng ----------------------------------------------------------

menage_echelle_VE <- maj_dep_preeng(bdd1= menage_echelle_TC_DPE,bdd2=menage_echelle_VE)



# SAVE FILE ---------------------------------------------------------------

menage_echelle_TC_VE<-menage_echelle_VE

save(menage_echelle_TC_VE, file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_TC_VE.RData",sep=""))



menage_echelle <- menage_echelle_TC_VE %>% select(-typmen5,-
percent_pkm_eligible,-potentiel_VE,-VE_rank_pess,- VE_rank_opt,-VE_rank,-solde_dette,-solde_elec,-solde_carb,-solde_veh)
save(menage_echelle,file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Input/menage_echelle.RData",sep=""))


compute_share(menage_echelle_VE)
compute_savings_rate(menage_echelle_VE)
compute_share_export(menage_echelle_TC_VE)
compute_savings_rate_export(menage_echelle_TC_VE)


# SUCCESS -----------------------------------------------------------------

print("2_VE_2010_2025 : SUCCESS")

