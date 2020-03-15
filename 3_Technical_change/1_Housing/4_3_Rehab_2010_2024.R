# scenario="AMS"
# scenario_classement="Optimiste"
# horizon=2035
# redistribution="niveau_vie"
# Iter=0



# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(decisionSupport)
library(readxl)
library(car)
# library(plyr)
library(dplyr)


# DATA --------------------------------------------------------------------


setwd("D:/CIRED/Projet_Ademe")
source("Code_global_Ademe/mutate_when.R")
source("Code_global_Ademe/compute_share.R")
source("Code_global_Ademe/compute_share_export.R")
source("Code_global_Ademe/verif_epargne_taux.R")
source("Code_global_Ademe/compute_savings_rate_export.R")
source("Code_global_Ademe/maj_dep_preeng.R")
source("Technical_change/Repayment.R")
source("Technical_change/TC_renovation_DPE/calc_energie_kWh_m2.R")



load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_42.RData",sep=""))
load("Donnees_brutes/Sorties ThreeMe/ThreeME.RData")
load("Technical_change/TC_renovation_DPE/list_source_usage.RData")
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/ident_accedants.RData",sep=""))

load("2010/depmen.RData")
menage_2010<-read.csv2("Donnees_brutes/BDF_2010/menage.csv",dec=",",sep=";")

load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))
# load("2025/FC_2010_2025.RData")
# taux_emprunt=0.03 #(à reprendre quand Gaël aura donné la vraie valeur).


# horizon=2025
###
# CHOIX PESSIMISTE VS OPTIMSITE -------------------------------------------
###

# 
# # scenario="PESSIMISTE"
# scenario="OPTIMISTE"
# print(paste("SCENARIO", scenario,sep=" "))
# 



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



# PREPARATION -------------------------------------------------------------

# Renommer datatable
menage_echelle<-menage_echelle_42
  rm(menage_echelle_42)


# Rajouter variables statut logement et du propriétaire
menage_echelle <- menage_echelle %>% left_join(depmen %>% select(ident_men, propri),by='ident_men') 


# Rajouter rev504 revenus locatifs
menage_2010 <- menage_2010 %>% select(ident_men, rev504)
# discount_rate<-read_excel("Donnees_brutes/CIRED/discount_rate_RES_IRF.xlsx") %>% 
  # select(quintileuc=quint_uc,discount_rate=MI_LC)

menage_echelle <- 
  menage_echelle %>% 
  left_join(menage_2010,by="ident_men") 


 # Recoder variable

menage_echelle$stalog_bis<-car::recode(menage_echelle$stalog," 1:2 =1 ; 3=3 ; 4:5= 2 ; 6=3")

menage_echelle$propri_bis<-car::recode(menage_echelle$propri," 1 =3 ; 2=2 ; 3:4= 3 ; 5:6=1; 7=3")

menage_echelle <- menage_echelle %>%
  mutate(stalog_propri=0)%>%
  mutate_when(is.na(propri),list(stalog_propri=stalog_bis))%>%
  mutate_when(!is.na(propri),list(stalog_propri=as.numeric(paste(stalog_bis,propri_bis,sep=""))))




# priority order for rehabilitation : 
## 1 : propriétaires
## 22 : locataires HLM
## 21 : locataires bailleurs privés
## 23 : locataires bailleurs autres
## 2 : locataire bailleur inconnu
## 3 : autre statut : usufruit, logé à titré gratuit
priority<-c(1,22,21,23,2,3)

# SELECTION MENAGES -------------------------------------------------------

# Les ménages ayant acheté des logements neufs sont soit à exclure, soit exclut d'office car étant de classe A. En fonction de l'hypothèse de classe DPE des nouvelles constructions. Le cas ici. 








menage_echelle<-
  menage_echelle %>%
  mutate(exclus=FALSE,REHAB=FALSE,year_rehab=0,solde_dette=0,hausse_loyer=0,solde_loyer=0,hausse_loyer_sum=0) %>%
  mutate_when(year_neuf>0,list(exclus=TRUE),
              ident_men %in% ident_accedants,list(exclus=TRUE)) %>%
  mutate_when(year_neuf==0, list(classe_arr=DPE_dep))%>%
  mutate(solde_int=0,solde_ener=0,principal_dette=0,solde_princ=0,subvention=0)%>% 
  mutate_when(solv>0.28,list(exclus=TRUE))%>%
  mutate_when(ident_men==8063,list(exclus=TRUE))%>% #menage trop fragile
  mutate_when(ident_men==11672,list(exclus=TRUE))%>% #menage trop fragile
  mutate_when(ident_men==6369,list(exclus=TRUE))%>% #(ménage qui bugge en AMS 2035 Median forfait => 2033, avec -263% de solde_int)
  mutate_when(ident_men==10583,list(exclus=TRUE))


# ANNEE PAR ANNEE
#important pour que les ménages puissent faire plusieurs REHAB

Cout_bailleur_prive=c()
Dette_bailleur_prive=c()
Solde_Ener_tot=c()
ident_rehab=c()
A1<-menage_echelle


# for (Y in 2010:2035){
for (Y in 2010:horizon){
  print(Y)
  ident_r<-c()
  
# DONNEES THREEME ---------------------------------------------------------
  Cost_m2=c()
  # travaux de rénovation énergétiques en volume par saut de classe (en M2) 
  # Transition de L vers M
  
  for (L in LETTERS[1:7]){
    #L : classe DPE de départ
    
    for (M in LETTERS[1:7]){
      # M : classe DPE d'arrivée
      # On vérifie que la transition est une amélioration (M "mieux" que L)
      
      if(L>M){
        
        #extraction stock m2 passant de M à L en 2010 (ThreeME)
        stock_m2<-as.numeric(
          ThreeME %>% 
            filter(Var==paste("REHAB_H01_C",L,"_C",M,"_2",sep="")) %>%
            filter(year==Y) %>%
            select(value)
        )
        
        #extraction coût des travaux pour passer de M à L en 2010 (ThreeME) en M€
        stock_euros<-as.numeric(
          ThreeME %>% 
            filter(Var==paste("PREHAB_H01_C",L,"_C",M,"_2*","REHAB_H01_C",L,"_C",M,"_2",sep="")) %>%
            filter(year==Y) %>%
            select(value)
        )
        
        # stock_euros/stock_m2 = coût de la réhabiliation par m2 (en €/m2)
        # Création matrice Cost_m2 : 
        # DPE_départ | DPE_arrivée | coût_m2 | coût_total_transition | m2_total_transition
        Cost_m2=rbind(Cost_m2,c(L,M,stock_euros/stock_m2*(10^6),stock_euros,stock_m2))
        
      }
    }
  }

  
  # taux de remboursement des rénovations énergétiques
  R_RMBS_NEWBUIL_H01_CA<-as.numeric(
      ThreeME %>%
        filter(Var=="R_RMBS_REHAB_H01_CB") %>%
        filter(year==Y) %>%
        select(value)
    )
  
  
  # Taux d'intérêts des emprunts liés aux travaux de réhabilitation des logements en %
  R_I_REHAB_H01_CG_2  <-  as.numeric(
      ThreeME %>%
        filter(Var=="R_I_REHAB_H01_CG_2") %>%
        filter(year==Y) %>%
        select(value)
    )
  
  colnames(Cost_m2)<-
    c(
      "classe_dep",
      "classe_arr",
      "cost_m2",
      "transition_tot_Meuros",
      "transition_tot_m2"
    )
  # convertir en data.frame
  Cost_m2<-as.data.frame(Cost_m2,stringsAsFactors=F)
  Cost_m2$cost_m2<-as.numeric( Cost_m2$cost_m2)
  Cost_m2$transition_tot_m2<-as.numeric( Cost_m2$transition_tot_m2)
  Cost_m2$transition_tot_Meuros<-as.numeric( Cost_m2$transition_tot_Meuros)
  

# DPE_STALOG_PROPRI -------------------------------------------------------

  menage_echelle <-
    menage_echelle %>%
    mutate(DPE_stalog_propri= paste(DPE_dep,stalog_propri,sep="_"))
  
  # rank=menage_echelle %>% group_by(DPE_stalog_propri)%>%summarise(count(DPE_stalog_propri))
  # col<-rank[,1]$x
  # rank<-data.frame(
  #   # t(
  #   rank[,1]$freq)
  #   # )
  # rank<-data.frame(col,rank[,1]) 
  # colnames(rank)<-c("DPE_stalog_propri","value")
  # 
  # rank_bis<-rank
  
  order=paste(rep(LETTERS[1:7],each=6),rep(priority,6),sep="_")
  # menage_echelle$DPE_stalog_propri<-as.factor(menage_echelle$DPE_stalog_propri)
  order_value<-rep(0,length(order))
  # Tab_order<-data.frame(order,order_value)
  # # menage_echelle_bis<-as.data.frame(menage_echelle)
  # Tab<-menage_echelle %>% group_by(DPE_stalog_propri)%>%summarise(count(DPE_stalog_propri))
  # Tab<-Tab[,1]
  # # colnames(Tab)<-c("DPE_stalog_propri","value")
  # Tab<-Tab_order %>% left_join(Tab,by=c("order"="DPE_stalog_propri"))
  # colnames(Tab)<-c('A','B','order')
  # Tab<-Tab %>% mutate_when(is.na(order),list(order=0))%>%select(order)
  # order_value<-as.data.frame(t(Tab))
  # for (i in 1:length(order)){
  #   order_value[i]<-(menage_echelle %>% filter(DPE_stalog_propri==order[i])%>%summarise(count(DPE_stalog_propri)))[1,]$freq
  #   # order_value[i]<-dplyr::count(menage_echelle %>% filter(DPE_stalog_propri==order[i]),ident_men)
  #   # (menage_echelle %>% filter(DPE_stalog_propri==order[i])%>%summarise(n()))
  # }
  
  for (i in 1:length(order)){
    table_order_value<-menage_echelle %>% filter(DPE_stalog_propri==order[i])
    order_value[i]<-ifelse(is.null(dim(table_order_value)),0,length(table_order_value$DPE_stalog_propri))                      
  }
  order_value[is.na(order_value)] <- 0
  order_value<-as.numeric(order_value)
  order_ter<-as.numeric(order_value)

  for (j in 0:6){
    for (i in 2:6){
      a<-6*j+1
      b<-6*j+i-1
    order_ter[6*j+i]<-sum(order_value[a:b])
    }
    order_ter[6*j+1]<-0
  }
  order<-as.character(order)
  order<-data.frame(order,order_ter)
  colnames(order)<-c("DPE_stalog_propri","rank_add")
  order$DPE_stalog_propri<-as.character(order$DPE_stalog_propri)
  # order$DPE_stalog_propri<-factor(order$DPE_stalog_propri)
  
  # menage_echelle <- menage_echelle %>% mutate(DPE_stalog_propri=factor(DPE_stalog_propri,levels=levels(order$DPE_stalog_propri))) 
  
  menage_echelle <- menage_echelle%>%left_join(order,by="DPE_stalog_propri")
  
  #Pour éviter les doublons dans le classement median, on rajoute une unité de marge dans les "rank_add"
  
  order_med=paste(rep(LETTERS[1:7],each=6),rep(priority,6),sep="_")
  order_med_value<-order_med
  for (i in 1:length(order_med)){
    # order_med_value[i]<-(menage_echelle %>% filter(DPE_stalog_propri==order_med[i])%>%summarise(count(DPE_stalog_propri)))[1,]$freq
    table_order_med_value<-menage_echelle %>% filter(DPE_stalog_propri==order_med[i])
    order_med_value[i]<-ifelse(is.null(dim(table_order_med_value)),0,length(table_order_med_value$DPE_stalog_propri))         
  }
  order_med_value[is.na(order_med_value)] <- 0
  order_med_value<-as.numeric(order_med_value)+1
  order_med_ter<-as.numeric(order_med_value)
  
  for (j in 0:6){
    for (i in 2:6){
      a<-6*j+1
      b<-6*j+i-1
      order_med_ter[6*j+i]<-sum(order_med_value[a:b])
    }
    order_med_ter[6*j+1]<-0
  }
  order_med<-as.character(order_med)
  order_med<-data.frame(order_med,order_med_ter)
  colnames(order_med)<-c("DPE_stalog_propri","rank_add_med")
  order_med$DPE_stalog_propri<-as.character(order_med$DPE_stalog_propri)
  
  menage_echelle <- menage_echelle%>%left_join(order_med,by="DPE_stalog_propri")
  
  
  # order_bis<-data.frame(order,order_value,order_ter)
  # for (L in LETTERS[1:7]){
  #   priority_list<-paste(L,priority,sep="_")
  #   for (i in 2:6){
  #     
  #     if(priority_list[i]%in%rank$DPE_stalog_propri){
  #       rank_bis
  #     }}
    
 
  
# CLASSEMENT MENAGES ------------------------------------------------------
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  
  menage_echelle<-
    menage_echelle %>% 
    group_by(DPE_stalog_propri) %>% 
    dplyr::mutate(kWh_rank_opt =row_number(-ener_dom)) %>% 
    ungroup()
  
  


  menage_echelle <-
    menage_echelle %>% 
    group_by(DPE_stalog_propri) %>% 
    dplyr::mutate(kWh_rank_pess =max(kWh_rank_opt,na.rm=T)-kWh_rank_opt+1) %>% 
    ungroup()
  
  menage_echelle <-
    menage_echelle %>% 
    group_by(DPE_stalog_propri) %>% 
    dplyr::mutate(kWh_rank_med =kWh_rank_pess-kWh_rank_opt) %>% 
    mutate(L=max(kWh_rank_opt)) %>%
    mutate_when(is.na(kWh_rank_med),list(kWh_rank_med=0))%>%
    mutate_when(
      kWh_rank_med<0,
      list(
        kWh_rank_med = ifelse(
          is.wholenumber(L/2),
          -kWh_rank_med+1,
          -kWh_rank_med-1)
      )
    ) %>%
    select(-L) %>%
    ungroup()
  
  menage_echelle <- 
    menage_echelle %>%
    mutate(kWh_rank_opt=kWh_rank_opt+rank_add,
           kWh_rank_pess=kWh_rank_pess+rank_add,
           kWh_rank_med=kWh_rank_med+rank_add_med) %>%
    select(-rank_add,-rank_add_med)
  # 
  
  
  
  
  # menage_echelle <- 
  #   menage_echelle %>%
  #   mutate(kWh_rank_med=kWh_rank_med+rank_add)%>%
  #   select(-rank_add)
  
  
  
  menage_echelle <-
    menage_echelle %>% 
    group_by(DPE_dep) %>% 
    dplyr::mutate(kWh_rank_rich=row_number(-RDB/coeffuc)) %>% 
    ungroup()
  
  menage_echelle <-
    menage_echelle %>% 
    group_by(DPE_dep) %>% 
    dplyr::mutate(kWh_rank_poor=max(kWh_rank_rich)-kWh_rank_rich+1) %>% 
    ungroup()
  
  

  
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
    mutate_when(stalog==6,list(kWh_rank=0)) %>%
    mutate_when(stalog==3,list(kWh_rank=0)) %>%
    mutate_when(exclus,list(kWh_rank=0)) %>%
    mutate_when(year_neuf>0,list(kWh_rank=0))
  # Filtre STALOG==6 => exclusion de la réhabiltation (cf pdf)

  
  

# BASCULE -----------------------------------------------------------------
  for (dep in LETTERS[1:7]){
    #dep : classe DPE de départ
    
    # Création d'une table pour la boucle par DPE de départ
    menage_echelle_classe <- 
      menage_echelle %>% 
      filter(DPE_dep==dep) %>% 
      arrange(kWh_rank) %>% 
      mutate(surfpond=surfhab_d*pondmen)
    
    i=1 #compteur de rang
    while(!i %in% menage_echelle_classe$kWh_rank& i<max(menage_echelle_classe$kWh_rank,na.rm=T)){i=i+1}
    
    for (arr in LETTERS[1:7]){
      # arr : classe DPE d'arrivée
      # On vérifie que la transition est une amélioration (arr "mieux" que dep)
      
      
      
      if(dep>arr){
        
        # print(paste(dep,"vers",arr,sep=" "))
        
        
        # extraction du prix au m2 de la transition en question : dep -> arr
        stock_m2_trans <-
          as.numeric(
            Cost_m2 %>% 
              filter(classe_dep==dep) %>% 
              filter(classe_arr==arr)%>% 
              select(transition_tot_m2)
          )
        
        sum=0
        
        while(sum<stock_m2_trans & i<max(menage_echelle_classe$kWh_rank,na.rm=T)){
          sum =
            sum +
            as.numeric((menage_echelle_classe %>% filter(kWh_rank==i) %>% select(surfpond))[1,]) #Parfois plusieurs rangs identiques en médian ... par exemple en pour DPE G en 2021
          
          # identifiant du ménage sélectionné 
          im<-as.numeric((menage_echelle_classe %>% filter(kWh_rank==i) %>% select(ident_men))[1,])
          # print(im)
          ident_r<-c(ident_r,im)
          # Modification des variables REHAB et class_arr dans la base globale
          menage_echelle<- menage_echelle %>% 
            mutate_when(ident_men==im,list(REHAB=TRUE,year_rehab=Y,classe_arr=arr,kWh_rank=0))
          
          # Itération, le non prise en compte des constructions neuves 
          # fait disparaîtres certains rangs du classement
          i=i+1
          while(!i %in% menage_echelle_classe$kWh_rank & i<max(menage_echelle_classe$kWh_rank,na.rm=T)){i=i+1}
          
        }
        
      }
    }
  }
  
  
  rm(menage_echelle_classe,i,sum,stock_m2_trans)


  

# GESTION BUDGET ----------------------------------------------------------

  # menage_echelle <- 
  #   menage_echelle %>%
  #   mutate_when(
  #     # Condition
  #     is.na(REHAB),
  #     # Action
  #     list(REHAB=FALSE),
  #     # Condition
  #     is.na(classe_arr),
  #     # Action
  #     list(classe_arr=DPE_dep)) %>% 
  #   mutate(solde_dette=0,solde_ener=0)  


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
  
  
  
# SOLDE_ENER --------------------------------------------------------------

  for (dep in LETTERS[1:7]){
    # classe de départ
    
    for (arr in LETTERS[1:7]){
      # classe arrivée
      if(dep>arr){
        
        # Extraction coût de la transition au m2 (dep->arr)
        cost_m2 <-
          as.numeric(
            Cost_m2 %>% 
              filter(classe_dep==dep) %>% 
              filter(classe_arr==arr)%>% 
              select(cost_m2)
          )
        
        # Taux de subvention des travaux par l'Etat, identique selon les transitions
        subvention_rate<-as.numeric(
          ThreeME %>% 
            filter(Var==paste("R_SUB_H01_C",dep,"_C",arr,sep="")) %>%
            filter(year==Y) %>%
            select(value)
        )
        
        # Coefficient de gain énergétique 
        rate_gain_ener<-as.numeric(
          Mat_gain_ener %>% 
            filter(DPE_before==dep) %>% 
            filter(DPE_after==arr) %>% 
            select(value))
        # print(rate_gain_ener)
        
        # if(Y==horizon){rate_gain_ener<-rate_gain_ener*1/2} #pinaillage
        
        if(dim(menage_echelle %>% filter(year_rehab==Y & DPE_dep==dep & classe_arr==arr) %>% select(ident_men))[1]>0)
        {
        menage_echelle <- 
          menage_echelle %>% 
          mutate_when(
            # Condition
            year_rehab==Y &
              DPE_dep==dep & 
              classe_arr==arr,
            # Action
            list(
              principal_dette=cost_m2*surfhab_d*(1-subvention_rate),
              subvention=cost_m2*surfhab_d*subvention_rate,
              
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
        if(Y>2010){
        menage_echelle$solde_int <-menage_echelle$solde_int+sapply(menage_echelle$principal_dette, function(X) as.numeric(int_princ(loan=X, 
                                                                                                                       n=1/  R_RMBS_NEWBUIL_H01_CA,
                                                                                                                       year_purchase = Y,
                                                                                                                       horizon=horizon,
                                                                                                                       i=  R_I_REHAB_H01_CG_2  ,
                                                                                                                       pf=1)[1]))
        menage_echelle$solde_princ<-menage_echelle$solde_princ+sapply(menage_echelle$principal_dette, function(X) as.numeric(int_princ(loan=X,
                                                                                                                      n=1/  R_RMBS_NEWBUIL_H01_CA,
                                                                                                                      year_purchase = Y,
                                                                                                                      horizon=horizon,
                                                                                                                      i=  R_I_REHAB_H01_CG_2  ,
                                                                                                                      pf=1
        )[2]))}
        
        
        
        
        
        }
      }
    }
  }
  Dette_bailleur_prive <-
    menage_echelle %>%
    filter(year_rehab==Y) %>%
    filter(stalog>=4 & stalog<=5) %>%
    filter(propri==5 || propri==6) %>%
    summarise(sum(principal_dette*pondmen))
  
  
  #Gestion des hausses de loyer
  tot_rev504<-as.numeric(menage_echelle %>% summarise(sum(rev504*(FC$revpat)*pondmen)))
  
  
  menage_echelle <- 
    menage_echelle %>%
    mutate_when(!is.na(rev504) & rev504>0, 
                list(solde_dette=rev504*(FC$revpat)/tot_rev504*(
                  as.numeric(Dette_bailleur_prive))))
  
  sauv_int_vent<-menage_echelle
  if(!Dette_bailleur_prive[1]==0){
  for (im in (menage_echelle %>% filter(!is.na(rev504) & rev504>0))$ident_men){
    
    menage_echelle<-
      menage_echelle %>%
      mutate_when(ident_men==im,list(hausse_loyer=amort.period(Loan=solde_dette,n=as.numeric(1/R_RMBS_NEWBUIL_H01_CA),i=R_I_REHAB_H01_CG_2,pf=1)[2]))
    
    menage_echelle<-menage_echelle %>%
      mutate_when(ident_men==im,
                  list(hausse_loyer_sum=hausse_loyer_sum+hausse_loyer,
                       solde_int =  solde_int+as.numeric(int_princ(loan=solde_dette,n=as.numeric(1/R_RMBS_NEWBUIL_H01_CA),year_purchase=Y,horizon=horizon,i=R_I_REHAB_H01_CG_2,pf=1)[1]),
                       solde_princ = solde_princ+as.numeric(int_princ(loan=solde_dette,n=as.numeric(1/R_RMBS_NEWBUIL_H01_CA),year_purchase=Y,horizon=horizon,i=R_I_REHAB_H01_CG_2,pf=1)[2])))
    
  }
  
  
  Hausses_loyer <- menage_echelle %>% summarise(sum(pondmen*hausse_loyer))
  if(Hausses_loyer>0){
  menage_echelle <- 
    menage_echelle %>%
    mutate_when(is.na(propri),list(propri=999999)) %>%
    mutate_when(REHAB & stalog>=4 & stalog<=5 & propri<=6 & propri>=5, 
                list(solde_loyer=solde_loyer+as.numeric(Hausses_loyer/Dette_bailleur_prive*principal_dette)))
  }
  }
  print(length(ident_r))
  ident_rehab<-rbind(ident_rehab, menage_echelle %>% filter(year_rehab==Y)%>%select(ident_men,DPE_dep,classe_arr,year_rehab,solde_int, solde_princ,principal_dette))
  ident_rehab<-ident_rehab %>% dplyr::arrange(ident_men)
  
  
  if(!Y==horizon){
  menage_echelle<-
    menage_echelle %>% 
    mutate(DPE_dep=classe_arr)%>% 
    mutate(principal_dette=0)%>%
    mutate(subvention=0)}
  

    #pour la rénovation de l'année suivante, les ménages doivent partir de leur nouvelle classe DPE.

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
  
  
  energie_dom_surf(menage_echelle)
  menage_echelle<- 
    menage_echelle %>% 
    select(-ener_dom_surf,-ener_dom) %>%
    left_join(dep_source_usage,by="ident_men")
  
  # assign(paste("menage",Y,sep="_"),menage_echelle)
  # print(compute_savings_rate_export(menage_echelle))
  
}
sauv_int<-menage_echelle  
# menage_echelle<-sauv_int

  



  # Due à la fusion Sources et Dep_sources sont redondants, la mise à jour de Sources permet de déduire facilement le solde sur tous les sources d'énergie
  menage_echelle$solde_ener<-
    rowSums(menage_echelle[sources]) -
    rowSums(menage_echelle[dep_sources])
  
  # A<-menage_echelle %>% filter(abs(solde_ener)>10^(-9))%>% select(ident_men)
  
  # menage_echelle %>% filter(REHAB) %>% filter(!ident_men %in% A$ident_men) %>% select(ident_men)
  # setdiff(A$ident_men,)
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

 


menage_echelle <- 
  menage_echelle %>%
  mutate(autres_services=autres_services+solde_int,
         solde_int_total=solde_int_total+solde_int,
         solde_princ_total=solde_princ_total+solde_princ,
         Hors_budget=Hors_budget+solde_princ,
         loyers=loyers+solde_loyer,
         rev_patrimoine=rev_patrimoine+hausse_loyer_sum,
         rev504=rev504+hausse_loyer_sum)


menage_echelle<-menage_echelle %>% 
    mutate_when(stalog>=4,
                list(solde_int=0,solde_princ=0),
                stalog==6,
                list(solde_ener=0)
                )

solde<- 
  menage_echelle %>%
  mutate(solde=
           solde_ener+  #<0 => économie
           solde_int+ #>0
           # solde_princ+ #>0
           solde_loyer+ #>0
           -hausse_loyer_sum) %>% #<0 => comme une économie 
    select(ident_men,solde)

menage_echelle<-
  menage_echelle %>%
  mutate_when(year_rehab==horizon & stalog<3,
              list(BTP=BTP+principal_dette,
                   Rcons=Rcons+principal_dette
              ))

for (im in (menage_echelle %>% filter(!is.na(rev504) & rev504>0))$ident_men){
  menage_echelle<-
    menage_echelle %>%
    mutate_when(ident_men==im,list(BTP=BTP+solde_dette,
                                   Rcons=Rcons+solde_dette))
  
  }

Cout_bailleur_public <-as.numeric(
  menage_echelle %>%
    filter(year_rehab==horizon) %>%
    filter(stalog>=4 & stalog<=5) %>%
    filter(propri==2) %>%
    summarise(sum((principal_dette+subvention)*pondmen)))

# pour les propriétaires, pour les locataires (on compte la subvention via les locataires même si ce sont les propriétaires bailleurs qui la touchent)
Subvention<-
  as.numeric(
    menage_echelle %>%
      filter(year_rehab==horizon) %>%
      # filter(stalog>=4 & stalog<=5) %>%
      filter(!propri==2) %>%
      summarise(sum(subvention*pondmen)))

sBCE<-as.numeric(Subvention/(Subvention+menage_echelle%>%summarise(sum(pondmen*BTP))))

save(sBCE,file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/sBCE.RData",sep=""))
save(Subvention,file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/Subvention_rehab.RData",sep=""))
save(Cout_bailleur_public,file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/Cout_bailleur_public.RData",sep=""))

  sauv_avant_reventil<-menage_echelle
  A2<-menage_echelle%>% filter(ident_men==i)
  # %>% select(-rev504)
  
  source("Technical_change/Econometrie_solde_budg_Logement.R")
  # source("Technical_change/Econometrie_solde_budg_bouclage_autres.R")
  
  Ventil_solde(solde,menage_echelle)
  

  menage_echelle<-A
  
  
  Solde_Ener_tot<-
    rbind(Solde_Ener_tot,
          c(Y,
    menage_echelle %>%
      filter(REHAB) %>%
      filter(stalog>=4 & stalog<=5) %>%
      filter(propri==2) %>%
      summarise(sum(solde_ener*pondmen))))
  
  
    
  # Recalcul des toutes les variables 
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
  

  
 
  energie_dom_surf(menage_echelle)
  
  menage_echelle<- 
    menage_echelle %>% 
    select(-ener_dom_surf,-ener_dom) %>%
    left_join(dep_source_usage,by="ident_men")
  
A2<-menage_echelle %>% select(-kWh_rank_pess,-kWh_rank_opt,-kWh_rank,-solde_dette,-solde_ener)

menage_echelle$dep_energie=rowSums(menage_echelle[dep_sources])
menage_echelle$dep_energie_logement=rowSums(menage_echelle[
  c("Elec_ECS","Gaz_ECS","GPL_ECS","Fuel_ECS","Solides_ECS","Urbain_ECS","Elec_chauff","Gaz_chauff",
    "GPL_chauff","Fuel_chauff","Solides_chauff","Urbain_chauff","Elec_clim")])


# VERS LA PROCHAINE ETAPE -------------------------------------------------

  # ident_rehab=cbind(ident_rehab,c(Y,menage_echelle%>%filter(REHAB)%>%select(ident_men)))
  # ident_rehab=c(ident_rehab,ident_r)
  menage_echelle$REHAB<-FALSE
  # menage_echelle$DPE_dep<-menage_echelle$classe_arr
  

  

compute_savings_rate_export(menage_echelle)




# SAVE --------------------------------------------------------------------

menage_echelle_43<-menage_echelle %>% mutate(DPE_2024=DPE_dep) %>% select(-stalog,-propri,-REHAB ,-kWh_rank_pess,-kWh_rank_opt,-kWh_rank) 
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_42.RData",sep=""))


compute_share(menage_echelle_43)

# inter<-intersect(colnames(menage_echelle_43), colnames(menage_echelle_42))
# not_inter<-setdiff(colnames(menage_echelle_43), colnames(menage_echelle_42))



# save(menage_echelle_43, file="Technical_change/TC_renovation_DPE/menage_echelle_43.RData")
# 
# save(dep_ener_2025_43, file="Technical_change/TC_renovation_DPE/dep_ener_2025_43.RData")


# VENTILATION COUTS BAILLEURS PRIVES --------------------------------------

# Identifier les ménages propriétaires

# Attention màj du REV504 avec un ancien TC de croissance rev global, utiliser le nouveau TC REVPAT




# A3<-menage_echelle_43bis %>% filter(ident_men==i) %>% select(-rev504)
# View(rbind(A1,A2,A3))
# dim(A1)
# dim(A2)
# dim(A3)


# menage_echelle_43<-menage_echelle_43bis

source("Technical_change/TC_renovation_DPE/calc_energie_kWh_m2.R")

energie_dom_surf(menage_echelle_43)

menage_echelle_43<- 
  menage_echelle_43 %>% 
  select(-ener_dom_surf,-ener_dom) %>%
  left_join(dep_source_usage,by="ident_men")


# Re-join les NEUF et les pas NEUF



# Maj_dep_preeng ----------------------------------------------------------

menage_echelle <- maj_dep_preeng(bdd1= menage_echelle_42,bdd2=menage_echelle)






# SAVE --------------------------------------------------------------------
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_42.RData",sep=""))
# menage_echelle_43<-menage_echelle_43 %>% select(colnames(menage_echelle_42 %>% select(-DPE_2025)),DPE_2024)

menage_echelle <- menage_echelle %>% mutate(DPE_horizon=classe_arr)

inter<-intersect(colnames(menage_echelle_43), colnames(menage_echelle_42))
not_inter<-setdiff(colnames(menage_echelle_43), colnames(menage_echelle_42))

# dep_ener_horizon_43<-menage_echelle_43 %>% select(ident_men,not_inter,-DPE_2024)
menage_echelle_43<-menage_echelle %>% select(inter,year_rehab,DPE_horizon)

save(menage_echelle_43, file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_43.RData",sep=""))





# save(dep_ener_2025_43, file="Technical_change/TC_renovation_DPE/dep_ener_2025_43.RData")

compute_share(menage_echelle_43)
compute_savings_rate(menage_echelle_43)
print(compute_share_export(menage_echelle_43))
print(compute_savings_rate_export(menage_echelle_43))
# Verif -------------------------------------------------------------------
# 
# # Augmentation des loyers en 2010 et 2024 ? 
# # 2010
# weighted.mean(menage_echelle_42$rev_patrimoine,menage_echelle_42$pondmen)
# # 2014
# weighted.mean(menage_echelle_43$rev_patrimoine,menage_echelle_43$pondmen)
# # %
# weighted.mean(menage_echelle_42$rev_patrimoine,menage_echelle_42$pondmen)/
# weighted.mean(menage_echelle_43$rev_patrimoine,menage_echelle_43$pondmen)
# 
# # ou
# (weighted.mean(menage_echelle_43$rev_patrimoine,menage_echelle_43$pondmen)-
#   weighted.mean(menage_echelle_42$rev_patrimoine,menage_echelle_42$pondmen)) /
#   weighted.mean(menage_echelle_42$rev_patrimoine,menage_echelle_42$pondmen)
# # +3% sur les loyers à cause de la rénovation énergétique sur la période 2010-2024

# SUCCESS -----------------------------------------------------------------

print("4_3_Rehab_2010_2024 : SUCCESS")




# TESTS -------------------------------------------------------------------

# table(menage_echelle$year_rehab)
# 
# # Devrait être vide
# menage_echelle %>% filter(year_neuf>0 & year_rehab>0) %>% select(ident_men)
#                                
# 
# #Prendre un ménage => devrait avoir Rcons inchangé, épargne inchangée, dep_ener_conso qui baisse, classe_arr<DPE_dep
# menage_echelle %>% filter(year_rehab>0) %>% select(ident_men)
# i=627
# A42<-menage_echelle_42%>%filter(ident_men==i)
# A43<-menage_echelle%>%filter(ident_men==i)
# A42$Rcons
# A43$Rcons

