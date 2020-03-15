# OBJECTIF : Les ménages achetant un logement neufs en 2010 le sont également en 2025.



# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(FinancialMath)
source("Code_global_Ademe/mutate_when.R")
source("Code_global_Ademe/compute_share.R")

source("Code_global_Ademe/compute_savings_rate_export.R")
source("Code_global_Ademe/verif_epargne_taux.R")
source("Code_global_Ademe/compute_share_export.R")
source("Code_global_Ademe/maj_dep_preeng.R")
source("Technical_change/Repayment.R")
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))
sources=c("Elec","Gaz","Fuel","GPL","Urbain","Solides")
dep_sources=paste("dep",sources,sep="_")

# DATA --------------------------------------------------------------------

setwd("D:/CIRED/Projet_Ademe")

# Donnes ThreeME : m2 et valeurs d'achats de logement neufs trajectoire
load("Donnees_brutes/Sorties ThreeMe/ThreeME.RData")
load("Technical_change/TC_renovation_DPE/list_source_usage.RData")

#2010
load("2010/c05_forme_2010.RData")
load("2010/menage_DPE.RData")
load("2010/depmen.RData")


#horizon
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/menage_echelle_1.RData",sep=""))



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


# PREPARATION DATA --------------------------------------------------------
menage_echelle_1<-menage_echelle
menage_echelle<-menage_echelle %>% mutate(DPE_dep=DPE_pred)


# DONNEES THREE_ME  -------------------------------------------------

# Constructions neuves en m2 : NEWBUIL_H01_2

NEWBUIL_H01_2_horizon<-
  as.numeric(
    ThreeME %>%
      filter(Var=="NEWBUIL_H01_2") %>%
      filter(year==horizon) %>%
      select(value)
  )

NEWBUIL_H01_2_2010<-
  as.numeric(
    ThreeME %>%
      filter(Var=="NEWBUIL_H01_2") %>%
      filter(year==2010) %>%
      select(value)
  )

# Constructions neuves en m2 par classe énergétique : A	
NEWBUIL_H01_CA_2<-
  as.numeric(
    ThreeME %>%
      filter(Var=="NEWBUIL_H01_CA_2") %>%
      filter(year==horizon) %>%
      select(value)
  )

BUIL_H01_2_horizon<-
  as.numeric(
    ThreeME %>%
      filter(Var=="BUIL_H01_2") %>%
      filter(year==horizon) %>%
      select(value)
  )

# Constructions neuves en m2 par classe énergétique : B	
NEWBUIL_H01_CB_2<-
  as.numeric(
    ThreeME %>%
      filter(Var=="NEWBUIL_H01_CB_2") %>%
      filter(year==horizon) %>%
      select(value)
  )


# Dépenses en constructions neuves en valeur (M€ courants)
PNEWBUIL_H01_2_NEWBUIL_H01_2_horizon <-
  as.numeric(
    ThreeME %>%
      filter(Var=="PNEWBUIL_H01_2*NEWBUIL_H01_2") %>%
      filter(year==horizon) %>%
      select(value)
  )*10^6

# Dépenses en constructions neuves en valeur (M€ courants)
PNEWBUIL_H01_2_NEWBUIL_H01_2_2010 <-
  as.numeric(
    ThreeME %>%
      filter(Var=="PNEWBUIL_H01_2*NEWBUIL_H01_2") %>%
      filter(year==2010) %>%
      select(value)
  )*10^6

# Prix du m2 de logement neuf en horizon
PNEWBUIL_H01_2_horizon<-
  PNEWBUIL_H01_2_NEWBUIL_H01_2_horizon /
  NEWBUIL_H01_2_horizon


# Prix du m2 de logement neuf en 2010
PNEWBUIL_H01_2_2010<-
  PNEWBUIL_H01_2_NEWBUIL_H01_2_2010 /
  NEWBUIL_H01_2_2010

#ratio du prix du m2 neuf en 2010 et horizon
ratio_prix_m2<-PNEWBUIL_H01_2_horizon/PNEWBUIL_H01_2_2010

# taux de remboursement des constructions neuves
R_RMBS_NEWBUIL_H01_CA<-
  as.numeric(
    ThreeME %>%
      filter(Var=="R_RMBS_NEWBUIL_H01_CA") %>%
      filter(year==horizon) %>%
      select(value)
  )


# Taux d'intérêts des emprunts liés à la construction neuve en %
R_I_BUIL_H01_CG_2<-  
  as.numeric(
  ThreeME %>%
    filter(Var=="R_I_BUIL_H01_CG_2") %>%
    filter(year==horizon) %>%
    select(value)
)

# 
ratio_neuf_ancien_3ME<-NEWBUIL_H01_2_horizon/BUIL_H01_2_horizon*100 # 0.82 %

conso_moy_dep=data.frame("A"=0, "B"=0, "C"=0, "D"=0, "E"=0, "F"=0, "G"=0)
for (i in LETTERS[1:7]){
  conso_moy_dep[i]<-
    as.numeric(
      ThreeME %>% 
        filter(Var==
                 paste("ENER_BUIL_H01_C",i,"_2*11630/BUIL_H01_C",i,"_2",sep="")
        ) %>% 
        filter(year==horizon) %>% 
        select(value)
    )
}

Mat_gain_ener<-data.frame("DPE_before"=sort(rep(LETTERS[1:7],7)),"DPE_after"=rep(LETTERS[1:7],7))

Mat_gain_ener$value_after<-sapply(Mat_gain_ener$DPE_after,function(x) as.numeric(conso_moy_dep[x]))
Mat_gain_ener$value_before<-sapply(Mat_gain_ener$DPE_before,function(x) as.numeric(conso_moy_dep[x]))
Mat_gain_ener$value<-(Mat_gain_ener$value_after-Mat_gain_ener$value_before)/Mat_gain_ener$value_before

Mat_gain_ener <- Mat_gain_ener %>% select(-c(value_after,value_before))





# [FACULTATIF] Pre-analyse des accedants  ---------------------------------
# 
# 
# # Quel poids dans la population ? 
# sum(ident_accedants$pondmen)
# # [1] 454 521
# 
# # Quelle classe de DPE pour ces ménages ? 
# ident_accedants <- ident_accedants %>% left_join(menage_DPE,by="ident_men")
# # table(ident_accedants$DPE_pred)
# # A  B  C  D  E  F  G 
# # 2  5 28 56 58 23  1 
# 
# 
# # Tracer distribution des DPE parmi les accédants en 2010 et les autres
# # Distributions des DPE
# load("2010/dpe_stock_2010.RData")
# 
# 
# list_dpe_accedants<- 
#   ident_accedants %>% 
#   mutate(surf_pond=surfhab_d*pondmen) %>% 
#   group_by(DPE_pred) %>% 
#   summarise_at("surf_pond",sum)
# 
# list_dpe_accedants <-
#   as.data.frame(list_dpe_accedants[1:7,2])
# 
# dat=data.frame(
#   "cat_DPE"=rep(c("A" , "B" , "C" , "D" , "E" , "F" , "G"),2),
#   
#   "DPE"=c(dpe_stock_2010$value/sum(dpe_stock_2010$value),
#           (list_dpe_accedants$surf_pond/sum(list_dpe_accedants$surf_pond))),
#   
#   "statut"=c(rep("Pop_TOT",7),
#              rep("Accedant_2010",7))
# )
# 
# 
# ggplot(dat,aes(fill=statut,x=cat_DPE,y=DPE))+geom_bar(stat="identity",position="dodge") + ggtitle("Distribution des classes DPE : \n comparaison entre la population 2010 \n et les accédants à la propriété en 2010")
# 
# 
# 



# VENTILATION -------------------------------------------------------------

# ANACQ Année d'acquisition de cette résidence secondaire
# ANCONS
# Année d'achèvement de l'immeuble
# 1 En 1948 ou avant
# 2 De 1949 à 1961
# 3 De 1962 à 1967
# 4 De 1968 à 1974
# 5 De 1975 à 1981
# 6 De 1982 à 1989
# 7 De 1990 à 1998
# 8 De 1999 à 2003
# 9 En 2004 et après
# 10 En construction

# Hypothèses : on garde les mêmes ménages qui achètent des logements en 2010 et horizon pour ne pas distordre les budgets. 
# On ventile les "constructions neuves" sur les achats de logements. Affectation de constructions neuves pour atteindre le ratio "constructions neuves" sur "stock total de m2" de ThreeME. Soit 0.81% en horizon. La variable Ancons ne permet pas d'être très précis, mais nous sommes obligés pour atteindre le ratio demandé de m2 d'aller puiser dans des achats de logements construits depuis 1975 ... Hypothèse forte. 1ère hypothèse de classement, seconde hypothèse sur la classe DPE, on privilégie les classes DPE élevées (pour ne pas perturber trop les budgets), puis ensuite année d'acquisition de la résidence principale. 

# [FACULTATIF] TEST EN TOUS GENRES

# ident_accedants_2004 <- ident_accedants %>% filter(ancons==9)
# sum(ident_accedants_2004$surfhab_d*ident_accedants_2004$pondmen)
# # [1] 3 023 654
# sum(ident_accedants_2004$surfhab_d*ident_accedants_2004$pondmen)/sum(menage_echelle$surfhab_d*menage_echelle$pondmen)*100
# # =0.12%
# sum(menage_echelle$surfhab_d*menage_echelle$pondmen)
# # [1] 2 516 672 069
# 
# # A comparer avec le stock de m2 neufs de 2010 de ThreeME (en relatif)
# NEWBUIL_H01_2_horizon
# # [1] 21 887 835
# 
# # On remplit le stock de m2 avec les logements les plus récents
# ratio_neuf_ancien_3ME<-NEWBUIL_H01_2_horizon/BUIL_H01_2_horizon*100 # 0.82 %
# BUIL_H01_2_horizon
# # [1] 2 675 615 661
# 
# ident_accedants_99 <- ident_accedants %>% filter(ancons>=5)
# sum(ident_accedants_99$surfhab_d*ident_accedants_99$pondmen)
# # [1] 20 298 771
# sum(ident_accedants_99$surfhab_d*ident_accedants_99$pondmen)/sum(menage_echelle$surfhab_d*menage_echelle$pondmen)*100
# # =0.81%



# Classement --------------------------------------------------------------
#1er classement optimiste ou pessimiste. Second classement sur probabilité d'achat de neuf (ancons, DPE_pred, anacq)
# scenario="PESSIMISTE"
# scenario_classement="OPTIMISTE"
# print(paste("SCENARIO", scenario,sep=" "))

# 1er classement

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

menage_echelle<- menage_echelle %>% mutate_when(is.na(ener_dom),list(ener_dom=0))



menage_echelle<-
  menage_echelle %>% 
  group_by(DPE_dep) %>% 
  dplyr::mutate(kWh_rank_opt =row_number(-ener_dom)) %>% 
  ungroup()

menage_echelle <-
  menage_echelle %>% 
  group_by(DPE_dep) %>% 
  dplyr::mutate(kWh_rank_pess =max(kWh_rank_opt,na.rm=T)-kWh_rank_opt+1) %>% 
  ungroup()

menage_echelle <-
  menage_echelle %>% 
  group_by(DPE_dep) %>% 
  dplyr::mutate(kWh_rank_med =kWh_rank_pess-kWh_rank_opt) %>% 
  mutate(L=max(kWh_rank_opt)) %>%
  mutate_when(is.na(kWh_rank_med),list(kWh_rank_med=0)) %>% #ligne à supprimer dès que possible !!! pas normal
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



# SELECTION MENAGES ACCEDANT horizon -----------------------------------------

# Les dépenses d'achat de logement (c13711) ne sont pas à l'échelle dans c05_forme_2010, mais on récupère uniquement les identifiants ménages

ident_accedants <- 
  c05_forme_2010 %>% filter(c13711>0) %>% select(ident_men,c13711)
# 174 ménages

ident_accedants <- 
  ident_accedants %>% 
  left_join(menage_echelle %>% select(ident_men, pondmen,surfhab_d,ener_dom_surf,ener_dom,DPE_dep),by="ident_men") %>% 
  filter(ener_dom_surf>0) %>% #personne ne fait de travaux si pas de conso d'énergie
  filter(!DPE_dep=="A") # les ménages déjà en A exclus, sinon on se retrouve avec dep=A, arr=B

#Ajout des caractéristiques anacq et ancons de depmen
# ajout DPE_pred de menage_dpe
ident_accedants <- 
  ident_accedants %>% 
  left_join(depmen %>% select(ident_men, anacq, ancons),by="ident_men") %>%
  # left_join(menage_DPE, by="ident_men") %>%
  left_join(menage_echelle %>% select(ident_men,kWh_rank),by="ident_men")

save(ident_accedants,file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/ident_accedants.RData",sep=""))


ident_accedants_bis <- 
  ident_accedants %>% 
  filter(ancons<=9) %>% 
  dplyr::arrange(.,-kWh_rank,-ancons, DPE_dep,-anacq)



# SELECTION DES MENAGES ---------------------------------------------------


ratio=0
count=1

ident_accedants_bis$neuf_horizon<-c(
  rep(TRUE,count),
  rep(FALSE,length(ident_accedants_bis$ident_men)-count)
  )

ident_accedants_bis <- 
  ident_accedants_bis %>%
  dplyr::arrange(.,-neuf_horizon,kWh_rank)

# DPE A
count=1
DPE_A=0
while(DPE_A<NEWBUIL_H01_CA_2){
  count=count+1
  DPE_A<-sum(ident_accedants_bis$surfhab_d[1:count]*ident_accedants_bis$pondmen[1:count])
}
count_A=count

#DPE B
DPE_B=0
while(DPE_B<NEWBUIL_H01_CB_2){
  count=count+1
  DPE_B<-sum(ident_accedants_bis$surfhab_d[count_A:count]*ident_accedants_bis$pondmen[count_A:count])
}
ident_accedants_bis <- 
  ident_accedants_bis %>% 
  mutate(classe_arr=DPE_dep) %>%
  mutate_when(ident_men %in% ident_accedants_bis$ident_men[1:count_A],list(classe_arr="A",neuf_horizon=TRUE)) %>%
  mutate_when(ident_men %in% ident_accedants_bis$ident_men[count_A:count],list(classe_arr="B",neuf_horizon=TRUE))
  

ident_accedants <- ident_accedants %>% left_join(ident_accedants_bis %>% select(ident_men, neuf_horizon,classe_arr),by="ident_men") %>%  select(ident_men, neuf_horizon,classe_arr)
#74 ménages sont supposés acheter des logements neufs en horizon

rm(ident_accedants_bis)


# ident_accedants[which(ident_accedants$neuf_horizon),"DPE_horizon"]<-"A"

# ident_accedants$coeff_gain_ener<-rep(0,length(ident_accedants$ident_men))
# 
# #gain d'énergie pour les ménages changeant de DPE
# for (i in 1:length(ident_accedants$ident_men)){
#   x<-ident_accedants[i,]
#   ident_accedants$coeff_gain_ener[i]<-as.numeric(Mat_gain_ener%>% filter(DPE_before==x$DPE_pred) %>% filter(DPE_after==x$DPE_horizon) %>% select(value))
# }

menage_echelle <- 
  menage_echelle %>%
  left_join(ident_accedants, by="ident_men") %>%
  mutate_when(is.na(neuf_horizon),list(neuf_horizon=FALSE),
              is.na(classe_arr),list(classe_arr=DPE_dep)) %>%
  mutate(solde_dette=0,solde_ener=0,solde_int=0,solde_princ=0,principal_dette=0,solde_int_total=0,solde_princ_total=0)


# NOUVELLES CLASSES DPE ---------------------------------------------------

# ANCIENNES CLASSES DPE
# > table(menage_DPE$DPE)
# 
# A    B    C    D    E    F    G 
# 56  492 1388 2201 3213 2205  734 


# NOUVELLES CLASSES DPE
menage_DPE_neuf_horizon<-menage_echelle %>% select(ident_men, neuf_horizon,classe_arr)
save(menage_DPE_neuf_horizon,file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change/","menage_DPE_neuf_",horizon,".RData",sep=""))





# ALTERATION CONSO ENERGIE ------------------------------------------------


energie_domestique<-c("Elec_ECS" , "Gaz_ECS",
                      "GPL_ECS" , "Fuel_ECS" , "Solides_ECS" , "Urbain_ECS" ,
                      "Elec_chauff" , "Gaz_chauff" , "GPL_chauff" , "Fuel_chauff",
                      "Solides_chauff" , "Urbain_chauff" , "Elec_clim" , "Gaz_clim", 
                      "GPL_clim" , "Fuel_clim" , "Solides_clim" , "Urbain_clim")

for (dep in LETTERS[1:7]){
  # classe de départ
  
  for (arr in LETTERS[1:2]){ #NB :en 2025 on ne construit que des classes A et B (arrêt des classes C en 2019)
    # classe arrivée
    if(dep>arr){
      print(paste('dep is ',dep," , arr is ",arr,sep=""))
      # Extraction coût de la transition au m2 (dep->arr) Ici le prix de la construction est identique
      # cost_m2 <- PNEWBUIL_H01_2_horizon
      
      # # Taux de subvention des travaux par l'Etat, identique selon les transitions
      # subvention_rate<-as.numeric(
      #   ThreeME %>% 
      #     filter(Var==paste("R_SUB_H01_C",dep,"_C",arr,sep="")) %>%
      #     filter(year==Y) %>%
      #     select(value)
      # )
      
      # Coefficient de gain énergétique (multiplié par 1/2 pour centrer les consommations des constructions de fin et de début d'année)
      rate_gain_ener<-as.numeric(
        Mat_gain_ener %>% 
          filter(DPE_before==dep) %>% 
          filter(DPE_after==arr) %>% 
          select(value))*1/2
      # print(rate_gain_ener)
      
      if(dim(menage_echelle %>% filter(neuf_horizon & DPE_dep==dep & classe_arr==arr) %>% select(ident_men))[1]>0)
        {
      menage_echelle <- 
        menage_echelle %>% 
        mutate_when(
          # Condition
          neuf_horizon &
            DPE_dep==dep & 
            classe_arr==arr,
          # Action
          list(
            principal_dette=(ratio_prix_m2-1)*c13711, #on ne prend en compte que le surcoût de la construction propre à l'horizon (ainsi ne compte-t-on pas deux fois les frais financiers et le coût qui apparaissent dans le budget des ménages)
            #Energie 
            # (coeff 0.5 car en moyenne des gains sur 6 mois 
            # (rénovation en début ou fin d'année))
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
      
      
      
      menage_echelle$solde_int <-  sapply(menage_echelle$principal_dette, function(X) as.numeric(int_princ(loan=X,
                                                                n=1/R_RMBS_NEWBUIL_H01_CA,
                                                                year_purchase = horizon,
                                                                horizon=horizon,
                                                                i=R_I_BUIL_H01_CG_2,
                                                                pf=1)[1]))
        menage_echelle$solde_princ<-sapply(menage_echelle$principal_dette, function(X) as.numeric(int_princ(loan=X,
                                  n=1/R_RMBS_NEWBUIL_H01_CA,
                                  year_purchase = horizon,
                                  horizon=horizon,
                                  i=R_I_BUIL_H01_CG_2,
                                  pf=1
                                  )[2]))
          # )
      # )
      }
      
    }
  }
}

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

A<-menage_echelle %>% filter(abs(solde_ener)>10^(-9))%>% select(ident_men,solde_ener)

menage_echelle %>% filter(neuf_horizon) %>%filter(!ident_men %in% A$ident_men) %>% select(ident_men)
# 5797
# View(rbind(menage_echelle))

# Verif : tous les ménages ayant un solde ener non nul sont les ménages neuf_horizon
length(setdiff(A$ident_men,ident_accedants %>% filter(neuf_horizon)))
#89
dim(menage_echelle %>% filter(neuf_horizon)%>% select(ident_men))
#89 1

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




# ##TEST
# solde_verif<-solde %>% filter(!solde==0)
# table(solde_verif$solde)
# Ménages sélectionnés dont le solde est à 0 (68 ménages à solde négatif pour 74 ménages sélectionnés)
# temp<-ident_accedants %>% filter(neuf_horizon) %>% select(ident_men)
# setdiff(temp$ident_men,solde_verif$ident_men)
# [1]  1523  2058  5570  6941 14539 15340    
# dépenses d'énergie nulles ou presques nulles (moins de 3% en tout)
  
# View(solde_verif)

# sources=c("Elec", "Gaz", "GPL", "Fuel", "Solides", "Urbain")
# dep_sources=paste("dep",sources,sep="_")
# for (source in sources){
#   sourcebis=paste(source,"_",sep="")
#   solde[source]<- rowSums(solde%>% select(starts_with(sourcebis))) # gain en euro par source => négatif !!!! 
# }

# table(solde$solde-rowSums(solde[sources])<10^(-9))
# # Verif
# sum(rowSums(menage_echelle[dep_sources]))
# 
# table(solde$solde/rowSums(menage_echelle[dep_sources]))


# menage_echelle[energie_domestique]<-menage_echelle[energie_domestique] * (1+menage_echelle$coeff_gain_ener/100)
# menage_echelle_bis<-menage_echelle

# for (source in sources){
#   sourcebis=paste(source,"_",sep="")
#   menage_echelle_bis[source]<-rowSums(menage_echelle%>% select(starts_with(sourcebis)))
# }

# menage_echelle <- menage_echelle_bis %>% select(-coeff_gain_ener)


source("Technical_change/TC_renovation_DPE/calc_energie_kWh_m2.R")

energie_dom_surf(menage_echelle)

menage_echelle<- 
  menage_echelle %>%
  select(-ener_dom_surf,-ener_dom) %>%
  left_join(dep_source_usage,by="ident_men")


A2<-menage_echelle
# ALTERATION BUDGETS ------------------------------------------------------


# Comme on ne considère que des ménages ayant déjà investi en 2010, nous n'avons pas besoin de l'hypothèse de % du financement par la dette, il suffit de mettre à jour toutes les dépenses c13211, c13221, c13711 (remboursement prêt, remboursement prêt, achat)
# Cela n'aurait pas été le cas si nous avions du créer un "nouveau" montant de dette pour un ménage non accédant dans le budget qu'il nous a montré. 
# Ces mises à l'échelle sont faites dès la mise à l'échelle par le taux de croissance du RDB


# Conséquences sur plusieurs chapitres 
# :
#   1. Budget achat : done
#   2. Remb dette : done
#   3. Conso éner : done
#   
# => solde budgétaire (ici le montant du gain ener est reventilé sur tous les montants, y compris éner par l'économétrie de la demande)




# EQUILIBRE BUDGET PAR ECONOMETRIE ----------------------------------------

list_dep_autres_ener=c("dep_Elec","dep_Gaz","dep_GPL","dep_Fuel","dep_Urbain", "dep_Solides","dep_energie_logement")


# menage_echelle$dep_Gaz<-menage_echelle$dep_Gaz+ solde$Gaz 
# menage_echelle$dep_Elec<-menage_echelle$dep_Elec+ solde$Elec 
# menage_echelle$dep_GPL<-menage_echelle$dep_GPL+ solde$GPL 
# menage_echelle$dep_Fuel<-menage_echelle$dep_Fuel+ solde$Fuel
# menage_echelle$dep_Urbain<-menage_echelle$dep_Urbain + solde$Urbain
# menage_echelle$dep_Solides<-menage_echelle$dep_Solides+ solde$Solides
# menage_echelle$dep_energie_logement<-rowSums(menage_echelle[dep_sources])


# Test => #Dep_elec et Elec sont cohérents
table((menage_echelle$dep_Elec-menage_echelle$Elec)<10^(-9))
table((menage_echelle$dep_Gaz-menage_echelle$Gaz)<10^(-9))
table((menage_echelle$dep_Fuel-menage_echelle$Fuel)<10^(-9))
table((menage_echelle$dep_GPL-menage_echelle$GPL)<10^(-9))
table((menage_echelle$dep_Urbain-menage_echelle$Urbain)<10^(-9))
table((menage_echelle$dep_Solides-menage_echelle$Solides)<10^(-9))

# 
# View(rbind(menage_echelle %>% filter(ident_men==1272),menage_echelle %>% filter(ident_men==1272)))


# VERIF
# table(solde$ener_dom_surf*menage_echelle$surfhab_d-rowSums(solde[sources])<10^(-9))

# solde<-solde %>% select(ident_men,solde)
 #Rcons n'augmente pas, les frais de constructions du logement sont fixés, juste reventilation des économies d'énergies.

# A<-menage_echelle



# SOLDE_DETTE -------------------------------------------------------------

solde<-menage_echelle %>% 
  mutate(solde=solde_ener+solde_int
         # +solde_princ
         ) %>%
  select(ident_men,solde)

menage_echelle <- 
  menage_echelle %>%
  mutate(Rcons=Rcons+principal_dette)%>%
  mutate(year_neuf=0)


menage_echelle <- 
  menage_echelle %>%
  mutate_when(neuf_horizon,list(year_neuf=horizon))%>%
  select(-neuf_horizon)

sauv<-menage_echelle
source("Technical_change/Econometrie_solde_budg_Logement.R")
# source("Technical_change/Econometrie_solde_budg_bouclage_autres.R")
# # 
# # # A<-menage_echelle



Ventil_solde(solde,menage_echelle)
# return A

menage_echelle_41<-A


# DETTE
menage_echelle_41 <- 
  menage_echelle_41 %>%
  mutate(autres_services=autres_services+solde_int,
         Hors_budget=Hors_budget+solde_princ+principal_dette,
         c13711=c13711+principal_dette,
         solde_int_total=solde_int_total+solde_int,
         solde_princ_total=solde_princ_total+solde_princ)

# Rcons
menage_echelle_41$Rcons <- 
  rowSums(menage_echelle_41[list_dep])

# Parts budgétaires
for (k in list_dep){
  menage_echelle_41[paste("part",k,sep="_")]<-menage_echelle_41[k]/menage_echelle_41$Rcons
}

# Epargne
menage_echelle_41$epargne <- 
  menage_echelle_41$RDB - 
  menage_echelle_41$Rcons + 
  menage_echelle_41$rev_exceptionnel

# Ratio_S
menage_echelle_41$ratio_S <-  
  menage_echelle_41$epargne / 
  menage_echelle_41$Rcons 

# Taux épargne
menage_echelle_41$taux_epargne<- ifelse(menage_echelle_41$RDB==0,0,
                                          menage_echelle_41$epargne / 
                                            menage_echelle_41$RDB)






source("Technical_change/TC_renovation_DPE/calc_energie_kWh_m2.R")

energie_dom_surf(menage_echelle_41)

menage_echelle_41<- 
  menage_echelle_41 %>%
  select(-ener_dom_surf,-ener_dom) %>%
  left_join(dep_source_usage,by="ident_men")
# %>%
#   mutate(year_neuf=0)
# %>%
#   mutate_when(neuf_horizon,list(year_neuf=horizon))%>%
#   select(-neuf_horizon)

#Maj C13711
menage_echelle_41 <- 
  menage_echelle_41 %>%
  mutate_when(year_neuf==0,list(c13711=c13711*FC$rdb))



# Test
table(abs(menage_echelle_41$Elec-menage_echelle_41$dep_Elec)<10^(-9))
table((menage_echelle_41$dep_Gaz-menage_echelle_41$Gaz)<10^(-9))
table((menage_echelle_41$dep_Fuel-menage_echelle_41$Fuel)<10^(-9))
table((menage_echelle_41$dep_GPL-menage_echelle_41$GPL)<10^(-9))
table((menage_echelle_41$dep_Urbain-menage_echelle_41$Urbain)<10^(-9))
table((menage_echelle_41$dep_Solides-menage_echelle_41$Solides)<10^(-9))
# Les problèmes c'est les valeurs négatives.

# View(menage_echelle_41%>%filter((Elec-dep_Elec)>10^(-9))%>% select(ident_men,contains("Elec")))

# View(rbind(menage_echelle %>% filter(ident_men==12268),menage_echelle_41 %>% filter(ident_men==12268)%>% select(colnames(menage_echelle))))


# Maj_dep_preeng ----------------------------------------------------------

menage_echelle_41 <- maj_dep_preeng(bdd1= menage_echelle_1,bdd2=menage_echelle_41)




# SAVE --------------------------------------------------------------------


save(menage_echelle_41, file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_41.RData",sep=""))



#### TEST
# On reventile sans financement par la dette, Rcons est constant, épargne aussi. 

i=933
# View(rbind(menage_echelle%>% filter(ident_men==i),A %>% filter(ident_men==i),A%>% filter(ident_men==i)-menage_echelle%>% filter(ident_men==i)))




# C<-A%>% filter(ident_men==i)-menage_echelle%>% filter(ident_men==i)
# 
# D <-B%>% filter(ident_men==i)-menage_echelle%>% filter(ident_men==i)

# rowSums(C[list_dep])
# C$Rcons
# # [1] 2066.876
# C$RDB
# # [1] 0
# rowSums(D["dep_energie_logement"])

# FIN TEST


#TEST share
compute_share(menage_echelle_41)

# View(menage_echelle %>% filter(!abs(menage_echelle$agriculture-menage_echelle_41$agriculture)<10^(-6)) %>% select(ident_men, neuf_horizon,agriculture))






# SUCCESS -----------------------------------------------------------------

print("4_1_Achat_neuf_horizon : SUCCESS")

