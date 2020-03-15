
# 
# scenario="AMS"
# scenario_classement="Optimiste"
# horizon=2035
# redistribution="forfait"
# Iter=1
# Iter_last=0

# LIBRARY -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(plyr)
library(matrixStats)
library(data.table)
# library(stargazer)
library(pracma)

# DATA --------------------------------------------------------------------



list_dep_18=c("agriculture",
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

## MICRO
setwd("D:/CIRED/Projet_Ademe/")
source("D:/CIRED/Projet_Ademe/Code_global_Ademe/mutate_when.R")
source("D:/CIRED/Projet_Ademe/Code_global_Ademe/compute_share.R")
source("D:/CIRED/Projet_Ademe/Code_global_Ademe/verif_epargne_taux.R")
source("D:/CIRED/Projet_Ademe/Code_global_Ademe/compute_share_export.R")
source("D:/CIRED/Projet_Ademe/Code_global_Ademe/compute_share_export_bis.R")
source("D:/CIRED/Projet_Ademe/Code_global_Ademe/compute_savings_rate_export.R")
source("D:/CIRED/Projet_Ademe/Code_global_Ademe/positive_share.R")
source("Code_global_Ademe/maj_dep_preeng.R")
load("2010/tco2_meuros_2010.RData")
#Load last micro database

load(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter_last,"/Output/menage_echelle.RData",sep=""))

save(menage_echelle,file=paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/menage_echelle.RData",sep=""))


menage_iter_last<-menage_echelle

# MACRO
load(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM.RData",sep=""))

#Facteurs de croissance (FC) entre les itérations. A la marge ! 
FC <- 
  IMACLIM %>%
  filter(year==9999) %>%
  filter(model=="Marge")%>% #Careful, model here is "Marge" and not IMACLIM or ThreeME
  filter(Variable %in% c("revact",
                         "revpat",
                         "revchom",
                         "revsoc",
                         "revetranger",
                         "rdb",
                         "tauIR",
                         "tauAID",
                         "A01",
                         "A02",
                         "A03",
                         "A04",
                         "A05",
                         "A06",
                         "A07",
                         "A08",
                         "A09",
                         "A10",
                         "A11",
                         "A12",
                         "A13",
                         "A14")) %>%
  select(Variable,value)

print(FC)
# save(FC,file=paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_",Iter,"/Input/FC_2010_",as.character(horizon),".RData",sep=""))


TCO_tot<-as.numeric(IMACLIM %>% 
                      filter(year==horizon) %>%
                      filter(model=="IMACLIM")%>%
                      filter(Variable=="TCO_RTCD_tot")%>%
                      select(value))*10^6

TCO_RTCD <- as.numeric(IMACLIM %>% 
                         filter(year==horizon) %>%
                         filter(model=="IMACLIM")%>%
                         filter(Variable=="TCO_RTCD")%>%
                         select(value))

FC <- FC %>%
  mutate(value=as.numeric(value)) %>%
  spread(key=Variable,value=value) 



save(FC,file=
       paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/FC_",horizon,"_marge.RData",sep="")
)
# 
# IP_2010_2025_bis<-
#   IMACLIM %>%
#   filter(year==horizon) %>%
#   filter(model=="IMACLIM")%>% #Careful, model here is "IMACLIM" to have the price levels
#   filter(Variable %in% c("A01",
#                          "A02",
#                          "A03",
#                          "A04",
#                          "A05",
#                          "A06",
#                          "A07",
#                          "A08",
#                          "A09",
#                          "A10",
#                          "A11",
#                          "A12",
#                          "A13",
#                          "A14")) %>%
#   select(Variable,value)
# 
# 
# IP_2010_2025_bis <- IP_2010_2025_bis %>%
#   mutate(value=as.numeric(value)) %>%
#   spread(key=Variable,value=value)

TCO<-as.numeric(IMACLIM %>% 
                  filter(year==horizon) %>%
                  filter(model=="IMACLIM")%>%
                  filter(Variable=="TCO")%>%
                  select(value))*10^6



# Mise à l'échelle des revenus  -------------------------------------------

menage_echelle<-
  menage_echelle %>%
  mutate(rev_tot_macro=
           rev_sociaux_autres+
           chomage+
           rev_etranger+
           rev_activites_sans_etranger+
           rev_patrimoine)

menage_echelle<-
  menage_echelle %>%
  mutate(FC_rdb_menage=
           (FC$revsoc*rev_sociaux_autres+
              chomage*FC$revchom+
              rev_activites_sans_etranger * FC$revact+
              rev_etranger * FC$revetranger+
              rev_patrimoine * FC$revpat)/rev_tot_macro)

menage_echelle<-
  menage_echelle %>%
  mutate_when(is.na(FC_rdb_menage),list(FC_rdb_menage=FC$rdb))

menage_echelle <- 
  menage_echelle %>%
  mutate(retraites = retraites * (FC$revsoc)) %>%
  mutate(chomage = chomage * (FC$revchom)) %>%
  mutate(rev_activites_sans_etranger = rev_activites_sans_etranger * (FC$revact)) %>%
  mutate(rev_etranger = rev_etranger * (FC$revetranger)) %>%
  mutate(rev_exceptionnel=rev_exceptionnel * (FC_rdb_menage)) %>%
  mutate(rev_patrimoine = rev_patrimoine * (FC$revpat)) %>%
  mutate(rev_sociaux_autres = rev_sociaux_autres * (FC$revsoc)) %>%
  mutate(rev700=rev700 * (FC_rdb_menage))%>%
  mutate(rev701=rev701 * (FC_rdb_menage))%>%
  mutate(rev999=rev999 * (FC_rdb_menage))%>%
  mutate(rev801=rev801 * (FC_rdb_menage))


menage_echelle <- 
  menage_echelle %>%
  mutate(rev_sociaux = rev_sociaux_autres+chomage) %>% ###bien faire gaffe à ça !
  mutate(rev_activites=rev_activites_sans_etranger+rev_etranger)

menage_echelle$RDBAI <- 
  rowSums(menage_echelle[c("rev_activites","rev_sociaux","rev_patrimoine","rev700","rev701","rev999")])

menage_echelle <- menage_echelle %>% mutate_when(is.na(taux_IR),list(taux_IR=0),is.na(taux_AID),list(taux_AID=0))
menage_echelle$taux_AID<-menage_echelle$taux_AID * (FC$tauAID)

menage_echelle$taux_IR<-menage_echelle$taux_IR * (FC$tauIR)


#Cas particulier du ménage 8234 qui paye plus de 100% d'impôts en itération
men8234<-menage_echelle%>%filter(ident_men==8234)
if(men8234$taux_IR+men8234$taux_AID>1){
  taux<-1-men8234$taux_AID
  # print(taux)
  menage_echelle<-menage_echelle %>% mutate_when(ident_men==8234,list(taux_IR=taux))
}

menage_echelle %>% filter(ident_men==8234)%>%select(taux_IR,taux_AID)

menage_echelle$impot_revenu <- 
  menage_echelle$taux_IR * menage_echelle$RDBAI
menage_echelle$AID <- menage_echelle$taux_AID  * menage_echelle$RDBAI


menage_echelle$RDB <- menage_echelle$RDBAI - rowSums(menage_echelle[c("impot_revenu","AID")])

#Cas particulier des ménages 2548 et 10828 qui ont des NaN en AID, IR et donc en RDB
menage_echelle <- 
  menage_echelle %>%
  mutate_when(RDBAI==0,list(RDB=0))


#Taxe Carbone (TCO) retrocédée



RDB_tot <- as.numeric(menage_echelle %>% summarise(sum(pondmen*RDB)))
RDB_UC_tot <- as.numeric(menage_echelle %>% summarise(sum(pondmen*RDB/coeffuc)))
# 
# TCO_tot <- TCO_RTCD * RDB_tot

TCO_tot_UC<- TCO_tot / menage_echelle %>%summarise(sum(coeffuc*pondmen))




# TCO payée ---------------------------------------------------------------

FC_2010_h <-
  IMACLIM %>%
  filter(year==horizon) %>%
  filter(model=="IMACLIM")%>%
  filter(Variable %in% c("A01",
                         "A02",
                         "A03",
                         "A04",
                         "A05",
                         "A06",
                         "A07",
                         "A08",
                         "A09",
                         "A10",
                         "A11",
                         "A12",
                         "A13",
                         "A14")) %>%
  select(Variable,value)


FC_2010_h <- FC_2010_h %>%
  mutate(value=as.numeric(value)) %>%
  spread(key=Variable,value=value)


coeff_CL_2010<-as.numeric(tco2_meuros_2010$coeff_CL_2010)
coeff_Oil_2010<-as.numeric(tco2_meuros_2010$coeff_Oil_2010)
coeff_Gaz_2010<-as.numeric(tco2_meuros_2010$coeff_Gaz_2010)

menage_echelle <-
  menage_echelle %>%
  mutate(CL_2010=dep_Solides/FC_2010_h$A04)%>%
  mutate(Oil_2010=(carb_lubr+dep_Fuel+dep_GPL)/FC_2010_h$A07)%>%
  mutate(Gaz_2010=(dep_Gaz+dep_Urbain)/FC_2010_h$A03)

#Emissions  : € de TCO = €2010(dep) * tCO2/€2010(dep)*€(TCO)/tC02
menage_echelle <-
  menage_echelle %>%
  mutate(TCO_CL=CL_2010*coeff_CL_2010*TCO)%>%
  mutate(TCO_Oil=Oil_2010*coeff_Oil_2010*TCO)%>%
  mutate(TCO_Gaz=Gaz_2010*coeff_Gaz_2010*TCO)

#Taxe carbone par ménage
menage_echelle$TCO_paid=rowSums(menage_echelle[c("TCO_CL","TCO_Oil","TCO_Gaz")])

facteur_dec1=2.31247
facteur_tuu0 =3.381498




#code redistribution 
# 1 : forfait
# 2 : pro rata niveau de vie
# 3 : le premier décile touche 1,8 fois le montant moyen rétrocédé par ménage, et les déciles suivants un montant qui décroît linéairement jusqu’à épuiser la taxe collectée au 9ème décile, le 10ème étant exclu
# 4 : le premier tuu touche 1,8 fois le montant moyen rétrocédé par ménage, et les déciles suivants un montant qui décroît linéairement jusqu’à épuiser la taxe collectée au 8e tuu, Paris étant exclu

if(redistribution=="ssrec"){
  menage_echelle <- 
    menage_echelle %>%
    mutate(rev_TCO=0)
}


if(redistribution=="forfait"){
  
  sum_pond<-as.numeric(menage_echelle%>%summarise(sum(pondmen*coeffuc)))
  menage_echelle <- 
    menage_echelle %>%
    mutate(rev_TCO=1/sum_pond *coeffuc* TCO_tot)
  
  menage_echelle%>%summarise(sum(pondmen*rev_TCO))-TCO_tot
  (menage_echelle%>%summarise(sum(pondmen*rev_TCO))-TCO_tot)/TCO_tot
  
}

if(redistribution=="niveau_vie"){
  menage_echelle <- 
    menage_echelle %>%
    mutate(rev_TCO=RDB/coeffuc/RDB_UC_tot * TCO_tot)
  
  menage_echelle%>%summarise(sum(pondmen*rev_TCO))-TCO_tot
  (menage_echelle%>%summarise(sum(pondmen*rev_TCO))-TCO_tot)/TCO_tot
}


#Attention, augmenter le revenu des ménages en rétrocédant de la taxe, va accroitre l'assiette de cette même taxe et donc modifier le montant médian de taxe payée par le premier décile/ruraux. 
# pour converger, on utilise le montant de taxe carbone de l'itération précédente après micro-simulation des dépenses. 

if(redistribution=="decile"){ 
  detach(package:plyr)
  
  
  Tab_dec<-
    menage_echelle %>%
    group_by(decucn)%>%
    summarise(sum(pondmen*coeffuc))
  
  Tab_dec<-Tab_dec%>%mutate(decucn=as.numeric(decucn)-1) #pour se rapprocher de la solution avec tuu, on décale les déciles d'un. Le décile 1, devient 0, annule la pente de la régression
  
  # # Emissions agrégées par UC
  # Tab_emissions<-menage_forme_2010%>%group_by(decucn)%>%summarise(sum(pondmen*TCO_paid/coeffuc))
  
  
  
  # men_1<-menage_echelle%>%filter(decucn==1)%>%filter(TCO_paid>1)
  # 
  # med_0<-3*weightedMedian(x=men_1$TCO_paid,w=men_1$pondmen)
  # med_0<-weightedMedian(x=men_1$TCO_paid,w=men_1$pondmen)
  # med_0<-max(x=men_1$TCO_paid,w=men_1$pondmen)
  
  # pente=(sum(Tab_dec[1:9,2])-TCO_tot/as.numeric(TCO_tot_UC*facteur_dec1))/sum(Tab_dec[1:9,2]*Tab_dec[1:9,1])
  
  
  x_dec= bisect(function(x) (Tab_dec[1,2]*(1-x)^0+
                               Tab_dec[2,2]*(1-x)^1+
                               Tab_dec[3,2]*(1-x)^2+
                               Tab_dec[4,2]*(1-x)^3+
                               Tab_dec[5,2]*(1-x)^4+
                               Tab_dec[6,2]*(1-x)^5+
                               Tab_dec[7,2]*(1-x)^6+
                               Tab_dec[8,2]*(1-x)^7+
                               Tab_dec[9,2]*(1-x)^8)*as.numeric(facteur_dec1*TCO_tot_UC)-TCO_tot,0,1)$root
  
  
  
  
  # x_dec=0.197063
  # x_dec=0.19706308222731
  
  distrib<-function(i,coeffuc){
    i<-as.double(i)-1 #attention le as.numeric translate les tuu de 1
    # ifelse(i==9,return(0),ifelse(i==0,return(as.numeric(TCO_tot_UC*facteur_dec1)*coeffuc),return(coeffuc*(1-i*pente)*as.numeric(TCO_tot_UC*facteur_dec1))))
    ifelse(i==9,return(0),return(coeffuc*((1-x_dec)**i)*as.numeric(TCO_tot_UC*facteur_dec1)))
    # if(i==9){return(0)}
    # if(i==0){return(med_0)}else{
    #   return((1-i*pente)*med_0)}
  }
  

  menage_echelle<-
    menage_echelle %>%
    group_by(1:n())%>%
    mutate(rev_TCO=as.numeric(distrib(i=decucn,coeffuc=as.numeric(coeffuc))))%>%
    ungroup()
  
  menage_echelle<-menage_echelle %>% ungroup()
  
  TCO_rtcd2<-menage_echelle %>%
    group_by(decucn)%>%
    summarise(weighted.mean(x=rev_TCO,w=pondmen,na.rm=T))
  TCO_rtcd2
  # 
  menage_echelle%>%summarise(sum(pondmen*rev_TCO))-TCO_tot
  (menage_echelle%>%summarise(sum(pondmen*rev_TCO))-TCO_tot)/TCO_tot
}



if(redistribution=="tuu"){
  detach(package:plyr)
  
  Tab_tuu<-
    menage_echelle %>%
    group_by(tuu)%>%
    summarise(sum(pondmen*coeffuc))
  # colnames(Tab_dec)<-c("tuu","P")
  # Tab_tuu<-Tab_dec%>%mutate(tuu=as.numeric(tuu)-1)
  
  x_tuu= bisect(function(x) (Tab_tuu[1,2]*(1-x)^0+
                               Tab_tuu[2,2]*(1-x)^1+
                               Tab_tuu[3,2]*(1-x)^2+
                               Tab_tuu[4,2]*(1-x)^3+
                               Tab_tuu[5,2]*(1-x)^4+
                               Tab_tuu[6,2]*(1-x)^5+
                               Tab_tuu[7,2]*(1-x)^6+
                               Tab_tuu[8,2]*(1-x)^7)*as.numeric(facteur_tuu0*TCO_tot_UC)-TCO_tot,0,1)$root
  
  
  # x_tuu=0.5094564
  # men_0<-menage_echelle %>%filter(tuu==0)%>%filter(TCO_paid>1)
  # med_0<-10*weightedMedian(x=men_0$TCO_paid,w=men_0$pondmen)
  # med_0<-weightedMean(x=men_0$TCO_paid,w=men_0$pondmen)
  # max_0<-max(x=men_0$TCO_paid)
  # med_0<-600
  
  # pente=(sum(Tab_dec[1:8,2])-TCO_tot/as.numeric(facteur_tuu0*TCO_tot_UC))/sum(Tab_dec[1:8,2]*Tab_dec[1:8,1])
  
  distrib<-function(i,coeffuc){
    i<-as.double(i)-1 #attention le as.numeric translate les tuu de 1
    ifelse(i==8,return(0),return(coeffuc*((1-x_tuu)**i)*as.numeric(TCO_tot_UC*facteur_tuu0)))
  }
  
  menage_echelle<-
    menage_echelle %>%
    group_by(1:n())%>%
    mutate(rev_TCO=distrib(tuu,as.numeric(coeffuc)))%>%
    ungroup()
  
  menage_echelle<-menage_echelle %>% ungroup()
  
  TCO_rtcd2<-menage_echelle %>%
    group_by(tuu)%>%
    summarise(weighted.mean(x=rev_TCO,w=pondmen,na.rm=T))
  TCO_rtcd2
  # 
  (menage_echelle%>%summarise(sum(pondmen*rev_TCO))-TCO_tot)/TCO_tot
}

  

menage_echelle$RDB <-
  menage_echelle$RDB+
  menage_echelle$rev_TCO
  
  
  
  

# Calcul taux de croissance du RDB pour chaque ménage
menage_echelle$TC_RDB_nominal <- (menage_echelle$RDB - menage_iter_last$RDB)/menage_iter_last$RDB
mean(menage_echelle$TC_RDB_nominal ,na.rm=T)

# Le passage du revenu nominal au revenu reel est assuré par le déflateur de Stone, produit des prix à la puissance des parts budgétaires

# FC entre Iter 0 et Iter 1, IP stone = taux de croissance de l'IP stone entre les deux itérations. 
menage_echelle$IP_stone <-          
  FC$A01**menage_echelle$share_A01 *
  FC$A02**menage_echelle$share_A02*
  FC$A03**menage_echelle$share_A03*
  FC$A04**menage_echelle$share_A04*
  FC$A05**menage_echelle$share_A05*
  FC$A06**menage_echelle$share_A06*
  FC$A07**menage_echelle$share_A07*
  FC$A08**menage_echelle$share_A08*
  FC$A09**menage_echelle$share_A09*
  FC$A10**menage_echelle$share_A10*
  FC$A11**menage_echelle$share_A11*
  FC$A12**menage_echelle$share_A12*
  FC$A13**menage_echelle$share_A13*
  FC$A14**menage_echelle$share_A14


mean(menage_echelle$IP_stone ,na.rm=T)





menage_echelle$RDB_reel <-  menage_echelle$RDB/menage_echelle$IP_stone


menage_echelle$TC_RDB_reel <- (menage_echelle$RDB_reel-menage_iter_last$RDB)/menage_iter_last$RDB
mean(menage_echelle$TC_RDB_reel ,na.rm=T)


menage_echelle <- menage_echelle %>% 
  mutate_when(is.na(TC_RDB_reel),list(TC_RDB_reel=0))%>%
  mutate_when(is.infinite(TC_RDB_reel),list(TC_RDB_reel=0))%>%
  mutate_when(is.na(IP_stone),list(IP_stone=0))

### test
# 1+mean(menage_echelle$TC_RDB_nominal,na.rm=T)
# [1] 1.193249
# FC$rdb
# [1] 1.187501
# CCL : cohérent
####

# Mise échelle dep --------------------------------------------------------

iter=TRUE #Pour la première itération
nb_iter_RDB=0
# menage_echelle_sauv_iter_0<-menage_echelle

while(iter & nb_iter_RDB<35  ){ #& nb_iter_RDB<1 
  sauv_menage_echelle<-menage_echelle
  nb_iter_RDB=nb_iter_RDB+1
  print(nb_iter_RDB)
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
             "veh_occasion")
  
  #Traitement à part des dépenses issues de l'ENL 2013, application des élasticités correspondantes de BDF pour l'électricité, le gaz, les autres énergies et les carburants. 
  #formule : en notant D la dépense, P le prix, RDB le revenu brut disponible, l'apostrophe désignant la variable mise à l'échelle : 
  # D'=D*(1+elast_prix*Delta_prix/prix+elast_rev*Delta_RDB/RDB) * P'/P
  # or P'/P=TC_prix
  # or Delta_prix=1+TC_prix
  # Les valeurs de TC pour les prix ne sont pas des taux de croissance mais des ratios de prix (P'/P)
  # En revanche TC_RDB est un vrai taux de croissance
  # A02
  menage_echelle$dep_Elec <-
    menage_iter_last$dep_Elec*
    (1+menage_echelle$elast_prix_A02*(FC$A02/menage_echelle$IP_stone-1)) * (1+menage_echelle$elast_rev_A02*menage_echelle$TC_RDB_reel)*FC$A02
  
  # A03
  menage_echelle$dep_Gaz<-menage_iter_last$dep_Gaz*(1+menage_echelle$elast_prix_A03*(-1+FC$A03/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A03*menage_echelle$TC_RDB_reel)*FC$A03
  # A04
  menage_echelle[list_dep_autres_ener]<-menage_iter_last[list_dep_autres_ener]*(1+menage_echelle$elast_prix_A04*(-1+FC$A04/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*FC$A04
  
  # Dep_logement
  menage_echelle$dep_energie_logement<-rowSums(menage_echelle[c("dep_Elec","dep_Gaz",list_dep_autres_ener)])
  
  
  # REMARQUE : A05 est mis à l'échelle normalement, la désagrégation entre rehah et reno a été faite dans la mise en forme.
  
  for (i in c(1,5:9)){
    A0I<-paste("A0",i,sep="")
    elast_prix<-paste("elast_prix_A0",i,sep="")
    elast_rev<-paste("elast_rev_A0",i,sep="")
    
    menage_echelle[list_cat[i]]<- 
      menage_iter_last[list_cat[i]]*
      (1+menage_echelle[elast_prix]*(as.numeric(FC[A0I])/menage_echelle$IP_stone-1))*
      (1+menage_echelle[elast_rev]*menage_echelle$TC_RDB_reel)*as.numeric(FC[A0I])
  }
  
  for (i in c(10:14)){
    A0I<-paste("A",i,sep="")
    elast_prix<-paste("elast_prix_A",i,sep="")
    elast_rev<-paste("elast_rev_A",i,sep="")
    
    menage_echelle[list_cat[i]]<- 
      menage_iter_last[list_cat[i]]*
      (1+menage_echelle[elast_prix]*(-1+as.numeric(FC[A0I])/menage_echelle$IP_stone))*
      (1+menage_echelle[elast_rev]*menage_echelle$TC_RDB_reel)*as.numeric(FC[A0I])
  }
  
  
  
  # #Traitement des hors budgets :
  # #pas d'élasticité prix, élasticité revenu =1 par hypothèse, taux de croissance du rdb,
  menage_echelle$Hors_budget<-
    menage_iter_last$Hors_budget*(1 + menage_echelle$TC_RDB_nominal)
  
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
                "veh_occasion")
  
  
  
  
  
  menage_echelle<- menage_echelle %>% mutate(dep_autres_energies=dep_Fuel+dep_GPL+dep_Solides+dep_Urbain)
  
  menage_echelle <- 
    menage_echelle %>%
    mutate(c13711_neuf=0)%>%
    mutate_when(year_neuf==horizon,list(c13711_neuf=c13711))
  
  menage_echelle$Rcons_bis <- rowSums(menage_echelle[list_dep])+menage_echelle$rev801+menage_echelle$c13711_neuf
  
  
  menage_echelle$Rcons <- rowSums(menage_echelle[list_dep_18])
  
  menage_echelle$Delta <- menage_echelle$Rcons - sauv_menage_echelle$Rcons
  menage_echelle$Delta_bis<-menage_echelle$Rcons_bis - sauv_menage_echelle$Rcons_bis
  
  
  
  for (i in 1:14){
    k=list_dep[i]
    if(i<10){
      menage_echelle[paste("share_A0",i,sep="")]<-menage_echelle[k]/menage_echelle$Rcons_bis}
    else{menage_echelle[paste("share_A",i,sep="")]<-menage_echelle[k]/menage_echelle$Rcons_bis}
  }
  menage_echelle$share_A13<-(menage_echelle$loyers+menage_echelle$rev801)/menage_echelle$Rcons_bis
  
  menage_echelle$share_A05<-(menage_echelle$BTP+menage_echelle$c13711_neuf)/menage_echelle$Rcons_bis 
  
  
  menage_echelle <- 
    menage_echelle %>%
    mutate_when(is.na(share_A01),list(share_A01=0),
                is.na(share_A02),list(share_A02=0),
                is.na(share_A03),list(share_A03=0),
                is.na(share_A04),list(share_A04=0),
                is.na(share_A05),list(share_A05=0),
                is.na(share_A06),list(share_A06=0),
                is.na(share_A07),list(share_A07=0),
                is.na(share_A08),list(share_A08=0),
                is.na(share_A09),list(share_A09=0),
                is.na(share_A10),list(share_A10=0),
                is.na(share_A11),list(share_A11=0),
                is.na(share_A12),list(share_A12=0),
                is.na(share_A13),list(share_A13=0),
                is.na(share_A14),list(share_A14=0))
  
  # share_2025=rep(1,14)
  # for (i in 1:14){
  #   if(i<10){
  #     k=paste("share_A0",i,sep="")
  #     share_2025[i]=as.numeric(menage_echelle %>% summarise(weighted.mean(get(k),pondmen,na.rm=T)))}
  #   else{ 
  #     k=paste("share_A",i,sep="")
  #     share_2025[i]=as.numeric(menage_echelle %>% summarise(weighted.mean(get(k),pondmen,na.rm=T)))}
  # }
  # share_2025
  compute_share(menage_echelle)
  # revenu reel 2010 = revenu nominal 2010
  menage_echelle$IP_stone <-          
    FC$A01**menage_echelle$share_A01 *
    FC$A02**menage_echelle$share_A02*
    FC$A03**menage_echelle$share_A03*
    FC$A04**menage_echelle$share_A04*
    FC$A05**menage_echelle$share_A05*
    FC$A06**menage_echelle$share_A06*
    FC$A07**menage_echelle$share_A07*
    FC$A08**menage_echelle$share_A08*
    FC$A09**menage_echelle$share_A09*
    FC$A10**menage_echelle$share_A10*
    FC$A11**menage_echelle$share_A11*
    FC$A12**menage_echelle$share_A12*
    FC$A13**menage_echelle$share_A13*
    FC$A14**menage_echelle$share_A14
  

  
  menage_echelle$RDB_reel <-  menage_echelle$RDB/menage_echelle$IP_stone

  
  menage_echelle$TC_RDB_reel <- (menage_echelle$RDB_reel-menage_iter_last$RDB)/menage_iter_last$RDB
  menage_echelle <- menage_echelle %>% mutate_when(is.na(TC_RDB_reel),list(TC_RDB_reel=0),is.infinite(TC_RDB_reel),list(TC_RDB_reel=0))
  
  
  tol=abs((menage_echelle$RDB_reel-sauv_menage_echelle$RDB_reel)/sauv_menage_echelle$RDB_reel)
  max(tol,na.rm=T)
  print(max(tol,na.rm=T))
  if(max(tol,na.rm=T)>10^-3){iter=TRUE} else {iter=FALSE}
  # A<-data.frame("ident_men"=menage_echelle$ident_men,"tol"=tol)
  # A%>%filter(tol==max(tol,na.rm=T))
}




# A02
menage_echelle$dep_Elec <-
  menage_iter_last$dep_Elec*
  (1+menage_echelle$elast_prix_A02*(FC$A02/menage_echelle$IP_stone-1)) * (1+menage_echelle$elast_rev_A02*menage_echelle$TC_RDB_reel)*FC$A02

# A03
menage_echelle$dep_Gaz<-menage_iter_last$dep_Gaz*(1+menage_echelle$elast_prix_A03*(-1+FC$A03/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A03*menage_echelle$TC_RDB_reel)*FC$A03
# A04
menage_echelle[list_dep_autres_ener]<-menage_iter_last[list_dep_autres_ener]*(1+menage_echelle$elast_prix_A04*(-1+FC$A04/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*FC$A04

# Dep_logement
menage_echelle$dep_energie_logement<-rowSums(menage_echelle[c("dep_Elec","dep_Gaz",list_dep_autres_ener)])


# REMARQUE : A05 est mis à l'échelle normalement, la désagrégation entre rehah et reno a été faite dans la mise en forme.

for (i in c(1,5:9)){
  A0I<-paste("A0",i,sep="")
  elast_prix<-paste("elast_prix_A0",i,sep="")
  elast_rev<-paste("elast_rev_A0",i,sep="")
  
  menage_echelle[list_cat[i]]<- 
    menage_iter_last[list_cat[i]]*
    (1+menage_echelle[elast_prix]*(as.numeric(FC[A0I])/menage_echelle$IP_stone-1))*
    (1+menage_echelle[elast_rev]*menage_echelle$TC_RDB_reel)*as.numeric(FC[A0I])
}

for (i in c(10:14)){
  A0I<-paste("A",i,sep="")
  elast_prix<-paste("elast_prix_A",i,sep="")
  elast_rev<-paste("elast_rev_A",i,sep="")
  
  menage_echelle[list_cat[i]]<- 
    menage_iter_last[list_cat[i]]*
    (1+menage_echelle[elast_prix]*(-1+as.numeric(FC[A0I])/menage_echelle$IP_stone))*
    (1+menage_echelle[elast_rev]*menage_echelle$TC_RDB_reel)*as.numeric(FC[A0I])
}


menage_echelle$c13711<- 
  menage_iter_last$c13711*
  (1+menage_echelle$elast_prix_A05*(-1+as.numeric(FC$A05)/menage_echelle$IP_stone))*
  (1+menage_echelle$elast_rev_A05*menage_echelle$TC_RDB_reel)*as.numeric(FC$A05)




# Nouveau Rcons -----------------------------------------------------------


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

menage_echelle$Rcons <- rowSums(menage_echelle[list_dep])

# Nouvelles parts budgétaires ---------------------------------------------

for (k in list_dep){
  menage_echelle[paste("part",k,sep="_")]<-menage_echelle[k]/menage_echelle$Rcons
}

# Epargne
menage_echelle$epargne <- 
  menage_echelle$RDB - 
  menage_echelle$Rcons + 
  menage_echelle$rev_exceptionnel

# Taux Epargne
menage_echelle$taux_epargne <- ifelse(menage_echelle$RDB==0,0,
                                           menage_echelle$epargne/menage_echelle$RDB)
menage_echelle <- 
  menage_echelle %>%
  mutate_when(RDB==0,
              list(taux_epargne=0))

#Ratio_S
menage_echelle$ratio_S <- 
  menage_echelle$epargne/menage_echelle$Rcons  # probleme pour RDB = 0 (ratio_S = -1)

# # Taux Effort Financier
# menage_echelle$taux_effort_financier<-(menage_forme_2010$taux_effort_financier*menage_forme_2010$RDB)*(1 + menage_echelle$TC_RDB)/menage_echelle$RDB
# # 0.08959135




# Eliminer les NA ---------------------------------------------------------

menage_echelle$share_A01[is.na(menage_echelle$share_A01)] <- 0
menage_echelle$share_A02[is.na(menage_echelle$share_A01)] <- 0
menage_echelle$share_A03[is.na(menage_echelle$share_A01)] <- 0
menage_echelle$share_A04[is.na(menage_echelle$share_A01)] <- 0
menage_echelle$share_A05[is.na(menage_echelle$share_A01)] <- 0
menage_echelle$share_A06[is.na(menage_echelle$share_A01)] <- 0
menage_echelle$share_A07[is.na(menage_echelle$share_A01)] <- 0
menage_echelle$share_A08[is.na(menage_echelle$share_A01)] <- 0
menage_echelle$share_A09[is.na(menage_echelle$share_A01)] <- 0
menage_echelle$share_A10[is.na(menage_echelle$share_A01)] <- 0
menage_echelle$share_A11[is.na(menage_echelle$share_A01)] <- 0
menage_echelle$share_A12[is.na(menage_echelle$share_A01)] <- 0
menage_echelle$share_A13[is.na(menage_echelle$share_A01)] <- 0
menage_echelle$agriculture[is.na(menage_echelle$agriculture)] <- 0
menage_echelle$dep_Elec[is.na(menage_echelle$dep_Elec)] <- 0
menage_echelle$dep_Gaz[is.na(menage_echelle$dep_Gaz)] <- 0
menage_echelle$dep_GPL[is.na(menage_echelle$dep_GPL)] <- 0
menage_echelle$dep_Fuel[is.na(menage_echelle$dep_Fuel)] <- 0
menage_echelle$dep_Urbain[is.na(menage_echelle$dep_Urbain)] <- 0
menage_echelle$dep_Solides[is.na(menage_echelle$dep_Solides)] <- 0
menage_echelle$BTP[is.na(menage_echelle$BTP)] <- 0
menage_echelle$prod_veh[is.na(menage_echelle$prod_veh)] <- 0
menage_echelle$carb_lubr[is.na(menage_echelle$carb_lubr)] <- 0
menage_echelle$transp_rail_air[is.na(menage_echelle$transp_rail_air)] <- 0
menage_echelle$transp_routes_eau[is.na(menage_echelle$transp_routes_eau)] <- 0
menage_echelle$loisirs_com[is.na(menage_echelle$loisirs_com)] <- 0
menage_echelle$autres_services[is.na(menage_echelle$autres_services)] <- 0
menage_echelle$autres[is.na(menage_echelle$autres)] <- 0
menage_echelle$loyers[is.na(menage_echelle$loyers)] <- 0
menage_echelle$veh_occasion[is.na(menage_echelle$veh_occasion)] <- 0



# Maj_dep_preeng ----------------------------------------------------------

menage_echelle <- maj_dep_preeng(bdd1= menage_iter_last,bdd2=menage_echelle)





# Save files --------------------------------------------------------------

# save(menage_calibr_2010,file="2010/menage_calibr_2010.RData")

save(menage_echelle,file=
       paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep="")
)

     
# save(menage_echelle,file="2025/Iteration_1/Input/menage_echelle.RData")



# Next Step ---------------------------------------------------------------

# Introduction du changement technique
# fichier : Technical_change/TC_Renovation_DPE/1_mise_echelle_energies_detaillees.R


# SUCCESS -----------------------------------------------------------------

print("2_Mise_echelle_dep_elast : SUCCESS")



# Parts -------------------------------------------------------------------



# load("2010/menage_forme_2010.RData")
# share_2010<-compute_share(menage_forme_2010)
# share_2025<-compute_share(menage_echelle)
# Parts=as.data.frame(rbind(share_2010,share_2025))
# colnames(Parts)=col
# rownames(Parts)<-c(2010,2025)
# View(Parts)
# 
# stargazer(Parts,type="text",summary=FALSE)
# 
# epargne_2010<-sum(menage_calibr_2010$taux_epargne*menage_calibr_2010$pondmen*menage_calibr_2010$RDB /
#                     sum(menage_calibr_2010$RDB*menage_calibr_2010$pondmen),
#                   na.rm = T)
# taux_epargne<-sum(menage_echelle$taux_epargne*menage_echelle$pondmen*menage_echelle$RDB /
#                     sum(menage_echelle$RDB*menage_echelle$pondmen),
#                   na.rm = T)
# epargne_2010
# # [1] 0.05759094
# taux_epargne
# # [1] 0.07137707

# compute_share(menage_echelle)
# compute_savings_rate(menage_echelle)
compute_share_export(menage_echelle)
compute_savings_rate_export(menage_echelle)



# 
# # Export csv --------------------------------------------------------------
epargne<-compute_savings_rate_export(menage_echelle)
epargne

# share<-compute_share_export(menage_echelle)
# ELEC, GAS, OIL, CL sont des variations par rapports à 2010 en euros constants €2010
# library(plyr)
# Print("compute_share_export_bis")
share<-compute_share_export_bis(menage_echelle,s=scenario,h=horizon,sc=scenario_classement,r=redistribution,iter=Iter
                                )
# share<-compute_share_export(menage_echelle)
share

load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/Cout_bailleur_public.RData",sep=""))
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/Subvention_rehab.RData",sep=""))

sBCE<-as.numeric(Subvention/(Subvention+menage_echelle%>%summarise(sum(pondmen*BTP))))
Cout_bailleur_public<-as.numeric(Cout_bailleur_public)

export<-t(data.frame(share,"epargne"=epargne,"sBCE"=sBCE,"Renovation_BS"=Cout_bailleur_public))


exp<-cbind(rownames(export),export[,1])
exp_df<-data.table(exp)



file.remove("D:/CIRED/Projet_Ademe/IMACLIM/Input_macro.csv")
data.table::fwrite(exp_df,file="D:/CIRED/Projet_Ademe/IMACLIM/Input_macro.csv",dec=".",sep=";",col.names = F,row.names = F)
write.csv(exp,file="D:/CIRED/Projet_Ademe/IMACLIM/Input_macro_check.csv")
write.csv(export,file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/export_Iter_",Iter,".csv",sep=""))

