# horizon=2025

# LIBRARY -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(plyr)
library(stargazer)
library(dplyr)
library(matrixStats)
library(pracma)



# DATA --------------------------------------------------------------------


## MICRO
setwd("D:/CIRED/Projet_Ademe/")
source("Code_global_Ademe/mutate_when.R")
source("Code_global_Ademe/compute_share.R")
source("Code_global_Ademe/positive_share.R")
source("Code_global_Ademe/maj_dep_preeng.R")
load("2010/menage_forme_2010.RData")
# load("2010/menage_calibr_2010.RData")
# load("2025/menage_calibr_2025.RData")

EMS<-read_excel(path=paste("IMACLIM/EMS.xlsx",sep=""),range=paste(scenario,"!B1:AF5",sep=""),col_names=T)

menage_echelle<-menage_forme_2010

# menage_echelle<-menage_calibr_2010


# MACRO
load(paste(scenario,"/",horizon,"/IMACLIM.RData",sep=""))
load("Donnees_brutes/Sorties ThreeMe/ThreeME.RData")
#Facteurs de croissance (FC)
FC <- 
  IMACLIM %>%
  filter(year==horizon) %>%
  filter(model=="IMACLIM")%>%
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
  
  FC <- FC %>%
    mutate(value=as.numeric(value)) %>%
    spread(key=Variable,value=value) 
  
  
  
  save(FC,file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))

  
  # TCO_RTCD <- as.numeric(IMACLIM %>% 
  #   filter(year==horizon) %>%
  #   filter(model=="IMACLIM")%>%
  #   filter(Variable=="TCO_RTCD")%>%
  #   select(value))
  
  TCO_tot<-as.numeric(IMACLIM %>% 
                        filter(year==horizon) %>%
                        filter(model=="IMACLIM")%>%
                        filter(Variable=="TCO_RTCD_tot")%>%
                        select(value))*10^6
  
  
TCO<-as.numeric(IMACLIM %>% 
                  filter(year==horizon) %>%
                  filter(model=="IMACLIM")%>%
                  filter(Variable=="TCO")%>%
                  select(value))*10^6
  
  
## ELASTICITES

# Importer élasticités prix et revenus de chaque TYPO x DECILE
load("Donnees_brutes/Econometrie_demande/elasticite_demande.RData") #Elast


# Nom de tous les catégories de "A01" à "A014")
Cat<-names(FC)[1:14]

#Pour chaque classe décilextypo on calcule l'effet prix et revenu. La somme de ces deux effets sera importé pour chaque ménage


list_A=c()
for (i in 1:9){list_A=c(list_A,paste("A0",i,sep=""))}
for (i in 10:14){list_A=c(list_A,paste("A",i,sep=""))}
# list_A
list_elast_rev<-paste("elast_rev",list_A,sep="_")
list_elast_prix<-paste("elast_prix",list_A,sep="_")


# Elasticité prix --------------------------------------------------------------
TC_prix <-
  FC %>%
  select(Cat) %>%
  gather(key="CODADEME",value="Facteur_Croissance") %>%
  mutate(TC=as.numeric(Facteur_Croissance)-1)
# save(TC_prix,file=paste(scenario,"/",horizon,"/",scenario_classement,"/","Iteration_0/Input/TC_prix_2010_",horizon,".RData",sep=""))

Elast_prix <-
  Elast %>% 
  filter(typ_elast=="prix") %>% 
  left_join(.,TC_prix,by="CODADEME") %>% 
  select(CODADEME,Typo, Decile,elast) %>%
  dplyr::rename(.,elast_prix=elast) %>%
  spread(key=CODADEME,value=elast_prix)%>%
  mutate(Decile=as.numeric(Decile))
colnames(Elast_prix)<-c("Typo","Decile",list_elast_prix) 





# Elasticité revenu ------------------------------------------------------------

Elast_rev <-
  Elast %>% filter(typ_elast=="rev") %>% 
  # mutate_when(CODADEME=="A07" &typ_elast=="rev",list(elast=0)) %>% #### ATTENTION à ENLEVER §§ TEST
  mutate(elast_rev=elast) %>%
  select(CODADEME,Typo, Decile,elast_rev)%>%
  spread(key=CODADEME,value=elast_rev) %>%
  mutate(Decile=as.numeric(Decile)) 

colnames(Elast_rev)<-c("Typo","Decile",list_elast_rev) 

## 


# Elasticités des Ménages -------------------------------------------------

menage_echelle$decucn <- as.numeric(menage_echelle$decucn)

menage_echelle<- 
  menage_echelle %>% 
  left_join(Elast_prix,by=c("typo2010f"="Typo","decucn"="Decile")) %>%
  left_join(Elast_rev,by=c("typo2010f"="Typo","decucn"="Decile")) 



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

# mean(menage_echelle$rev_sociaux_autres/menage_echelle$rev_tot_macro,na.rm=T) #0.4087486
# mean(menage_echelle$chomage/menage_echelle$rev_tot_macro,na.rm=T) #0.03399645
# mean(menage_echelle$rev_activites_sans_etranger/menage_echelle$rev_tot_macro,na.rm=T) #0.5220058
# mean(menage_echelle$rev_etranger/menage_echelle$rev_tot_macro,na.rm=T) #0.001779618
# mean(menage_echelle$rev_patrimoine/menage_echelle$rev_tot_macro,na.rm=T) #0.03346953

# menage_echelle%>%summarise(sum(rev_sociaux_autres*pondmen))/menage_echelle%>%summarise(sum(rev_tot_macro*pondmen)) #0.3093684
# menage_echelle%>%summarise(sum(chomage*pondmen))/menage_echelle%>%summarise(sum(rev_tot_macro*pondmen)) #0.0293001
# menage_echelle%>%summarise(sum(rev_activites_sans_etranger*pondmen))/menage_echelle%>%summarise(sum(rev_tot_macro*pondmen)) #0.6222393
# menage_echelle%>%summarise(sum(rev_etranger*pondmen))/menage_echelle%>%summarise(sum(rev_tot_macro*pondmen)) #0.002079752
# menage_echelle%>%summarise(sum(rev_patrimoine*pondmen))/menage_echelle%>%summarise(sum(rev_tot_macro*pondmen)) #0.03701243

# 0.3093684*FC$revsoc+0.0293001*FC$revchom+0.6222393*FC$revact+0.002079752*FC$revetranger+0.03701243*FC$revpat
# 1.575786

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

# menage_echelle <- 
#   menage_echelle %>%
#   mutate(retraites = retraites * (FC$revsoc)) %>%
#   mutate(chomage = chomage * (FC$revchom)) %>%
#   mutate(rev_activites_sans_etranger = rev_activites_sans_etranger * (FC$revact)) %>%
#   mutate(rev_etranger = rev_etranger * (FC$revetranger)) %>%
#   mutate(rev_exceptionnel=rev_exceptionnel * (FC$rdb)) %>%
#   mutate(rev_patrimoine = rev_patrimoine * (FC$revpat)) %>%
#   mutate(rev_sociaux_autres = rev_sociaux_autres * (FC$revsoc)) %>%
#   mutate(rev700=rev700 * (FC$rdb))%>%
#   mutate(rev701=rev701 * (FC$rdb))%>%
#   mutate(rev999=rev999 * (FC$rdb))%>%
#   mutate(rev801=rev801 * (FC$rdb))
  
menage_echelle <- 
  menage_echelle %>%
  mutate(rev_sociaux = rev_sociaux_autres+chomage) %>% ###bien faire gaffe à ça !
  mutate(rev_activites=rev_activites_sans_etranger+rev_etranger)

menage_echelle$RDBAI <- 
  rowSums(menage_echelle[c("rev_activites","rev_sociaux","rev_patrimoine","rev700","rev701","rev999")])

menage_echelle$taux_AID<-menage_echelle$taux_AID * (FC$tauAID)

menage_echelle$taux_IR<-menage_echelle$taux_IR * (FC$tauIR)

menage_echelle$impot_revenu <- 
  menage_echelle$taux_IR * menage_echelle$RDBAI
menage_echelle$AID <- menage_echelle$taux_AID  * menage_echelle$RDBAI


menage_echelle$RDB <- menage_echelle$RDBAI - rowSums(menage_echelle[c("impot_revenu","AID")])

#Cas particulier des ménages 2548 et 10828 qui ont des NaN en AID, IR et donc en RDB
menage_echelle <- 
  menage_echelle %>%
  mutate_when(RDBAI==0,list(RDB=0))
# cas Particulier du ménage 8234 qui paie 88% de son RDBAI en impôt en 2010, et 97% en 2035.Mise à zéro du RDB par commodité contre -3783 sinon. 
menage_echelle <- 
  menage_echelle %>%
  mutate_when(ident_men==8234,list(RDB=0))


#Taxe Carbone (TCO) retrocédée
RDB_tot <- as.numeric(menage_echelle %>% summarise(sum(pondmen*RDB)))
RDB_UC_tot <- as.numeric(menage_echelle %>% summarise(sum(pondmen*RDB/coeffuc)))

# TCO_tot <- TCO_RTCD * RDB_tot

TCO_tot_UC<-TCO_tot/menage_echelle %>% summarise(sum(pondmen*coeffuc))




# TCO payée ---------------------------------------------------------------
# 
# #tonnes C02
# EMS<-EMS %>% gather(key=year,value=emission,-1)%>%filter(!Var=="EMS_HH_2")
#  
# prix constant 2010 tCO2/ dépenses par cat
# coeff_CL_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_21_2")%>%select(emission))/
#   as.numeric(menage_forme_2010 %>%summarise(sum(pondmen*dep_Solides))) # en tC02
# 
# coeff_Oil_2010<-as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_22_2")%>%select(emission))/
#   as.numeric(menage_forme_2010 %>% summarise(sum(pondmen*(carb_lubr+dep_Fuel+dep_GPL)))) # en tC02/Millions €
# 
# coeff_Gaz_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_24_2")%>%select(emission))/
#   as.numeric(menage_forme_2010 %>%summarise(sum(pondmen*(dep_Gaz+dep_Urbain)))) #
# #Horizon CL, Oil, Gaz dépenses à l'horizon en millions d'euros 2010 (M€2010)
# 
# 
# tco2_meuros_2010<-data.frame(coeff_CL_2010,coeff_Oil_2010,coeff_Gaz_2010)
# save(tco2_meuros_2010,file="2010/tco2_meuros_2010.RData")
# 
# menage_echelle <- 
#   menage_echelle %>% 
#   mutate(CL_2010=dep_Solides/FC$A04)%>% 
#   mutate(Oil_2010=(carb_lubr+dep_Fuel+dep_GPL)/FC$A07)%>%
#   mutate(Gaz_2010=(dep_Gaz+dep_Urbain)/FC$A03)
# 
# #Emissions  : € de TCO = €2010(dep) * tCO2/€2010(dep)*€(TCO)/tC02
# menage_forme_2010 <- 
#   # menage_forme_2010 %>% 
#   mutate(TCO_CL=dep_Solides*coeff_CL_2010)%>%
#   mutate(TCO_Oil=(carb_lubr+dep_Fuel+dep_GPL)*coeff_Oil_2010)%>%
#   mutate(TCO_Gaz=(dep_Gaz+dep_Urbain)*coeff_Gaz_2010)
# 
# 
# menage_echelle <- 
#   menage_echelle %>% 
#   mutate(TCO_CL=CL_2010*coeff_CL_2010*TCO)%>%
#   mutate(TCO_Oil=Oil_2010*coeff_Oil_2010*TCO)%>%
#   mutate(TCO_Gaz=Gaz_2010*coeff_Gaz_2010*TCO)
# 
# 
# 
# #Taxe carbone par ménage
# menage_echelle$TCO_paid=rowSums(menage_echelle[c("TCO_CL","TCO_Oil","TCO_Gaz")])
# # menage_forme_2010$TCO_paid=rowSums(menage_forme_2010[c("TCO_CL","TCO_Oil","TCO_Gaz")])
# 
# emissions_UC_2010<-menage_forme_2010 %>% summarise(sum(TCO_paid*pondmen)/sum(coeffuc*pondmen)) #moyenne par UC de la TCO paid
# men_dec_1<-menage_forme_2010 %>% filter(decucn==1)%>%filter(TCO_paid>1)%>%summarise(weighted.mean(x=TCO_paid/coeffuc,w=pondmen)) # Médiane de la TCO par UC du 1er décile
# facteur_dec1<-men_dec_1/emissions_UC_2010
facteur_dec1 = 2.31247

# x_dec=0.19706308222731

# library(pracma)


# men_tuu_0<-menage_forme_2010 %>% filter(tuu==0)%>%filter(TCO_paid>1)%>%summarise(weighted.mean(x=TCO_paid/coeffuc,w=pondmen))
# facteur_tuu0<-men_tuu_0/emissions_UC_2010
facteur_tuu0 = 3.381498



# x_tuu=0.509456451186799 #Excel


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
  
}

if(redistribution=="niveau_vie"){
menage_echelle <- 
  menage_echelle %>%
  mutate(rev_TCO=RDB/coeffuc/RDB_UC_tot * TCO_tot)
}

# if(redistribution=="decile"){ #approx, hyp : tous les déciles ont le même nombre de ménages
#   detach(package:plyr)
#   alpha=-0.8*TCO_tot/36
#   Beta=-10*alpha
#   
#   Tab_dec<-
#     menage_echelle %>% 
#     group_by(decucn)%>%
#     summarise(sum(pondmen))
#   
#   distrib<-function(i){
#     i<-as.numeric(i)
#     if(i==1){return(1.8*TCO_tot/9/as.numeric(Tab_dec[i,2]))}
#     return((alpha*i+Beta)/as.numeric(Tab_dec[i,2]))
#   }
#   menage_echelle<-
#     menage_echelle %>%
#     group_by(1:n())%>%
#     mutate(rev_TCO=distrib(decucn))%>%
#     ungroup()
#   
#   menage_echelle<-menage_echelle %>% ungroup()
# }

if(redistribution=="decile"){ 
  detach(package:plyr)
  
  
  Tab_dec<-
    menage_echelle %>%
    group_by(decucn)%>%
    summarise(sum(pondmen*coeffuc))
  
  Tab_dec<-Tab_dec%>%mutate(decucn=as.numeric(decucn)-1) #pour se rapprocher de la solution avec tuu, on décale les déciles d'un. Le décile 1, devient 0, annule la pente de la régression
  
  # Emissions agrégées par UC
  # Tab_emissions<-menage_forme_2010%>%group_by(decucn)%>%summarise(sum(pondmen*TCO_paid/coeffuc))
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
  
  
  # men_1<-menage_echelle%>%filter(decucn==1)%>%filter(TCO_paid>1)
  # 
  # med_0<-3*weightedMedian(x=men_1$TCO_paid,w=men_1$pondmen)
  # med_0<-weightedMedian(x=men_1$TCO_paid,w=men_1$pondmen)
  # med_0<-max(x=men_1$TCO_paid,w=men_1$pondmen)
  
  # pente=(sum(Tab_dec[1:9,2])-TCO_tot/as.numeric(TCO_tot_UC*facteur_dec1))/sum(Tab_dec[1:9,2]*Tab_dec[1:9,1])
  
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
        menage_echelle%>%summarise(sum(pondmen*rev_TCO))-TCO_tot
}

menage_echelle$RDB <-
  menage_echelle$RDB+
  menage_echelle$rev_TCO




# Test
# 
# menage_echelle_bis<-menage_echelle
# 
# menage_echelle_bis$RDB <-
#   menage_echelle$RDB+
#   menage_echelle$rev_TCO
# 
# weighted.mean((menage_echelle_bis$RDB-menage_echelle$RDB)/menage_echelle$RDB,menage_echelle$pondmen,na.rm=T)
# # +0.7% de croissance du RDB des ménages dues à la TCO rétrocédé

# Calcul taux de croissance du RDB pour chaque ménage
menage_echelle$TC_RDB_nominal <- (menage_echelle$RDB - menage_forme_2010$RDB)/menage_forme_2010$RDB

# Le passage du revenu nominal au revenu reel est assuré par le déflateur de Stone, produit des prix à la puissance des parts budgétaires

# revenu reel 2010 = revenu nominal 2010
menage_echelle$IP_stone <-  1
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
menage_echelle <-
  menage_echelle %>%
  mutate_when(is.na(RDB_reel),list(RDB_reel=0))

menage_echelle$TC_RDB_reel <- (menage_echelle$RDB_reel-menage_forme_2010$RDB)/menage_forme_2010$RDB

### test
# 1+mean(menage_echelle$TC_RDB)
# [1] 1.193249
# TC$rdb
# [1] 1.187501
# CCL : cohérent
####

# Mise échelle dep --------------------------------------------------------

iter=TRUE #Pour la première itération
nb_iter_RDB=0

while(iter & nb_iter_RDB<60){
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
  menage_forme_2010$dep_Elec*
  (1+menage_echelle$elast_prix_A02*(FC$A02/menage_echelle$IP_stone-1)) * 
  (1+menage_echelle$elast_rev_A02*menage_echelle$TC_RDB_reel)*
  FC$A02

# A03
menage_echelle$dep_Gaz<-menage_forme_2010$dep_Gaz*(1+menage_echelle$elast_prix_A03*(-1+FC$A03/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A03*menage_echelle$TC_RDB_reel)*FC$A03
# A04
menage_echelle[list_dep_autres_ener]<-menage_forme_2010[list_dep_autres_ener]*(1+menage_echelle$elast_prix_A04*(-1+FC$A04/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*FC$A04

# Dep_logement
menage_echelle$dep_energie_logement<-rowSums(menage_echelle[c("dep_Elec","dep_Gaz",list_dep_autres_ener)])


# REMARQUE : A05 est mis à l'échelle normalement, la désagrégation entre rehah et reno a été faite dans la mise en forme.

for (i in c(1,5:9)){
  A0I<-paste("A0",i,sep="")
  elast_prix<-paste("elast_prix_A0",i,sep="")
  elast_rev<-paste("elast_rev_A0",i,sep="")
  
  menage_echelle[list_cat[i]]<- 
    menage_forme_2010[list_cat[i]]*
    (1+menage_echelle[elast_prix]*(as.numeric(FC[A0I])/menage_echelle$IP_stone-1))*
    (1+menage_echelle[elast_rev]*menage_echelle$TC_RDB_reel)*as.numeric(FC[A0I])
}

for (i in c(10:14)){
  A0I<-paste("A",i,sep="")
  elast_prix<-paste("elast_prix_A",i,sep="")
  elast_rev<-paste("elast_rev_A",i,sep="")
  
  menage_echelle[list_cat[i]]<- 
    menage_forme_2010[list_cat[i]]*
    (1+menage_echelle[elast_prix]*(-1+as.numeric(FC[A0I])/menage_echelle$IP_stone))*
    (1+menage_echelle[elast_rev]*menage_echelle$TC_RDB_reel)*as.numeric(FC[A0I])
}



# #Traitement des hors budgets :
# #pas d'élasticité prix, élasticité revenu =1 par hypothèse, taux de croissance du rdb,
menage_echelle$Hors_budget<-
  menage_forme_2010$Hors_budget*(1 + menage_echelle$TC_RDB_nominal)

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


menage_echelle<- menage_echelle %>% mutate(dep_autres_energies=dep_Fuel+dep_GPL+dep_Solides+dep_Urbain)
menage_echelle$Rcons_bis <- rowSums(menage_echelle[list_dep])+menage_echelle$rev801
## Attention on n'a pas rajouté les constructions neuves en 2025, elles ne sont pas encore attribuées


for (i in 1:14){
  k=list_dep[i]
  if(i<10){
    menage_echelle[paste("share_A0",i,sep="")]<-menage_echelle[k]/menage_echelle$Rcons_bis}
  else{menage_echelle[paste("share_A",i,sep="")]<-menage_echelle[k]/menage_echelle$Rcons_bis}
}
menage_echelle$share_A13<-(menage_echelle$loyers+menage_echelle$rev801)/menage_echelle$Rcons_bis

share_horizon=rep(1,14)
for (i in 1:14){
  if(i<10){
    k=paste("share_A0",i,sep="")
    share_horizon[i]=as.numeric(menage_echelle %>% summarise(weighted.mean(get(k),pondmen,na.rm=T)))}
  else{ 
    k=paste("share_A",i,sep="")
    share_horizon[i]=as.numeric(menage_echelle %>% summarise(weighted.mean(get(k),pondmen,na.rm=T)))}
}
share_horizon

# revenu reel 2010 = revenu nominal 2010
menage_echelle$IP_stone <-
  FC$A01**menage_echelle$share_A01*
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
menage_echelle <-
  menage_echelle %>%
  mutate_when(is.na(RDB_reel),list(RDB_reel=0))

menage_echelle$TC_RDB_reel <- (menage_echelle$RDB_reel-menage_forme_2010$RDB)/menage_forme_2010$RDB


tol=abs((menage_echelle$RDB_reel-sauv_menage_echelle$RDB_reel)/sauv_menage_echelle$RDB_reel)
max(tol,na.rm=T)
print(max(tol,na.rm=T))
if(max(tol,na.rm=T)>10^-3){iter=TRUE} else {iter=FALSE}

}




# A02
menage_echelle$dep_Elec <-
  menage_forme_2010$dep_Elec*
  (1+menage_echelle$elast_prix_A02*(FC$A02/menage_echelle$IP_stone-1)) * (1+menage_echelle$elast_rev_A02*menage_echelle$TC_RDB_reel)*FC$A02

# A03
menage_echelle$dep_Gaz<-menage_forme_2010$dep_Gaz*(1+menage_echelle$elast_prix_A03*(-1+FC$A03/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A03*menage_echelle$TC_RDB_reel)*FC$A03
# A04
menage_echelle[list_dep_autres_ener]<-menage_forme_2010[list_dep_autres_ener]*(1+menage_echelle$elast_prix_A04*(-1+FC$A04/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*FC$A04

# Dep_logement
menage_echelle$dep_energie_logement<-rowSums(menage_echelle[c("dep_Elec","dep_Gaz",list_dep_autres_ener)])


# REMARQUE : A05 est mis à l'échelle normalement, la désagrégation entre rehah et reno a été faite dans la mise en forme.

for (i in c(1,5:9)){
  A0I<-paste("A0",i,sep="")
  elast_prix<-paste("elast_prix_A0",i,sep="")
  elast_rev<-paste("elast_rev_A0",i,sep="")
  
  menage_echelle[list_cat[i]]<- 
    menage_forme_2010[list_cat[i]]*
    (1+menage_echelle[elast_prix]*(as.numeric(FC[A0I])/menage_echelle$IP_stone-1))*
    (1+menage_echelle[elast_rev]*menage_echelle$TC_RDB_reel)*as.numeric(FC[A0I])
}

for (i in c(10:14)){
  A0I<-paste("A",i,sep="")
  elast_prix<-paste("elast_prix_A",i,sep="")
  elast_rev<-paste("elast_rev_A",i,sep="")
  
  menage_echelle[list_cat[i]]<- 
    menage_forme_2010[list_cat[i]]*
    (1+menage_echelle[elast_prix]*(-1+as.numeric(FC[A0I])/menage_echelle$IP_stone))*
    (1+menage_echelle[elast_rev]*menage_echelle$TC_RDB_reel)*as.numeric(FC[A0I])
}

menage_echelle$rev801 <- 
  menage_forme_2010$rev801*
  (1+menage_echelle$elast_prix_A13*(-1+as.numeric(FC$A13)/menage_echelle$IP_stone))*
  (1+menage_echelle$elast_rev_A13*menage_echelle$TC_RDB_reel)*as.numeric(FC$A13)







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


# Mise à l'échelle des surfaces -------------------------------------------

BUIL_H01_2_2010<-
  as.numeric(
    ThreeME %>%
      filter(Var=="BUIL_H01_2") %>%
      filter(year==2010) %>%
      select(value)
  )
POP_TOT_2010<-
  as.numeric(
    ThreeME %>%
      filter(Var=="POP_TOT") %>%
      filter(year==2010) %>%
      select(value)
  )

surfhab_hab_2010<-BUIL_H01_2_2010/POP_TOT_2010

BUIL_H01_2_horizon<-
  as.numeric(
    ThreeME %>%
      filter(Var=="BUIL_H01_2") %>%
      filter(year==horizon) %>%
      select(value)
  )



POP_TOT_horizon<-
  as.numeric(
    ThreeME %>%
      filter(Var=="POP_TOT") %>%
      filter(year==horizon) %>%
      select(value)
  )

surfhab_hab_horizon<-BUIL_H01_2_horizon/POP_TOT_horizon



ratio_surfhab<-surfhab_hab_horizon/surfhab_hab_2010


menage_echelle <- 
  menage_echelle %>%
  mutate(surfhab_d=surfhab_d*ratio_surfhab)


# Maj_dep_preeng ----------------------------------------------------------

menage_echelle <- maj_dep_preeng(bdd1= menage_forme_2010,bdd2=menage_echelle)


# Save files --------------------------------------------------------------

# save(menage_calibr_2010,file="2010/menage_calibr_2010.RData")

save(menage_echelle,file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","/Iteration_0/Output/menage_echelle_0.RData",sep=""))




# Next Step ---------------------------------------------------------------

# Introduction du changement technique
# fichier : Technical_change/TC_Renovation_DPE/1_mise_echelle_energies_detaillees.R


# SUCCESS -----------------------------------------------------------------

print("2_Mise_echelle_dep_elast : SUCCESS")



# Parts -------------------------------------------------------------------



# load("2010/menage_forme_2010.RData")
# share_2010<-compute_share(menage_forme_2010)
# share_horizon<-compute_share(menage_echelle)
# Parts=as.data.frame(rbind(share_2010,share_horizon))
# colnames(Parts)=col
# rownames(Parts)<-c(2010,horizon)
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
