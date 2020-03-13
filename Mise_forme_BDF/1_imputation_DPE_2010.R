# L'objectif de cette fonction est d'estimer une fonction de régression logistiquant permettant d'estimer les classes de DPE des ménages de BDF,le résultat se trouve dans appariement_menages_DPE, colonne DPE_pred
# Pas besoin de faire fonctionner un autre code que celui-ci, il se charger de sourcer tous les scripts R qu'il requiert. (1_1_estimateur_DPE_phebus)


# LIBRARIES  ----------------------------------------------------------------

library(tidyverse)
library(car)

setwd("D:/CIRED/Projet_Ademe")



# CHARGER STOCK DPE ---------------------------------------------------------------

#Stock de DPE en 2010 pour caler les variables agrégées => dpe_stock_2010

#LOAD
load("Donnees_brutes/Sorties ThreeMe/ThreeME.RData")

#VARIABLES
dpe_stock_2010<- ThreeME %>% 
  filter(year==2010 
         & Var %in% 
           c("BUIL_H01_CA_2" , "BUIL_H01_CB_2" , "BUIL_H01_CC_2" , 
             "BUIL_H01_CD_2" , "BUIL_H01_CE_2" , "BUIL_H01_CF_2" , "BUIL_H01_CG_2"))

#CONVERT DATAFRAME 
# (7 obs, 3 variables)
dpe_stock_2010 <- data.frame(dpe_stock_2010)

#Extraire nom de la classe de DPE
dpe_stock_2010<- dpe_stock_2010 %>% 
  mutate(DPE= str_replace_all(Var, pattern="BUIL_H01_C",replacement="")) %>% 
  mutate(DPE= str_replace_all(DPE, patter="_2",replacement=""))


save(dpe_stock_2010,file="2010/dpe_stock_2010.RData")


# DECUCN
decucn_2010<-read_excel("Donnees_brutes/Econometrie_demande/decucn_2010.xlsx")
decucn_2010 <- 
  decucn_2010 %>% 
  mutate(ident_men=as.integer(ident_men))


# ESTIMATEUR DPE SUR DONNEES SOCIO-ECO ----------------------------------------------------------

# Load estm_dpe_acp, 
# list 27 paramètres
source("Mise_forme_BDF/1_1_estimateur_dpe_phebus.R")


# LOAD DONNEES BDF -------------------------------------------------------------

# Base INSEE menage
menage<-
  read.csv("Donnees_brutes/BDF_2010/menage.csv",
           header=TRUE,
           sep=";",
           dec=".",
           stringsAsFactors = FALSE)

depmen<-
  read.csv("Donnees_brutes/BDF_2010/depmen.csv",
           header=TRUE,
           sep=";",
           dec=".",
           stringsAsFactors = FALSE)

# load menage_calibr_2010 avec ménages pré_selectionnés
load("2010/menage_calibr_2010.RData")





# CREER BASE DE DONNEES --------------------------------------------------

#appariement entre la base ménage (variables explicatives) et la nouvelle estimation de DPE 
appariement_menages_DPE<-
  menage %>% 
  select(ident_men,typlog,tuu)


pond<-
  menage_calibr_2010 %>% 
  select(ident_men,pondmen)

rm(menage_calibr_2010)

depmen <- 
  depmen %>% 
  select(ident_men, stalog,sourcp,ancons,surfhab_d)

appariement_menages_DPE<- 
  pond %>% 
  left_join(.,appariement_menages_DPE, by="ident_men") %>% 
  left_join(.,depmen, by="ident_men")%>%
  left_join(decucn_2010,by="ident_men")

#pour équivalence variables Phébus/BDF se référer au document "2018-09-26 Mapping données Phébus x BDF.odt"
# BATI_PERIODE + ESTOC + Revenu_Insee_quintile + EHST + RP_TAILLE_UU + is_elec + MI





# RECODAGE DES VARIABLES --------------------------------------------------

# recodage des variables pour correspondante entre BDF et Phébus

## ANCONS
appariement_menages_DPE$BATI_PERIODE<- car::recode(appariement_menages_DPE$ancons, "1 = 1 ; 2:3=2 ; 4:6=3 ; 7:8=4 ; 9:10=5")


## STALOG
appariement_menages_DPE$ESTOC<- car::recode(appariement_menages_DPE$stalog, "1:2 = 1 ; 3=2 ; 4:5=3 ; 6=4")

## DECU1
appariement_menages_DPE <- appariement_menages_DPE%>%rename(replace=c("decucn"="Revenu_Insee_quintile"))


## Surfhab_d
appariement_menages_DPE$EHST <- appariement_menages_DPE$surfhab_d
appariement_menages_DPE[which(appariement_menages_DPE$EHST==999),"EHST"]<-0
appariement_menages_DPE[which(is.na(appariement_menages_DPE$EHST)),"EHST"]<-0

## MI_corr 
appariement_menages_DPE$MI <- car::recode(appariement_menages_DPE$typlog,"1:2=1 ; 3:6=0")


## TUU (menage)
appariement_menages_DPE$RP_TAILLE_UU <- as.numeric(appariement_menages_DPE$tuu)


## sourcp
appariement_menages_DPE$is_elec <- appariement_menages_DPE$sourcp
appariement_menages_DPE[which(appariement_menages_DPE$is_elec>1),"is_elec"]<-0
appariement_menages_DPE[which(is.na(appariement_menages_DPE$is_elec)),"is_elec"]<-0

# select col
appariement_menages_DPE<-appariement_menages_DPE %>% select(ident_men, pondmen, BATI_PERIODE,ESTOC,Revenu_Insee_quintile,EHST,MI, RP_TAILLE_UU,is_elec)


# Estimation --------------------------------------------------------------

pred <- predict(
  estm_dpe_acp,
  appariement_menages_DPE,
  type="probs",
  na.pass=TRUE)


colnames(pred)<-c("A","B","C","D","E","F","G")

appariement_menages_DPE[c("A","B","C","D","E","F","G")]<-pred
appariement_menages_DPE$DPE_pred<-19
# table(is.na(appariement_menages_DPE))



# STOCKS M2 ---------------------------------------------------------------

# Mise à l'échelle des stocks de m2
stock_m2_bdf <- appariement_menages_DPE %>% summarise(sum(EHST*pondmen))
stock_m2_threeME<- dpe_stock_2010 %>% summarise(sum(value))
dpe_stock_2010_3ME<- dpe_stock_2010 %>% mutate(value=value*as.numeric(stock_m2_bdf/stock_m2_threeME))

#verification, ratio=1
stock_m2_threeME2<- dpe_stock_2010_3ME %>% summarise(sum(value))
stock_m2_bdf/stock_m2_threeME2


# ATTRIBUTION DPE ---------------------------------------------------------

for (i in LETTERS[1:7]){
  count=0
  while(count<dpe_stock_2010_3ME %>% filter(DPE==i) %>% select(value)){
    indice=which.max(appariement_menages_DPE[,i])
    appariement_menages_DPE$DPE_pred[indice]<-i
    appariement_menages_DPE[indice,
                            LETTERS[1:7]]<- -1
    count=count+appariement_menages_DPE$EHST[indice]*appariement_menages_DPE$pondmen[indice]
  }
}


# Distributions des DPE

list_dpe_pred<- 
  appariement_menages_DPE %>% 
  mutate(surf_pond=EHST*pondmen) %>% 
  group_by(DPE_pred) %>% 
  summarise_at("surf_pond",sum)


dat=data.frame(
  "cat_DPE"=rep(c("A" , "B" , "C" , "D" , "E" , "F" , "G"),2),
  
  "DPE"=c(dpe_stock_2010$value,
          list_dpe_pred$surf_pond),
  
  "statut"=c(rep("DPE_réel",7),
             rep("DPE_pred",7))
)


print(ggplot(dat,aes(fill=statut,x=cat_DPE,y=DPE))+geom_bar(stat="identity",position="dodge")+ggtitle("Multinomial logit"))




# SAVE DPE ----------------------------------------------------------------
menage_DPE<-appariement_menages_DPE %>% select(ident_men,DPE_pred)

save(menage_DPE,file="2010/menage_DPE.RData")

# SUCCESS -----------------------------------------------------------------

print("1_3_imputation_DPE_2010 : SUCCESS")
