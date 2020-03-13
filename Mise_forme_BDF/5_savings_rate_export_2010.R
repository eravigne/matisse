
# LIBRARY -----------------------------------------------------------------
library(tidyverse)

# DATA -------------------------------------------------------------------
load("2010/menage_calibr_2010.RData")
# load("2010/c05_2010.RData")
load("2010/depmen.RData")
source("D:/CIRED/Projet_Ademe/Code_global_ADEME/mutate_when.R")
source("D:/CIRED/Projet_Ademe/Code_global_ADEME/compute_share_export.R")
# CALC --------------------------------------------------------------------

menage<-menage_calibr_2010


menage$AID[is.na(menage$AID)]<-0
menage$impot_revenu[is.na(menage$impot_revenu)]<-0  
menage$rev_TCO<-0

menage$RDB_new <- rowSums(menage[c("rev_activites","rev_patrimoine","rev_sociaux","rev_TCO")])-rowSums(menage[c("impot_revenu","AID")])


menage<-
  menage %>%
  # left_join(c05_forme_2010%>%select(c13711,ident_men),by="ident_men")%>%
  left_join(depmen %>% select(ident_men, anacq,ancons),by="ident_men")

# menage$c13711[is.na(menage$c13711)]<-0
menage$anacq[is.na(menage$anacq)]<-0
menage$ancons[is.na(menage$ancons)]<-0


# Ancons est une variable catégorielle. On considère le niveau 9 : logement construit après 2004. L'hypothèse sous-jacente est que ne l'on vend pas un logemement moins de 6 ans après l'avoir construit. 
menage <-
  menage %>%
  mutate(c13711_neuf=0)%>%
  mutate_when(ancons>=9& c13711>0,list(c13711_neuf=c13711))
# > table(menage$c13711>80000)
# 
# FALSE  TRUE 
# 10173   116 
# > table(menage$c13711_neuf>0)
# 32511372 m2
# 

# ancons>=9& c13711>0
# FALSE  TRUE 
# 10281     8 
# Représentent 2377663 m2
# 10342067,12 dans 3ME 





menage$Rcons_new<-rowSums(menage[c("agriculture",
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
                                   "c13711_neuf")])

menage$taux_epargne_bis<-(menage$RDB_new-menage$Rcons_new)/menage$RDB_new

epargne_2010<-sum((menage$RDB_new-menage$Rcons_new)*menage$pondmen,na.rm = T)/sum(menage$RDB_new*menage$pondmen,na.rm=T)
# 
# > epargne_2010
# [1] 0.1424462

# Et si jamais je prends tous les c13711 je tombe à [1] 0.05931124





# verif -------------------------------------------------------------------

R<-sum(menage$RDB_new*menage$pondmen,na.rm=T)
D<-sum(menage$Rcons_new*menage$pondmen,na.rm=T)
(R-D)/R

