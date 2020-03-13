setwd("D:/CIRED/Projet_Ademe")
library(tidyverse)


load("2010/menage_calibr_2010.RData")
menage<-menage_calibr_2010
load("2010/depmen.RData")
menage$c13711[is.na(menage$c13711)]<-0
menage$rev801[is.na(menage$rev801)]<-0
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
           "loyers")

menage<-
  menage %>%
  # left_join(c05_forme_2010%>%select(c13711,ident_men),by="ident_men")%>%
  left_join(depmen %>% select(ident_men, anacq,ancons),by="ident_men")

# menage$c13711[is.na(menage$c13711)]<-0
menage$anacq[is.na(menage$anacq)]<-0
menage$ancons[is.na(menage$ancons)]<-0

menage <-
  menage %>%
  mutate(c13711_neuf=0)%>%
  mutate_when(ancons>=9& c13711>0,list(c13711_neuf=c13711))
# on somme à 7 milliards

menage<- menage %>% 
  mutate(dep_autres_energies=dep_Fuel+dep_GPL+dep_Solides+dep_Urbain)%>%
  mutate(BTP=BTP+c13711_neuf)

menage$loyers<-menage$loyers+menage$rev801
menage$Rcons_bis <- rowSums(menage[list_dep])


sum(rowSums(menage[list_dep])*menage$pondmen)
sum_tot<-menage%>%summarise(sum(Rcons_bis*pondmen))
sum_tot




save(sum_tot,file="2010/Rcons_bis_tot_2010.RData")
