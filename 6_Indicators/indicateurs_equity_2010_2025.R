# 
# # LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(stargazer)
library(matrixStats)
library(ineq)
# library(gglorenz)
library(readxl)
library(ggthemes)
library(ggsci)
library(data.table)

library(FactoMineR)
library(ggplot2)
# library(dplyr)
library(tidyverse)
library("corrplot")
library(nnet)
library(reshape)
library(scales)
library(plyr)
library(cowplot)
library(stringr)
library(car)
detach(package:plyr)    
library(dplyr)
# 
# 
# # DATA --------------------------------------------------------------------
# setwd("D:/CIRED/Projet_Ademe")
load("D:/CIRED/Projet_Ademe/2010/menage_calibr_2010.RData")
# Iter=1
# scenario="AMS"
# scenario_classement="Optimiste"
# redistribution="niveau_vie"
# horizon=2035
# load(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
# load("D:/CIRED/Projet_Ademe/2010/c05_2010.RData")
source("D:/CIRED/Projet_Ademe/Code_global_Ademe/mutate_when.R")
# 
# bdd1<-menage_calibr_2010
# bdd2<-menage_echelle

# Plot Atkinson INCOME -----------------------------------------------------------

plot_atkinson_income<-function(bdd1,bdd2,f){
T_bis_1=f(bdd1)
T_bis_2=f(bdd2)

dat1<-T_bis_1 %>% 
  mutate(decuc=as.character(c(seq(1:10),"all"))) %>% 
  mutate(decuc=factor(decuc, levels=unique(decuc))) %>%
  gather(key = epsilon,value=atkinson,-1)

dat2 <- T_bis_2 %>% 
  mutate(decuc=as.character(c(seq(1:10),"all"))) %>% 
  mutate(decuc=factor(decuc, levels=unique(decuc))) %>%
  gather(key = epsilon,value=atkinson,-1)

dat<-rbind(dat1,dat2)
dat<- dat %>% mutate(year=c(rep(2010,66),rep(2025,66)))

g_inc <- 
  ggplot(dat,aes(x=decuc,y=atkinson,color=epsilon))+geom_point(size=2) + facet_grid(~year)
print(g_inc)
}

# #TEST
# plot_atkinson_income(bdd1,bdd2,atkison_decile_income)
# plot_atkinson_income(bdd1,bdd2,atkison_typo_income)


plot_atkinson_income_2<-function(bdd1,bdd2,f){
  T_bis_1=f(bdd1)
  T_bis_2=f(bdd2)
  
  dat1<-T_bis_1 %>% 
    mutate(decuc=as.character(c(seq(1:10),"all"))) %>% 
    mutate(decuc=factor(decuc, levels=unique(decuc))) %>%
    gather(key = epsilon,value=atkinson,-1)
  
  dat2 <- T_bis_2 %>% 
    mutate(decuc=as.character(c(seq(1:10),"all"))) %>% 
    mutate(decuc=factor(decuc, levels=unique(decuc))) %>%
    gather(key = epsilon,value=atkinson,-1)
  
  dat<-rbind(dat1,dat2)
  dat<- dat %>% mutate(year=c(rep(2010,66),rep(2025,66)))
  
  g_inc <- 
    ggplot(dat,aes(x=as.factor(year),y=atkinson,color=epsilon))+geom_point(size=2) + facet_grid(~decuc)
  print(g_inc)
}

# Plot Atkinson TYPO -----------------------------------------------------------

plot_atkinson_typo<-function(bdd1,bdd2,f){
  T_bis_1=f(bdd1)
  T_bis_2=f(bdd2)
  
    dat1<-T_bis_1 %>% 
      mutate(typo=as.character(c(seq(1:4),"all"))) %>% 
      mutate(typo=factor(typo, levels=unique(typo))) %>%
      gather(key = epsilon,value=atkinson,-1)
    
    dat2 <- T_bis_2 %>% 
      mutate(typo=as.character(c(seq(1:4),"all"))) %>% 
      mutate(typo=factor(typo, levels=unique(typo))) %>%
      gather(key = epsilon,value=atkinson,-1)
    
    dat<-rbind(dat1,dat2)
    dat<- dat %>% mutate(year=c(rep(2010,30),rep(2025,30)))
    
    g_inc <- 
      ggplot(dat,aes(x=typo,y=atkinson,color=epsilon))+geom_point(size=2) + facet_grid(~year)
    print(g_inc)
  }

plot_atkinson_typo_2<-function(bdd1,bdd2,f){
  T_bis_1=f(bdd1)
  T_bis_2=f(bdd2)
  
  dat1<-T_bis_1 %>% 
    mutate(typo=as.character(c(seq(1:4),"all"))) %>% 
    mutate(typo=factor(typo, levels=unique(typo))) %>%
    gather(key = epsilon,value=atkinson,-1)
  
  dat2 <- T_bis_2 %>% 
    mutate(typo=as.character(c(seq(1:4),"all"))) %>% 
    mutate(typo=factor(typo, levels=unique(typo))) %>%
    gather(key = epsilon,value=atkinson,-1)
  
  dat<-rbind(dat1,dat2)
  dat<- dat %>% mutate(year=c(rep(2010,30),rep(2025,30)))
  
  g_inc <- 
    ggplot(dat,aes(x=as.factor(year),y=atkinson,color=epsilon))+geom_point(size=2) + facet_grid(~typo)
  print(g_inc)
}
# ATKINSON INDEX decile Income ----------------------------------------------------------

atkison_decile_income<- function(bdd){
    epsi=c(0.1,0.5,1.5,2,2.5)
    
    # E=1
    T<-
      bdd %>%
      filter(RDB>0) %>%
      group_by(decucn) %>%
      summarise(
        1-exp(sum(log(RDB/weighted.mean(RDB,pondmen))*pondmen/sum(pondmen))))
    
    B<-bdd %>%
      filter(RDB>0) %>%
      summarise(
        1-exp(sum(log(RDB/weighted.mean(RDB,pondmen))*pondmen/sum(pondmen))))
    
    T<-T[,2]
    T<-rbind(T,B)
    
    #autres epsilon
    for (E in epsi){
      A<- bdd %>% filter(RDB>0)%>% group_by(decucn) %>% summarise(
        1-sum(
          (pondmen/sum(pondmen))*
            (
              (RDB/weighted.mean(RDB,pondmen))^(1-E))
        )^(1/(1-E))
      )
      B<-bdd %>% filter(RDB>0)%>% summarise(
        1-sum(
          (pondmen/sum(pondmen))*
            (
              (RDB/weighted.mean(RDB,pondmen))^(1-E))
        )^(1/(1-E))
      )
      
      A<-rbind(A[,2],B)
      T=c(T,A)
    }
    
    #Preparation Data.frame
    T_bis<-as.data.frame(T)
    # T_bis
    colnames(T_bis)<-c(1,0.1,0.5,1.5,2,2.5)
    T_bis<-T_bis %>% mutate(decuc=c(paste("decile",seq(1:10),sep="_"),"all"))
    T_bis<-T_bis[,c(7,2,3,1,4,5,6)]
    # T_bis
    # View(T_bis)
    
    # Resultats
    # stargazer(T_bis,summary=F)

    # print(T_bis)
    return(T_bis)
  }


#Test
# atkison_decile_income(bdd1)



# Energie Atksion decile -----------------------------------------------------------------

atkison_decile_energie<- function(bdd){
  epsi=c(0.1,0.5,1.5,2,2.5)
  
  # E=1
  T<-
    bdd %>%
    filter(dep_energie_logement>0) %>%
    group_by(decucn) %>%
    summarise(
      1-exp(sum(log(dep_energie_logement/weighted.mean(dep_energie_logement,pondmen))*pondmen/sum(pondmen))))
  B<-bdd %>%
    filter(dep_energie_logement>0) %>%
    summarise(
      1-exp(sum(log(dep_energie_logement/weighted.mean(dep_energie_logement,pondmen))*pondmen/sum(pondmen))))
  
  T<-T[,2]
  T<-rbind(T,B)
  
  #autres epsilon
  for (E in epsi){
    A<- bdd %>% filter(dep_energie_logement>0)%>% group_by(decucn) %>% summarise(
      1-sum(
        (pondmen/sum(pondmen))*
          (
            (dep_energie_logement/weighted.mean(dep_energie_logement,pondmen))^(1-E))
      )^(1/(1-E))
    )
    B<-bdd %>% filter(dep_energie_logement>0)%>% summarise(
      1-sum(
        (pondmen/sum(pondmen))*
          (
            (dep_energie_logement/weighted.mean(dep_energie_logement,pondmen))^(1-E))
      )^(1/(1-E))
    )
    
    A<-rbind(A[,2],B)
    T=c(T,A)
  }
  
  #Preparation Data.frame
  T_bis<-as.data.frame(T)
  # T_bis
  colnames(T_bis)<-c(1,0.1,0.5,1.5,2,2.5)
  T_bis<-T_bis %>% mutate(decuc=c(paste("decile",seq(1:10),sep="_"),"all"))
  T_bis<-T_bis[,c(7,2,3,1,4,5,6)]
  # T_bis
  # View(T_bis)
  
  # Resultats
  # stargazer(T_bis,summary=F)
  
  return(T_bis)
}





# Fuel Atkinson decile ------------------------------------------------------------
atkison_decile_fuel<- function(bdd){
  epsi=c(0.1,0.5,1.5,2,2.5)
  
  # E=1
  T<-
    bdd %>%
    filter(carb_lubr>0) %>%
    group_by(decucn) %>%
    summarise(
      1-exp(sum(log(carb_lubr/weighted.mean(carb_lubr,pondmen))*pondmen/sum(pondmen))))
  B<-bdd %>%
    filter(carb_lubr>0) %>%
    summarise(
      1-exp(sum(log(carb_lubr/weighted.mean(carb_lubr,pondmen))*pondmen/sum(pondmen))))
  
  T<-T[,2]
  T<-rbind(T,B)
  
  
  #autres epsilon
  for (E in epsi){
    A<- bdd %>% filter(carb_lubr>0)%>% group_by(decucn) %>% summarise(
      1-sum(
        (pondmen/sum(pondmen))*
          (
            (carb_lubr/weighted.mean(carb_lubr,pondmen))^(1-E))
      )^(1/(1-E))
    )
    B<-bdd %>% filter(carb_lubr>0)%>% summarise(
      1-sum(
        (pondmen/sum(pondmen))*
          (
            (carb_lubr/weighted.mean(carb_lubr,pondmen))^(1-E))
      )^(1/(1-E))
    )
    
    A<-rbind(A[,2],B)
    T=c(T,A)
  }
  
  #Preparation Data.frame
  T_bis<-as.data.frame(T)
  # T_bis
  colnames(T_bis)<-c(1,0.1,0.5,1.5,2,2.5)
  T_bis<-T_bis %>% mutate(decuc=c(paste("decile",seq(1:10),sep="_"),"all"))
  T_bis<-T_bis[,c(7,2,3,1,4,5,6)]
  # T_bis
  # View(T_bis)
  
  # Resultats
  # stargazer(T_bis,summary=F)
  
  return(T_bis)
}








# ATKINSON INDEX income typo ----------------------------------------------------------
atkison_typo_income<- function(bdd){
  epsi=c(0.1,0.5,1.5,2,2.5)
  
  # E=1
  T<-
    bdd %>%
    filter(RDB>0) %>%
    group_by(typo2010f) %>%
    summarise(
      1-exp(sum(log(RDB/weighted.mean(RDB,pondmen))*pondmen/sum(pondmen))))
  B<-bdd %>%
    filter(RDB>0) %>%
    summarise(
      1-exp(sum(log(RDB/weighted.mean(RDB,pondmen))*pondmen/sum(pondmen))))
  
  T<-T[,2]
  T<-rbind(T,B)
  
  #autres epsilon
  for (E in epsi){
    A<- bdd %>% filter(RDB>0)%>% group_by(typo2010f) %>% summarise(
      1-sum(
        (pondmen/sum(pondmen))*
          (
            (RDB/weighted.mean(RDB,pondmen))^(1-E))
      )^(1/(1-E))
    )
    B<-bdd %>% filter(RDB>0)%>% summarise(
      1-sum(
        (pondmen/sum(pondmen))*
          (
            (RDB/weighted.mean(RDB,pondmen))^(1-E))
      )^(1/(1-E))
    )
    
    A<-rbind(A[,2],B)
    T=c(T,A)
  }
  
  #Preparation Data.frame
  T_bis<-as.data.frame(T)
  # T_bis
  colnames(T_bis)<-c(1,0.1,0.5,1.5,2,2.5)
  T_bis<-T_bis %>% mutate(typo=c(paste("typo",seq(1:4),sep="_"),"all"))
  T_bis<-T_bis[,c(7,2,3,1,4,5,6)]
  # T_bis
  # View(T_bis)
  
  # Resultats
  # stargazer(T_bis,summary=F)
  # assign(paste("U_bis",i,sep="_"),T_bis)
  return(T_bis)
}






# Energie Atksion typo -----------------------------------------------------------------

atkison_typo_energie<- function(bdd){

  epsi=c(0.1,0.5,1.5,2,2.5)
  
  # E=1
  T<-
    bdd %>%
    filter(dep_energie_logement>0) %>%
    group_by(typo2010f) %>%
    summarise(
      1-exp(sum(log(dep_energie_logement/weighted.mean(dep_energie_logement,pondmen))*pondmen/sum(pondmen))))
  B<-bdd %>%
    filter(dep_energie_logement>0) %>%
    summarise(
      1-exp(sum(log(dep_energie_logement/weighted.mean(dep_energie_logement,pondmen))*pondmen/sum(pondmen))))
  
  T<-T[,2]
  T<-rbind(T,B)
  
  #autres epsilon
  for (E in epsi){
    A<- bdd %>% filter(dep_energie_logement>0)%>% group_by(typo2010f) %>% summarise(
      1-sum(
        (pondmen/sum(pondmen))*
          (
            (dep_energie_logement/weighted.mean(dep_energie_logement,pondmen))^(1-E))
      )^(1/(1-E))
    )
    B<-bdd %>% filter(dep_energie_logement>0)%>% summarise(
      1-sum(
        (pondmen/sum(pondmen))*
          (
            (dep_energie_logement/weighted.mean(dep_energie_logement,pondmen))^(1-E))
      )^(1/(1-E))
    )
    
    A<-rbind(A[,2],B)
    T=c(T,A)
  }
  
  #Preparation Data.frame
  T_bis<-as.data.frame(T)
  # T_bis
  colnames(T_bis)<-c(1,0.1,0.5,1.5,2,2.5)
  T_bis<-T_bis %>% mutate(typo=c(paste("typo",seq(1:4),sep="_"),"all"))
  T_bis<-T_bis[,c(7,2,3,1,4,5,6)]
  # T_bis
  # View(T_bis)
  
  # Resultats
  # stargazer(T_bis,summary=F)
  # assign(paste("D_bis",i,sep="_"),T_bis)
  return(T_bis)
}





# Fuel Atkinson typo ------------------------------------------------------------
atkison_typo_fuel<- function(bdd){
  epsi=c(0.1,0.5,1.5,2,2.5)
  
  # E=1
  T<-
    bdd %>%
    filter(carb_lubr>0) %>%
    group_by(typo2010f) %>%
    summarise(
      1-exp(sum(log(carb_lubr/weighted.mean(carb_lubr,pondmen))*pondmen/sum(pondmen))))
  B<-bdd %>%
    filter(carb_lubr>0) %>%
    summarise(
      1-exp(sum(log(carb_lubr/weighted.mean(carb_lubr,pondmen))*pondmen/sum(pondmen))))
  
  T<-T[,2]
  T<-rbind(T,B)
  
  
  #autres epsilon
  for (E in epsi){
    A<- bdd %>% filter(carb_lubr>0)%>% group_by(typo2010f) %>% summarise(
      1-sum(
        (pondmen/sum(pondmen))*
          (
            (carb_lubr/weighted.mean(carb_lubr,pondmen))^(1-E))
      )^(1/(1-E))
    )
    B<-bdd %>% filter(carb_lubr>0)%>% summarise(
      1-sum(
        (pondmen/sum(pondmen))*
          (
            (carb_lubr/weighted.mean(carb_lubr,pondmen))^(1-E))
      )^(1/(1-E))
    )
    
    A<-rbind(A[,2],B)
    T=c(T,A)
  }
  
  #Preparation Data.frame
  T_bis<-as.data.frame(T)
  # T_bis
  colnames(T_bis)<-c(1,0.1,0.5,1.5,2,2.5)
  T_bis<-T_bis %>% mutate(typo=c(paste("typo",seq(1:4),sep="_"),"all"))
  T_bis<-T_bis[,c(7,2,3,1,4,5,6)]
  # T_bis
  # View(T_bis)
  
  # Resultats
  # stargazer(T_bis,summary=F)
  # assign(paste("G_bis",i,sep="_"),T_bis)
  return(T_bis)
}




# Ratio Indicator Fuel poverty  --------------------------------------------------------------------

#Fuel energy + carb
ratio_fuel_poverty<- function(bdd){
R <- bdd %>%
  filter(((carb_lubr+dep_Fuel)/RDB)>0.1) %>%
  summarise(sum(pondmen))
R<-as.numeric(R)/sum(bdd$pondmen)*100
return(as.numeric(R))
}

#Carb
ratio_fuel_poverty_bis<- function(bdd){
  R <- bdd %>%
    filter(((carb_lubr)/RDB)>0.1) %>%
    summarise(sum(pondmen))
  R<-as.numeric(R)/sum(bdd$pondmen)*100
  return(as.numeric(R))
}




# Pauvreté monétaire ------------------------------------------------------
pauvrete<- function(bdd){
  
  # bdd$rev_tot<-rowSums(bdd[c("rev_activites","rev_sociaux","rev_patrimoine","rev700","rev701","rev999","rev_TCO")])
  # bdd$rev_totUC<-bdd$rev_tot/bdd$coeffuc
  bdd$RDB_UC<-bdd$RDB/bdd$coeffuc
  
  wgm<-D100(bdd,50)
  seuil<-0.6*wgm
  
  
  R <- bdd %>%
    filter(RDB_UC<=seuil) %>%
    summarise(sum(pondmen))
  R<-as.numeric(R)/sum(bdd$pondmen)*100
  return(as.numeric(R))
}




# Nombre de pauvres --------------------------------------------------------

nombre_pauvre<-function(bdd,var){
  # bdd$rev_tot<-rowSums(bdd[c("rev_activites","rev_sociaux","rev_patrimoine","rev700","rev701","rev999","rev_TCO")])
  # bdd<-bdd %>% mutate(TEE_log=dep_energie_logement/rev_tot)
  # 
  R <- bdd %>%
    group_by(get(var))%>%
    filter(decucn<4)%>%
    # filter(((dep_energie_logement)/rev_tot)>0.08) %>%
    summarise(sum(pondmen))
  # R<-lapply(as.numeric(R)/as.numeric(bdd%>%group_by(get(var))%>%summarise(sum(pondmen))),function(x)x*100)
  R1<-R
  if(var=="decucn"){
    suppressWarnings(R1<-rbindlist(list(R,data.frame(cbind(c(4,5,6,7,8,9,10),rep(0,7)))),fill=F))}
  R3<-sapply(R1,function(x) as.numeric(x))
  R2<-R3/sapply(bdd%>%group_by(get(var))%>%summarise(sum(pondmen)),function(x) as.numeric(x))*100
  R<-cbind(R1[,1],R2[,2])
  
  R<-as.data.frame(R)
  
  
  return(R)
}


# Precarité ---------------------------------------------------------------

# precarite_ener<- function(bdd){
#   R <- bdd %>% 
#     filter(((dep_energie_logement)/RDB)>0.1) %>%
#     summarise(sum(pondmen))
#   R<-R/sum(bdd$pondmen)*100
#   return(R[,2])
# }
# 
# precarite_ener_var<- function(bdd,var){
#   if(!var %in% colnames(bdd)){
#     bdd<-bdd%>%left_join(appmen_depensesactiv_2010%>%select(ident_men, get(var)),by="ident_men")
#   }
#   R <- bdd %>% group_by(get(var))%>%
#     filter(((dep_energie_logement)/RDB)>0.1) %>%
#     summarise(sum(pondmen))
#   R<-R/sum(bdd$pondmen)*100
#   return(R[,2])
# }

# Précarité Ener Logement -------------------------------------------------

precarite_log<- function(bdd){
  
  bdd$rev_tot<-rowSums(bdd[c("rev_activites","rev_sociaux","rev_patrimoine","rev700","rev701","rev999","rev_TCO")])
  bdd<-bdd %>% mutate(TEE_log=dep_energie_logement/rev_tot)


  R <- bdd %>%
    filter(decucn<4)%>%
    filter(TEE_log>0.08) %>%
    summarise(sum(pondmen))
  R<-as.numeric(R)/sum(bdd$pondmen)*100
  return(as.numeric(R))
}


precarite_log_var_2010<-function(var){
  bdd$rev_tot<-rowSums(bdd[c("rev_activites","rev_sociaux","rev_patrimoine","rev700","rev701","rev999")])
  bdd<-bdd %>% mutate(TEE_log=dep_energie_logement/rev_tot)
  
  R <- bdd %>%
    group_by(get(var))%>%
    filter(decucn<4)%>%
    filter(((dep_energie_logement)/rev_tot)>0.08) %>%
    summarise(sum(pondmen))
  # R<-lapply(as.numeric(R)/as.numeric(bdd%>%group_by(get(var))%>%summarise(sum(pondmen))),function(x)x*100)
  R1<-R
  if(var=="decucn"){
    suppressWarnings(R1<-rbindlist(list(R,data.frame(cbind(c(4,5,6,7,8,9,10),rep(0,7)))),fill=F))}
  R3<-sapply(R1,function(x) as.numeric(x))
  R2<-R3/sapply(bdd%>%group_by(get(var))%>%summarise(sum(pondmen)),function(x) as.numeric(x))*100
  R<-cbind(R1[,1],R2[,2])
  
  R<-as.data.frame(R)
  
  
  return(R)
}





precarite_log_var<-function(bdd,var){
  bdd$rev_tot<-rowSums(bdd[c("rev_activites","rev_sociaux","rev_patrimoine","rev700","rev701","rev999","rev_TCO")])
  bdd<-bdd %>% mutate(TEE_log=dep_energie_logement/rev_tot)
  
  R <- bdd %>%
    group_by(get(var))%>%
    filter(decucn<4)%>%
    filter(((dep_energie_logement)/rev_tot)>0.08) %>%
    summarise(sum(pondmen))
  # R<-lapply(as.numeric(R)/as.numeric(bdd%>%group_by(get(var))%>%summarise(sum(pondmen))),function(x)x*100)
  R1<-R
  if(var=="decucn"){
    suppressWarnings(R1<-rbindlist(list(R,data.frame(cbind(c(4,5,6,7,8,9,10),rep(0,7)))),fill=F))}
  R3<-sapply(R1,function(x) as.numeric(x))
  R2<-R3/sapply(bdd%>%group_by(get(var))%>%summarise(sum(pondmen)),function(x) as.numeric(x))*100
  R<-cbind(R1[,1],R2[,2])
  
    R<-as.data.frame(R)
  
  
  return(R)
}

precarite_log_var_relatif<-function(bdd,var){
  bdd$rev_tot<-rowSums(bdd[c("rev_activites","rev_sociaux","rev_patrimoine","rev700","rev701","rev999","rev_TCO")])
  bdd<-bdd %>% mutate(TEE_log=dep_energie_logement/rev_tot)
  
  
  R_pauvres <- bdd %>%
    group_by(get(var))%>%
    filter(decucn<4)%>%
    summarise(sum(pondmen))
  
  R <- bdd %>%
    group_by(get(var))%>%
    filter(decucn<4)%>%
    filter(((dep_energie_logement)/rev_tot)>0.08) %>%
    summarise(sum(pondmen))
  # R<-lapply(as.numeric(R)/as.numeric(bdd%>%group_by(get(var))%>%summarise(sum(pondmen))),function(x)x*100)
  R[,2]<-R[,2]/R_pauvres[,2]
  R<-as.data.frame(R)
  
  # R1<-R
  if(var=="decucn"){
    suppressWarnings(R<-rbindlist(list(R,data.frame(cbind(c(4,5,6,7,8,9,10),rep(0,7)))),fill=F))}
  # R3<-sapply(R1,function(x) as.numeric(x))
  # R2<-R3/sapply(bdd%>%group_by(get(var))%>%summarise(sum(pondmen)),function(x) as.numeric(x))*100
  # R<-cbind(R1[,1],R2[,2])
  
  R<-as.data.frame(R)
  
  
  return(R)
}



# Précarité Energétique Mobilité---------------------------------------------------
precarite_mob<- function(bdd){
  
  bdd$rev_tot<-rowSums(bdd[c("rev_activites","rev_sociaux","rev_patrimoine","rev700","rev701","rev999","rev_TCO")])
  bdd<-bdd %>% mutate(TEE_mob=carb_lubr/rev_tot)
  bdd_mob<-bdd %>% filter(carb_lubr>0)
  
  med_mob<-bdd_mob%>%summarise(weightedMedian(x=TEE_mob,w=pondmen))
  seuil_mob<-1.5*med_mob
  
  R <- bdd %>%
    filter(decucn<4)%>%
    filter(TEE_mob>as.numeric(seuil_mob)) %>%
    summarise(sum(pondmen))
  R<-as.numeric(R)/sum(bdd$pondmen)*100
  return(as.numeric(R))
}


seul_mob<-function(bdd){
  bdd$rev_tot<-rowSums(bdd[c("rev_activites","rev_sociaux","rev_patrimoine","rev700","rev701","rev999","rev_TCO")])
  
  bdd<-bdd %>% mutate(TEE_mob=carb_lubr/rev_tot)
  
  med_mob<-bdd%>%filter(TEE_mob>0)%>%summarise(weightedMedian(x=TEE_mob,w=pondmen,na.rm=T))
  seuil_mob<-1.5*med_mob
  return(seuil_mob)
}


precarite_mob_var<-function(bdd,var){
  
  bdd$rev_tot<-rowSums(bdd[c("rev_activites","rev_sociaux","rev_patrimoine","rev700","rev701","rev999","rev_TCO")])

  bdd<-bdd %>% mutate(TEE_mob=carb_lubr/rev_tot)

  med_mob<-bdd%>%filter(TEE_mob>0)%>%summarise(weightedMedian(x=TEE_mob,w=pondmen,na.rm=T))
  seuil_mob<-1.5*med_mob
  # print(seuil_mob)
    
  R <- bdd %>%
    group_by(get(var))%>%
    filter(decucn<4)%>%
    filter(((carb_lubr)/rev_tot)>seuil_mob) %>%
    summarise(sum(pondmen))
  # R<-lapply(as.numeric(R)/as.numeric(bdd%>%group_by(get(var))%>%summarise(sum(pondmen))),function(x)x*100)
  R1<-R
  if(var=="decucn"){
    suppressWarnings(R1<-rbindlist(list(R,data.frame(cbind(c(4,5,6,7,8,9,10),rep(0,7)))),fill=F))}
  R3<-sapply(R1,function(x) as.numeric(x))
  R2<-R3/sapply(bdd%>%group_by(get(var))%>%summarise(sum(pondmen)),function(x) as.numeric(x))*100
  R<-cbind(R1[,1],R2[,2])
  
  R<-as.data.frame(R)
  
  
  # 
  # bdd$rev_tot<-rowSums(bdd[c("rev_activites","rev_sociaux","rev_patrimoine","rev700","rev701","rev999","rev_TCO")])
  # bdd<-bdd %>% mutate(TEE_mob=carb_lubr/rev_tot)
  # bdd_mob<-bdd %>% filter(carb_lubr>0)
  # 
  # med_mob<-bdd_mob%>%summarise(weightedMedian(x=TEE_mob,w=pondmen))
  # seuil_mob<-1.5*med_mob
  # 
  # 
  # 
  # R1<-R
  # if(var=="decucn"){
  #   suppressWarnings(R1<-rbindlist(list(R,data.frame(cbind(c(4,5,6,7,8,9,10),rep(0,7)))),fill=F))}
  # R3<-sapply(R1,function(x) as.numeric(x))
  # R2<-R3/sapply(bdd%>%group_by(get(var))%>%summarise(sum(pondmen)),function(x) as.numeric(x))*100
  # R<-cbind(R1[,1],R2[,2])
  # 
  
  return(R)
}


# Low Income High Costs (LIHC) --------------------------------------------

LIHC<- function(bdd,FC){
# the median energy cost, is set by ranking households equivalised (by UC)  energy requirements and using the median value of the dataset

bdd <- bdd %>% mutate_when(is.na(dep_energie_logement),list(dep_energie_logement=0,pondmen=0))
med_energy<-weightedMedian(x=bdd$dep_energie_logement/bdd$coeffuc,w=bdd$pondmen)
# verif
# as.numeric(bdd %>% filter(dep_energie_logement/coeffuc>med_energy)%>%summarise(sum(pondmen)))/sum(bdd$pondmen)

# The second threshold, After Housing Cost (AHC) income, is calculated in a similar way. Each household’s required energy costs are deducted from their equivalised income. These are then ranked and 60 per cent of the median value is calculated. This is the income threshold.

load("D:/CIRED/Projet_Ademe/2010/c05_2010.RData")
men <- bdd %>% left_join(c05 %>% select(ident_men,c04111,c13211),by="ident_men")%>% mutate(AHC=(RDB-loyers - c04111* as.numeric(FC%>%filter(Variable=="rdb")%>%select(value)))/coeffuc)
med_income <- 0.6 * weightedMedian(x=men$AHC,w=bdd$pondmen)
rm(c05)
#  60median equivalised residual income (after housing costs) plus their estimated fuel spending after equivalisation.

fuel_poor_abs<-bdd %>% left_join(men %>% select(ident_men,AHC),by="ident_men") %>% filter(AHC<med_income+dep_energie_logement/coeffuc) %>% filter(dep_energie_logement/coeffuc>med_energy) %>% summarise(sum(pondmen))
fuel_poor_rate <- as.numeric(fuel_poor_abs)/sum(bdd$pondmen)
return(fuel_poor_rate)}




# GINI --------------------------------------------------------------------



# Gini_ener<- function(bdd){
#   bdd<-bdd %>% filter(dep_energie_logement>0)
# Gini_ener<-ineq(bdd$dep_energie_logement*bdd$pondmen)
# return(Gini_ener)}
# 
# Gini_carb<- function(bdd){
# Gini_carb<-ineq(bdd$carb_lubr*bdd$pondmen)
# return(Gini_carb)}


Gini_inc_decile<-function(bdd){
  G_i=0
  G=0
  for (i in 1:10){
    G_i=G_i+i*bdd%>%filter(decucn==i)%>%summarise(sum(pondmen*RDB))
    G=G+bdd%>%filter(decucn==i)%>%summarise(sum(pondmen*RDB))
  }
  Gini=2*G_i/(10*G)-11/10
  return(Gini)
}

Gini_rev_tot_decile<-function(bdd){
  bdd$rev_tot<-rowSums(bdd[c("rev_activites","rev_sociaux","rev_patrimoine","rev700","rev701","rev999","rev_TCO")])
  G_i=0
  G=0
  for (i in 1:10){
    G_i=G_i+i*bdd%>%filter(decucn==i)%>%summarise(sum(pondmen*rev_tot))
    G=G+bdd%>%filter(decucn==i)%>%summarise(sum(pondmen*rev_tot))
  }
  Gini=2*G_i/(10*G)-11/10
  return(Gini)
}

Gini_inc<-function(bdd){
  bdd_new<-bdd %>% arrange(RDB)
  n=dim(bdd_new)[1]
  bdd_new$index<-seq(n)
  G_i=0
  G=0
  for (i in 1:n){
    G_i=G_i+i*bdd_new%>%filter(index==i)%>%summarise(sum(pondmen*RDB))
    G=G+bdd_new%>%filter(index==i)%>%summarise(sum(pondmen*RDB))
  }
  Gini=2*G_i/(n*G)-(n+1)/n
  return(Gini)
}
# 
Gini_ener<-function(bdd){
  bdd_new<-bdd %>% arrange(dep_energie_logement)
  n=dim(bdd_new)[1]
  bdd_new$index<-seq(n)
  G_i=0
  G=0
  for (i in 1:n){
    G_i=G_i+i*bdd_new%>%filter(index==i)%>%summarise(sum(pondmen*dep_energie_logement))
    G=G+bdd_new%>%filter(index==i)%>%summarise(sum(pondmen*dep_energie_logement))
  }
  Gini=2*G_i/(n*G)-(n+1)/n
  return(Gini)
}
# 
Gini_carb<-function(bdd){
  bdd_new<-bdd %>% arrange(carb_lubr)
  n=dim(bdd_new)[1]
  bdd_new$index<-seq(n)
  G_i=0
  G=0
  for (i in 1:n){
    G_i=G_i+i*bdd_new%>%filter(index==i)%>%summarise(sum(pondmen*carb_lubr))
    G=G+bdd_new%>%filter(index==i)%>%summarise(sum(pondmen*carb_lubr))
  }
  Gini=2*G_i/(n*G)-(n+1)/n
  return(Gini)
}

# return(c("Gini_inc"=Gini_inc,"Gini_ener"= Gini_ener,"Gini_carb"= Gini_carb))}

# LORENTZ -----------------------------------------------------------------

plot_Lorentz<-function(bdd,plot_RDB=TRUE,plot_energie=TRUE){
  if(plot_RDB==TRUE){ print(plot(Lc(bdd$RDB*bdd$pondmen),col="darkred",lwd=2))}
  if(plot_energie==TRUE) {print(plot(Lc(bdd$dep_energie_logement*bdd$pondmen),col="darkred",lwd=2))}
}

# GGLORENTZ ---------------------------------------------------------------
# 
# plot_Lorentz2<- function(bdd,plot_RDB=TRUE,plot_energie=TRUE){
# men<-bdd %>% mutate(RDB_pond=bdd$RDB*bdd$pondmen) %>% filter(RDB_pond>=0)
# men_en<-bdd %>% mutate(en_pond=bdd$dep_energie_logement*bdd$pondmen) %>% filter(en_pond>=0)
# #RDB
# if(plot_RDB==TRUE){
#   print(
# ggplot(men, aes(RDB_pond)) +
#   stat_lorenz(desc = F) +
#   coord_fixed() +
#   geom_abline(linetype = "dashed") +
#   theme_minimal())}
# 
# #Energie
# if(plot_energie==TRUE){
# 
# print(
#   ggplot(men_en, aes(en_pond)) +
#   stat_lorenz(desc = F) +
#   coord_fixed() +
#   geom_abline(linetype = "dashed") +
#   theme_minimal())}
# }
# Niveau de vie moyen def comme RDB/UC ------------------------------------


niveau_vie_moy<-function(bdd){
Niv_moy<-as.numeric(bdd %>% summarise(weighted.mean(RDB/coeffuc,pondmen)))
return(Niv_moy)
}


#Mobilité inter déciles
mob_decuc<-function(bdd){
  
  bdd_decuc<-bdd %>% select(decucn,new_decucn)
  data_table<-as.data.frame(table(bdd_decuc))
  data_table<-plyr::ddply(data_table, .(decucn), transform, rescale = rescale(Freq))
  g<-ggplot(data_table, aes(decucn,new_decucn)) + geom_tile(aes(fill=rescale),colour="white") +
    scale_fill_gradient(low = "white", high = "steelblue")+
    geom_text(aes(label=data_table$Freq))+
    theme(axis.text.x=element_text(size = 15),axis.text.y=element_text(size = 15))
  base_size <- 9
  g<-g + theme_grey(base_size = base_size)  + 
    #remove extra space & put x axis on top
    scale_x_discrete(expand = c(0, 0),position = "top") + 
    #remove extra space & reverse y axis order
    scale_y_discrete(expand = c(0, 0),limits = rev(levels(as.factor(data_table$decucn))))+
    xlab(label="Ancien décile de niveau de vie")+ylab(label="Nouveau décile de niveau de vie")+
    ggtitle("Matrice de confusion \n Evolution des déciles de niveau de vie")+
    theme(legend.position = "none", axis.ticks = element_blank(),
          
          axis.text.x = element_text(size = base_size * 0.8, 
                                     angle = 0, hjust = 0, colour = "grey50"),
          plot.title = element_text(hjust = 0.5))
  g
}

mob_decuc_compa<-function(bdd1,bdd2){
  
  bdd_decuc<-bdd1 %>% select(new_decucn,ident_men)%>%left_join(bdd2%>%select(ident_men, new_decucn),by="ident_men")%>%select(-ident_men)
  colnames(bdd_decuc)<-c("decuc_2010","decuc_2025")
  data_table<-as.data.frame(table(bdd_decuc))
  data_table<-plyr::ddply(data_table, .(decuc_2010), transform, rescale = rescale(Freq))
  g<-ggplot(data_table, aes(decuc_2010,decuc_2025)) + geom_tile(aes(fill=rescale),colour="white") +
    scale_fill_gradient(low = "white", high = "steelblue")+
    geom_text(aes(label=data_table$Freq))+
    theme(axis.text.x=element_text(size = 15),axis.text.y=element_text(size = 15))
  base_size <- 9
  g<-g + theme_grey(base_size = base_size)  + 
    #remove extra space & put x axis on top
    scale_x_discrete(expand = c(0, 0),position = "top") + 
    #remove extra space & reverse y axis order
    scale_y_discrete(expand = c(0, 0),limits = rev(levels(as.factor(data_table$decuc_2010))))+
    xlab(label="Ancien décile de niveau de vie")+ylab(label="Nouveau décile de niveau de vie")+
    ggtitle("Matrice de confusion \n Evolution des déciles de niveau de vie")+
    theme(legend.position = "none", axis.ticks = element_blank(),
          
          axis.text.x = element_text(size = base_size * 0.8, 
                                     angle = 0, hjust = 0, colour = "grey50"),
          plot.title = element_text(hjust = 0.5))
  g
}
  


# Moyenne revenu D9 / Moyenne revenu D1 -----------------------------------
D<-function(bdd,n){
  
  bdd <- 
    bdd%>%
    mutate(RDB_coeffuc=RDB/coeffuc)%>%
    mutate(dec_RDB= ntile(RDB_coeffuc, 10))%>%
    ungroup()
  
  min_dec<-bdd %>% group_by(dec_RDB)%>% filter(RDB_coeffuc==min(RDB_coeffuc))%>%select(ident_men)
  max_dec<-bdd %>% group_by(dec_RDB)%>% filter(RDB_coeffuc==max(RDB_coeffuc))%>%select(ident_men)
  max_dec<-data.frame(max_dec)
  min_dec<-data.frame(min_dec)
  colnames(min_dec)<-c("decile","ident_men")
  colnames(max_dec)<-c("decile","ident_men")
  #D1 c'est le max du décile 1 + le min du décile 2 moyennés
  D_inter<-(
    bdd %>% filter(ident_men==as.numeric((max_dec%>%filter(decile==n)%>%select(ident_men))))%>%summarise(RDB_coeffuc*pondmen)+
      bdd %>% filter(ident_men==as.numeric((min_dec%>%filter(decile==n+1)%>%select(ident_men))))%>%summarise(RDB_coeffuc*pondmen)
    )/(
      bdd %>% filter(ident_men==as.numeric((max_dec%>%filter(decile==n)%>%select(ident_men))))%>%select(pondmen)+
        bdd %>% filter(ident_men==as.numeric((min_dec%>%filter(decile==n+1)%>%select(ident_men))))%>%select(pondmen))
  return(D_inter)}

D100<-function(bdd,n){
  
  bdd <- 
    bdd%>%
    mutate(RDB_coeffuc=RDB/coeffuc)%>%
    mutate(dec_RDB= ntile(RDB_coeffuc, 100))
  
  min_dec<-bdd %>% group_by(dec_RDB)%>% filter(RDB_coeffuc==min(RDB_coeffuc))%>%select(ident_men)
  max_dec<-bdd %>% group_by(dec_RDB)%>% filter(RDB_coeffuc==max(RDB_coeffuc))%>%select(ident_men)
  max_dec<-data.frame(max_dec)
  min_dec<-data.frame(min_dec)
  colnames(min_dec)<-c("decile","ident_men")
  colnames(max_dec)<-c("decile","ident_men")
  #D1 c'est le max du décile 1 + le min du décile 2 moyennés
  D_inter<-(
    bdd %>% filter(ident_men==as.numeric((max_dec%>%filter(decile==n)%>%select(ident_men))))%>%summarise(RDB_coeffuc*pondmen)+
      bdd %>% filter(ident_men==as.numeric((min_dec%>%filter(decile==n+1)%>%select(ident_men))))%>%summarise(RDB_coeffuc*pondmen)
  )/(
    bdd %>% filter(ident_men==as.numeric((max_dec%>%filter(decile==n)%>%select(ident_men))))%>%select(pondmen)+
      bdd %>% filter(ident_men==as.numeric((min_dec%>%filter(decile==n+1)%>%select(ident_men))))%>%select(pondmen))
  return(as.numeric(D_inter))}


D100_cst<-function(s,h,sc,r,n){
  
  if (s==0){
    load("D:/CIRED/Projet_Ademe/2010/menage_forme_2010.RData")
    bdd<-menage_forme_2010
    IP<-1
  }else{
    Iterations<-read_excel("D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown.xlsx")
    Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc & Redistribution==r)%>%select(Iter_finale))
    
    load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
    IP<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="AJ47",col_names = F))/as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="R47",col_names = F))
    
    bdd<-menage_echelle
  }
  
 
  # D100 mais exprimés en € constants 2019
  bdd <- 
    bdd%>%
    mutate(RDB_coeffuc=RDB/coeffuc)%>%
    mutate(dec_RDB= ntile(RDB_coeffuc, 100))
  
  min_dec<-bdd %>% group_by(dec_RDB)%>% filter(RDB_coeffuc==min(RDB_coeffuc))%>%select(ident_men)
  max_dec<-bdd %>% group_by(dec_RDB)%>% filter(RDB_coeffuc==max(RDB_coeffuc))%>%select(ident_men)
  max_dec<-data.frame(max_dec)
  min_dec<-data.frame(min_dec)
  colnames(min_dec)<-c("decile","ident_men")
  colnames(max_dec)<-c("decile","ident_men")
  #D1 c'est le max du décile 1 + le min du décile 2 moyennés
  D_inter<-(
    bdd %>% filter(ident_men==as.numeric((max_dec%>%filter(decile==n)%>%select(ident_men))))%>%summarise(RDB_coeffuc*pondmen)+
      bdd %>% filter(ident_men==as.numeric((min_dec%>%filter(decile==n+1)%>%select(ident_men))))%>%summarise(RDB_coeffuc*pondmen)
  )/(
    bdd %>% filter(ident_men==as.numeric((max_dec%>%filter(decile==n)%>%select(ident_men))))%>%select(pondmen)+
      bdd %>% filter(ident_men==as.numeric((min_dec%>%filter(decile==n+1)%>%select(ident_men))))%>%select(pondmen))
  D_inter_2019<-D_inter/IP
  
  return(as.numeric(D_inter_2019))}

S_RDB<-function(bdd,n){
  bdd <- 
    bdd%>%
    mutate(RDB_coeffuc=RDB/coeffuc)%>%
    mutate(dec_RDB= ntile(RDB_coeffuc, 10))
  Sn<-bdd %>% filter(dec_RDB<=as.numeric(n))%>%summarise(sum(pondmen*RDB))/bdd%>%summarise(sum(pondmen*RDB))
  return(Sn)
  
}


S_RDB_cst<-function(s,h,sc,r,n){
  if (s==0){
    load("D:/CIRED/Projet_Ademe/2010/menage_forme_2010.RData")
    bdd<-menage_forme_2010
    IP<-1
  }else{
    Iterations<-read_excel("D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown.xlsx")
    Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc & Redistribution==r)%>%select(Iter_finale))
    
    load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
    IP<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="AJ47",col_names = F))/as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="Q47",col_names = F))
    
    bdd<-menage_echelle
  }
  
  
  bdd <- 
    bdd%>%
    mutate(RDB_coeffuc=RDB/coeffuc)%>%
    mutate(dec_RDB= ntile(RDB_coeffuc, 10))
  Sn<-bdd %>% filter(dec_RDB<=as.numeric(n))%>%summarise(sum(pondmen*RDB))/bdd%>%summarise(sum(pondmen*RDB))
  return(Sn)
  
}

# 
# 
#   
# D9D1 <- as.numeric(bdd %>% filter(decucn==9) %>% summarise(weighted.mean(RDB,pondmen)))/as.numeric(bdd %>% filter(decucn==1) %>% summarise(weighted.mean(RDB,pondmen)))

D9D1<-function(menage_echelle){return(D(menage_echelle,9)/D(menage_echelle,1))}
D5D1<-function(menage_echelle){return(D(menage_echelle,5)/D(menage_echelle,1))}
D9D5<-function(menage_echelle){return(D(menage_echelle,9)/D(menage_echelle,5))}

# TEE ---------------------------------------------------------------------

# TEE : taux d’effort énergétique : dep ener dom/RDB ; dep transp/RDB ; dep carb transp/RDB


TEE_all<-function(bdd){
  TEE_dep_ener <- bdd%>%mutate_when(is.na(dep_energie_logement),list(dep_energie_logement=0,pondmen=0))%>% summarise(sum(dep_energie_logement*pondmen)/sum(RDB*pondmen))+bdd %>% mutate_when(is.na(dep_energie_logement),list(dep_energie_logement=0,pondmen=0))%>%summarise(sum((carb_lubr*pondmen)/sum(RDB*pondmen)))
}


TEE_var<-function(bdd,var){
  TEE_dep_ener <- bdd %>% group_by(get(var)) %>%mutate_when(is.na(dep_energie_logement),list(dep_energie_logement=0,pondmen=0))%>% summarise(sum(dep_energie_logement*pondmen)/sum(RDB*pondmen))
  
  # TEE_dep_ener <- rbind(
  #   # TEE_dep_ener[,2],
  #                       bdd%>%mutate_when(is.na(dep_energie_logement),list(dep_energie_logement=0,pondmen=0))%>% summarise(sum(dep_energie_logement*pondmen)/sum(RDB*pondmen))
  #   )
  # TEE_dep_ener<-TEE_dep_ener %>% mutate(decile=c(paste("decile",seq(1:10),sep="_")))
  colnames(TEE_dep_ener) <- c("var","TEE_dep_ener")
  # TEE_dep_ener <- TEE_dep_ener[,c(2,1)] 
  
  
  # dep carb transp/RDB
  TEE_carb<-(bdd %>%mutate_when(is.na(dep_energie_logement),list(dep_energie_logement=0,pondmen=0))%>% group_by(get(var)) %>% summarise(sum((carb_lubr*pondmen)/sum(RDB*pondmen))))
  # TEE_carb<-TEE_carb %>% mutate(decile=c(paste("decile",seq(1:10),sep="_"),"all"))
  colnames(TEE_carb) <- c("var","TEE_carb")
  
  
  TEE <- cbind(TEE_dep_ener,TEE_carb[,2])
  TEE <- TEE %>% mutate(TEE=TEE_carb+TEE_dep_ener)%>%select(var,TEE)
  return(TEE)}

TEE_var_bis<-function(bdd,var){
  # bdd<-bdd %>% mutate(var=as.character(get(var)))
  TEE_dep_ener <- bdd %>% group_by(get(var)) %>%mutate_when(is.na(dep_energie_logement),list(dep_energie_logement=0,pondmen=0))%>% summarise(sum(dep_energie_logement*pondmen)/sum(RDB*pondmen))
  
  # TEE_dep_ener <- rbind(
  #   # TEE_dep_ener[,2],
  #                       bdd%>%mutate_when(is.na(dep_energie_logement),list(dep_energie_logement=0,pondmen=0))%>% summarise(sum(dep_energie_logement*pondmen)/sum(RDB*pondmen))
  #   )
  # TEE_dep_ener<-TEE_dep_ener %>% mutate(decile=c(paste("decile",seq(1:10),sep="_")))
  colnames(TEE_dep_ener) <- c("var","TEE_dep_ener")
  # TEE_dep_ener <- TEE_dep_ener[,c(2,1)] 
  
  
  # dep carb transp/RDB
  TEE_carb<-(bdd %>%mutate_when(is.na(dep_energie_logement),list(dep_energie_logement=0,pondmen=0))%>% group_by(get(var)) %>% summarise(sum((carb_lubr*pondmen)/sum(RDB*pondmen))))
  # TEE_carb<-TEE_carb %>% mutate(decile=c(paste("decile",seq(1:10),sep="_"),"all"))
  colnames(TEE_carb) <- c("var","TEE_carb")
  
  
  TEE <- cbind(TEE_dep_ener,TEE_carb[,2])
  # TEE <- TEE %>% mutate(TEE=TEE_carb+TEE_dep_ener)%>%select(var,TEE)
  return(TEE)}


#dep ener dom/RDB  

TEE_bis<-function(bdd){
  TEE_dep_ener <- bdd %>% group_by(decucn) %>% summarise(sum(dep_energie_logement*pondmen)/sum(RDB*pondmen))
  TEE_dep_ener <- rbind(TEE_dep_ener[,2],bdd%>% summarise(sum(dep_energie_logement*pondmen)/sum(RDB*pondmen)))
  TEE_dep_ener<-TEE_dep_ener %>% mutate(decile=c(paste("decile",seq(1:10),sep="_"),"all"))
  colnames(TEE_dep_ener) <- c("TEE_dep_ener","decile")
  TEE_dep_ener <- TEE_dep_ener[,c(2,1)] 
  
 
  
  
  # dep carb transp/RDB
  TEE_carb<-rbind((bdd %>% group_by(decucn) %>% summarise(sum((carb_lubr*pondmen)/sum(RDB*pondmen))))[,2], bdd %>% summarise(sum((carb_lubr*pondmen)/sum(RDB*pondmen))))
  TEE_carb<-TEE_carb %>% mutate(decile=c(paste("decile",seq(1:10),sep="_"),"all"))
  colnames(TEE_carb) <- c("TEE_carb","decile")
  
  TEE <- TEE_dep_ener %>% left_join(TEE_dep_transp, by="decile") %>% left_join(TEE_carb, by="decile") 
  return(TEE)}

TEE_bis<-function(bdd){
TEE_dep_ener <- bdd %>% group_by(decucn) %>% summarise(sum(dep_energie_logement*pondmen)/sum(RDB*pondmen))
TEE_dep_ener <- rbind(TEE_dep_ener[,2],bdd%>% summarise(sum(dep_energie_logement*pondmen)/sum(RDB*pondmen)))
TEE_dep_ener<-TEE_dep_ener %>% mutate(decile=c(paste("decile",seq(1:10),sep="_"),"all"))
colnames(TEE_dep_ener) <- c("TEE_dep_ener","decile")
TEE_dep_ener <- TEE_dep_ener[,c(2,1)] 

# dep transp/RDB
TEE_dep_transp<-rbind((bdd %>% group_by(decucn) %>% summarise(sum(((transp_routes_eau+transp_rail_air)*pondmen)/sum(RDB*pondmen))))[,2], bdd %>% summarise(sum(((transp_routes_eau+transp_rail_air)*pondmen)/sum(RDB*pondmen))))
TEE_dep_transp<-TEE_dep_transp %>% mutate(decile=c(paste("decile",seq(1:10),sep="_"),"all"))
colnames(TEE_dep_transp) <- c("TEE_dep_transp","decile")


# dep carb transp/RDB
TEE_carb<-rbind((bdd %>% group_by(decucn) %>% summarise(sum((carb_lubr*pondmen)/sum(RDB*pondmen))))[,2], bdd %>% summarise(sum((carb_lubr*pondmen)/sum(RDB*pondmen))))
TEE_carb<-TEE_carb %>% mutate(decile=c(paste("decile",seq(1:10),sep="_"),"all"))
colnames(TEE_carb) <- c("TEE_carb","decile")

TEE <- TEE_dep_ener %>% left_join(TEE_dep_transp, by="decile") %>% left_join(TEE_carb, by="decile") 
return(TEE)}



# dep_contraintes --------------------------------------------------------------------

# depenses_contraintes_decuc<-function(bdd){
# dep_preeng_code<-read_excel("D:/CIRED/Projet_Ademe/Donnees_brutes/Nomenclature_CIRED_ADEME/nomenclature_dep_preeng_coicop5.xlsx",sheet="nomenclature_code")
# 
# dep_preeng<-
#   c05 %>% select(ident_men,dep_preeng_code$COICOP)
# dep_preeng$dep_preeng<-rowSums(dep_preeng[c(dep_preeng_code$COICOP)])
# dep_preeng <- dep_preeng %>% left_join(bdd %>% select(ident_men,pondmen, Rcons,decucn,typo2010f),by='ident_men')
# dep_preeng <- dep_preeng %>% mutate(part_dep_preeng=dep_preeng/Rcons)
# 
# weighted.mean(dep_preeng$part_dep_preeng,dep_preeng$pondmen,na.rm=TRUE)
# 
# Table_dep_pre_eng<-dep_preeng %>% group_by(decucn) %>% summarise(weighted.mean(part_dep_preeng,pondmen,na.rm=TRUE))
# 
# 
# Table_dep_pre_eng <- rbind(Table_dep_pre_eng[1:10,2],weighted.mean(dep_preeng$part_dep_preeng,dep_preeng$pondmen,na.rm=TRUE))
# Table_dep_pre_eng<-Table_dep_pre_eng %>% mutate(decuc=c(paste("decile",seq(1:10),sep="_"),"all"))
# colnames(Table_dep_pre_eng)<-c("moyenne_part_dep_preeng","decile")
# return(Table_dep_pre_eng)
# }

# NEW VERSION
depenses_contraintes<-function(bdd1,bdd2, cat="decucn"){
  
  dep_preeng_code<-read_excel("D:/CIRED/Projet_Ademe/Donnees_brutes/Nomenclature_CIRED_ADEME/nomenclature_dep_preeng_coicop5.xlsx",sheet="nomenclature_code")

  dep_preeng1<-    bdd1
  dep_preeng1$dep_preeng<-rowSums(dep_preeng1[c(dep_preeng_code$COICOP)])
  dep_preeng1 <- dep_preeng1 %>% mutate(part_dep_preeng=dep_preeng/Rcons)

  weighted.mean(dep_preeng1$part_dep_preeng,dep_preeng1$pondmen,na.rm=TRUE)

  Table_dep_pre_eng<-dep_preeng1 %>% group_by(get(cat)) %>% summarise(weighted.mean(part_dep_preeng,pondmen,na.rm=TRUE))

  dep_preeng2<-    bdd2
  dep_preeng2$dep_preeng<-rowSums(dep_preeng2[c(dep_preeng_code$COICOP)])
  # dep_preeng2 <- dep_preeng2 %>% left_join(bdd2, by='ident_men')
  dep_preeng2 <- dep_preeng2 %>% mutate(part_dep_preeng=dep_preeng/Rcons)

  weighted.mean(dep_preeng2$part_dep_preeng,dep_preeng2$pondmen,na.rm=TRUE)

  Table_dep_pre_eng2<-dep_preeng2 %>% group_by(get(cat)) %>% summarise(weighted.mean(part_dep_preeng,pondmen,na.rm=TRUE))

  Table_dep_pre_eng<-cbind(Table_dep_pre_eng,Table_dep_pre_eng2[,2])


  return(Table_dep_pre_eng)
}

# 
# depenses_contraintes_var<-function(bdd,FC, var){
#   dep_preeng_code<-read_excel("D:/CIRED/Projet_Ademe/Donnees_brutes/Nomenclature_CIRED_ADEME/nomenclature_dep_preeng_coicop5.xlsx",sheet="nomenclature_code")
#   
#   dep_preeng1<-
#     c05 %>% select(ident_men,dep_preeng_code$COICOP)
#   
#   dep_preeng1$dep_preeng<-rowSums(dep_preeng1[c(dep_preeng_code$COICOP)])
#   dep_preeng1 <- dep_preeng1 %>% left_join(bdd1,by='ident_men')
#   dep_preeng1 <- dep_preeng1 %>% mutate(part_dep_preeng=dep_preeng/Rcons)
#   
#   weighted.mean(dep_preeng1$part_dep_preeng,dep_preeng1$pondmen,na.rm=TRUE)
#   
#   Table_dep_pre_eng<-dep_preeng1 %>% group_by(get(cat)) %>% summarise(weighted.mean(part_dep_preeng,pondmen,na.rm=TRUE))
#   
#   dep_preeng2<-
#     c05 %>% select(ident_men,dep_preeng_code$COICOP)
#   dep_preeng2$dep_preeng<-rowSums(dep_preeng2[c(dep_preeng_code$COICOP)])
#   dep_preeng2 <- dep_preeng2 %>% left_join(bdd2, by='ident_men')
#   dep_preeng2 <- dep_preeng2 %>% mutate(part_dep_preeng=dep_preeng/Rcons)
#   
#   weighted.mean(dep_preeng2$part_dep_preeng,dep_preeng2$pondmen,na.rm=TRUE)
#   
#   Table_dep_pre_eng2<-dep_preeng2 %>% group_by(get(cat)) %>% summarise(weighted.mean(part_dep_preeng,pondmen,na.rm=TRUE))
#   
#   Table_dep_pre_eng<-cbind(Table_dep_pre_eng,Table_dep_pre_eng2[,2])
#   
#   
#   return(Table_dep_pre_eng)
# }

# dat<-depenses_contraintes(bdd1,bdd2,cat="MI_corr")[1:2,]
# colnames(dat)<-c("MI_corr",2010,2025)
# 
# dat<-dat %>% gather(key=year,value=Per_dep_preeng,-1)
# ggplot(dat,aes(x=year,y=Per_dep_preeng))
# 
# g<-ggplot(dat,aes(x=factor(MI_corr),y=Per_dep_preeng,fill=factor(MI_corr)))+geom_bar(stat="identity")+facet_wrap(~year)+scale_x_discrete(breaks=c("0","1"),labels=c("Collective Housing","Individual Housing"))+ggtitle("Percentage of pre-committed expenditures in households' budget")+labs(x = "Housing",y="Budget share of pre-committed expenditures")+theme_stata()+scale_fill_npg()+ theme(legend.position="none")
# g





# IPEM --------------------------------------------------------------------


IPEM<-function(bdd){
# INCOME
wgm_inc<-weightedMedian(x=bdd$RDB/bdd$coeffuc,w=bdd$pondmen)
# 1517€/mois

low_income <<- bdd %>% filter(RDB/coeffuc <wgm_inc ) %>% select(ident_men,pondmen)
low_income %>% summarise(sum(pondmen))
# 13 804 514 #13 millions de ménages (50% des ménages, def median)

# HIGH FUEL SPENDING
men_mobility <- bdd %>% mutate_when(is.na(nbactifs),list(nbactifs=0)) %>% filter(nbactifs>0) %>% filter(carb_lubr>0)
wgm_fuel<-weightedMedian(x=men_mobility$carb_lubr/men_mobility$nbactifs,w=men_mobility$pondmen,na.rm=TRUE)
# 147€ par mois par actif

high_fuel_spending <<- bdd %>%mutate_when(is.na(nbactifs),list(nbactifs=0)) %>% filter(carb_lubr/nbactifs > 2*wgm_fuel) %>% select(ident_men)
# high_fuel_spending %>% summarise(sum(pondmen))
# 3 613 032 # soit 13% des ménages



# MOBILITY
AUTO<-read.csv("D:/CIRED/Projet_Ademe/Donnees_brutes/BDF_2010/AUTOMOBILE_metropole.csv",sep=";")
# pas seulement les km en voitures mais tous les moyens de transport. Perd son sens sans les reports modaux

men_auto <- AUTO %>% group_by(ident_men) %>% summarise(sum(km_auto))
colnames(men_auto)<-c("ident_men","km_auto")
if("km_auto" %in% colnames(bdd)){men_perf<-bdd %>%select(-km_auto)}else{men_perf<-bdd}

men_perf<-men_perf%>% left_join(men_auto, by="ident_men") 

men_perf<- men_perf%>% mutate(km_auto=replace_na(km_auto,0))%>%mutate_when(km_auto==99999, list(km_auto=0)) %>% mutate(veh_perf=carb_lubr/(km_auto*52)) %>% mutate(veh_perf=replace_na(veh_perf,0))

# POOR VEHICULE PERFORMANCE
poor_veh_perf<-men_perf %>% filter(veh_perf>0.1) %>% select(ident_men)
no_veh<-bdd %>% filter(nbvehic==0 | is.na(nbvehic)) %>% select(ident_men)
poor_veh_perf <<- union(poor_veh_perf,no_veh)

# POOR SPATIAL MATCHING
men_spatial_matching <- men_perf %>% mutate(spatial_matching=(km_auto*52)/coeffuc/12) %>% mutate(spatial_matching=replace_na(spatial_matching,0)) %>% mutate_when(is.infinite(spatial_matching),list(spatial_matching=0))

poor_spatial_matching <<- men_spatial_matching %>% filter(spatial_matching>352) %>% select(ident_men)


# EXTRA TIME

# #DEPINDIV 2 : modify BDG excel to change date format to hours for h_trans and alter_trans
DEPINDIV <- read_excel("D:/CIRED/Projet_Ademe/Donnees_brutes/BDF_2010/DEPINDIV2.xlsx")

# table(DEPINDIV$h_trans1)
# extra_time_TC <- DEPINDIV$h_trans3 - DEPINDIV$alter_trans5 #secondes
# extra_time_car <- DEPINDIV$h_trans5 - DEPINDIV$alter_trans3 #secondes

extra_time <- cbind(DEPINDIV$ident_men,DEPINDIV[,'h_trans3'],DEPINDIV[,'h_trans5'], DEPINDIV[,'alter_trans3'],DEPINDIV[,'alter_trans5'])
extra_time <- as.data.frame(extra_time)
colnames(extra_time)<-c("ident_men","h_trans3","h_trans5", "alter_trans3","alter_trans5")

extra_time <- extra_time %>% 
  mutate(vulnerable=FALSE) %>% 
  mutate(extra_time_TC=h_trans3-alter_trans5) %>% 
  mutate(extra_time_car=h_trans5 - alter_trans3) %>% 
  mutate_when(is.na(alter_trans5),list(alter_trans5=h_trans5))

# vulnerable if extra_time_TC>3600 secs, extra_time_TC < -3600 secs or is.na(alter_trans5)&!(is.na(h_trans3 ))
# extra_time2 permet de s'affranchir des NA pénibles dans les tests
extra_time2<- extra_time %>% mutate(extra_time_TC=replace_na(extra_time_TC,8639999823600)) %>%mutate(extra_time_car=replace_na(extra_time_car,8639999823600))  

extra_time <- 
  extra_time2 %>%
  mutate_when(abs(extra_time_TC)<16801 & extra_time_TC>3600,list(vulnerable=TRUE),
              abs(extra_time_car)<16801 & extra_time_car<(-3600), list(vulnerable=TRUE),
              is.na(alter_trans5) & !(is.na(h_trans3)),list(vulnerable=TRUE))

# ménage dont les temps alter et trans ne sont pas compréhénsibles.
extra_time <- extra_time %>% mutate_when(ident_men==12971 & extra_time_TC==-9600, list(vulnerable=FALSE))

ident_extra_time <<- extra_time %>% filter(vulnerable) %>% select(ident_men)

# MENAGES VULNERABLES IPEM
IPEM_table <-
  bdd %>%
  # FUEL POOR
  mutate(fuel_poor=FALSE) %>%
  mutate_when(ident_men %in% high_fuel_spending$ident_men & ident_men %in%low_income$ident_men,
              list(fuel_poor=TRUE)) %>%
  mutate_when(ident_men %in% ident_extra_time$ident_men & ident_men %in% low_income$ident_men,
              list(fuel_poor=TRUE)) %>%

  # FUEL VULNERABLE
  mutate(fuel_vulnerable=FALSE) %>%
  mutate_when(ident_men %in% intersect(intersect(poor_veh_perf$ident_men,poor_spatial_matching$ident_men),low_income$ident_men),
              list(fuel_vulnerable=TRUE)) %>%

  # FUEL DEPENDANT
  mutate(fuel_dependant=FALSE) %>%
  mutate_when(ident_men %in% high_fuel_spending$ident_men,
              list(fuel_dependant=TRUE))%>%
  select(ident_men,pondmen, fuel_poor,fuel_vulnerable,fuel_dependant)

# IPEM_table<-IPEM_table %>%ungroup()%>% mutate(decucn=factor(decucn,levels=seq(1,10)))

rate_fuel_poor<- (IPEM_table %>% filter(fuel_poor) %>%summarise(sum(pondmen)))/(bdd %>% summarise(sum(pondmen)))
rate_fuel_vulnerable <-(IPEM_table%>%filter(fuel_vulnerable) %>% summarise(sum(pondmen)))/(bdd %>% summarise(sum(pondmen)))

rate_fuel_dependant<- (IPEM_table %>% filter(fuel_dependant) %>% summarise(sum(pondmen)))/bdd %>%summarise(sum(pondmen))
df_IPEM <- data.frame("rate_fuel_poor"=rate_fuel_poor,"rate_fuel_vulnerable"=rate_fuel_vulnerable,"rate_fuel_dependant"=rate_fuel_dependant)
colnames(df_IPEM)<-c("rate_fuel_poor","rate_fuel_vulnerable","rate_fuel_dependant")
return(df_IPEM)
}






# Plot indicateurs --------------------------------------------------------
# n=2
# Indic<-c(
#     rep('ratio_fuel_poverty',n),
#     rep('LIHC',n),
#     rep('Gini_inc',n),
#     rep('Gini_ener',n),
#     # rep('Gini_carb',n),
#     # rep("niveau_vie_moy",n),
#     rep('D9D1',n)
#     # ,
#     # rep('TEE_dep_ener',n),
#     # rep('TEE_dep_transp',n),
#     # rep('TEE_carb',n)
#     )
# 
# 
# Year <-rep(c(2010,2025),5)
# 
# Values <- c(
#   ratio_fuel_poverty(bdd1),
#   ratio_fuel_poverty(bdd2),
#   LIHC(bdd1),
#   LIHC(bdd2),
#   Gini_inc(bdd1),
#   Gini_inc(bdd2),
#   Gini_ener(bdd1),
#   Gini_ener(bdd2),
#   # Gini_carb(bdd1),
#   # Gini_carb(bdd2),
#   # niveau_vie_moy(bdd1),
#   # niveau_vie_moy(bdd2),
#   D9D1(bdd1),
#   D9D1(bdd2)
#   # ,
#   # as.numeric(TEE(bdd1)%>% filter(decile=="all") %>% select(TEE_dep_ener)),
#   # as.numeric(TEE(bdd2)%>% filter(decile=="all") %>% select(TEE_dep_ener)),
#   # as.numeric(TEE(bdd1)%>% filter(decile=="all") %>% select(TEE_dep_transp)),
#   # as.numeric(TEE(bdd2)%>% filter(decile=="all") %>% select(TEE_dep_transp)),
#   # as.numeric(TEE(bdd1)%>% filter(decile=="all") %>% select(TEE_carb)),
#   # as.numeric(TEE(bdd2)%>% filter(decile=="all") %>% select(TEE_carb))
#   )
# 
# Df<-data.frame(Indic,as.factor(Year),Values)
# Df
# # Changer échelle par facet
# # Pourquoi Gini négatif
# 
# 
# 
# g<-ggplot(Df%>% filter(),aes(x=Year,y=Values,fill=Year))+geom_bar(stat="identity")+facet_wrap(~Indic,scales="free")
# # +scale_x_discrete(breaks=c("2010","2025"),labels=c("2010","2025"))
# g
# 
# 
# # 
# # for (col in Indic(1))
# #   pdf("Indicateurs/indic.pdf")
# #   g<-ggplot(Df%>% filter(),aes(x=Year,y=Values,color=Year))+geom_bar(stat="identity")+facet_grid(~Indic,scales="free")
# #   g
# 


Sn<-function(n){
  Sn_2010<-as.numeric(S_RDB(menage_forme_2010,n)/S_RDB(menage_forme_2010,10))
  Exp<-rep(Sn_2010,length(Scenarios)*length(Scenario_classement))
  S<-rep(Scenarios,length(Scenario_classement))
  H<-rep(2010,length(Scenarios)*length(Scenario_classement))
  SC<-rep(Scenario_classement,length(Scenarios))
  for (s in Scenarios){
    for (h in Horizon){
      for (sc in Scenario_classement){
        S<-c(S,s)
        H<-c(H,h)
        h=as.numeric(h)
        SC<-c(SC,sc)
        Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc)%>%select(Iter_finale))
        load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
        A<-S_RDB(menage_echelle,n)/S_RDB(menage_echelle,10)
        Exp<-c(Exp,as.numeric(A))
      }
    }}
  # S<-S[-1]
  # H<-H[-1]
  # SC<-SC[-1]
  # Exp<-Exp[-1]
  
  df<-data.frame(S,H,SC,Exp)
  colnames(df)<-c("scenario","horizon","scenario_classement","value")
  df$scenario_classement<-factor(df$scenario_classement,levels=c("Optimiste","Median","Pessimiste"))
  return(df)
}

SnSm<-function(n,m){
  Sn_2010<-as.numeric((1-(S_RDB(menage_forme_2010,n)/S_RDB(menage_forme_2010,10)))/(S_RDB(menage_forme_2010,m)/S_RDB(menage_forme_2010,10)))
  Exp<-rep(Sn_2010,length(Scenarios)*length(Scenario_classement))
  S<-rep(Scenarios,length(Scenario_classement))
  H<-rep(2010,length(Scenarios)*length(Scenario_classement))
  SC<-rep(Scenario_classement,length(Scenarios))
  for (s in Scenarios){
    for (h in Horizon){
      for (sc in Scenario_classement){
        S<-c(S,s)
        H<-c(H,h)
        h=as.numeric(h)
        SC<-c(SC,sc)
        Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc)%>%select(Iter_finale))
        load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
        A<-as.numeric((1-(S_RDB(menage_echelle,n)/S_RDB(menage_echelle,10)))/(S_RDB(menage_echelle,m)/S_RDB(menage_echelle,10)))
        Exp<-c(Exp,as.numeric(A))
      }
    }}
  # S<-S[-1]
  # H<-H[-1]
  # SC<-SC[-1]
  # Exp<-Exp[-1]
  
  df<-data.frame(S,H,SC,Exp)
  colnames(df)<-c("scenario","horizon","scenario_classement","value")
  df$scenario_classement<-factor(df$scenario_classement,levels=c("Optimiste","Median","Pessimiste"))
  return(df)
}


D100n<-function(n){
  Dn_2010<-as.numeric(D100(menage_forme_2010,n))/1000
  Exp<-rep(Dn_2010,length(Scenarios)*length(Scenario_classement))
  S<-rep(Scenarios,length(Scenario_classement))
  H<-rep(2010,length(Scenarios)*length(Scenario_classement))
  SC<-rep(Scenario_classement,length(Scenarios))
  for (s in Scenarios){
    for (h in Horizon){
      for (sc in Scenario_classement){
        S<-c(S,s)
        H<-c(H,h)
        h=as.numeric(h)
        SC<-c(SC,sc)
        Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc)%>%select(Iter_finale))
        load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
        A<-D100(menage_echelle,n)
        Exp<-c(Exp,as.numeric(A)/1000)
      }
    }}
  # S<-S[-1]
  # H<-H[-1]
  # SC<-SC[-1]
  # Exp<-Exp[-1]
  
  df<-data.frame(S,H,SC,Exp)
  colnames(df)<-c("scenario","horizon","scenario_classement","value")
  df$scenario_classement<-factor(df$scenario_classement,levels=c("Optimiste","Median","Pessimiste"))
  return(df)
}


DnDm_comp100<-function(n,m){
  
  
  Dn_2010<-D100(menage_forme_2010,n)
  Dm_2010<-D100(menage_forme_2010,m)
  DnDm_2010<-as.numeric(Dn_2010/Dm_2010)
  
  Exp<-rep(DnDm_2010,length(Scenarios)*length(Scenario_classement))
  S<-rep(Scenarios,length(Scenario_classement))
  H<-rep(2010,length(Scenarios)*length(Scenario_classement))
  SC<-rep(Scenario_classement,length(Scenarios))
  
  for (s in Scenarios){
    for (h in Horizon){
      for (sc in Scenario_classement){
        S<-c(S,s)
        H<-c(H,h)
        h=as.numeric(h)
        SC<-c(SC,sc)
        Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc)%>%select(Iter_finale))
        load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
        A<-D100(menage_echelle,n)/D100(menage_echelle,m)
        Exp<-c(Exp,as.numeric(A))
      }
    }}
  # S<-S[-1]
  # H<-H[-1]
  # SC<-SC[-1]
  # Exp<-Exp[-1]
  
  df<-data.frame(S,H,SC,Exp)
  colnames(df)<-c("scenario","horizon","scenario_classement","value")
  df$scenario_classement<-factor(df$scenario_classement,levels=c("Optimiste","Median","Pessimiste"))
  return(df)
}


# Attention retournent des valeurs en € constants 2019 !
D100_01<-function(s,h,sc,r){return(D100_cst(s,h,sc,r,1))}
D100_05<-function(s,h,sc,r){return(D100_cst(s,h,sc,r,5))}
D100_10<-function(s,h,sc,r){return(D100_cst(s,h,sc,r,10))}
D100_50<-function(s,h,sc,r){return(D100_cst(s,h,sc,r,50))}
D100_90<-function(s,h,sc,r){return(D100_cst(s,h,sc,r,90))}
D100_95<-function(s,h,sc,r){return(D100_cst(s,h,sc,r,95))}
D100_9505<-function(s,h,sc,r){D100_95(s,h,sc,r)/D100_05(s,h,sc,r)}
D100_9050<- function(s,h,sc,r){D100_90(s,h,sc,r)/D100_50(s,h,sc,r)}
D100_9010<-function(s,h,sc,r){D100_90(s,h,sc,r)/D100_10(s,h,sc,r)}
D100_5010<-function(s,h,sc,r){D100_50(s,h,sc,r)/D100_10(s,h,sc,r)}


# Euros constants 2019
S_2<-function(s,h,sc,r){S_RDB_cst(s,h,sc,r,2)}
S_5<-function(s,h,sc,r){S_RDB_cst(s,h,sc,r,5)}
S_8<-function(s,h,sc,r){S_RDB_cst(s,h,sc,r,8)}
S_28<-function(s,h,sc,r){(1-S_8(s,h,sc,r))/S_2(s,h,sc,r)}




# Taux TCO ----------------------------------------------------------------

# 
# taxe_carbone<-function(s,h,sc,r,var){
#   
#   Iterations<-read_excel("D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown.xlsx")
#   Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc & Redistribution==r)%>%select(Iter_finale))
#   load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
#   
#   
#   IP<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="AJ47",col_names = F))/as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="R47",col_names = F))
#   
#   
# bdd<-menage_echelle
#   ## Poids moyen de la TCO dans le RDB des ménages
#   bdd<-
#     bdd %>%
#     mutate(TCO_paid_pc=TCO_paid/RDB)
#   
#   bdd<- bdd %>% mutate_when(RDB==0,list(TCO_paid_pc=NA))
#   # 
#   TCO_paid_pc<-bdd %>%
#     group_by(get(var))%>%
#     summarise(weighted.mean(x=TCO_paid_pc,w=pondmen,na.rm=T))
#   
#   
#   TCO_paid<-bdd %>%
#     group_by(get(var))%>%
#     summarise(weighted.mean(x=TCO_paid/IP,w=pondmen,na.rm=T))
#   
#   TCO_paid_agg<-bdd %>%
#     group_by(get(var))%>%
#     summarise(sum(TCO_paid*pondmen)/sum(pondmen)/IP)
#   
#   ## attention : pourcentage de la TCO dans l'ensemble du décile sum(TCO_paid*pondmen)/sum(pondmen*RDB) et non pas le pourcentage moyen
#   TCO_paid_pc_agg<-bdd %>%
#     group_by(get(var))%>%
#     summarise(sum(TCO_paid*pondmen)/sum(pondmen*RDB))
#   
#   
#   R<-data.frame("decucn"=seq(1,10),TCO_paid_pc_agg[,2],TCO_paid_pc[,2],TCO_paid_agg[,2],TCO_paid[,2])
#   colnames(R)<-c("decucn","TCO_paid_pc_agg","TCO_paid_pc","TCO_paid_agg","TCO_paid")
#   
#   # R
#   
#   return(R)
# }
# 
# 
# 
# paiement_TCO<-function(var,s,h,sc,r){
#   
#   Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc & Redistribution==r)%>%select(Iter_finale))
#   load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
#   TCO_rate_bis<-read_excel(path=paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""),range="B135:C138",col_names=F)
#   TCO_rate<-cbind(TCO_rate_bis[1:4,2])
#   TCO_rate<-as.data.frame(t(TCO_rate))
#   colnames(TCO_rate)<-c("CL","Oil","Elec","Gas")
#   
#   
#   menage_echelle <- 
#     menage_echelle %>%
#     mutate(TCO_paid=
#              TCO_rate$CL*(dep_Solides)+
#              TCO_rate$Oil*(carb_lubr+dep_Fuel+dep_GPL)+
#              TCO_rate$Elec*(dep_Elec)+
#              TCO_rate$Gas*(dep_Gaz+dep_Urbain))
#     
#   menage_echelle <- 
#     menage_echelle %>%
#     mutate(TCO_paid_pc=TCO_paid/RDB)%>%
#     mutate(TCO_rtcd_pc=rev_TCO/RDB)
#   
#   menage_echelle <-
#     menage_echelle %>%
#     mutate_when(RDB==0,list(TCO_paid_pc=0))
#   
#   TCO_paid<-menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(weighted.mean(x=TCO_paid,w=pondmen,na.rm=T))
#   
#   
#   ## Poids moyen de la TCO dans le RDB des ménages
#   TCO_paid_pc_bis<-menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(weighted.mean(x=TCO_paid_pc,w=pondmen,na.rm=T))
#   
#   
#   ## attention : pourcentage de la TCO dans l'ensemble du décile sum(TCO_paid*pondmen)/sum(pondmen*RDB) et non pas le pourcentage moyen
#   TCO_paid_pc<-menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(sum(TCO_paid*pondmen)/sum(pondmen*RDB))
#   
#   ## TCO rtcd moyenne
#   TCO_rtcd_pc_bis<-menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(weighted.mean(x=TCO_rtcd_pc,w=pondmen,na.rm=T))
#   
#   
#   ## attention : pourcentage de la TCO dans l'ensemble du décile sum(TCO_paid*pondmen)/sum(pondmen*RDB) et non pas le pourcentage moyen
#   TCO_rtcd_pc<-menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(sum(rev_TCO*pondmen)/sum(pondmen*RDB))
#   
#   TCO_rtcd<-menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(weighted.mean(x=rev_TCO,w=pondmen,na.rm=T))
#   
#     df<-data.frame(TCO_paid[,1],TCO_paid[,2],TCO_paid_pc_bis[,2],TCO_rtcd[,2],TCO_rtcd_pc[,2])
#     colnames(df)<-c("var",
#                     "TCO_paid",
#                     "TCO_paid_pc",
#                     "TCO_rtcd",
#                     "TCO_rtcd_pc"
#     )
#     
#    
#     
#     # colnames(df)<-c("var",
#     #                 "TCO payée (€)",
#     #                 "TCO payée moyenne (en % du RDB)",
#     #                 "TCO rétrocédée (€)",
#     #                 "TCO rétrocédée moyenne (% du RDB)"
#     #                 )
#     return(df)
# }
# 
# 
# tot_TCO<-function(var,s,h,sc,r){
#   Iterations<-read_excel("D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown.xlsx")
#   Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc & Redistribution==r)%>%select(Iter_finale))
#   load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
#   TCO_rate_bis<-read_excel(path=paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""),range="B135:C138",col_names=F)
#   TCO_rate<-cbind(TCO_rate_bis[1:4,2])
#   TCO_rate<-as.data.frame(t(TCO_rate))
#   colnames(TCO_rate)<-c("CL","Oil","Elec","Gas")
#   
#   
#   menage_echelle <- 
#     menage_echelle %>%
#     mutate(TCO_paid=
#              TCO_rate$CL*(dep_Solides)+
#              TCO_rate$Oil*(carb_lubr+dep_Fuel+dep_GPL)+
#              TCO_rate$Elec*(dep_Elec)+
#              TCO_rate$Gas*(dep_Gaz+dep_Urbain))
#   
#   tot_rtcd<-as.numeric(menage_echelle %>% summarise(sum(pondmen*rev_TCO)))
#   tot_paid<-as.numeric(menage_echelle %>% summarise(sum(pondmen*TCO_paid)))
#   
#   tot_rtcd/tot_paid-1
#   
#   df<-data.frame(tot_paid,tot_rtcd)
#   colnames(df)<-c("tot_paid","tot_rtcd")
#   
#   
#   
#   # colnames(df)<-c("var",
#   #                 "TCO payée (€)",
#   #                 "TCO payée moyenne (en % du RDB)",
#   #                 "TCO rétrocédée (€)",
#   #                 "TCO rétrocédée moyenne (% du RDB)"
#   #                 )
#   return(df)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# calc_TCO<-function(var,s,h,sc,r,Iter=NA,output=NA){
#   
#   if(s=="AMS"){scen=s}
#   if(s=="AME"){scen=s}
#   if(s=="ssTCO"){scen=s}
#   if(s=="AMS"){scen=s}
#   if(s=="AMS"){scen=s}
#   
#   
#  
#   EMS<-read_excel(path=paste("D:/CIRED/Projet_Ademe/IMACLIM/EMS.xlsx",sep=""),range=paste(s,"!B1:AF5",sep=""),col_names=T)
#   EMS<-EMS %>% gather(key=year,value=emission,-1)
#   colnames(EMS)<-c("Variable","year","emission")
#   
#   EMS<-EMS%>%filter(!Variable=="EMS_HH_2")
#   EMS #tonnes C02
#   
#   load("D:/CIRED/Projet_Ademe/2010/menage_forme_2010.RData")
#   
#   # if(!var %in% colnames(menage_forme_2010)){
#   #   load("D:/CIRED/Projet_Ademe/2010/appmen_depensesactiv_2010.RData")
#   #   menage_forme_2010<-
#   #     menage_forme_2010 %>%
#   #     left_join(appmen_depensesactiv_2010 %>% select(ident_men,var),by="ident_men")
#   # }
#   
# # prix constant 2010 tCO2/ dépenses par cat
#   coeff_CL_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Variable=="EMS_HH_21_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
#     summarise(sum(pondmen*dep_Solides))) # en tC02/Millions €
#   
#   coeff_Oil_2010<-as.numeric(EMS%>%filter(year==2010)%>%filter(Variable=="EMS_HH_22_2")%>%select(emission))/ as.numeric(menage_forme_2010 %>%
#                          summarise(sum(pondmen*(carb_lubr+dep_Fuel+dep_GPL)))) # en tC02/Millions €
#   
#   coeff_Gaz_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Variable=="EMS_HH_24_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
#                          summarise(sum(pondmen*(dep_Gaz+dep_Urbain)))) # en tC02/Millions €
#   
#   
#   # Load Horizon
#   if(is.na(Iter)){Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc & Redistribution==r)%>%select(Iter_finale))}
#  
#   load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
#   
#   if(!var %in% colnames(menage_echelle)){
#     menage_echelle<-
#       menage_echelle %>%
#       left_join(appmen_depensesactiv_2010 %>% select(ident_men,var),by="ident_men")}
#   if(Iter==0){
#     suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code_iter_",as.character(Iter),".xlsx",sep=""),skip=1))
#   }else{
#    suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",as.character(Iter),".xlsx",sep="")))}
# 
#     
#     output_macro <-  output_macro %>%  gather(key=year_model,value=value,-c(1:3))
#     
#     IMACLIM<-  output_macro  %>%  separate(col="year_model",into=c("year","model"),sep="_")
#     FC <- 
#       IMACLIM %>%
#       filter(year==h) %>%
#       filter(model=="IMACLIM")%>%
#       filter(Variable %in% c(
#         "A01",
#         "A02",
#         "A03",
#         "A04",
#         "A05",
#         "A06",
#         "A07",
#         "A08",
#         "A09",
#         "A10",
#         "A11",
#         "A12",
#         "A13")) %>%
#       select(Variable,value)
#     
#     FC <- FC %>%
#       mutate(value=as.numeric(value)) %>%
#       spread(key=Variable,value=value) 
#   
#   
#   
#   #Horizon CL, Oil, Gaz dépenses à l'horizon en millions d'euros 2010 (M€2010)
#       
#     menage_echelle <- 
#       menage_echelle %>% 
#       mutate(CL_2010=dep_Solides/FC$A04)%>% 
#       mutate(Oil_2010=(carb_lubr+dep_Fuel+dep_GPL)/FC$A07)%>%
#       mutate(Gaz_2010=(dep_Gaz+dep_Urbain)/FC$A03)
#     
#     
#     # TCO horizon (€/tcO2)
#     TCO<-as.numeric(read_excel(path=paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""),range="C103",col_names=F))*10^6
#     
#     #Emissions  : € de TCO = €2010(dep) * tCO2/€2010(dep)*€(TCO)/tC02
#     menage_echelle <- 
#       menage_echelle %>% 
#       mutate(TCO_CL=CL_2010*coeff_CL_2010*TCO)%>%
#       mutate(TCO_Oil=Oil_2010*coeff_Oil_2010*TCO)%>%
#       mutate(TCO_Gaz=Gaz_2010*coeff_Gaz_2010*TCO)
#     
#     #Taxe carbone par ménage
#     menage_echelle$TCO_paid=rowSums(menage_echelle[c("TCO_CL","TCO_Oil","TCO_Gaz")])
#   
#     
#     tot_paid<-as.numeric(menage_echelle %>% summarise(sum(pondmen*TCO_paid)))
#   
#     tot_rtcd<-as.numeric(menage_echelle %>% summarise(sum(pondmen*rev_TCO)))
#     ratio_tco<-tot_rtcd/tot_paid-1
#     
#     # menage_echelle <- menage_echelle %>% mutate(rev_TCO=rev_TCO*ratio_tco)
#    
#   #
#   # TCO_rate<-cbind(TCO_rate_bis[1:4,2])
#   # TCO_rate<-as.data.frame(t(TCO_rate))
#   # colnames(TCO_rate)<-c("CL","Oil","Elec","Gas")
#   
#   
#   # menage_echelle <- 
#   #   menage_echelle %>%
#   #   mutate(TCO_paid=
#   #            TCO_rate$CL*(dep_Solides)+
#   #            TCO_rate$Oil*(carb_lubr+dep_Fuel+dep_GPL)+
#   #            TCO_rate$Elec*(dep_Elec)+
#   #            TCO_rate$Gas*(dep_Gaz+dep_Urbain))
#   
#   menage_echelle <- 
#     menage_echelle %>%
#     mutate(TCO_paid_pc=TCO_paid/RDB)%>%
#     mutate(TCO_rtcd_pc=rev_TCO/RDB)
#   
#   # menage_echelle <-
#     # menage_echelle %>%
#     # mutate_when(RDB==0,list(TCO_paid_pc=0))
#   
#   
#   
#   TCO_paid<-menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(weighted.mean(x=TCO_paid,w=pondmen,na.rm=T))
#   
#   
#   ## Poids moyen de la TCO dans le RDB des ménages
#   TCO_paid_pc_bis<-menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(weighted.mean(x=TCO_paid_pc,w=pondmen,na.rm=T))
#     # summarise(mean(x=TCO_paid_pc,na.rm=T))
#   ## Poids moyen de la TCO dans le RDB des ménages
#   # TCO_paid_pc_bis<-menage_echelle %>%
#     # group_by(get(var))%>%
#     # summarise(weighted.mean(x=TCO_paid_pc,w=pondmen,na.rm=T))
#   # 
#   
#   
#   ## attention : pourcentage de la TCO dans l'ensemble du décile sum(TCO_paid*pondmen)/sum(pondmen*RDB) et non pas le pourcentage moyen
#   TCO_paid_pc<- menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(sum(TCO_paid*pondmen)/sum(pondmen*RDB)) 
#   
#   
#    # /menage_echelle %>%
#     # group_by(get(var))%>%
#     # summarise(sum(pondmen*RDB))
#   
#   ## TCO rtcd moyenne
#   # TCO_rtcd_pc_bis<-menage_echelle %>%
#     # group_by(get(var))%>%
#     # summarise(Weighted.mean(x=TCO_rtcd_pc,w=pondmen,na.rm=T))
#   
#   TCO_rtcd_pc_bis<-menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(weightedMedian(x=TCO_rtcd_pc,w=pondmen,na.rm=T))
#   
#   
#   ## attention : pourcentage de la TCO dans l'ensemble du décile sum(TCO_paid*pondmen)/sum(pondmen*RDB) et non pas le pourcentage moyen
#   TCO_rtcd_pc<-menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(sum(rev_TCO*pondmen)/sum(pondmen*RDB))
#   
#   TCO_rtcd<-menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(weighted.mean(x=rev_TCO,w=pondmen,na.rm=T))
#   
#   TCO_net<-TCO_rtcd-TCO_paid
#   TCO_net_pc<-TCO_rtcd_pc-TCO_paid_pc
#   
#   df<-data.frame(TCO_paid[,1],TCO_paid[,2],TCO_paid_pc[,2],TCO_paid_pc_bis[,2],TCO_rtcd[,2],TCO_rtcd_pc[,2],TCO_rtcd_pc_bis[,2],TCO_net[,2],TCO_net_pc[,2])
#   colnames(df)<-c("var",
#                   "TCO_paid",
#                   "TCO_paid_pc",
#                   "TCO_paid_pc_bis",
#                   "TCO_rtcd",
#                   "TCO_rtcd_pc",
#                   "TCO_rtcd_pc_bis",
#                   "TCO_net",
#                   "TCO_net_pc"
#   )
#   
#   if(is.na(output)){
#     return(df)
#   }else{
#     return(df%>%select(var,output))
#   }
#   
#   # colnames(df)<-c("var",
#   #                 "TCO payée (€)",
#   #                 "TCO payée moyenne (en % du RDB)",
#   #                 "TCO rétrocédée (€)",
#   #                 "TCO rétrocédée moyenne (% du RDB)"
#   #                 )
#  
# }
# 
# 
# calc_TCO_euros_cst<-function(var,s,h,sc,r,Iter=NA,output=NA){
#   
#   if(s=="AMS"){scen=s}
#   if(s=="AME"){scen=s}
#   if(s=="ssTCO"){scen=s}
#   if(s=="AMS"){scen=s}
#   if(s=="AMS"){scen=s}
#   
#   
#  
#   
#   
#   EMS<-read_excel(path=paste("D:/CIRED/Projet_Ademe/IMACLIM/EMS.xlsx",sep=""),range=paste(s,"!B1:AF5",sep=""),col_names=T)
#   EMS<-EMS %>% gather(key=year,value=emission,-1)
#   colnames(EMS)<-c("Variable","year","emission")
#   
#   EMS<-EMS%>%filter(!Variable=="EMS_HH_2")
#   EMS #tonnes C02
#   
#   load("D:/CIRED/Projet_Ademe/2010/menage_forme_2010.RData")
#   
#   # if(!var %in% colnames(menage_forme_2010)){
#   #   load("D:/CIRED/Projet_Ademe/2010/appmen_depensesactiv_2010.RData")
#   #   menage_forme_2010<-
#   #     menage_forme_2010 %>%
#   #     left_join(appmen_depensesactiv_2010 %>% select(ident_men,var),by="ident_men")
#   # }
#   
#   # prix constant 2010 tCO2/ dépenses par cat
#   coeff_CL_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Variable=="EMS_HH_21_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
#                                                                                                                          summarise(sum(pondmen*dep_Solides))) # en tC02/Millions €
#   
#   coeff_Oil_2010<-as.numeric(EMS%>%filter(year==2010)%>%filter(Variable=="EMS_HH_22_2")%>%select(emission))/ as.numeric(menage_forme_2010 %>%
#                                                                                                                           summarise(sum(pondmen*(carb_lubr+dep_Fuel+dep_GPL)))) # en tC02/Millions €
#   
#   coeff_Gaz_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Variable=="EMS_HH_24_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
#                                                                                                                           summarise(sum(pondmen*(dep_Gaz+dep_Urbain)))) # en tC02/Millions €
#   
#   
#   # Load Horizon
#   if(is.na(Iter)){Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc & Redistribution==r)%>%select(Iter_finale))}
#   
#   load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
#   
#   if(!var %in% colnames(menage_echelle)){
#     menage_echelle<-
#       menage_echelle %>%
#       left_join(appmen_depensesactiv_2010 %>% select(ident_men,var),by="ident_men")}
#   if(Iter==0){
#     suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code_iter_",as.character(Iter),".xlsx",sep=""),skip=1))
#   }else{
#     suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",as.character(Iter),".xlsx",sep="")))}
#   
#   
#   output_macro <-  output_macro %>%  gather(key=year_model,value=value,-c(1:3))
#   
#   IMACLIM<-  output_macro  %>%  separate(col="year_model",into=c("year","model"),sep="_")
#   FC <- 
#     IMACLIM %>%
#     filter(year==h) %>%
#     filter(model=="IMACLIM")%>%
#     filter(Variable %in% c(
#       "A01",
#       "A02",
#       "A03",
#       "A04",
#       "A05",
#       "A06",
#       "A07",
#       "A08",
#       "A09",
#       "A10",
#       "A11",
#       "A12",
#       "A13")) %>%
#     select(Variable,value)
#   
#   FC <- FC %>%
#     mutate(value=as.numeric(value)) %>%
#     spread(key=Variable,value=value) 
#   
#   
#   
#   #Horizon CL, Oil, Gaz dépenses à l'horizon en millions d'euros 2010 (M€2010)
#   
#   menage_echelle <- 
#     menage_echelle %>% 
#     mutate(CL_2010=dep_Solides/FC$A04)%>% 
#     mutate(Oil_2010=(carb_lubr+dep_Fuel+dep_GPL)/FC$A07)%>%
#     mutate(Gaz_2010=(dep_Gaz+dep_Urbain)/FC$A03)
#   
#   
#   # TCO horizon (€/tcO2)
#   TCO<-as.numeric(read_excel(path=paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""),range="C103",col_names=F))*10^6
#   
#   #Emissions  : € de TCO = €2010(dep) * tCO2/€2010(dep)*€(TCO)/tC02
#   menage_echelle <- 
#     menage_echelle %>% 
#     mutate(TCO_CL=CL_2010*coeff_CL_2010*TCO)%>%
#     mutate(TCO_Oil=Oil_2010*coeff_Oil_2010*TCO)%>%
#     mutate(TCO_Gaz=Gaz_2010*coeff_Gaz_2010*TCO)
#   
#   #Taxe carbone par ménage
#   menage_echelle$TCO_paid=rowSums(menage_echelle[c("TCO_CL","TCO_Oil","TCO_Gaz")])
#   
#   IP<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="AJ47",col_names = F))/as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="R47",col_names = F))
#   
#   
#   menage_echelle<-
#     menage_echelle %>%
#     mutate(TCO_paid=TCO_paid/IP)%>%
#     mutate(rev_TCO=rev_TCO/IP)
#   
#   
#   tot_paid<-as.numeric(menage_echelle %>% summarise(sum(pondmen*TCO_paid)))
#   
#   tot_rtcd<-as.numeric(menage_echelle %>% summarise(sum(pondmen*rev_TCO)))
#   ratio_tco<-tot_rtcd/tot_paid-1
#   
#   # menage_echelle <- menage_echelle %>% mutate(rev_TCO=rev_TCO*ratio_tco)
#   
#   #
#   # TCO_rate<-cbind(TCO_rate_bis[1:4,2])
#   # TCO_rate<-as.data.frame(t(TCO_rate))
#   # colnames(TCO_rate)<-c("CL","Oil","Elec","Gas")
#   
#   
#   # menage_echelle <- 
#   #   menage_echelle %>%
#   #   mutate(TCO_paid=
#   #            TCO_rate$CL*(dep_Solides)+
#   #            TCO_rate$Oil*(carb_lubr+dep_Fuel+dep_GPL)+
#   #            TCO_rate$Elec*(dep_Elec)+
#   #            TCO_rate$Gas*(dep_Gaz+dep_Urbain))
#   
#   menage_echelle <- 
#     menage_echelle %>%
#     mutate(TCO_paid_pc=TCO_paid/RDB)%>%
#     mutate(TCO_rtcd_pc=rev_TCO/RDB)
#   
#   # menage_echelle <-
#   # menage_echelle %>%
#   # mutate_when(RDB==0,list(TCO_paid_pc=0))
#   
#   
#   
#   TCO_paid<-menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(weighted.mean(x=TCO_paid,w=pondmen,na.rm=T))
#   
#   
#   ## Poids moyen de la TCO dans le RDB des ménages
#   TCO_paid_pc_bis<-menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(weighted.mean(x=TCO_paid_pc,w=pondmen,na.rm=T))
#   # summarise(mean(x=TCO_paid_pc,na.rm=T))
#   ## Poids moyen de la TCO dans le RDB des ménages
#   # TCO_paid_pc_bis<-menage_echelle %>%
#   # group_by(get(var))%>%
#   # summarise(weighted.mean(x=TCO_paid_pc,w=pondmen,na.rm=T))
#   # 
#   
#   
#   ## attention : pourcentage de la TCO dans l'ensemble du décile sum(TCO_paid*pondmen)/sum(pondmen*RDB) et non pas le pourcentage moyen
#   TCO_paid_pc<- menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(sum(TCO_paid*pondmen)/sum(pondmen*RDB)) 
#   
#   
#   # /menage_echelle %>%
#   # group_by(get(var))%>%
#   # summarise(sum(pondmen*RDB))
#   
#   ## TCO rtcd moyenne
#   # TCO_rtcd_pc_bis<-menage_echelle %>%
#   # group_by(get(var))%>%
#   # summarise(Weighted.mean(x=TCO_rtcd_pc,w=pondmen,na.rm=T))
#   
#   TCO_rtcd_pc_bis<-menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(weightedMedian(x=TCO_rtcd_pc,w=pondmen,na.rm=T))
#   
#   
#   ## attention : pourcentage de la TCO dans l'ensemble du décile sum(TCO_paid*pondmen)/sum(pondmen*RDB) et non pas le pourcentage moyen
#   TCO_rtcd_pc<-menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(sum(rev_TCO*pondmen)/sum(pondmen*RDB))
#   
#   TCO_rtcd<-menage_echelle %>%
#     group_by(get(var))%>%
#     summarise(weighted.mean(x=rev_TCO,w=pondmen,na.rm=T))
#   
#   TCO_net<-TCO_rtcd-TCO_paid
#   TCO_net_pc<-TCO_rtcd_pc-TCO_paid_pc
#   
#   df<-data.frame(TCO_paid[,1],TCO_paid[,2],TCO_paid_pc[,2],TCO_paid_pc_bis[,2],TCO_rtcd[,2],TCO_rtcd_pc[,2],TCO_rtcd_pc_bis[,2],TCO_net[,2],TCO_net_pc[,2])
#   colnames(df)<-c("var",
#                   "TCO_paid",
#                   "TCO_paid_pc",
#                   "TCO_paid_pc_bis",
#                   "TCO_rtcd",
#                   "TCO_rtcd_pc",
#                   "TCO_rtcd_pc_bis",
#                   "TCO_net",
#                   "TCO_net_pc"
#   )
#   
#   if(is.na(output)){
#     return(df)
#   }else{
#     return(df%>%select(var,output))
#   }
#   
#   # colnames(df)<-c("var",
#   #                 "TCO payée (€)",
#   #                 "TCO payée moyenne (en % du RDB)",
#   #                 "TCO rétrocédée (€)",
#   #                 "TCO rétrocédée moyenne (% du RDB)"
#   #                 )
#   
# }
# 
# 
# 
# calc_TCO_all<-function(s,h,sc,r,Iter=NA){
#   
#   Iterations<-read_excel("D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown.xlsx")
#   EMS<-read_excel(path=paste("D:/CIRED/Projet_Ademe/IMACLIM/EMS.xlsx",sep=""),range=paste(s,"!B1:AF5",sep=""),col_names=T)
#   EMS<-EMS %>% gather(key=year,value=emission,-1)%>%filter(!Var=="EMS_HH_2")
#   EMS #tonnes C02
#   
#   load("D:/CIRED/Projet_Ademe/2010/menage_forme_2010.RData")
#   
# 
#   # prix constant 2010 tCO2/ dépenses par cat
#   coeff_CL_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_21_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
#                                                                                                                     summarise(sum(pondmen*dep_Solides))) # en tC02/Millions €
#   
#   coeff_Oil_2010<-as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_22_2")%>%select(emission))/ as.numeric(menage_forme_2010 %>%
#                                                                                                                      summarise(sum(pondmen*(carb_lubr+dep_Fuel+dep_GPL)))) # en tC02/Millions €
#   
#   coeff_Gaz_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_24_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
#                                                                                                                      summarise(sum(pondmen*(dep_Gaz+dep_Urbain)))) # en tC02/Millions €
#   
#   
#   # Load Horizon
#   if(is.na(Iter)){Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc & Redistribution==r)%>%select(Iter_finale))}
#   
#   load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
#   
#   
#   if(Iter==0){
#     suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code_iter_",as.character(Iter),".xlsx",sep=""),skip=1))
#   }else{
#     suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",as.character(Iter),".xlsx",sep="")))}
#   
#   
#   output_macro <-  output_macro %>%  gather(key=year_model,value=value,-c(1:3))
#   
#   IMACLIM<-  output_macro  %>%  separate(col="year_model",into=c("year","model"),sep="_")
#   FC <- 
#     IMACLIM %>%
#     filter(year==h) %>%
#     filter(model=="IMACLIM")%>%
#     filter(Variable %in% c(
#       "A01",
#       "A02",
#       "A03",
#       "A04",
#       "A05",
#       "A06",
#       "A07",
#       "A08",
#       "A09",
#       "A10",
#       "A11",
#       "A12",
#       "A13")) %>%
#     select(Variable,value)
#   
#   FC <- FC %>%
#     mutate(value=as.numeric(value)) %>%
#     spread(key=Variable,value=value) 
#   
#   
#   
#   #Horizon CL, Oil, Gaz dépenses à l'horizon en millions d'euros 2010 (M€2010)
#   
#   menage_echelle <- 
#     menage_echelle %>% 
#     mutate(CL_2010=dep_Solides/FC$A04)%>% 
#     mutate(Oil_2010=(carb_lubr+dep_Fuel+dep_GPL)/FC$A07)%>%
#     mutate(Gaz_2010=(dep_Gaz+dep_Urbain)/FC$A03)
#   
#   
#   # TCO horizon (€/tcO2)
#   TCO<-as.numeric(read_excel(path=paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""),range="C103",col_names=F))*10^6
#   
#   print(paste("Taxe_carbone : ",TCO, "€/tonne CO2"))
#   
#   #Emissions  : € de TCO = €2010(dep) * tCO2/€2010(dep)*€(TCO)/tC02
#   menage_echelle <- 
#     menage_echelle %>% 
#     mutate(TCO_CL=CL_2010*coeff_CL_2010*TCO)%>%
#     mutate(TCO_Oil=Oil_2010*coeff_Oil_2010*TCO)%>%
#     mutate(TCO_Gaz=Gaz_2010*coeff_Gaz_2010*TCO)
#   
#   #Taxe carbone par ménage
#   menage_echelle$TCO_paid=rowSums(menage_echelle[c("TCO_CL","TCO_Oil","TCO_Gaz")])
#   
#   
#   tot_paid<-as.numeric(menage_echelle %>% summarise(sum(pondmen*TCO_paid)))
#   print(paste("TCO payée : ",tot_paid,"€"))
#   tot_rtcd<-as.numeric(menage_echelle %>% summarise(sum(pondmen*rev_TCO)))
#   print(paste("TCO rétrocédée : ",tot_rtcd,"€"))
#   ratio_tco<-tot_paid/tot_rtcd
#   print(paste("ratio : ",ratio_tco))
#   
#   # menage_echelle <- menage_echelle %>% mutate(rev_TCO=rev_TCO*ratio_tco)
#   
#   #
#   # TCO_rate<-cbind(TCO_rate_bis[1:4,2])
#   # TCO_rate<-as.data.frame(t(TCO_rate))
#   # colnames(TCO_rate)<-c("CL","Oil","Elec","Gas")
#   
#   
#   # menage_echelle <- 
#   #   menage_echelle %>%
#   #   mutate(TCO_paid=
#   #            TCO_rate$CL*(dep_Solides)+
#   #            TCO_rate$Oil*(carb_lubr+dep_Fuel+dep_GPL)+
#   #            TCO_rate$Elec*(dep_Elec)+
#   #            TCO_rate$Gas*(dep_Gaz+dep_Urbain))
#   
#   # menage_echelle <- 
#     # menage_echelle %>%
#     # mutate(TCO_paid_pc=TCO_paid/RDB)%>%
#     # mutate(TCO_rtcd_pc=rev_TCO/RDB)
#   
#   # return(menage_echelle)
# }
# 


calc_TCO_all_new<-function(s,h,sc,r,Iter=NA){
  
  Iterations<-read_excel("D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown.xlsx")
  EMS<-read_excel(path=paste("D:/CIRED/Projet_Ademe/IMACLIM/EMS.xlsx",sep=""),range=paste(s,"!B1:AF5",sep=""),col_names=T)
  EMS<-EMS %>% gather(key=year,value=emission,-1)%>%filter(!Var=="EMS_HH_2")
  EMS #tonnes C02
  
  load("D:/CIRED/Projet_Ademe/2010/menage_forme_2010.RData")
  
  
  # prix constant 2010 tCO2/ dépenses par cat
  coeff_CL_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_21_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
                                                                                                                    summarise(sum(pondmen*dep_Solides))) # en tC02/Millions €
  
  coeff_Oil_2010<-as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_22_2")%>%select(emission))/ as.numeric(menage_forme_2010 %>%
                                                                                                                     summarise(sum(pondmen*(carb_lubr+dep_Fuel+dep_GPL)))) # en tC02/Millions €
  
  coeff_Gaz_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_24_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
                                                                                                                     summarise(sum(pondmen*(dep_Gaz+dep_Urbain)))) # en tC02/Millions €
  
  
  # Load Horizon
  if(is.na(Iter)){Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc & Redistribution==r)%>%select(Iter_finale))}
  
  load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
  
  
  if(Iter==0){
    suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code_iter_",as.character(Iter),".xlsx",sep=""),skip=1))
  }else{
    suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",as.character(Iter),".xlsx",sep="")))}
  
  
  output_macro <-  output_macro %>%  gather(key=year_model,value=value,-c(1:3))
  
  IMACLIM<-  output_macro  %>%  separate(col="year_model",into=c("year","model"),sep="_")
  FC <- 
    IMACLIM %>%
    filter(year==h) %>%
    filter(model=="IMACLIM")%>%
    filter(Variable %in% c(
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
      "A13")) %>%
    select(Variable,value)
  
  FC <- FC %>%
    mutate(value=as.numeric(value)) %>%
    spread(key=Variable,value=value) 
  
  
  
  #Horizon CL, Oil, Gaz dépenses à l'horizon en millions d'euros 2010 (M€2010)
  
  menage_echelle <- 
    menage_echelle %>% 
    mutate(CL_2010=dep_Solides/FC$A04)%>% 
    mutate(Oil_2010=(carb_lubr+dep_Fuel+dep_GPL)/FC$A07)%>%
    mutate(Gaz_2010=(dep_Gaz+dep_Urbain)/FC$A03)
  
  
  # TCO horizon (€/tcO2)
  TCO<-as.numeric(read_excel(path=paste("D:/CIRED/Projet_Ademe/Results/",s,"/",h,"/",sc,"/",r,"/IMACLIM_3ME.xlsx",sep=""),range="C103",col_names=F))*10^6
  
  print(paste("Taxe_carbone : ",TCO, "€/tonne CO2"))
  
  #evolution coeff C02/€
  coeff_dep_ems<-read_csv("D:/CIRED/Projet_Ademe/IMACLIM/coeff_dep_ems.csv")
  TC_coeff <- 
    coeff_dep_ems %>%
    filter(year==h)%>%
    filter(scenario==s)%>%
    select(Variable,TC_coeff_emission)%>%
    spread(key=Variable,value=TC_coeff_emission)
  
  #Emissions  : € de TCO = €2010(dep) * tCO2/€2010(dep)*€(TCO)/tC02
  menage_echelle <- 
    menage_echelle %>% 
    mutate(TCO_CL=CL_2010*coeff_CL_2010*TC_coeff$C21*TCO)%>%
    mutate(TCO_Oil=Oil_2010*coeff_Oil_2010*TC_coeff$C22*TCO)%>%
    mutate(TCO_Gaz=Gaz_2010*coeff_Gaz_2010*TC_coeff$C24*TCO)
  
  #Taxe carbone par ménage
  menage_echelle$TCO_paid=rowSums(menage_echelle[c("TCO_CL","TCO_Oil","TCO_Gaz")])
  
  
  tot_paid<-as.numeric(menage_echelle %>% summarise(sum(pondmen*TCO_paid)))
  print(paste("TCO payée : ",tot_paid,"€"))
  tot_rtcd<-as.numeric(menage_echelle %>% summarise(sum(pondmen*rev_TCO)))
  print(paste("TCO rétrocédée : ",tot_rtcd,"€"))
  ratio_tco<-tot_paid/tot_rtcd
  print(paste("ratio : ",ratio_tco))
  
  # menage_echelle <- menage_echelle %>% mutate(rev_TCO=rev_TCO*ratio_tco)
  
  #
  # TCO_rate<-cbind(TCO_rate_bis[1:4,2])
  # TCO_rate<-as.data.frame(t(TCO_rate))
  # colnames(TCO_rate)<-c("CL","Oil","Elec","Gas")
  
  
  # menage_echelle <- 
  #   menage_echelle %>%
  #   mutate(TCO_paid=
  #            TCO_rate$CL*(dep_Solides)+
  #            TCO_rate$Oil*(carb_lubr+dep_Fuel+dep_GPL)+
  #            TCO_rate$Elec*(dep_Elec)+
  #            TCO_rate$Gas*(dep_Gaz+dep_Urbain))
  
  # menage_echelle <- 
  # menage_echelle %>%
  # mutate(TCO_paid_pc=TCO_paid/RDB)%>%
  # mutate(TCO_rtcd_pc=rev_TCO/RDB)
  
  # return(menage_echelle)
}


calc_TCO_all_new_var<-function(s,h,sc,r,var,Iter=NA){
  
  Iterations<-read_excel("D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown.xlsx")
  EMS<-read_excel(path=paste("D:/CIRED/Projet_Ademe/IMACLIM/EMS.xlsx",sep=""),range=paste(s,"!B1:AF5",sep=""),col_names=T)
  EMS<-EMS %>% gather(key=year,value=emission,-1)%>%filter(!Var=="EMS_HH_2")
  EMS #tonnes C02
  
  load("D:/CIRED/Projet_Ademe/2010/menage_forme_2010.RData")
  
  
  # prix constant 2010 tCO2/ dépenses par cat
  coeff_CL_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_21_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
                                                                                                                    summarise(sum(pondmen*dep_Solides))) # en tC02/Millions €
  
  coeff_Oil_2010<-as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_22_2")%>%select(emission))/ as.numeric(menage_forme_2010 %>%
                                                                                                                     summarise(sum(pondmen*(carb_lubr+dep_Fuel+dep_GPL)))) # en tC02/Millions €
  
  coeff_Gaz_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_24_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
                                                                                                                     summarise(sum(pondmen*(dep_Gaz+dep_Urbain)))) # en tC02/Millions €
  
  
  # Load Horizon
  if(is.na(Iter)){Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc & Redistribution==r)%>%select(Iter_finale))}
  
  load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
  
  
  if(Iter==0){
    suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code_iter_",as.character(Iter),".xlsx",sep=""),skip=1))
  }else{
    suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",as.character(Iter),".xlsx",sep="")))}
  
  
  output_macro <-  output_macro %>%  gather(key=year_model,value=value,-c(1:3))
  
  IMACLIM<-  output_macro  %>%  separate(col="year_model",into=c("year","model"),sep="_")
  FC <- 
    IMACLIM %>%
    filter(year==h) %>%
    filter(model=="IMACLIM")%>%
    filter(Variable %in% c(
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
      "A13")) %>%
    select(Variable,value)
  
  FC <- FC %>%
    mutate(value=as.numeric(value)) %>%
    spread(key=Variable,value=value) 
  
  
  
  #Horizon CL, Oil, Gaz dépenses à l'horizon en millions d'euros 2010 (M€2010)
  
  menage_echelle <- 
    menage_echelle %>% 
    mutate(CL_2010=dep_Solides/FC$A04)%>% 
    mutate(Oil_2010=(carb_lubr+dep_Fuel+dep_GPL)/FC$A07)%>%
    mutate(Gaz_2010=(dep_Gaz+dep_Urbain)/FC$A03)
  
  
  # TCO horizon (€/tcO2)
  
  # TCO<-as.numeric(read_excel(path=paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""),range="C103",col_names=F))*10^6
  
  TCO<-as.numeric(read_excel(path=paste("D:/CIRED/Projet_Ademe/Results/",s,"/",h,"/",sc,"/",r,"/IMACLIM_3ME.xlsx",sep=""),range="C103",col_names=F))*10^6
  
  
  # print(paste("Taxe_carbone : ",TCO, "€/tonne CO2"))
  
  #evolution coeff C02/€
  coeff_dep_ems<-read_csv("D:/CIRED/Projet_Ademe/IMACLIM/coeff_dep_ems.csv")
  TC_coeff <- 
    coeff_dep_ems %>%
    filter(year==h)%>%
    filter(scenario==s)%>%
    select(Variable,TC_coeff_emission)%>%
    spread(key=Variable,value=TC_coeff_emission)
  
  #Emissions  : € de TCO = €2010(dep) * tCO2/€2010(dep)*€(TCO)/tC02
  menage_echelle <- 
    menage_echelle %>% 
    mutate(TCO_CL=CL_2010*coeff_CL_2010*TC_coeff$C21*TCO)%>%
    mutate(TCO_Oil=Oil_2010*coeff_Oil_2010*TC_coeff$C22*TCO)%>%
    mutate(TCO_Gaz=Gaz_2010*coeff_Gaz_2010*TC_coeff$C24*TCO)
  
  #Taxe carbone par ménage
  menage_echelle$TCO_paid=rowSums(menage_echelle[c("TCO_CL","TCO_Oil","TCO_Gaz")])
  
  #mettre à l'échelle de 2020
if(h==2025){  
  IP<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="AI47",col_names = F))/as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="R47",col_names = F))}
  if(h==2030){  
    IP<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="AH47",col_names = F))/as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="R47",col_names = F))}
  if(h==2035){  
    IP<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="AJ47",col_names = F))/as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="R47",col_names = F))}
  


  bdd<-menage_echelle
  
  bdd$rev_tot<-rowSums(bdd[c("rev_activites","rev_sociaux","rev_patrimoine","rev700","rev701","rev999","rev_TCO")])
    ## Poids moyen de la TCO dans le RDB des ménages
    bdd<-
      bdd %>%
      mutate(TCO_paid_pc=TCO_paid/rev_tot)

    bdd<- bdd %>% mutate_when(RDB==0,list(TCO_paid_pc=NA))
    #
    TCO_paid_pc<-bdd %>%
      group_by(get(var))%>%
      summarise(weighted.mean(x=TCO_paid_pc,w=pondmen,na.rm=T))


    TCO_paid<-bdd %>%
      group_by(get(var))%>%
      summarise(weighted.mean(x=TCO_paid/IP,w=pondmen,na.rm=T))

    TCO_paid_agg<-bdd %>%
      group_by(get(var))%>%
      summarise(sum(TCO_paid*pondmen)/sum(pondmen)/IP)

    ## attention : pourcentage de la TCO dans l'ensemble du décile sum(TCO_paid*pondmen)/sum(pondmen*RDB) et non pas le pourcentage moyen
    TCO_paid_pc_agg<-bdd %>%
      group_by(get(var))%>%
      summarise(sum(TCO_paid*pondmen)/sum(pondmen*rev_tot))
    
    TCO_paid_pc_agg<-bdd %>%
      group_by(get(var))%>%
      summarise(sum(TCO_paid*pondmen)/sum(pondmen*rev_tot))
    
    rev_TCO_agg<- bdd %>%
      group_by(get(var))%>%
      summarise(sum(rev_TCO*pondmen)/sum(pondmen)/IP)
    
    rev_TCO<- bdd %>%
      group_by(get(var))%>%
      summarise(weighted.mean(x=rev_TCO/IP,w=pondmen,na.rm=T))
    
    TCO_net_agg <- TCO_paid_agg[,2] - rev_TCO_agg[,2]
    TCO_net<- bdd %>%
      group_by(get(var))%>%
      summarise(weighted.mean(x=(TCO_paid-rev_TCO)/IP,w=pondmen,na.rm=T))


    R<-data.frame(TCO_paid_pc_agg[,1],TCO_paid_pc_agg[,2],TCO_paid_pc[,2],TCO_paid_agg[,2],TCO_paid[,2],rev_TCO_agg[,2],TCO_net_agg,rev_TCO[,2],TCO_net[,2])
    
    colnames(R)<-c(var,"TCO_paid_pc_agg","TCO_paid_pc","TCO_paid_agg","TCO_paid","rev_TCO_agg","TCO_net_agg","rev_TCO","TCO_net")

    # R
    

    return(R)
  
}


calc_TCO_perdants<-function(s,h,sc,r,var,Iter=NA){
  
  Iterations<-read_excel("D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown.xlsx")
  EMS<-read_excel(path=paste("D:/CIRED/Projet_Ademe/IMACLIM/EMS.xlsx",sep=""),range=paste(s,"!B1:AF5",sep=""),col_names=T)
  EMS<-EMS %>% gather(key=year,value=emission,-1)%>%filter(!Var=="EMS_HH_2")
  EMS #tonnes C02
  
  load("D:/CIRED/Projet_Ademe/2010/menage_forme_2010.RData")
  
  
  # prix constant 2010 tCO2/ dépenses par cat
  coeff_CL_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_21_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
                                                                                                                    summarise(sum(pondmen*dep_Solides))) # en tC02/Millions €
  
  coeff_Oil_2010<-as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_22_2")%>%select(emission))/ as.numeric(menage_forme_2010 %>%
                                                                                                                     summarise(sum(pondmen*(carb_lubr+dep_Fuel+dep_GPL)))) # en tC02/Millions €
  
  coeff_Gaz_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_24_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
                                                                                                                     summarise(sum(pondmen*(dep_Gaz+dep_Urbain)))) # en tC02/Millions €
  
  
  # Load Horizon
  if(is.na(Iter)){Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc & Redistribution==r)%>%select(Iter_finale))}
  
  load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
  
  
  if(Iter==0){
    suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code_iter_",as.character(Iter),".xlsx",sep=""),skip=1))
  }else{
    suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",as.character(Iter),".xlsx",sep="")))}
  
  
  output_macro <-  output_macro %>%  gather(key=year_model,value=value,-c(1:3))
  
  IMACLIM<-  output_macro  %>%  separate(col="year_model",into=c("year","model"),sep="_")
  FC <- 
    IMACLIM %>%
    filter(year==h) %>%
    filter(model=="IMACLIM")%>%
    filter(Variable %in% c(
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
      "A13")) %>%
    select(Variable,value)
  
  FC <- FC %>%
    mutate(value=as.numeric(value)) %>%
    spread(key=Variable,value=value) 
  
  
  
  #Horizon CL, Oil, Gaz dépenses à l'horizon en millions d'euros 2010 (M€2010)
  
  menage_echelle <- 
    menage_echelle %>% 
    mutate(CL_2010=dep_Solides/FC$A04)%>% 
    mutate(Oil_2010=(carb_lubr+dep_Fuel+dep_GPL)/FC$A07)%>%
    mutate(Gaz_2010=(dep_Gaz+dep_Urbain)/FC$A03)
  
  
  # TCO horizon (€/tcO2)
  
  # TCO<-as.numeric(read_excel(path=paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""),range="C103",col_names=F))*10^6
  
  TCO<-as.numeric(read_excel(path=paste("D:/CIRED/Projet_Ademe/Results/",s,"/",h,"/",sc,"/",r,"/IMACLIM_3ME.xlsx",sep=""),range="C103",col_names=F))*10^6
  
  
  # print(paste("Taxe_carbone : ",TCO, "€/tonne CO2"))
  
  #evolution coeff C02/€
  coeff_dep_ems<-read_csv("D:/CIRED/Projet_Ademe/IMACLIM/coeff_dep_ems.csv")
  TC_coeff <- 
    coeff_dep_ems %>%
    filter(year==h)%>%
    filter(scenario==s)%>%
    select(Variable,TC_coeff_emission)%>%
    spread(key=Variable,value=TC_coeff_emission)
  
  #Emissions  : € de TCO = €2010(dep) * tCO2/€2010(dep)*€(TCO)/tC02
  menage_echelle <- 
    menage_echelle %>% 
    mutate(TCO_CL=CL_2010*coeff_CL_2010*TC_coeff$C21*TCO)%>%
    mutate(TCO_Oil=Oil_2010*coeff_Oil_2010*TC_coeff$C22*TCO)%>%
    mutate(TCO_Gaz=Gaz_2010*coeff_Gaz_2010*TC_coeff$C24*TCO)
  
  #Taxe carbone par ménage
  menage_echelle$TCO_paid=rowSums(menage_echelle[c("TCO_CL","TCO_Oil","TCO_Gaz")])
  
  #mettre à l'échelle de 2020
  if(h==2025){  
    IP<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="AI47",col_names = F))/as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="R47",col_names = F))}
  if(h==2030){  
    IP<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="AH47",col_names = F))/as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="R47",col_names = F))}
  if(h==2035){  
    IP<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="AJ47",col_names = F))/as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range="R47",col_names = F))}
  
  

  bdd<-menage_echelle %>% mutate(perdant=FALSE)
  bdd <- 
    bdd %>% 
    mutate_when(TCO_paid>=rev_TCO,list(perdant=TRUE))
  
  if(var=="tuu"){
    
    bdd["tuu"] <- data.frame(lapply(bdd["tuu"],as.double),stringsAsFactors = FALSE)
    R1<-(bdd %>% group_by(tuu) %>% filter(perdant) %>% summarise(sum(pondmen)))/bdd%>% group_by(tuu) %>% summarise(sum(pondmen))
    R2<-bdd %>% group_by(tuu) %>% filter(perdant) %>% summarise(sum(pondmen)) 
  }else{
  
  R1<-bdd %>% group_by(get(var)) %>% filter(perdant) %>% summarise(sum(pondmen))/bdd%>% group_by(get(var)) %>% summarise(sum(pondmen))
  R2<-bdd %>% group_by(get(var)) %>% filter(perdant) %>% summarise(sum(pondmen))}
  
  
  R<-data.frame(R2[,1],R1[,2])
  
  colnames(R)<-c(var,"perdants")
  
  # R
  
  
  return(R)
  
}



emissions_all<-function(s,h,sc,r,Iter=NA){
  
  
  Iterations<-read_excel("D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown.xlsx")
  EMS<-read_excel(path=paste("D:/CIRED/Projet_Ademe/IMACLIM/EMS.xlsx",sep=""),range=paste(s,"!B1:AF5",sep=""),col_names=T)
  EMS<-EMS %>% gather(key=year,value=emission,-1)%>%filter(!Var=="EMS_HH_2")
  EMS #tonnes C02
  
  load("D:/CIRED/Projet_Ademe/2010/menage_forme_2010.RData")
  
  
  # prix constant 2010 tCO2/ dépenses par cat
  coeff_CL_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_21_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
                                                                                                                    summarise(sum(pondmen*dep_Solides))) # en tC02/Millions €
  
  coeff_Oil_2010<-as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_22_2")%>%select(emission))/ as.numeric(menage_forme_2010 %>%
                                                                                                                     summarise(sum(pondmen*(carb_lubr+dep_Fuel+dep_GPL)))) # en tC02/Millions €
  
  coeff_Gaz_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_24_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
                                                                                                                     summarise(sum(pondmen*(dep_Gaz+dep_Urbain)))) # en tC02/Millions €
  
  
  # Load Horizon
  if(is.na(Iter)){Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc & Redistribution==r)%>%select(Iter_finale))}
  
  load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
  
  
  if(Iter==0){
    suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code_iter_",as.character(Iter),".xlsx",sep=""),skip=1))
  }else{
    suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",as.character(Iter),".xlsx",sep="")))}
  
  
  output_macro <-  output_macro %>%  gather(key=year_model,value=value,-c(1:3))
  
  IMACLIM<-  output_macro  %>%  separate(col="year_model",into=c("year","model"),sep="_")
  FC <- 
    IMACLIM %>%
    filter(year==h) %>%
    filter(model=="IMACLIM")%>%
    filter(Variable %in% c(
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
      "A13")) %>%
    select(Variable,value)
  
  FC <- FC %>%
    mutate(value=as.numeric(value)) %>%
    spread(key=Variable,value=value) 
  
  
  
  #Horizon CL, Oil, Gaz dépenses à l'horizon en millions d'euros 2010 (M€2010)
  
  menage_echelle <- 
    menage_echelle %>% 
    mutate(CL_2010=dep_Solides/FC$A04)%>% 
    mutate(Oil_2010=(carb_lubr+dep_Fuel+dep_GPL)/FC$A07)%>%
    mutate(Gaz_2010=(dep_Gaz+dep_Urbain)/FC$A03)
  
  
  # TCO horizon (€/tcO2)
  # TCO<-as.numeric(read_excel(path=paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""),range="C103",col_names=F))*10^6
  # 
  # print(paste("Taxe_carbone : ",TCO, "€/tonne CO2"))
  
  #Emissions  : € de TCO = €2010(dep) * tCO2/€2010(dep)*€(TCO)/tC02
  menage_echelle <- 
    menage_echelle %>% 
    mutate(Em_CL=CL_2010*coeff_CL_2010)%>%
    mutate(Em_Oil=Oil_2010*coeff_Oil_2010)%>%
    mutate(Em_Gaz=Gaz_2010*coeff_Gaz_2010)
  
  menage_echelle <- menage_echelle %>% mutate(emissions=Em_CL+Em_Oil+Em_Gaz)
  
  #Taxe carbone par ménage
 
  Emissions<-as.numeric(menage_echelle %>% summarise(sum(pondmen*emissions)))
  return(Emissions)

}


emissions_var<-function(s,h,sc,r,Iter=NA,output="decucn"){
  
  EM_2010<-emissions_2010()
  
  # detach("package:plyr")
  Iterations<-read_excel("D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown.xlsx")
  EMS<-read_excel(path=paste("D:/CIRED/Projet_Ademe/IMACLIM/EMS.xlsx",sep=""),range=paste(s,"!B1:AF5",sep=""),col_names=T)
  EMS<-EMS %>% gather(key=year,value=emission,-1)%>%filter(!Var=="EMS_HH_2")
  EMS #tonnes C02
  
  load("D:/CIRED/Projet_Ademe/2010/menage_forme_2010.RData")
  
  
  # prix constant 2010 tCO2/ dépenses par cat
  coeff_CL_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_21_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
                                                                                                                    summarise(sum(pondmen*dep_Solides))) # en tC02/Millions €
  
  coeff_Oil_2010<-as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_22_2")%>%select(emission))/ as.numeric(menage_forme_2010 %>%
                                                                                                                     summarise(sum(pondmen*(carb_lubr+dep_Fuel+dep_GPL)))) # en tC02/Millions €
  
  coeff_Gaz_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_24_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
                                                                                                                     summarise(sum(pondmen*(dep_Gaz+dep_Urbain)))) # en tC02/Millions €
  
  
  # Load Horizon
  if(is.na(Iter)){Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc & Redistribution==r)%>%select(Iter_finale))}
  
  load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
  
  
  if(Iter==0){
    suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code_iter_",as.character(Iter),".xlsx",sep=""),skip=1))
  }else{
    suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",as.character(Iter),".xlsx",sep="")))}
  
  
  output_macro <-  output_macro %>%  gather(key=year_model,value=value,-c(1:3))
  
  IMACLIM<-  output_macro  %>%  separate(col="year_model",into=c("year","model"),sep="_")
  FC <- 
    IMACLIM %>%
    filter(year==h) %>%
    filter(model=="IMACLIM")%>%
    filter(Variable %in% c(
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
      "A13")) %>%
    select(Variable,value)
  
  FC <- FC %>%
    mutate(value=as.numeric(value)) %>%
    spread(key=Variable,value=value) 
  
  
  
  #Horizon CL, Oil, Gaz dépenses à l'horizon en millions d'euros 2010 (M€2010)
  
  menage_echelle <- 
    menage_echelle %>%
    mutate(CL_2010=dep_Solides/FC$A04)%>% 
    mutate(Oil_2010=(carb_lubr+dep_Fuel+dep_GPL)/FC$A07)%>%
    mutate(Gaz_2010=(dep_Gaz+dep_Urbain)/FC$A03)
  
  
  # TCO horizon (€/tcO2)
  # TCO<-as.numeric(read_excel(path=paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""),range="C103",col_names=F))*10^6
  # 
  # print(paste("Taxe_carbone : ",TCO, "€/tonne CO2"))
  
  #Emissions  : € de TCO = €2010(dep) * tCO2/€2010(dep)*€(TCO)/tC02
  menage_echelle <- 
    menage_echelle %>% 
    mutate(Em_CL=CL_2010*coeff_CL_2010)%>%
    mutate(Em_Oil=Oil_2010*coeff_Oil_2010)%>%
    mutate(Em_Gaz=Gaz_2010*coeff_Gaz_2010)
  
  menage_echelle <- menage_echelle %>% mutate(emissions=Em_CL+Em_Oil+Em_Gaz)
  
  #Taxe carbone par ménage
  Emissions1<-menage_echelle %>%group_by(get(output))%>%summarise(sum(pondmen*emissions))
  
  Emissions2<-Emissions1/as.numeric(menage_echelle%>%summarise(sum(pondmen*emissions)))
  
  
  EM<-cbind(Emissions1[,1],(Emissions1[,2]-EM_2010)/EM_2010)
  
  
  
  return(EM)
}


emissions_2010<-function(){
  EMS<-read_excel(path=paste("D:/CIRED/Projet_Ademe/IMACLIM/EMS.xlsx",sep=""),range=paste(s,"!B1:AF5",sep=""),col_names=T)
  EMS<-EMS %>% gather(key=year,value=emission,-1)%>%filter(!Var=="EMS_HH_2")
  EMS #tonnes C02
  
  load("D:/CIRED/Projet_Ademe/2010/menage_forme_2010.RData")
  
  
  # prix constant 2010 tCO2/ dépenses par cat
  coeff_CL_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_21_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
                                                                                                                    summarise(sum(pondmen*dep_Solides))) # en tC02/Millions €
  
  coeff_Oil_2010<-as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_22_2")%>%select(emission))/ as.numeric(menage_forme_2010 %>%
                                                                                                                     summarise(sum(pondmen*(carb_lubr+dep_Fuel+dep_GPL)))) # en tC02/Millions €
  
  coeff_Gaz_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_24_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
                                                                                                                     summarise(sum(pondmen*(dep_Gaz+dep_Urbain)))) # en tC02/Millions €
  
  menage_echelle<-menage_forme_2010
 
  
  
  
  #Horizon CL, Oil, Gaz dépenses à l'horizon en millions d'euros 2010 (M€2010)
  
  menage_echelle <- 
    menage_echelle %>%
    mutate(CL_2010=dep_Solides)%>% 
    mutate(Oil_2010=(carb_lubr+dep_Fuel+dep_GPL))%>%
    mutate(Gaz_2010=(dep_Gaz+dep_Urbain))
  
  
  # TCO horizon (€/tcO2)
  # TCO<-as.numeric(read_excel(path=paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""),range="C103",col_names=F))*10^6
  # 
  # print(paste("Taxe_carbone : ",TCO, "€/tonne CO2"))
  
  #Emissions  : € de TCO = €2010(dep) * tCO2/€2010(dep)*€(TCO)/tC02
  menage_echelle <- 
    menage_echelle %>% 
    mutate(Em_CL=CL_2010*coeff_CL_2010)%>%
    mutate(Em_Oil=Oil_2010*coeff_Oil_2010)%>%
    mutate(Em_Gaz=Gaz_2010*coeff_Gaz_2010)
  
  menage_echelle <- menage_echelle %>% mutate(emissions=Em_CL+Em_Oil+Em_Gaz)
  
  #Taxe carbone par ménage
  Emissions1<-menage_echelle %>%group_by(get(var))%>%summarise(sum(pondmen*emissions))
  
  Emissions2<-Emissions1/as.numeric(menage_echelle%>%summarise(sum(pondmen*emissions)))
  
  EM<-cbind(Emissions1[,1],Emissions1[,2])
  
  
  return(Emissions1[,2])
  
}


matrice_correlation<-function(bdd){
  
  library(plyr)
  comp_stock<-bdd %>% select(tuu,decucn)
  data_table<-as.data.frame(table(comp_stock))
  data_table<-plyr::ddply(data_table, .(decucn), transform, rescale = rescale(Freq))
  # source : https://stackoverflow.com/questions/30040420/heat-map-per-column-with-ggplot2/30040582#30040582
  
  
  g<-ggplot(data_table, aes(tuu, decucn)) + geom_tile(aes(fill=rescale),colour="white") +
    scale_fill_gradient(low = "white", high = "steelblue")+
    geom_text(aes(label=data_table$Freq))+
    theme(axis.text.x=element_text(size = 15),axis.text.y=element_text(size = 15))
  base_size <- 9
  g<-g + theme_grey(base_size = base_size)  + 
    #remove extra space & put x axis on top
    scale_x_discrete(expand = c(0, 0),position = "top") + 
    #remove extra space & reverse y axis order
    scale_y_discrete(expand = c(0, 0))+
    # xlab(label="Prédiction de la classe de DPE")+ylab(label="Classe de DPE")+
    # ggtitle("Matrice de confusion \n Estimation des DPE - Base Phébus")+
    theme(legend.position = "none", axis.ticks = element_blank(),
          
          axis.text.x = element_text(size = base_size * 0.8, 
                                     angle = 0, hjust = 0, colour = "grey50"),
          plot.title = element_text(hjust = 0.5))
  g
  detach("package:plyr")
}



calc_TCO_paid<-function(s,h,sc,r,Iter=NA){
  
  
  
  
  EMS<-read_excel(path=paste("D:/CIRED/Projet_Ademe/IMACLIM/EMS.xlsx",sep=""),range=paste(s,"!B1:AF5",sep=""),col_names=T)
  EMS<-EMS %>% gather(key=year,value=emission,-1)
  colnames(EMS)<-c("Variable","year","emission")
  
  EMS<-EMS%>%filter(!Variable=="EMS_HH_2")
  EMS #tonnes C02
  
  load("D:/CIRED/Projet_Ademe/2010/menage_forme_2010.RData")
  
  # if(!var %in% colnames(menage_forme_2010)){
  #   load("D:/CIRED/Projet_Ademe/2010/appmen_depensesactiv_2010.RData")
  #   menage_forme_2010<-
  #     menage_forme_2010 %>%
  #     left_join(appmen_depensesactiv_2010 %>% select(ident_men,var),by="ident_men")
  # }
  
  # prix constant 2010 tCO2/ dépenses par cat
  coeff_CL_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Variable=="EMS_HH_21_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
                                                                                                                         summarise(sum(pondmen*dep_Solides))) # en tC02/Millions €
  
  coeff_Oil_2010<-as.numeric(EMS%>%filter(year==2010)%>%filter(Variable=="EMS_HH_22_2")%>%select(emission))/ as.numeric(menage_forme_2010 %>%
                                                                                                                          summarise(sum(pondmen*(carb_lubr+dep_Fuel+dep_GPL)))) # en tC02/Millions €
  
  coeff_Gaz_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Variable=="EMS_HH_24_2")%>%select(emission))/as.numeric(menage_forme_2010 %>%
                                                                                                                          summarise(sum(pondmen*(dep_Gaz+dep_Urbain)))) # en tC02/Millions €
  
  
  # Load Horizon
  if(is.na(Iter)){Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc & Redistribution==r)%>%select(Iter_finale))}
  
  load(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
  
  
  if(Iter==0){
    suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code_iter_",as.character(Iter),".xlsx",sep=""),skip=1))
  }else{
    suppressWarnings(output_macro<-read_excel(paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/Output_macro_code_iter",as.character(Iter),".xlsx",sep="")))}
  
  
  output_macro <-  output_macro %>%  gather(key=year_model,value=value,-c(1:3))
  
  IMACLIM<-  output_macro  %>%  separate(col="year_model",into=c("year","model"),sep="_")
  FC <- 
    IMACLIM %>%
    filter(year==h) %>%
    filter(model=="IMACLIM")%>%
    filter(Variable %in% c(
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
      "A13")) %>%
    select(Variable,value)
  
  FC <- FC %>%
    mutate(value=as.numeric(value)) %>%
    spread(key=Variable,value=value) 
  
  
  
  #Horizon CL, Oil, Gaz dépenses à l'horizon en millions d'euros 2010 (M€2010)
  
  menage_echelle <- 
    menage_echelle %>% 
    mutate(CL_2010=dep_Solides/FC$A04)%>% 
    mutate(Oil_2010=(carb_lubr+dep_Fuel+dep_GPL)/FC$A07)%>%
    mutate(Gaz_2010=(dep_Gaz+dep_Urbain)/FC$A03)
  
  
  # TCO horizon (€/tcO2)
  TCO<-as.numeric(read_excel(path=paste("D:/CIRED/Projet_Ademe/",s,"/",h,"/",sc,"/",r,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""),range="C103",col_names=F))*10^6
  
  #Emissions  : € de TCO = €2010(dep) * tCO2/€2010(dep)*€(TCO)/tC02
  menage_echelle <- 
    menage_echelle %>% 
    mutate(TCO_CL=CL_2010*coeff_CL_2010*TCO)%>%
    mutate(TCO_Oil=Oil_2010*coeff_Oil_2010*TCO)%>%
    mutate(TCO_Gaz=Gaz_2010*coeff_Gaz_2010*TCO)
  
  #Taxe carbone par ménage
  menage_echelle$TCO_paid=rowSums(menage_echelle[c("TCO_CL","TCO_Oil","TCO_Gaz")])
  return(menage_echelle)}



