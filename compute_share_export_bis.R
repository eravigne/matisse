# compute_share_export_bis<-function(s,h,sc,r){
compute_share_export_bis<-function(menage_echelle,s,h,sc,r,iter){
  
  library(tidyverse)
  # print(paste(s,h,sc,r,iter,sep=" & "))

  source("D:/CIRED/Projet_Ademe/Code_global_ADEME/mutate_when.R")
  #comme compute_share_export, renvoie des parts pour 13 biens et services, pour les quatre biens énergétiques (A02,A03,A04 et A07, renvoie la progression en dépenses réelles (divisée par Indice de Stone) depuis 2010) 
  

# Load --------------------------------------------------------------------

  # Iterations
  # Iterations<-read_excel("D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown.xlsx")
  # Iter<-as.numeric(Iterations %>% filter(Horizon==h & Scenario==s & Scenario_classement==sc& Redistribution==r)%>%select(Iter_finale))
  # Iter<-iter

  # Base micro projetée à l'horizon
  # load(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))

  # BDFE 2010
  load("D:/CIRED/Projet_Ademe/2010/menage_calibr_2010.RData")

#   
#Colonne dans Output_macro
  if(horizon==2025){X="AH"}
  if(horizon==2030){X="AI"}
  if(horizon==2035){X="AJ"}
  
  
# # Indices de prix ---------------------------------------------------------
if(Iter>0){
  IP_A02<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range=paste(X,"13",sep=""),col_names = F))

  IP_A03<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range=paste(X,"14",sep=""),col_names = F))

  IP_A04<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range=paste(X,"15",sep=""),col_names = F))

  IP_A07<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range=paste(X,"18",sep=""),col_names = F))
  }

  if(Iter==0){
    IP_A02<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code_iter_0.xlsx",sep=""),sheet=s,range=paste(X,"14",sep=""),col_names = F))

    IP_A03<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code_iter_0.xlsx",sep=""),sheet=s,range=paste(X,"15",sep=""),col_names = F))

    IP_A04<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code_iter_0.xlsx",sep=""),sheet=s,range=paste(X,"16",sep=""),col_names = F))

    IP_A07<-as.numeric(read_excel(paste("D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code_iter_0.xlsx",sep=""),sheet=s,range=paste(X,"19",sep=""),col_names = F))
  }
#   
# 
# # 2010 --------------------------------------------------------------------
#   
  # if("plyr" %in% rownames(installed.packages())){detach("package:plyr", unload=TRUE)}
  try(
    detach("package:plyr"), 
    silent=T
    )
  energie_2010<- menage_calibr_2010 %>% select(ident_men, pondmen,carb_lubr, dep_Elec, dep_Gaz,dep_Solides, dep_Fuel,dep_GPL,dep_Urbain,carb_lubr)%>%mutate(Oil_2010=dep_Fuel+dep_GPL+carb_lubr) %>% mutate(Elec_2010=dep_Elec)%>%mutate(Gaz_2010=dep_Gaz+dep_Urbain)%>% mutate(Solides_2010=dep_Solides)%>%select(ident_men,pondmen,Elec_2010,Gaz_2010,Oil_2010,Solides_2010)

  energie_2010_bis <- energie_2010 %>% gather(key=ener, value=value, -c(1:2))


  energie_2010_ag <- energie_2010_bis %>% group_by(ener) %>% summarise(sum(pondmen*value))
# 
# # Horizon -----------------------------------------------------------------
# 
  energie_horizon<- menage_echelle %>%
    select(ident_men, pondmen, carb_lubr,dep_Elec, dep_Gaz,dep_Solides, dep_Fuel,dep_GPL,dep_Urbain)%>%
    mutate(Oil=(dep_Fuel+dep_GPL)/IP_A04+carb_lubr/IP_A07) %>%
    mutate(Elec=dep_Elec/IP_A02)%>%mutate(Gaz=dep_Gaz/IP_A03+dep_Urbain/IP_A04)%>%
    mutate(Solides=dep_Solides/IP_A04)%>%
    select(ident_men,pondmen,Elec,Gaz,Oil,Solides)


  energie_bis <- energie_horizon %>% gather(key=ener, value=value, -c(1:2))


  energie_ag <- energie_bis %>% group_by(ener) %>% summarise(sum(pondmen*value))
#   
#   
  # Parts="OK"
# 
# # Parts -------------------------------------------------------------------
# 
menage<-menage_echelle
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

  list_dep_non_ener<-c("agriculture",
                       "BTP",
                       "prod_veh",
                       "transp_rail_air",
                       "transp_routes_eau",
                       "loisirs_com",
                       "autres_services",
                       "autres",
                       "loyers")
  

  menage <-
    menage %>%
    mutate(c13711_neuf=0)%>%
    mutate_when(year_neuf==horizon,list(c13711_neuf=c13711))

  
  menage<-
    menage %>%
    mutate(BTP=BTP+c13711_neuf)


  menage$loyers<-menage$loyers+menage$rev801
  menage$Rcons_bis <- rowSums(menage[list_dep_non_ener])
#   
#   
  # sur les 13 biens de la désagrégation on ne considère que les 9 biens non énergétiques
  for (i in 1:13){
    k=list_dep[i]
    # print(k)
    if(i<10){
      menage[paste("share_A0",i,sep="")]<-menage[k]/menage$Rcons_bis}
    else{ menage[paste("share_A",i,sep="")]<-menage[k]/menage$Rcons_bis}
  }
#   
#   
#   
  share=rep(1,13)
  col=rep(1,13)
  for (i in 1:13){
    if(i<10){
      k=paste("share_A0",i,sep="")
      col[i]=k
      share[i]= as.numeric(menage %>% summarise(weighted.mean(get(k),pondmen*Rcons_bis)))
      }
    else{
      k=paste("share_A",i,sep="")
      col[i]=k
      share[i]= as.numeric(menage %>% summarise(weighted.mean(get(k),pondmen*Rcons_bis)))
      }
  }
#   

  Parts=as.data.frame(rbind(share))
  colnames(Parts)=col
  
  Parts$share_A02 <- as.numeric((energie_ag[1,2] - energie_2010_ag[1,2])/energie_2010_ag[1,2])
  Parts$share_A03 <- as.numeric((energie_ag[2,2] - energie_2010_ag[2,2])/energie_2010_ag[2,2])
  Parts$share_A04 <- as.numeric((energie_ag[4,2] - energie_2010_ag[4,2])/energie_2010_ag[4,2])
  Parts$share_A07 <- as.numeric((energie_ag[3,2] - energie_2010_ag[3,2])/energie_2010_ag[3,2])

  colnames(Parts)<-c("share_A01","ELEC","GAS","CL","share_A05","share_A06","OIL","share_A08","share_A09","share_A10","share_A11","share_A12","share_A13")
#   


  # View(Parts)
  # library(plyr)
  return(Parts)
  
}

# compute_share(menage_calibr_2010)
# # compute_share(menage_echelle_2025)
