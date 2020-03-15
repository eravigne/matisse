


# pondR <- function(Iter,m2VP){
  
#Comment 06/05/2019 : en faisant tourner le code sans les contraintes INSEE ni le taux d'épargne, on obtient des résultats tels qu'attendus (pas de ménage qui tombe à une pondération nulle), 25 minutes pour faire tourner l'algorithme. 
# a faire : vérifier que les agrégats INSEE ne sont pas trop loin des agrégats à respecter, s'ils sont proches c'est embêtant, cela veut dire que les contraintes INSEE sont trop fortes pour respecter les contraintes macro. 
# Iter=1
  # LIBRARIES ---------------------------------------------------------------
  
  library(tidyverse)
  library(ggplot2)
  library(dplyr)
  library(quadprog)
  
 
  # DATA --------------------------------------------------------------------
  setwd("D:/CIRED/Projet_Ademe/")
  load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/menage_contraintes.RData",sep=""))
  load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/pond_init.RData",sep=""))
  load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/menage_echelle.RData",sep=""))
  load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/agreg_best.RData",sep=""))
  load("2010/menage_forme_2010.RData")
  source("Code_global_ADEME/compute_share.R")
  source("Code_global_ADEME/verif_epargne_taux.R")
  
  # save(agreg_best,file=paste("2025/Iteration_",Iter,"/Input/erreur2/agreg_best_erreur2.RData",sep=""))
  # save(menage_contraintes,file=paste("2025/Iteration_",Iter,"/Input/erreur2/menage_contraintes_erreur2.RData",sep=""))
  # save(pond_init,file=paste("2025/Iteration_",Iter,"/Input/erreur2/pond_init_erreur2.RData",sep=""))  
  # save(menage_echelle_2025,file=paste("2025/Iteration_",Iter,"/Input/erreur2/menage_echelle_2025_erreur2.RData",sep=""))
  
  
     
  # Selection
  # m2VP indique la prise en compte ou non pour le calage des stocks de surface habitée et du parc de véhicules
  # if(m2VP==F){
  #   menage_contraintes <-
  #     menage_contraintes %>% select(-surfhab_d,-nbvehic)
  #   agreg_best <- 
  #     agreg_best %>% select(-surfhab_d,-nbvehic)
  # }
  
  #-------------------------------------------------------------------------------
  #             Importation
  #-------------------------------------------------------------------------------
  # (A) BASE DE DONNEE
  data<-menage_contraintes %>% select(-ident_men)
  dim(data)
  typeof(data)
  ones<-cbind(rep(1,dim(data)[1]))
  dim(ones)
  data<-as.matrix(cbind(ones,data))
  dim(data)
  typeof(data)
  
  # (W) POIDS INITIAUX
  pond_init <- as.matrix(pond_init)
  dim(pond_init)
  typeof(pond_init)
  # (S) AGREGAT INITIAUX
  Amat <- t(data)
  dim(Amat)
  agreg_init <- (Amat %*% pond_init)
  dim(agreg_init)
  typeof(agreg_init)
  typeof(Amat)
  
  # (T) AGREGAT A RESPECTER
  agreg_best <- t(agreg_best)
  dim(agreg_best)
  typeof(agreg_best)
  
  
  #-------------------------------------------------------------------------------
  #             Transformation
  #-------------------------------------------------------------------------------
  # (Vmat) MATRICE DIAGONALE DES POIDS A MINIMISER
  Vmat <- as.numeric(t(1/((pond_init)^2)))
  Vmat <- as.matrix(diag(Vmat))
  n = nrow(Vmat)

  
  
  # (dvec) VECTEUR A MINIMISER
  dvec <- t(vector("numeric",n))
  
  # (Amat) CONTRAINTE MATRICIELLE
  
  # (bvec) CONTRAINTE D EGALITE (valeurs de départ)
  bvec <- agreg_best - agreg_init
  # View(cbind(rownames(agreg_init),rownames(agreg_best)))
  # View(cbind(agreg_init, agreg_best,bvec/agreg_init))

  # (uvec) CONTRAINTE D INEGALITE 
  uvec <- -1 * pond_init
  
  A<-cbind(t(Amat),diag(n))
  b<-rbind(bvec,-pond_init)
  
  
  dim(Vmat)
  dim(b)
  dim(A)
  dim(dvec)
  
  
  
  #-------------------------------------------------------------------------------
  #             Solveur
  #-------------------------------------------------------------------------------
  print("Repondation : Work in Progress")
  
  # Start the clock!
  ptm <- proc.time()
  sol<-solve.QP(Vmat,dvec,A,b,meq=length(agreg_best))

  # Stop the clock
  print(proc.time() - ptm)
  
  sol$iterations
  head(sol$solution)
  
  print(paste("Repondation : DONE (",(proc.time() - ptm)[3]/60," min)",sep=""))
  diff_pond<-sol$solution
  save(diff_pond,file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/diff_pond.RData",sep=""))
  
  #-------------------------------------------------------------------------------
  #             Output
  #-------------------------------------------------------------------------------
  # PONDERATIONS FINALES
  pond_final <- pond_init + sol$solution
  print(pond_final[1:5])
  
  # ponderations initiales
  print(pond_init[1:5])
  
  # ecart de variation nominal
  print(sol$solution[1:5])
  
  # sauvegarde
  save(pond_final, file = 
         paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/pond_final.RData",sep=""))

  # save(pond_init,file=
  #        paste("2025/Iteration_",Iter,"/Output/erreur2/pond_init_erreur2.RData",sep=""))
  # save(pond_final, file =
  #        paste("2025/Iteration_",Iter,"/Output/erreur2/pond_final_erreur2.RData",sep=""))
  # save(pond_final, file =
  #        paste("2025/Iteration_",Iter,"/Output/erreur2/pond_final_erreur2.RData",sep=""))
  # save(agreg_init, file =
  #        paste("2025/Iteration_",Iter,"/Input/erreur2/agreg_init_erreur2.RData",sep=""))
  
  # AGREGATS FINAUX
  agreg_final <- (Amat %*% pond_final)
  print("Init")
  head(agreg_init)
  print("Best")
  head(agreg_best)
  print("Final")
  head(agreg_final)
  
  # DENSITE des modifications de PONDERATIONS
  diff<-(pond_final-pond_init)/pond_init
  max(diff)
  mean(diff)*100
  diff<-as.data.frame(diff)
  library(ggplot2)
  g<-ggplot(diff,aes(pondmen))+geom_density()+scale_x_continuous(limits = c(-2.5, 2.5))
  # g
  g<-ggplot(diff,aes(pondmen))+geom_density()+scale_x_continuous(limits = c(-1, 1))
  # g
  
  pond<-data.frame(menage_echelle%>%select(ident_men),"init"=pond_init,"final"=pond_final,"diff"=diff)
  colnames(pond)<-c("ident_men","init","final","diff")
  
  
  menage_echelle <- 
    menage_echelle %>%
    mutate(pondmen = pond_final)
  
  # sauvegarde
  save(agreg_final, file = 
         paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/agreg_final.RData",sep=""))

  save(menage_echelle,file = 
         paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))

  
  write.csv2(t(agreg_final),file = 
               paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/agreg_final.csv",sep=""))
  
  
  print(
    paste("Repondération Itération n°",Iter," : SUCCESS",sep="")
  )
  
# }

  
  
  # Export parts budgétaires + taux_épargne
 #  
 # share_2010<-compute_share(menage_forme_2010)
 # share_2025<-compute_share(menage_echelle_2025)
 # Parts=as.data.frame(rbind(share_2010,share_2025))
 # colnames(Parts)=col
 # rownames(Parts)<-c(2010,2025)
 # View(Parts)
 #  
 #  #taux épargne
 #  epargne_2010<-sum(menage_forme_2010$taux_epargne*menage_forme_2010$pondmen*menage_forme_2010$RDB /
 #                      sum(menage_forme_2010$RDB*menage_forme_2010$pondmen),
 #                      na.rm = T)
 #  epargne_2025<-sum(menage_echelle_2025$taux_epargne*menage_echelle_2025$pondmen*menage_echelle_2025$RDB /
 #                    sum(menage_echelle_2025$RDB*menage_echelle_2025$pondmen),
 #                    na.rm = T)
 #  # epargne_mise_échelle<-sum(A2$taux_epargne*A2$pondmen*A2$RDB /
 #  #                             sum(A2$RDB*A2$pondmen),
 #  #                           na.rm = T)
 #  epargne_2010
 #  # epargne_mise_échelle
 #  epargne_2025
