

# LIBRARIES ---------------------------------------------------------------


library(FactoMineR)
library(tidyverse)
library("corrplot")
library(nnet)
library(reshape)
library(scales)
library(plyr)
library(cowplot)
library(stringr)

setwd("D:/CIRED/Projet_Ademe")

Phebus <-read.table("Donnees_brutes/PHEBUS/Clode_dpe_energie_decideur_revenu.csv",header=TRUE,sep=";",dec=".")


######################################"
# PREPARATION DES DONNEES -------------------------------------------------
######################################"

Variables=c("BATI_PERIODE",
            "ESTOC",
            "Revenu_Insee_quintile",
            "EHST",
            "TYP_LOG",
            "RP_TAILLE_UU",
            "Typ_Energie_Chauff_P2",
            "TROIS_CL_DPE",
            "ident")

#création variable is_elec, 1 si source principale énergie de chauffage, 2 si non, à partir Typ_energie_chauff_p2

Phebus$is_elec<-ifelse(Phebus$Typ_Energie_Chauff_P2=="Elec",1,2)
Phebus$is_elec<-ifelse(is.na(Phebus$is_elec),2,Phebus$is_elec)
Variables<-Variables[Variables!="Typ_Energie_Chauff_P2"] #On retire la variable typ_energie_chauff_p2
Variables=c(Variables,"is_elec") #on rajoute is_elec

#codage Quintile
Phebus$Revenu_Insee_quintile<-as.numeric(str_sub(Phebus$Revenu_Insee_quintile,2))

#codage DPE
Phebus$DPEb<- ifelse(Phebus$TROIS_CL_DPE=="A",1,
                     ifelse(Phebus$TROIS_CL_DPE=="B",2,
                            ifelse(Phebus$TROIS_CL_DPE=="C",3,
                                   ifelse(Phebus$TROIS_CL_DPE=="D",4,
                                          ifelse(Phebus$TROIS_CL_DPE=="E",5,
                                                 ifelse(Phebus$TROIS_CL_DPE=="F",6,
                                                        ifelse(Phebus$TROIS_CL_DPE=="G",7,0)))))))

Variables<-Variables[Variables!="TROIS_CL_DPE"] 
Variables=c(Variables,"DPEb") 
# Phebus$DPEb
# with(Phebus,table(DPEb))
# 1   2   3   4   5   6   7 
# 6  49 312 638 698 353 295 

Phebus$MI<-ifelse(Phebus$TYP_LOG=="Maison",1,
                  ifelse(Phebus$TYP_LOG=="Appart",0,3))
Variables<-Variables[Variables!="TYP_LOG"] 
Variables=c(Variables,"MI") 

# Bati_periode

Phebus$BATI_PERIODE<-car::recode(Phebus$BATI_PERIODE, "1:2 = 1 ; 3=2 ; 4=3 ; 5=4 ; 6=5")
#Rmq : recode can be used by both "car" and "dplyr" packages, works better with car, need to specify it. 


######################################"
# SELECTION DES DONNEES ---------------------------------------------------
######################################"


Phebus_bis<-Phebus[which(Phebus$DPEb %in% seq(1,8,1)),Variables]
Phebus_bis<-Phebus_bis[which(!is.na(Phebus_bis$DPEb)),]
Phebus_bis<-Phebus_bis[which(Phebus_bis$MI %in% c(0,1)),]
Phebus_bis<-Phebus_bis[which(!is.na(Phebus_bis$Revenu_Insee_quintile)),]
# Phebus_bis

Regresseurs <- Variables[!Variables%in% c("ident","DPEb")]

###############################################################
#ACP
################################################################

#Listes individus que je vais supprimer
ind_sup=c()
list_exclus=c()

#Individus originels, variables originelles = Regresseurs 
data_regresseurs<-cbind(Phebus_bis[Regresseurs],Phebus_bis['DPEb'])
dim(data_regresseurs)
data_regresseurs2<-data_regresseurs
init=1
C=1
iter=0

#Tant que l'étape d'avant enlève des individus sauf si on a déjà effectué trop d'itérations
suppressWarnings(while(C>0 & iter<6){
  iter=iter+1
  # #> Regresseurs
  # [1] "BATI_PERIODE"          "ESTOC"                 "Revenu_Insee_quintile" "EHST"                  "RP_TAILLE_UU"         
  # [6] "is_elec"               "MI"                  
  data_regresseurs<-cbind(data_regresseurs2[Regresseurs],data_regresseurs2['DPEb'])
  list_exclus<-cbind(list_exclus[Regresseurs],list_exclus['DPEb'])
  ind_sup=rbind(ind_sup,list_exclus)
  dim(data_regresseurs)
  dim(data_regresseurs2)
  dim(list_exclus)
  dim(ind_sup)
  # dpe_pca = PCA(data_regresseurs, ind.sup=ind_sup,scale.unit=TRUE, quanti.sup=c(8),ncp=7, graph=T) 
  
  
  #pour les passages suivants le premier
  if(!init==1){
    #on agrège les individus qu'on utilise dans la regression et ceux qu'on a exclu
    data_reg_ind_sup=rbind(data_regresseurs,ind_sup)
    borne_inf=dim(data_regresseurs)[1]+1
    borne_sup=borne_inf+dim(ind_sup)[1]-1
    #bornes sup et inf définissent les indices des individus exclus
    dpe_pca=PCA(data_reg_ind_sup,ind.sup=borne_inf:borne_sup,scale.unit=TRUE, quanti.sup=c(8),ncp=7, graph=T)
  }
  
  #Lors du premier passage
  if(init==1){
    dpe_pca=PCA(data_regresseurs, scale.unit=TRUE, quanti.sup=c(8),ncp=7, graph=T)
    init=0
  }
  
  
  
  dpe_pca2<-dpe_pca
  dpe_pca2<-as.data.frame(dpe_pca2$ind$contrib)
  dim(dpe_pca2)
  colnames(dpe_pca2)<-c("Dim.1","Dim.2","Dim.3","Dim.4","Dim.5","Dim.6","Dim.7")
  data_regresseurs2=cbind(data_regresseurs,dpe_pca2)
  list_exclus=c()
  
  dim(dpe_pca2)
  dim(data_regresseurs)
  dim(data_regresseurs2)
  
  #on ne filtre les individus que sur trois premières dimensions
  for (dim in c("Dim.1","Dim.2","Dim.3")){
    # print("dim(data_regresseurs2)")
    # print(dim(data_regresseurs2))
    # print("dim(list_exclus)")
    # print(dim(list_exclus))
    m=apply(dpe_pca2[dim],2,mean)
    #on définit 5 fois la moyenne comme sensibilité
    m<-4*as.numeric(m)
    list_exclus<-rbind(list_exclus,data_regresseurs2[which(data_regresseurs2[dim]>=m),])
    data_regresseurs2<-data_regresseurs2[which(data_regresseurs2[dim]<m),]
    # dim(list_exclus)
    # dim(dpe_pca2)
  }
  # C désigne le nombre d'exclus lors de la dernière itérations sur les deux premières dimensions
  C=dim(list_exclus)
  if(is.null(list_exclus)){C=0}
  print("C")
  print(C)
  print("data_regresseurs2")
  print(dim(data_regresseurs2))
})


# data_regresseurs2

#Tracé du diagramme de corrélation
dim(data_regresseurs2)
# corrplot(dpe_pca$var$cos2, is.corr=FALSE)

#traçons le diagramme de Cattel. Un coude dans le diagramme de Cattel indique le nombre de variables qu'il faut choisir pour la régression.
# plot(dpe_pca$eig[,2])
# plot(dpe_pca$eig[,2],type="l")


######################################"
# ESTIMATION MULTINOMIALE -------------------------------------------------
######################################"

#Estimation multinomiale (différentes solutions pour l'estimation : sur variables originelles, variables issues de l'ACP 
# ou mix des deux, nous retenons les variables originelles dont les performances sont équivalentes et l'interprétation intuitive)

estm_dpe_acp <- multinom(DPEb ~ BATI_PERIODE + ESTOC + Revenu_Insee_quintile + EHST + RP_TAILLE_UU + is_elec + MI, data = data_regresseurs2, Hess=T)


######################################"
# SUPPRIMER DONNEES MEMOIRE -------------------------------------------------
######################################"

rm(data_reg_ind_sup,data_regresseurs,data_regresseurs2,dpe_pca,dpe_pca2,ind_sup,list_exclus,Phebus,Phebus_bis,borne_inf,borne_sup,C,dim,init,iter,m,Regresseurs,Variables)

