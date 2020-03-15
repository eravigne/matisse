
# LIBRARIES ---------------------------------------------------------------
library(tidyverse)


# FUNCTION ----------------------------------------------------------------

#solde représente le solde budgétaire, positif ou négatif, issu d'un réarrangement 
# du budget pour intégrer le TC
# Vecteur colonne, nombre de ligne correspondant au nombre de ménage de 2025 : ident_men et colonne


Ventil_solde <- function(solde,menage){
  
  
  
  # DONNEES -----------------------------------------------------------------
  
  
  menage_bis<-menage
  menage_bis <- menage_bis %>% left_join(solde,by="ident_men")
  source("Code_global_Ademe/mutate_when.R")
  load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))
  
  # Ventilation -------------------------------------------------------------
  
  iter=TRUE #Pour la première itération
  nb_iter_RDB=0
  
  while(iter & nb_iter_RDB<1){
    sauv_menage<-    menage_bis
    nb_iter_RDB=nb_iter_RDB+1
    list_dep_autres_ener=c("dep_GPL","dep_Fuel","dep_Urbain", "dep_Solides")
    
    list_dep_15=c("agriculture",
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
               "veh_occasion",
               "Hors_budget")
    
    
    
    list_dep_14=c("agriculture",
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
    
    
    
    menage_bis<- menage_bis %>% mutate(dep_autres_energies=dep_Fuel+dep_GPL+dep_Solides+dep_Urbain)
    
    menage_bis <- 
      menage_bis %>%
      mutate(c13711_neuf=0)%>%
      mutate_when(year_neuf==horizon,list(c13711_neuf=c13711))
    
    menage_bis$Rcons_bis <- rowSums(menage_bis[list_dep_14])+menage_bis$rev801+menage_bis$c13711_neuf
    
    
    for (i in 1:14){
      k=list_dep_14[i]
      if(i<10){
        menage_bis[paste("share_A0",i,sep="")]<-ifelse(menage_bis$Rcons_bis==0,0,menage_bis[k]/menage_bis$Rcons_bis)}
      else{menage_bis[paste("share_A",i,sep="")]<-ifelse(menage_bis$Rcons_bis==0,0,menage_bis[k]/menage_bis$Rcons_bis)}
    }
    menage_bis$share_A13<-(menage_bis$loyers+menage_bis$rev801)/menage_bis$Rcons_bis
    
    menage_bis$share_A05<-(menage_bis$BTP+menage_bis$c13711_neuf)/menage_bis$Rcons_bis
    
    #sécurité
    menage_bis <- 
      menage_bis %>%
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
                  
                  
    menage_bis$IP_stone <-
      FC$A01**menage_bis$share_A01*
      FC$A02**menage_bis$share_A02*
      FC$A03**menage_bis$share_A03*
      FC$A04**menage_bis$share_A04*
      FC$A05**menage_bis$share_A05*
      FC$A06**menage_bis$share_A06*
      FC$A07**menage_bis$share_A07*
      FC$A08**menage_bis$share_A08*
      FC$A09**menage_bis$share_A09*
      FC$A10**menage_bis$share_A10*
      FC$A11**menage_bis$share_A11*
      FC$A12**menage_bis$share_A12*
      FC$A13**menage_bis$share_A13*
      FC$A14**menage_bis$share_A14
    
    
    
    menage_bis$RDB_reel <-  menage_bis$RDB/menage_bis$IP_stone
    menage_bis<-
      menage_bis %>%
      mutate_when(is.na(RDB_reel),list(RDB_reel==0))
    ##
    ### ENERGIE
    ##
    
    # A02
    menage_bis$dep_Elec<- 
      menage$dep_Elec*
      (1+ menage_bis$elast_rev_A02*
         ifelse(
           menage_bis$RDB_reel==0,
           0,
           -menage_bis$solde/menage_bis$RDB_reel))
    
    
    # A03
    menage_bis$dep_Gaz<- menage$dep_Gaz*(1+ menage_bis$elast_rev_A03*ifelse(menage_bis$RDB_reel==0,0,-menage_bis$solde/menage_bis$RDB_reel))
    
    
    # A04
    menage_bis[list_dep_autres_ener]<- menage[list_dep_autres_ener] *(1+menage_bis$elast_rev_A04*ifelse(menage_bis$RDB_reel==0,0,-menage_bis$solde/menage_bis$RDB_reel))
    
    
   
    
    ##
    ### BIENS HORS ENERGIE
    ##
    
    for (i in c(1,5:9)){
      A0I<-paste("elast_rev_A0",i,sep="")
      menage_bis[list_dep_15[i]]<-  menage[list_dep_15[i]]*(1+menage_bis[A0I]*ifelse(menage_bis$RDB_reel==0,0,-menage_bis$solde/menage_bis$RDB_reel))
    }
    
    for (i in 10:14){
      AI<-paste("elast_rev_A",i,sep="")
      menage_bis[list_dep_15[i]]<-  menage[list_dep_15[i]]*(1+ menage_bis[AI]*ifelse(menage_bis$RDB_reel==0,0,-menage_bis$solde/menage_bis$RDB_reel))
    }
    # NB : le Hors budget est exclus de la reventilation, nous ne connaissons pas le comportement des agents vis-à-vis cet agrégat qui regroupe des dépenses exceptionnelles et/ou importantes
    
    
    ##
    ### DELTA
    ##
    menage_bis<-menage_bis%>%
      mutate_when(is.na( agriculture ),list( agriculture =0))%>%            
      mutate_when(is.na( dep_Elec ),list( dep_Elec =0))%>%                  
      mutate_when(is.na( dep_Gaz ),list( dep_Gaz =0))%>%                    
      mutate_when(is.na( dep_GPL ),list( dep_GPL =0))%>%                    
      mutate_when(is.na( dep_Fuel ),list( dep_Fuel =0))%>%                  
      mutate_when(is.na( dep_Urbain ),list( dep_Urbain =0))%>%              
      mutate_when(is.na( dep_Solides ),list( dep_Solides =0))%>%            
      mutate_when(is.na( BTP ),list( BTP =0))%>%                            
      mutate_when(is.na( prod_veh ),list( prod_veh =0))%>%                  
      mutate_when(is.na( carb_lubr ),list( carb_lubr =0))%>%                
      mutate_when(is.na( transp_rail_air ),list( transp_rail_air =0))%>%    
      mutate_when(is.na( transp_routes_eau ),list( transp_routes_eau =0))%>%
      mutate_when(is.na( loisirs_com ),list( loisirs_com =0))%>%            
      mutate_when(is.na( autres_services ),list( autres_services =0))%>%    
      mutate_when(is.na( autres ),list( autres =0))%>%                      
      mutate_when(is.na( loyers ),list( loyers =0))%>%                      
      mutate_when(is.na( veh_occasion ),list( veh_occasion =0))%>%          
      mutate_when(is.na( Hors_budget ),list( Hors_budget =0))
    menage_bis$agriculture[which(menage_bis$agriculture<0)]<-0
    menage_bis$dep_Elec[which(menage_bis$dep_Elec<0)]<-0
    menage_bis$dep_Gaz[which(menage_bis$dep_Gaz<0)]<-0
    menage_bis$dep_GPL[which(menage_bis$dep_GPL<0)]<-0
    menage_bis$dep_Fuel[which(menage_bis$dep_Fuel<0)]<-0
    menage_bis$dep_Urbain[which(menage_bis$dep_Urbain<0)]<-0
    menage_bis$dep_Solides[which(menage_bis$dep_Solides<0)]<-0
    menage_bis$BTP[which(menage_bis$BTP<0)]<-0
    menage_bis$prod_veh[which(menage_bis$prod_veh<0)]<-0
    menage_bis$carb_lubr[which(menage_bis$carb_lubr<0)]<-0
    menage_bis$transp_rail_air[which(menage_bis$transp_rail_air<0)]<-0
    menage_bis$transp_routes_eau[which(menage_bis$transp_routes_eau<0)]<-0
    menage_bis$loisirs_com[which(menage_bis$loisirs_com<0)]<-0
    menage_bis$autres_services[which(menage_bis$autres_services<0)]<-0
    menage_bis$autres[which(menage_bis$autres<0)]<-0
    menage_bis$loyers[which(menage_bis$loyers<0)]<-0
    menage_bis$veh_occasion[which(menage_bis$veh_occasion<0)]<-0
    menage_bis$Hors_budget[which(menage_bis$Hors_budget<0)]<-0
  
    menage_bis$Rcons <- rowSums(menage_bis[list_dep_18])
    
    menage_bis$Delta <- menage_bis$Rcons - menage$Rcons
    # Un Delta positif indique que les dépenses de consommations après reventilation sont plus
    # importantes qu'à l'origine, cet excès doit être retiré du secteur A13, de tous les secteurs 
    # au pro rata de leur part budgétaire sinon. 
    
    # menage_bis <-
    #   menage_bis %>%
    #   mutate(Beta=0)
    # 
    # menage_bis <-
    #   menage_bis%>%
    #   mutate_when(
    #     Delta<=autres & !Delta==0,
    #     list(autres=autres-Delta))
    # 
    # menage_bis <-
    #   menage_bis%>%
    #   mutate_when(
    #     Delta>autres & !Delta==0,
    #     list(autres=0,
    #          Beta=Delta-autres))
    # 
    # for (dep in list_dep_18[1:17]){
    #   menage_bis[paste(dep,"solde",sep="_")] <- menage_bis[dep]-menage_bis[dep]/rowSums(menage_bis[list_dep_18[1:17]])*menage_bis$Beta
    # }


    
    menage_bis<- menage_bis %>% mutate(dep_autres_energies=dep_Fuel+dep_GPL+dep_Solides+dep_Urbain)
    
    menage_bis <- 
      menage_bis %>%
      mutate(c13711_neuf=0)%>%
      mutate_when(year_neuf==horizon,list(c13711_neuf=c13711))
    menage_bis$Rcons_bis <- rowSums(menage_bis[list_dep_14])+menage_bis$rev801+menage_bis$c13711_neuf
    
    
    for (i in 1:14){
      k=list_dep_14[i]
      if(i<10){
        menage_bis[paste("share_A0",i,sep="")]<-ifelse(menage_bis$Rcons_bis==0,0,menage_bis[k]/menage_bis$Rcons_bis)}
      else{menage_bis[paste("share_A",i,sep="")]<-ifelse(menage_bis$Rcons_bis==0,0,menage_bis[k]/menage_bis$Rcons_bis)}
    }
    menage_bis$share_A13<-(menage_bis$loyers+menage_bis$rev801)/menage_bis$Rcons_bis
    
    menage_bis$share_A05<-(menage_bis$BTP+menage_bis$c13711_neuf)/menage_bis$Rcons_bis
    #sécurité
    menage_bis <- 
      menage_bis %>%
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
    
    menage_bis$IP_stone <-
      FC$A01**menage_bis$share_A01*
      FC$A02**menage_bis$share_A02*
      FC$A03**menage_bis$share_A03*
      FC$A04**menage_bis$share_A04*
      FC$A05**menage_bis$share_A05*
      FC$A06**menage_bis$share_A06*
      FC$A07**menage_bis$share_A07*
      FC$A08**menage_bis$share_A08*
      FC$A09**menage_bis$share_A09*
      FC$A10**menage_bis$share_A10*
      FC$A11**menage_bis$share_A11*
      FC$A12**menage_bis$share_A12*
      FC$A13**menage_bis$share_A13*
      FC$A14**menage_bis$share_A14
    
    
    
    menage_bis$RDB_reel <-  menage_bis$RDB/menage_bis$IP_stone
    tol=abs((menage_bis$RDB_reel-sauv_menage$RDB_reel)/sauv_menage$RDB_reel)
    
    
    max(tol,na.rm=T)
    if(max(tol,na.rm=T)>10^-3){iter=TRUE} else {iter=FALSE}
    
    
  }
  
  # TESTS -------------------------------------------------------------------
  
  
  menage_bis %>% filter(ident_men==1549) %>% select(autres,Delta,Rcons)
  menage_bis %>% filter(ident_men==775) %>% select(autres,Delta,Rcons)
  menage_bis %>% filter(ident_men==1943) %>% select(autres,Delta,Rcons)
  menage %>% filter(ident_men==1943) %>% select(autres,Rcons)
  
  # test<-menage_bis %>% filter(ident_men==775)
  # test$Delta<=test$autres
  # test$autres=test$autres-test$Delta
  # 
  menage_bis$autres<-as.numeric(menage_bis$autres)
  menage_bis$Delta<-as.numeric(menage_bis$Delta)
  menage_bis$Rcons<-as.numeric(menage_bis$Rcons)
  
  
  
  
  
  menage_bis$epargne<- menage_bis$epargne-menage_bis$Delta
  
  ##Avec ou sans BETA ?
  #test
  # menage_bis %>% filter(ident_men==1549) %>% select(autres,Delta,Beta,Rcons)
  # menage_bis %>% filter(ident_men==11001) %>% select(autres,Delta,Beta,Rcons)
  
  # menage_bis %>% filter(ident_men==12268) %>% select(autres,Delta,Beta,Rcons)
  
  menage_renovation_ter<-menage_bis %>% filter(ident_men==1549)
  
  
  # # #appliquer également cette diminution des dépenses aux dépenses spéficiques d'énergie pour conserver l'égalité entre "source" et "dep_source" (problème pour ménage 12268 de 4_1_Achat_neuf par exemple (scn AMS, optimiste))
  # # => solution : mettre à l'échelle les dépenses d'énergies spécifiques à la toute fin
  # # Ne pas changer dans la boucle précédent les dep pour ne pas faire varier le ratio des dépenses en cours de route
  # for (dep in list_dep[1:17]){
  #   menage_bis[dep] <-menage_bis[paste(dep,"solde",sep="_")] 
  # }
  
  
  
  # Mise échelle les "sources" et "source_usage" ----------------------------
  
  
  
  #rmq : pas possible d'automatiser, problème avec le test ifelse sur une liste.
  El<-colnames(menage_bis %>% select(starts_with("Elec")))
  menage_bis[El] <- menage_bis[El]*ifelse(menage$dep_Elec==0,0,menage_bis$dep_Elec/menage$dep_Elec)
  
  Ga<-colnames(menage_bis %>% select(starts_with("Gaz")))
  menage_bis[Ga] <-menage_bis[Ga]*ifelse(menage$dep_Gaz==0,0,menage_bis$dep_Gaz/menage$dep_Gaz)
  
  Fu<-colnames(menage_bis %>% select(starts_with("Fuel")))
  menage_bis[Fu] <-menage_bis[Fu]*ifelse(menage$dep_Fuel==0,0,menage_bis$dep_Fuel/menage$dep_Fuel)
  
  Ur<-colnames(menage_bis %>% select(starts_with("Urbain")))
  menage_bis[Ur] <-menage_bis[Ur]*ifelse(menage$dep_Urbain==0,0,menage_bis$dep_Urbain/menage$dep_Urbain)
  
  So<-colnames(menage_bis %>% select(starts_with("Solides")))
  menage_bis[So] <-menage_bis[So]*ifelse(menage$dep_Solides==0,0,menage_bis$dep_Solides/menage$dep_Solides)
  
  Gp<-colnames(menage_bis %>% select(starts_with("GPL")))
  menage_bis[Gp] <-menage_bis[Gp]*ifelse(menage$dep_GPL==0,0,menage_bis$dep_GPL/menage$dep_GPL)
  
  
  
  # Mise à jour Rcons et autres variables -----------------------------------
  
  

  menage_bis$Rcons <- rowSums(menage_bis[list_dep_18])
  
  # Test
  # on devrait avoir Rcons - solde/2 = Rcons'
  # (attention : solde<0 donc Rcons augmente)
  # > 67384.2+60.57556/2
  # [1] 67414.49
  menage %>% filter(ident_men==775) %>% select(Rcons)   
  menage_bis %>% filter(ident_men==775) %>% select(Rcons,solde)
  
  
  
  # Nouvelles parts budgétaires ------------------------------------ --------
  
  
  for (k in list_dep_18){
    menage_bis[paste("part",k,sep="_")]<-menage_bis[k]/menage_bis$Rcons
  }
  
  
  table(menage_bis$RDB -  menage$RDB)
  table(menage_bis$Rcons -  menage$Rcons)
  
  table(menage$RDB-menage$Rcons==menage$epargne)
  
  
  # Epargne
  menage_bis$epargne <- 
    menage_bis$RDB - 
    menage_bis$Rcons + 
    menage_bis$rev_exceptionnel
  
  table(menage_bis$epargne-menage$epargne)
  
  menage_bis$ratio_S <-  menage_bis$epargne / menage_bis$Rcons  # probleme pour RDB = 0 (ratio_S = -1)
  
  
  menage_bis<-
    menage_bis %>% 
    select(colnames(menage))
  
  # menage_bis<-
  #   menage_bis %>%
  #   mutate(taux_effort_financier=((taux_effort_financier*RDB)+Delta)/RDB)
  # 
  
  # RETURN ------------------------------------------------------------------
  
  print(nb_iter_RDB)    
  A<<-menage_bis %>% select(colnames(menage))
  
  
  
}


