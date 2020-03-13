
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
  
  menage<- menage %>% mutate(dep_autres_energies=dep_Fuel+dep_GPL+dep_Solides+dep_Urbain)%>%mutate(BTP=BTP+c13711_neuf)
  menage$loyers<-menage$loyers+menage$rev801
  menage$Rcons_bis <- rowSums(menage[list_dep])
  sum(rowSums(menage[list_dep])*menage$pondmen)
sum_tot<-menage%>%summarise(sum(Rcons_bis*pondmen))
  

  
  for (i in 1:13){
    k=list_dep[i]
    print(k)
    if(i<10){
      menage[paste("share_A0",i,sep="")]<-menage[k]/menage$Rcons_bis
      assign(paste("vol_A0",i,sep=""),menage %>% summarise(sum(get(k)*pondmen)))
      }
    else{ menage[paste("share_A",i,sep="")]<-menage[k]/menage$Rcons_bis
    assign(paste("vol_A",i,sep=""),menage %>% summarise(sum(get(k)*pondmen)))}
  }
  
vol_tot<-vol_A01+vol_A02+vol_A03+vol_A04+vol_A05+vol_A06+vol_A07+vol_A08+vol_A09+vol_A10+vol_A11+vol_A12+vol_A13

vol_A01/vol_tot
  
  
  share=rep(1,13)
  vol=rep(1,13)
  col=rep(1,13)
  for (i in 1:13){
    if(i<10){
      k=paste("share_A0",i,sep="")
      col[i]=k
      share[i]=as.numeric(menage %>% summarise(weighted.mean(get(k),pondmen*Rcons_bis)))
      # vol[i]=as.numeric(menage %>% summarise(sum(get(k)*pondmen)))
      }
    else{ 
      k=paste("share_A",i,sep="")
      col[i]=k
      share[i]=as.numeric(menage %>% summarise(weighted.mean(get(k),pondmen*Rcons_bis)))
      # vol[i]=as.numeric(menage %>% summarise(sum(get(k)*pondmen)))
      }
  }
  
  
  
  menage<- menage %>% mutate(dep_autres_energies=dep_Fuel+dep_GPL+dep_Solides+dep_Urbain)%>%mutate(BTP=BTP+c13711_neuf)
  
  menage$loyers<-menage$loyers+menage$rev801
  
  value<-c(
    as.numeric((menage%>%summarise(sum(agriculture*pondmen)))[1,]),
    as.numeric((menage%>%summarise(sum(dep_Elec*pondmen)))[1,]),
    as.numeric((menage%>%summarise(sum(dep_Gaz*pondmen)))[1,]),
    as.numeric((menage%>%summarise(sum(dep_autres_energies*pondmen)))[1,]),
    as.numeric((menage%>%summarise(sum(BTP*pondmen)))[1,]),
    as.numeric((menage%>%summarise(sum(prod_veh*pondmen)))[1,]),
    as.numeric((menage%>%summarise(sum(carb_lubr*pondmen)))[1,]),
    as.numeric((menage%>%summarise(sum(transp_rail_air*pondmen)))[1,]),
    as.numeric((menage%>%summarise(sum(transp_routes_eau*pondmen)))[1,]),
    as.numeric((menage%>%summarise(sum(loisirs_com*pondmen)))[1,]),
    as.numeric((menage%>%summarise(sum(autres_services*pondmen)))[1,]),
    as.numeric((menage%>%summarise(sum(autres*pondmen)))[1,]),
    as.numeric((menage%>%summarise(sum(loyers*pondmen)))[1,]))
  
  
  Parts=as.data.frame(rbind(share,value))
  colnames(Parts)=col
  # View(Parts)
  # return(Parts)
  write.csv2(Parts,file="2010/parts_2010_export.csv")

  share_2010<-t(Parts)
  share_2010<-as.data.frame(rbind(share_2010,epargne_2010))
  save(share_2010,file="2010/share_2010.RData")

# compute_share(menage_calibr_2010)
# # compute_share(menage_echelle_2025)
