

compute_savings_rate_export<-function(menage){
  
  source("D:/CIRED/Projet_Ademe/Code_global_Ademe/mutate_when.R")
  menage$AID[is.na(menage$AID)]<-0
  menage$impot_revenu[is.na(menage$impot_revenu)]<-0  

  menage$RDB_new <- rowSums(menage[c("rev_activites","rev_patrimoine","rev_sociaux","rev_TCO")])-rowSums(menage[c("impot_revenu","AID")])
  
  menage$c13711[is.na(menage$c13711)]<-0
  
  menage <- 
    menage %>%
    mutate(c13711_neuf=0)%>%
    mutate_when(year_neuf==horizon,list(c13711_neuf=c13711))
  

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
  menage<-menage%>%filter(!ident_men%in% c(6369,10583))
  
  return(
    sum(menage$taux_epargne_bis*menage$pondmen*menage$RDB_new/sum(menage$RDB_new*menage$pondmen,na.rm=T),na.rm = T) 
    )
}
