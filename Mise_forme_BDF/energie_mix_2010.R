
# Objectif : # Conversion des dépenses énergétiques en consommation en volume d'énergie (kWh) en volumes d'énergie


# Classement des ménages à l'intérieur des classes DPE en fonction de leur consommation surfacique d'énergie pour les usages de chauffage, ECS et clim.

energie_mix_2010<-function(menage){
  
  menage_ener_dom<-menage
  
  # LIBRARIES ---------------------------------------------------------------
  
  library(plyr)
  library(reshape2)
  library(ggplot2)
  library(tidyverse)
  library(readxl)
  
  
  
  
  # IMPORT DATA -------------------------------------------------------------
  
  setwd("D:/CIRED/Projet_Ademe")
  # load("2025/dep_ener_2025.RData")
  
  # load("2025/menage_echelle_2025.RData")
  
  load("Technical_change/TC_renovation_DPE/list_source_usage.RData")
  
  # Import des prix d'énergie par classe de ménage : en €/MWh
  prix_classe <- read.csv2("Donnees_brutes/IMACLIM_SIMONA/Prix_energie_par_classe.csv", header = TRUE, sep = ";",dec = ".", fill = TRUE)
  
  #Importer taux de croissance des prix et des revenus
  # load(paste(scenario,"/",horizon,"/FC_2010_",horizon,".RData",sep=""))
  
  
  
  
  # PREPARATION DONNEES PRIX ENERGIE ----------------------------------------
  
  
  # prix energie par classe de ménage : dans l'ordre : par quintile, type de ménage et type de logement (individuel ou collectif)
  prix_classe <- arrange(prix_classe,quintile,typmen_corr,MI)
  
  
  
  
  
  
  
  # MISE A L'ECHELLE PRIX ENERGIE PAR CLASSE --------------------------------
  
  # Hypothèse : les prix des énergies varient de la même façon pour toutes les classes de ménage entre 2010 et 2025. 
  
  prix_classe_2025<-prix_classe
  
  # A02
  # prix_classe_2025$prix_elec<- prix_classe$prix_elec * FC$A02
  # 
  # # A03
  # prix_classe_2025$prix_gaz<- prix_classe$prix_elec * FC$A02
  # # A04
  # prix_classe_2025[c("prix_fuel","prix_gpl","prix_bois","prix_chaleur")]<- prix_classe[c("prix_fuel","prix_gpl","prix_bois","prix_chaleur")]* FC$A04
  
  
  
  
  
  
  
  # CLASSER MENAGES PAR CLASSE ----------------------------------------------
  
  
  # Matrice des prix de chaque énergie pour chaque classe
  prix_classe_mat <- data.matrix(prix_classe_2025[,c("prix_elec","prix_gaz","prix_fuel","prix_gpl","prix_bois","prix_chaleur")], rownames.force = NA)
  
  
  # Attribution d'un numéro de classe de ménage à chaque ligne de appmen (de 1 à 60)
  menage_ener_dom$classe_men <- 
    with(
      menage_ener_dom,
      as.integer(interaction(MI_corr, typmen_corr, quintileuc)))
  
  menage_ener_dom$classe_men <- 
    as.factor(menage_ener_dom$classe_men)
  
  
  # Traduction de la variable classe_men en matrice d'indicatrices d'appartenance à chacune des 60 classes
  dummies_classe_men <- model.matrix(~ classe_men, 
                                     data = menage_ener_dom, 
                                     contrasts.arg = list(
                                       classe_men = contrasts(
                                         menage_ener_dom$classe_men,
                                         contrasts = F)
                                     )
  )
  
  
  #Suppresssion de la colonne "Intercept", résidu de la méthode de régression employée pour construire les indicatrices
  dummies_classe_men <- dummies_classe_men[,-1] 
  
  
  
  
  
  
  # PRIX ENERGIE PAR MENAGE -------------------------------------------------
  
  
  # Produit matriciel entre les indicatrices et les classes (n ménages x 60 classes) %*% (60 classes x 6 sources énergie)
  prix_menages_2025 <- as.data.frame(dummies_classe_men %*% prix_classe_mat)
  prix_menages_2025_bis<-as.data.frame(prix_menages_2025)
  
  # Rajout colonne "ident_men" pour la fusion avec det_ener
  prix_menages_2025<-cbind(menage_ener_dom$ident_men,prix_menages_2025_bis)
  # renommer les colonnes
  colnames(prix_menages_2025)<-c("ident_men",
                                 "prix_Elec",
                                 "prix_Gaz",
                                 "prix_Fuel",
                                 "prix_GPL",
                                 "prix_Solides",
                                 "prix_Urbain")
  
  # Rajout des prix et pondération de chaque ménage dans dep_ener
  
  menage_ener_dom <- menage_ener_dom %>% left_join(prix_menages_2025,by="ident_men")
  menage_ener_dom
  
  
  
  
  
  # VOLUMES ENERGIE / MENAGE ------------------------------------------------
  
  
  # Pour convertir les dépenses en volumes, division par le prix moyen en 2025 de chaque source d'énergie.
  menage_ener_dom$vol_Elec<-menage_ener_dom$dep_Elec/menage_ener_dom$prix_Elec
  menage_ener_dom$vol_Gaz<-menage_ener_dom$dep_Gaz/menage_ener_dom$prix_Gaz
  menage_ener_dom$vol_GPL<-menage_ener_dom$dep_GPL/menage_ener_dom$prix_GPL
  menage_ener_dom$vol_Fuel<-menage_ener_dom$dep_Fuel/menage_ener_dom$prix_Fuel
  menage_ener_dom$vol_Solides<-menage_ener_dom$dep_Solides/menage_ener_dom$prix_Solides
  menage_ener_dom$vol_Urbain<-menage_ener_dom$dep_Urbain/menage_ener_dom$prix_Urbain
  
  menage_ener_dom$vol_tot<- menage_ener_dom %>% select(starts_with("vol_")) %>% rowSums() 
  
  
  sources=c("Elec","Gaz","GPL","Fuel","Solides","Urbain")
  dep_sources=paste("dep",sources,sep="_")
  
  
  
  
  
  # VOLUMES ENERGIE MACRO-ECONOMIQUES ----------------------------------------
  
  
  menage_ener_dom <- menage_ener_dom %>% mutate(Energie_pond=vol_tot*pondmen)
  
  sum(menage_ener_dom$Energie_pond)
  # 392 450 241
  
  menage_ener_dom$volpond_Elec<-menage_ener_dom$dep_Elec/menage_ener_dom$prix_Elec*menage_ener_dom$pondmen
  menage_ener_dom$volpond_Gaz<-menage_ener_dom$dep_Gaz/menage_ener_dom$prix_Gaz*menage_ener_dom$pondmen
  menage_ener_dom$volpond_GPL<-menage_ener_dom$dep_GPL/menage_ener_dom$prix_GPL*menage_ener_dom$pondmen
  menage_ener_dom$volpond_Fuel<-menage_ener_dom$dep_Fuel/menage_ener_dom$prix_Fuel*menage_ener_dom$pondmen
  menage_ener_dom$volpond_Solides<-menage_ener_dom$dep_Solides/menage_ener_dom$prix_Solides*menage_ener_dom$pondmen
  menage_ener_dom$volpond_Urbain<-menage_ener_dom$dep_Urbain/menage_ener_dom$prix_Urbain*menage_ener_dom$pondmen
  
  sum(menage_ener_dom$volpond_Elec)
  sum(menage_ener_dom$volpond_Gaz)
  sum(menage_ener_dom$volpond_GPL)
  sum(menage_ener_dom$volpond_Fuel)
  sum(menage_ener_dom$volpond_Solides)
  sum(menage_ener_dom$volpond_Urbain)
  
  sum(menage_ener_dom$volpond_Elec)/sum(menage_ener_dom$Energie_pond)
  sum(menage_ener_dom$volpond_Gaz)/sum(menage_ener_dom$Energie_pond)
  sum(menage_ener_dom$volpond_GPL)/sum(menage_ener_dom$Energie_pond)
  sum(menage_ener_dom$volpond_Fuel)/sum(menage_ener_dom$Energie_pond)
  sum(menage_ener_dom$volpond_Solides)/sum(menage_ener_dom$Energie_pond)
  sum(menage_ener_dom$volpond_Urbain)/sum(menage_ener_dom$Energie_pond)
  
  
  Mix_ener<-c(  sum(menage_ener_dom$volpond_Elec)/sum(menage_ener_dom$Energie_pond),
                sum(menage_ener_dom$volpond_Gaz)/sum(menage_ener_dom$Energie_pond),
                sum(menage_ener_dom$volpond_GPL)/sum(menage_ener_dom$Energie_pond),
                sum(menage_ener_dom$volpond_Fuel)/sum(menage_ener_dom$Energie_pond),
                sum(menage_ener_dom$volpond_Solides)/sum(menage_ener_dom$Energie_pond),
                sum(menage_ener_dom$volpond_Urbain)/sum(menage_ener_dom$Energie_pond))
  Mix_ener<-t(data.frame(Mix_ener))
  colnames(Mix_ener)<-sources
  return(Mix_ener)
  # ENERGIE DOMESTIQUE ------------------------------------------------------
  
  dep_source_usage<-menage_ener_dom[c("ident_men",list_source_usage)]
  
  dep_source_usage[colnames(dep_source_usage %>% select(contains("Elec")))]<-
    dep_source_usage[colnames(dep_source_usage %>% select(contains("Elec")))]/menage_ener_dom$prix_Elec
  
  dep_source_usage[colnames(dep_source_usage %>% select(contains("Gaz")))]<-
    dep_source_usage[colnames(dep_source_usage %>% select(contains("Gaz")))]/menage_ener_dom$prix_Gaz
  
  dep_source_usage[colnames(dep_source_usage %>% select(contains("GPL")))]<-
    dep_source_usage[colnames(dep_source_usage %>% select(contains("GPL")))]/menage_ener_dom$prix_GPL
  
  dep_source_usage[colnames(dep_source_usage %>% select(contains("Fuel")))]<-
    dep_source_usage[colnames(dep_source_usage %>% select(contains("Fuel")))]/menage_ener_dom$prix_Fuel
  
  dep_source_usage[colnames(dep_source_usage %>% select(contains("Solides")))]<-
    dep_source_usage[colnames(dep_source_usage %>% select(contains("Solides")))]/menage_ener_dom$prix_Solides
  
  dep_source_usage[colnames(dep_source_usage %>% select(contains("Urbain")))]<-
    dep_source_usage[colnames(dep_source_usage %>% select(contains("Urbain")))]/menage_ener_dom$prix_Urbain
  
  
  
  
  # Energie de chauffage
  dep_source_usage$ener_chauff<-
    dep_source_usage %>% select(contains("_chauff")) %>% rowSums()
  
  # Energie Eau Chaude Sanitaire
  dep_source_usage$ener_ECS<-
    dep_source_usage %>% select(contains("_ECS")) %>% rowSums()
  
  # Energie de climatisation
  dep_source_usage$ener_clim<-
    dep_source_usage %>% select(contains("_clim")) %>% rowSums()
  
  
  # Energie domestique (chauff+ECS+clim)
  dep_source_usage$ener_dom<-
    dep_source_usage$ener_chauff+
    dep_source_usage$ener_ECS+
    dep_source_usage$ener_clim
  # sum(dep_source_usage$ener_dom)
  
  # # Energie domestique surfacique
  # dep_source_usage$ener_dom_surf<-dep_source_usage$ener_dom/menage_ener_dom$surfhab_d
  # 
  mean(dep_source_usage$ener_dom_surf) #MWh
  # EN 2025
  # [1] 0.08071984
  # EN 2010
  # > mean(dep_source_usage$ener_dom_surf) #MWh
  # [1] 0.1145678
  
  # Tracer la distribution d'énergie surfacique
  # g<-ggplot(dep_source_usage,aes(x=dep_source_usage$ener_dom_surf*1000))+geom_histogram(aes(y=..density..), colour="black", fill="white")+
  #   geom_density(alpha=.7, fill="#FF6666")+xlim(0,1000)
  # g
  
  
  
  
  
  # EXPORTER DONNEES ENER_DOM  ----------------------------------------------
  
  # menage_ener_dom_2025<-dep_source_usage %>% select(ident_men,list_source_usage,ener_dom_surf)
  # menage_ener_dom_2025[list_source_usage]<-menage_ener_dom_2025[list_source_usage]*1000
  # menage_ener_dom<-
  #   menage_ener_dom %>%
  #   left_join(dep_source_usage %>% select(ident_men,ener_dom_surf),by="ident_men")
  
  # dep_source_usage<<-dep_source_usage %>% mutate(ener_dom_surf=ener_dom_surf*1000) %>% select(ident_men,ener_dom_surf)
  rm(dep_sources,menage_ener_dom)
  
  # save(menage_ener_dom_2025,file="2025/menage_ener_dom_2025.RData")
  
  
  # SUCCESS -----------------------------------------------------------------
  
  print("calc_energie_kWh_m2 : SUCCESS")
}
