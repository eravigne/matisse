# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(decisionSupport)
library(readxl)
library(car)
library(plyr)

# DATA --------------------------------------------------------------------


setwd("D:/CIRED/Projet_Ademe")
source("Code_global_Ademe/mutate_when.R")
source("Code_global_Ademe/compute_share.R")
source("Code_global_Ademe/compute_share_export.R")
source("Code_global_Ademe/verif_epargne_taux.R")
source("Code_global_Ademe/compute_savings_rate_export.R")
source("Code_global_Ademe/maj_dep_preeng.R")
source("Technical_change/Repayment.R")
source("Technical_change/TC_renovation_DPE/calc_energie_kWh_m2.R")
load("Technical_change/TC_renovation_DPE/list_source_usage.RData")

sources=c("Elec","Gaz","Fuel","GPL","Urbain","Solides")
dep_sources=paste("dep",sources,sep="_")
list_dep=c("agriculture",
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


# LOAD --------------------------------------------------------------------

load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_43.RData",sep=""))

menage_echelle<-menage_echelle_43

# Prix_menages ------------------------------------------------------------

# Import des prix d'énergie par classe de ménage : en €/MWh
prix_classe <- read.csv2("Donnees_brutes/IMACLIM_SIMONA/Prix_energie_par_classe.csv", header = TRUE, sep = ";",dec = ".", fill = TRUE)

#Importer taux de croissance des prix et des revenus
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))




# PREPARATION DONNEES PRIX ENERGIE ----------------------------------------


# prix energie par classe de ménage : dans l'ordre : par quintile, type de ménage et type de logement (individuel ou collectif)
prix_classe <- arrange(prix_classe,quintile,typmen_corr,MI)

# MISE A L'ECHELLE PRIX ENERGIE PAR CLASSE --------------------------------

# Hypothèse : les prix des énergies varient de la même façon pour toutes les classes de ménage entre 2010 et 2025. 

prix_classe_horizon<-prix_classe

# A02
prix_classe_horizon$prix_elec<- prix_classe$prix_elec * FC$A02

# A03
prix_classe_horizon$prix_gaz<- prix_classe$prix_gaz * FC$A03
# A04
prix_classe_horizon[c("prix_fuel","prix_gpl","prix_bois","prix_chaleur")]<- prix_classe[c("prix_fuel","prix_gpl","prix_bois","prix_chaleur")]* FC$A04




# CLASSER MENAGES PAR CLASSE ----------------------------------------------


# Matrice des prix de chaque énergie pour chaque classe
prix_classe_mat <- data.matrix(prix_classe_horizon[,c("prix_elec","prix_gaz","prix_fuel","prix_gpl","prix_bois","prix_chaleur")], rownames.force = NA)


# Attribution d'un numéro de classe de ménage à chaque ligne de appmen (de 1 à 60)
menage_echelle$classe_men <- 
  with(
    menage_echelle,
    as.integer(interaction(MI_corr, typmen_corr, quintileuc)))

menage_echelle$classe_men <- 
  as.factor(menage_echelle$classe_men)


# Traduction de la variable classe_men en matrice d'indicatrices d'appartenance à chacune des 60 classes
dummies_classe_men <- model.matrix(~ classe_men, 
                                   data = menage_echelle, 
                                   contrasts.arg = list(
                                     classe_men = contrasts(
                                       menage_echelle$classe_men,
                                       contrasts = F)
                                   )
)


#Suppresssion de la colonne "Intercept", résidu de la méthode de régression employée pour construire les indicatrices
dummies_classe_men <- dummies_classe_men[,-1] 






# PRIX ENERGIE PAR MENAGE -------------------------------------------------


# Produit matriciel entre les indicatrices et les classes (n ménages x 60 classes) %*% (60 classes x 6 sources énergie)
prix_menages_horizon <- as.data.frame(dummies_classe_men %*% prix_classe_mat)
prix_menages_horizon_bis<-as.data.frame(prix_menages_horizon)

# Rajout colonne "ident_men" pour la fusion avec det_ener
prix_menages_horizon<-cbind(menage_echelle$ident_men,prix_menages_horizon_bis)
# renommer les colonnes
colnames(prix_menages_horizon)<-c("ident_men",
                               "prix_Elec",
                               "prix_Gaz",
                               "prix_Fuel",
                               "prix_GPL",
                               "prix_Solides",
                               "prix_Urbain")

# Rajout des prix et pondération de chaque ménage dans dep_ener

menage_echelle <- menage_echelle %>% left_join(prix_menages_horizon,by="ident_men")


# VOLUMES ENERGIE / MENAGE ------------------------------------------------


# Pour convertir les dépenses en volumes, division par le prix moyen en 2025 de chaque source d'énergie.
# menage_echelle$vol_Elec<-menage_echelle$Elec/menage_echelle$prix_Elec
# menage_echelle$vol_Gaz<-menage_echelle$Gaz/menage_echelle$prix_Gaz
# menage_echelle$vol_GPL<-menage_echelle$GPL/menage_echelle$prix_GPL
# menage_echelle$vol_Fuel<-menage_echelle$Fuel/menage_echelle$prix_Fuel
# menage_echelle$vol_Solides<-menage_echelle$Solides/menage_echelle$prix_Solides
# menage_echelle$vol_Urbain<-menage_echelle$Urbain/menage_echelle$prix_Urbain


menage_echelle <- menage_echelle %>% mutate_when(is.na(Gaz_ECS),list(Gaz_ECS=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(GPL_ECS),list(GPL_ECS=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Fuel_ECS),list(Fuel_ECS=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Solides_ECS),list(Solides_ECS=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Urbain_ECS),list(Urbain_ECS=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Gaz_chauff),list(Gaz_chauff=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(GPL_chauff),list(GPL_chauff=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Fuel_chauff),list(Fuel_chauff=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Solides_chauff),list(Solides_chauff=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Urbain_chauff),list(Urbain_chauff=0))


for (x in c("Gaz_ECS","GPL_ECS","Fuel_ECS","Solides_ECS","Urbain_ECS","Gaz_chauff",
            "GPL_chauff","Fuel_chauff","Solides_chauff","Urbain_chauff")
){
  
  
  
  if (str_detect(x,"Gaz")){menage_echelle[paste(x,"vol",sep="_")]<-menage_echelle[x]/menage_echelle$prix_Gaz}
  if (str_detect(x,"GPL")){menage_echelle[paste(x,"vol",sep="_")]<-menage_echelle[x]/menage_echelle$prix_GPL}
  if (str_detect(x,"Fuel")){menage_echelle[paste(x,"vol",sep="_")]<-menage_echelle[x]/menage_echelle$prix_Fuel}
  if (str_detect(x,"Urbain")){menage_echelle[paste(x,"vol",sep="_")]<-menage_echelle[x]/menage_echelle$prix_Urbain}
  if (str_detect(x,"Solides")){menage_echelle[paste(x,"vol",sep="_")]<-menage_echelle[x]/menage_echelle$prix_Solides}
}




menage_echelle$vol_basc_elec_ECS <- rowSums(menage_echelle%>%select(ends_with("ECS_vol")))
menage_echelle$vol_basc_elec_chauff <- rowSums(menage_echelle%>%select(ends_with("chauff_vol")))


menage_echelle <-
  menage_echelle %>%
  mutate(dep_basc_elec_ECS=vol_basc_elec_ECS*prix_Elec)%>%
  mutate(dep_basc_elec_chauff=vol_basc_elec_chauff*prix_Elec)


menage_echelle$dep_non_elec<-rowSums(menage_echelle %>% select(c("Gaz_ECS","GPL_ECS","Fuel_ECS","Solides_ECS","Urbain_ECS","Gaz_chauff",
                                                                 "GPL_chauff","Fuel_chauff","Solides_chauff","Urbain_chauff")))

solde<- 
  menage_echelle %>%
  mutate(solde=dep_basc_elec_ECS+dep_basc_elec_chauff-dep_non_elec #<0 => économie si les dépenses équivalentes en élec sont plus faibles que les dépenses en gaz,fuel, solides,etc
  )%>%
select(ident_men,solde,classe_arr)

solde<-
  solde %>% 
  mutate_when(!classe_arr=="A",list(solde=0))%>%
select(-classe_arr)

# menage_echelle%>%filter(ident_men %in% c(sapply(solde%>%filter(classe_arr=="A" & solde==0)%>%select(ident_men),function(x) as.numeric(x))))%>% 
#   filter(!dep_basc_elec_ECS==0) %>%
#   filter( !dep_basc_elec_chauff==0)%>%
#   filter(!dep_non_elec==0)%>%
#   select(ident_men)
# 
# table(solde$solde==0)
# 
# FALSE  TRUE 
# 739  9550 
# > table(menage_echelle$classe_arr=="A")
# 
# FALSE  TRUE 
# 9422   867 
# > table(menage_echelle %%> filter(dep_non_elec==0)%>%select(classe_arr)=="A")
# Error: unexpected '>' in "table(menage_echelle %%>"
# > table(menage_echelle%>%filter(dep_non_elec==0)%>%select(classe_arr)=="A")
# 
# FALSE  TRUE 
# 2117   128 
# > 867-128
# [1] 739
  

menage_echelle <- 
  menage_echelle %>%
  mutate_when(classe_arr=="A",list(Gaz_ECS=0,
                                   GPL_ECS=0,
                                   Fuel_ECS=0,
                                   Solides_ECS=0,
                                   Urbain_ECS=0,
                                   Gaz_chauff=0,
                                   GPL_chauff=0,
                                   Fuel_chauff=0,
                                   Solides_chauff=0,
                                   Urbain_chauff=0,
                                   Elec_chauff=Elec_chauff+dep_basc_elec_chauff,
                                   Elec_ECS=Elec_ECS+dep_basc_elec_ECS))



# Reventilation -----------------------------------------------------------


sauv_avant_reventil<-menage_echelle

source("Technical_change/Econometrie_solde_budg_Logement.R")
# source("Technical_change/Econometrie_solde_budg_bouclage_autres.R")

Ventil_solde(solde,menage_echelle)


menage_echelle<-A
  


  
# Rcons
menage_echelle$Rcons <- 
  rowSums(menage_echelle[list_dep])



# Recalcul des toutes les variables  --------------------------------------

# Parts budgétaires
for (k in list_dep){
  menage_echelle[paste("part",k,sep="_")]<-menage_echelle[k]/menage_echelle$Rcons
}

# Epargne
menage_echelle$epargne <- 
  menage_echelle$RDB - 
  menage_echelle$Rcons + 
  menage_echelle$rev_exceptionnel

# Ratio_S
menage_echelle$ratio_S <-  
  menage_echelle$epargne / 
  menage_echelle$Rcons 

# Taux épargne
menage_echelle$taux_epargne<- ifelse(menage_echelle$RDB==0,0,
                                     menage_echelle$epargne / 
                                       menage_echelle$RDB)



source("Technical_change/TC_renovation_DPE/calc_energie_kWh_m2.R")

menage_echelle <- menage_echelle %>% select(colnames(menage_echelle_43))

energie_dom_surf(menage_echelle)

menage_echelle<- 
  menage_echelle %>% 
  select(-ener_dom_surf,-ener_dom) %>%
  left_join(dep_source_usage,by="ident_men")

menage_echelle$dep_energie=rowSums(menage_echelle[dep_sources])
menage_echelle$dep_energie_logement=rowSums(menage_echelle[
  c("Elec_ECS","Gaz_ECS","GPL_ECS","Fuel_ECS","Solides_ECS","Urbain_ECS","Elec_chauff","Gaz_chauff",
    "GPL_chauff","Fuel_chauff","Solides_chauff","Urbain_chauff","Elec_clim")])





# Ecraser colonnes surnuméraires ------------------------------------------



inter<-intersect(colnames(menage_echelle), colnames(menage_echelle_43))


# dep_ener_horizon_43<-menage_echelle_43 %>% select(ident_men,not_inter,-DPE_2024)
menage_echelle_44<-menage_echelle %>% select(inter)




# Maj_dep_preeng ----------------------------------------------------------

menage_echelle_44 <- maj_dep_preeng(bdd1= menage_echelle_43,bdd2=menage_echelle_44)






# Export ------------------------------------------------------------------


menage_echelle_TC_DPE<-menage_echelle_44
save(menage_echelle_TC_DPE, file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_TC_DPE.RData",sep=""))



# Test --------------------------------------------------------------------

print(compute_share_export(menage_echelle_44))
print(compute_savings_rate_export(menage_echelle_44))


print("4_4 Success Bascule A")

