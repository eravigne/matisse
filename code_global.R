
# LIBRARIEs ----------------------------------------------------------------

setwd("D:/CIRED/Projet_Ademe")
library(tidyverse)

source("Code_global_ADEME/compute_share.R")
source("Code_global_ADEME/verif_epargne_taux.R")
source("Code_global_ADEME/compute_share_export.R")
source("Code_global_ADEME/compute_share_export_bis.R")
source("Code_global_ADEME/compute_savings_rate_export.R")


# SCENARIO ----------------------------------------------------------------

# horizon=2035
# scenario="AMS"
# scenario_classement="Optimiste"
# redistribution="forfait"
# Iter=0


# MISE EN FORME -----------------------------------------------------------
# 
source("D:/CIRED/Projet_Ademe/Mise_forme_BDF/0_mise_forme_data_3ME.R")
# rm(list = ls())
# Réécrire comme une fonction avec scénario
# #
# source("D:/CIRED/Projet_Ademe/Mise_forme_BDF/0_mise_forme_elast.R")
# # # rm(list = ls())
# #
# source("D:/CIRED/Projet_Ademe/Mise_forme_BDF/1_imputation_DPE_2010.R")
# # # rm(list = ls())
# #
# # # code mise_en_forme_bdf
# source("D:/CIRED/Projet_Ademe/Mise_forme_BDF/2_mise_forme_BDF.R")
# # # rm(list = ls())
# # # Economie 14 secteurs
# #
# source("D:/CIRED/Projet_Ademe/Mise_forme_BDF/4_contraintes_calages.R")
# # #
# source("D:/CIRED/Projet_Ademe/Mise_forme_BDF/5_savings_rate_export_2010.R")
# # epargne_2010
# # # [1] 0.1424462
# #
# source("D:/CIRED/Projet_Ademe/Mise_forme_BDF/parts_export.R")

# Parts

# MISE à l'ECHELLE --------------------------------------------------------


source("D:/CIRED/Projet_Ademe/Mise_echelle_BDFE/0_donnees_IMACLIM.R")
# rm(list = ls())
# Réécrire comme une fonction avec scénario

# source("D:/CIRED/Projet_Ademe/Mise_echelle_BDFE/2_Mise_echelle_dep_elast_19_11_2019.R")
source("D:/CIRED/Projet_Ademe/Mise_echelle_BDFE/2_Mise_echelle_dep_elast.R")
# rm(list = ls())


source("D:/CIRED/Projet_Ademe/Mise_echelle_BDFE/share_savings_rate_export.R")
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/menage_echelle_export.RData",sep=""))
share_echelle

# Changement Technique DPE -------------------------------------------------

source("D:/CIRED/Projet_Ademe/Technical_change/TC_renovation_DPE/1_mise_echelle_energies_detaillees.R")
# rm(list = ls())

source("D:/CIRED/Projet_Ademe/Technical_change/TC_renovation_DPE/4_1_Achat_neuf_2025.R")
# rm(list = ls())
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_41.RData",sep=""))
# # # compute_share(menage_echelle_41)
# # # compute_savings_rate(menage_echelle_41)
compute_share_export(menage_echelle_41)
compute_savings_rate_export(menage_echelle_41)



source("D:/CIRED/Projet_Ademe/Technical_change/TC_renovation_DPE/4_2_Achat_2010_2024.R")
# rm(list = ls())
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_42.RData",sep=""))
# # compute_share(menage_echelle_42)
# # compute_savings_rate(menage_echelle_42)
compute_share_export(menage_echelle_42)
compute_savings_rate_export(menage_echelle_42)


source("D:/CIRED/Projet_Ademe/Technical_change/TC_renovation_DPE/4_3_Rehab_2010_2024.R")
# rm(list = ls())
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_43.RData",sep=""))
# compute_share(menage_echelle_43)
# compute_savings_rate(menage_echelle_43)
compute_share_export(menage_echelle_43)
# compute_share_export_bis(menage_echelle_43)
compute_savings_rate_export(menage_echelle_43)
# source("D:/CIRED/Projet_Ademe/Technical_change/TC_renovation_DPE/4_4_Rehab_2025.R")
# rm(list = ls())

source("D:/CIRED/Projet_Ademe/Technical_change/TC_renovation_DPE/4_4_Bascule_A.R")
# rm(list = ls())


# Changement technique VE -------------------------------------------------

# source("D:/CIRED/Projet_Ademe/VE/TC_VE/1_VE_classement_2025.R")
# rm(list = ls())

source("D:/CIRED/Projet_Ademe/Technical_change/TC_VE/2_VE_2010_2025.R")
# rm(list = ls())

load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_TC_VE.RData",sep=""))
# # compute_share(menage_echelle_TC_VE)
# # compute_savings_rate(menage_echelle_TC_VE)
compute_share_export(menage_echelle_TC_VE)
# compute_share_export_bis(scenario,horizon,scenario_classement,redistribution)
compute_savings_rate_export(menage_echelle_TC_VE)

#1er CALAGE ------------------------------------------------------------------

source("D:/CIRED/Projet_Ademe/Reponderation/1_contraintes_macro.R")
# contraintes_macro(Iter=2)
# rm(list = ls())

source("D:/CIRED/Projet_Ademe/Reponderation/2_contraintes_micro.R")
# contraintes_micro(Iter=2)
# rm(list = ls())


source("D:/CIRED/Projet_Ademe/Reponderation/3_pondR.R")
# pondR(Iter=2,m2VP=TRUE)

source("D:/CIRED/Projet_Ademe/Reponderation/4_export_agregats.R")


source("D:/CIRED/Projet_Ademe/Reponderation/5_export_share_savings.R")
# rm(list = ls())

load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
compute_share_export(menage_echelle)
compute_savings_rate_export(menage_echelle)

# LOAD & SAVE -------------------------------------------------------------



# 1. Mise forme BDF 2010 ----
# 2. Importer résultats Imaclim/ThreeMe ----
# 3. Segmentation en typologie des ménages pour appliquer élasticité ----
# 4. Mise échelle revenus ----
# 5. Mise échelle parts budgets 2025 ----
# 6. Mise échelles dépenses à partir parts ----
# 7. Calage sur marge ----
# 8. Changement technique ----
# 9. Export des données agrégées pour Imaclim ----
# 10. Enregistrer fichiers ----

