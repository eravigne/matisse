
# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(readxl)

# DATA --------------------------------------------------------------------
setwd("D:/CIRED/Projet_Ademe")
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
source("Code_global_ADEME/compute_savings_rate_export.R")
source("Code_global_ADEME/compute_share_export.R")
source("Code_global_ADEME/compute_share_export_bis.R")


# SAVINGS -----------------------------------------------------------------

# La différence avec RDBAI c'est l'absence des transferts entre ménages. rev700, rev701 et rev999
# REV700 :Sommes reçues régulièrement d'un autre ménage (qui doit les verser obligatoirement
# REV701  :Sommes reçues régulièrement d'un autre ménage (qui les verse librement)
# REV999 Autres ressources
# Les taux d'imposition sont calculés en prenant en compte les transferts des autres ménages qui sont imposés (hors exonération sous réserve d'une limite de revenus maximaux.)

#NB : il faudrait retirer également les transferts vers l'étranger, ne semblent pas apparaître dans BDF.


epargne<-compute_savings_rate_export(menage_echelle)     
epargne
# epargne_ratio<-epargne/epargne_2010

# Export csv --------------------------------------------------------------

load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/Cout_bailleur_public.RData",sep=""))
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/sBCE.RData",sep=""))

# share<-compute_share_export(menage_echelle)
# ELEC, GAS, OIL, CL sont des variations par rapports à 2010 en euros constants €2010
share<-compute_share_export_bis(menage_echelle,s=scenario,h=horizon,sc=scenario_classement,r=redistribution,iter=0)
share
export<-t(data.frame(share,"epargne"=epargne,"sBCE"=sBCE,"Renovation_BS"=Cout_bailleur_public))
# save(export,file="D:/CIRED/Projet_Ademe/2025/Iteration_0/Output/export_Iter_1_micro.csv")



# write.csv(export,file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/export_Iter_1_micro.csv",sep=""))
# write.csv(export,file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/Input_macro.csv",sep=""))
write.csv(export,file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/export_Iter_",Iter,".csv",sep=""))



