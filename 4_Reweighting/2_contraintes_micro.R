# Iter=0
# contraintes_micro <- function(Iter){

# LIBRARIES ---------------------------------------------------------------

library(tidyverse)

# DATA --------------------------------------------------------------------

setwd("D:/CIRED/Projet_Ademe")
load("2010/menage_contraintes_2010.RData")
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/menage_echelle.RData",sep=""))
source("Code_global_Ademe/mutate_when.R")

# Ajout Data DPE ----------------------------------------------------------

# menage_echelle
menage_echelle$DPE_horizon<-as.factor(menage_echelle$DPE_horizon)
dummies_dpe_horizon <- model.matrix(~ DPE_horizon, data = menage_echelle, 
                                 contrasts.arg = list(DPE_horizon = contrasts(menage_echelle$DPE_horizon, contrasts = F)))[,-1]
colnames(dummies_dpe_horizon)<-paste("DPE_m2",LETTERS[1:7],sep="_")
dummies_dpe_horizon<-data.frame("ident_men"=menage_echelle$ident_men,dummies_dpe_horizon)
menage_echelle <-
  menage_echelle %>%
  left_join(dummies_dpe_horizon,by='ident_men') %>%
  mutate_at(vars(starts_with("DPE_m2_")),function(x) as.numeric(x*menage_echelle$surfhab_d))
# head(menage_echelle %>% select(starts_with("DPE_m2_")))
# On exclut G pour le bouclage avec surfhah_d

# AGREGATE DATA -----------------------------------------------------------

menage_contraintes <-
  menage_contraintes_2010 %>%
  left_join(
    menage_echelle %>%
           select(ident_men,
                  npers,
                  nbactoccup,
                  nbchomeurs,
                  rev_activites_sans_etranger,
                  rev_patrimoine,
                  chomage,
                  rev_sociaux_autres,
                  rev_etranger,
                  surfhab_d,
                  DPE_m2_A,
                  DPE_m2_B,
                  DPE_m2_C,
                  DPE_m2_D,
                  DPE_m2_E,
                  DPE_m2_F,
                  nbvehic,
                  VE, 
                  new_VT,
                  new_VE
                      ),    
    by="ident_men") 
# menage_contraintes <-
  # menage_contraintes %>%   
  # mutate(taux_epargne =  menage_echelle$epargne/sum(menage_echelle$RDB*menage_echelle$pondmen))
# #on transforme le taux d'épargne pour pouvoir le sommer comme toutes les autres variables dans l'opération matricielle
# 
menage_contraintes <-
  menage_contraintes %>%
  mutate_when(is.na(nbactoccup),list(nbactoccup=0),
              is.na(nbchomeurs),list(nbchomeurs=0),
              is.na(chomage),list(chomage=0),
              is.na(nbvehic),list(nbvehic=0)
              )
# %>%
#   select(ident_men,
#          npers,
#          nbactoccup,
#          nbchomeurs,
#          rev_activites,
#          rev_patrimoine,
#          chomage,
#          rev_sociaux_autres,
#          rev_etranger,
#          #                   taux_epargne,
#          surfhab_d,
#          nbvehic
#   )
# table(is.na(menage_contraintes$nbactoccup))
# table(is.na(menage_contraintes$rev_patrimoine))


# View(menage_contraintes_horizon)


# POND_INIT ---------------------------------------------------------------

pond_init <-
  menage_echelle %>%
  select(pondmen)


# SAVE FILES --------------------------------------------------------------

save(menage_contraintes,file=
       paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/menage_contraintes.RData",sep=""))
save(pond_init,file=
       paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/pond_init.RData",sep=""))
# save(menage_echelle,file=
       # paste("2025/Iteration_",Iter,"/Input/menage_echelle.RData",sep=""))
# }
print("Repondation : 1_contraintes_micro : SUCCESS")

