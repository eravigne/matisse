
# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(readxl)

# DATA --------------------------------------------------------------------

Elast<- read_excel("D:/CIRED/Projet_Ademe/Donnees_brutes/Econometrie_demande/ESSAI_2019_04_15_elast_prix_rev.xlsx")

Elast <-
  Elast %>%
  gather(key=CODADEME_typ,value=elast,-c(1,2)) %>%
  separate(CODADEME_typ,into=c("CODADEME","typ_elast"),sep="_")

colnames(Elast)<- 
  c("Decile",
    "Typo",
    "CODADEME",
    "typ_elast",
    "elast"
    )

# View(Elast)


# SAVE --------------------------------------------------------------------

save(Elast,file="D:/CIRED/Projet_Ademe/Donnees_brutes/Econometrie_demande/elast_prix_rev.RData")
