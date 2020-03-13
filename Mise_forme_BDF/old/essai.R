load("2010/menage_forme_essai.RData")
load("2010/solde.RData")

menage_forme$decuc1<-as.numeric(as.character(menage_forme$decuc1))

source("Mise_forme_BDF/1_1_ventilation_rehab.R")

Ventil_solde(menage_forme,solde)
print(head(menage_forme$decuc1))

i=1549
View(rbind(menage_forme%>% filter(ident_men==i),A %>% filter(ident_men==i),A%>% filter(ident_men==i)-menage_forme%>% filter(ident_men==i)))
dim(A)



C<-(A%>% filter(ident_men==i)-menage_echelle_2025%>% filter(ident_men==i))
rowSums(C[list_dep[-c(16)]]) # 23115.94
rowSums(C[list_dep]) # 23115.94