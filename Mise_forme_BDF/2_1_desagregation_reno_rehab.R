# OBJECTIF : Désagreger les dépenses de gros_travaux de 2010 entre les travaux de rénovation "classique" et de "réhabiliation énergétique". 
# PRINCIPE :  On trace les seuils correspondant à la rénovation de la classe X à A, de X à B, X à C, etc. 
# On a de ThreeME on a la somme en € de transition de X à A, de X à B, de X à C, etc. On remplit cette somme d'investissement ThreeME en partant des seuils et en montant dans les dépenses GT par m2, juste en dessous de ce seuil si ça ne marche pas. On part des seuils pour rester au plus des montants théoriques. En effet plus les GT sont important plus il y a de chance qu'il s'agisse d'une unique salve de travaux pour les ménages (peu de proba de faire deux fois des GT dans l'année)
# Par définition, n a enlevé des ménages les 7 milliards de rénovation énergétique sans changer leur classe de DPE => on a fait aucune rehab en 2010 et toutes les rehab 2025 vont être exogènes, les dep gros travaux qui restent sont hors rehab énergétiques. 
# 
# Cette désagrégation en 2010 va nous donner Gros travaux = gros_travaux_reno + gros_travaux_rehab
# 
# Pour projeter 2025 on applique l'économétrie seulement à gros_travaux_réno  
# Donc ma projection de revenu 
# 2010 : R1 + … + R8 = R 
# 2025 : R'1 + …. + R'8 + rehab_2010 = R'+ rehab_2010 

# LIBRARIES ---------------------------------------------------------------

library(tidyverse)


# DATA --------------------------------------------------------------------

setwd("D:/CIRED/Projet_Ademe/")


load("2010/c05_2010.RData")
load("2010/menage_DPE.RData")
load("Donnees_brutes/Sorties ThreeMe/ThreeME.RData")

# Extract c13_2010 -------------------------------------------------------------

c13_2010<- 
  c05 %>% 
  select(ident_men,starts_with("c13"))

# Sélection des ménages de c13
c13_2010 <- 
  c13_2010 %>% 
  filter(ident_men %in% menage_forme$ident_men)

save(c13_2010,file="2010/c13_2010.RData")








# PREPARATION DONNEES COUT REHAB THREEME ----------------------------------


Cost_m2=c()
# travaux de rénovation énergétiques en volume par saut de classe (en M2) Transition de L vers M
for (L in LETTERS[1:7]){
  #L : classe DPE de départ
  
  for (M in LETTERS[1:7]){
    # M : classe DPE d'arrivée
    # On vérifie que la transition est une amélioration (M "mieux" que L)
    
    if(L>M){
      
      #extraction stock m2 passant de M à L en 2010 (ThreeME)
      stock_m2<-as.numeric(
        ThreeME %>% 
          filter(Var==paste("REHAB_H01_C",L,"_C",M,"_2",sep="")) %>%
          filter(year==2010) %>%
          select(value)
      )
      
      #extraction coût des travaux pour passer de M à L en 2010 (ThreeME) en M€
      stock_euros<-as.numeric(
        ThreeME %>% 
          filter(Var==paste("PREHAB_H01_C",L,"_C",M,"_2*","REHAB_H01_C",L,"_C",M,"_2",sep="")) %>%
          filter(year==2010) %>%
          select(value)
      )
      
      # stock_euros/stock_m2 = coût de la réhabiliation par m2 (en €/m2)
      # Création matrice Cost_m2 : DPE_départ | DPE_arrivée | coût_m2 | coût_total_transition
      Cost_m2=rbind(Cost_m2,c(L,M,stock_euros/stock_m2*(10^6),stock_euros))
    }
  }
}

colnames(Cost_m2)<-
  c(
    "classe_dep",
    "classe_arr",
    "cost_m2",
    "transition_tot_Meuros"
  )
# convertir en data.frame
Cost_m2<-as.data.frame(Cost_m2,stringsAsFactors=F)










# Préparation données ménages ---------------------------------------------


# Création de ménage_GT_2010
menage_GT_2010<-
  menage_forme %>% 
  select(ident_men, pondmen,surfhab_d) 

# Sélection des ménages ayant réalisé des gros travaux en 2010 (résidence principale ou secondaire)
c13_2010_GT<-
  c13_2010 %>% 
  filter(c13411+c13421>0) %>% 
  select(c13411,c13421,ident_men)%>% 
  mutate(GT=c13411+c13421)

# Ajouter les classes DPE de 2010 (classes de départ) 
c13_2010_GT<- 
  c13_2010_GT %>% 
  left_join(menage_GT_2010,by="ident_men") %>% 
  left_join(menage_DPE,by="ident_men") 





# CLASSEMENT DES MENAGES PAR GT AU M2 PAR DPE -----------------------------



# Ajouter GT/m2
c13_2010_GT <- 
  c13_2010_GT %>% 
  mutate(GT_m2=GT/surfhab_d)

# Classement
# Precision utiliser mutate de dplyr et pas de plyr
c13_2010_GT<-
  c13_2010_GT %>% 
  group_by(DPE_pred) %>% 
  dplyr::mutate(GT_rank =row_number(-GT_m2)) %>% 
  ungroup()




# MENAGE REHAB ENERGETIQUE 2010 -------------------------------------------


Seuils=c()
# classe DPE de départ
for (dep in LETTERS[2:7]){
  
  # restriction de la bdd des ménages à GT>0 tq leur classe DPE == dep, classement selon rang 
  c13_2010_GT_classe <- 
    c13_2010_GT %>% 
    filter(DPE_pred==dep) %>% 
    arrange(GT_rank)
  
  #création pour chaque DPE de départ d'une matrice donnant le rang des ménages pratiquant une REHAB
  seuil_rank=c()
  
  for (arr in LETTERS[1:7]){
    
    if(dep>arr){
      
      # print(paste(dep,"vers",arr,sep=" "))
      
      # extraction du prix au m2 de la transition en question : dep -> arr
      cost_m2_trans <-
        as.numeric(
          Cost_m2 %>% 
            filter(classe_dep==dep) %>% 
            filter(classe_arr==arr)%>% 
            select(cost_m2)
        )
      
      # seuil_rank indique le dernier ménage pouvant "s'offir" une telle réhabilitation,
      # dont les dépenses en GT par m2 couvrent le coût de la réhabilitation
      seuil_rank<-c(
        as.numeric(
          c13_2010_GT_classe %>% 
            filter(GT_m2>cost_m2_trans) %>% 
            summarise(max(GT_rank))
        ),
        seuil_rank
      )
      
      # Si la première valeur de seuil_rank (dernière ajoutée, pour la dernière transition dep-> arr) 
      # est infinie alors cela signifie qu'aucun ménage n'a dépensé assez pour une telle transition
      # le premier ménage éligible est donc le premier (décalage ensuite si réalise une transition plus chère)
      if(is.infinite(seuil_rank[1])){seuil_rank[1]<-1}
      
      
      # GT_rehab : coût de la réhabiliation acquitée par le ménage et le total représenté par le ménage (pondmen)
      c13_2010_GT_classe <- 
        c13_2010_GT_classe %>% 
        mutate(GT_rehab=cost_m2_trans*surfhab_d) %>% 
        mutate(GT_pond_rehab=cost_m2_trans*surfhab_d*pondmen)
      
      # extraction du total macro de la transition dep->arr
      cost_tot_transition<-
        as.numeric(
          Cost_m2 %>% 
            filter(classe_dep==dep) %>% 
            filter(classe_arr==arr)%>% 
            select(transition_tot_Meuros)
        )
      
      #init
      sum=0
      #i va désigner le nombre de ménage au dessus du ménage "seuil" de la transition 
      # concerné par la réhabilitation dep->arr
      i=0
      # j désigne le nombre de ménage en dessous du ménage "seuil" de la transition
      # concerné par la réhabilitation
      # dans le cas de la transition de G vers les classes les plus hautes, aucun ménage 
      # n'a de dépenses suffisament élevées, seuil_rank est donc toujours égal à 1, 
      # j est donc alors égal au j atteint par la transition précédente +1.
      # Si length(seuil_rank)=1 alors seuil_rank[2]=NA => arr= DPE_A, donc j=1, 
      # le premier ménage à regarder "sous le seuil" est celui qui vient directement sous le seuil
      # si on a déjà réalisé des transitions, il faut s'assurer que le seuil pour 
      # la seconde transition est plus élevé que pour la première, sinon, j est 
      # d'autant plus elevé pour ne pas compter deux fois le même ménage
      j = ifelse(
        is.na(seuil_rank[2]),
        1,
        ifelse(
          seuil_rank[1]-seuil_rank[2]>0 ,
          1,
          1+seuil_rank[2]-seuil_rank[1]
        )
      )
      
      # Tant que le nombre de ménage réhabilitant leur logement ne somme pas au montant macro de dep -> arr
      while(sum<cost_tot_transition){
        # 
        if(
          ((seuil_rank[1]-i>seuil_rank[2]) || 
           (is.na(seuil_rank[2]) & seuil_rank[1]-i >0)) & 
          (1-is.na(seuil_rank[1]-i>seuil_rank[2] || (is.na(seuil_rank[2]) & seuil_rank[1]-i >0)))
        )
          # tant de conditions compliquées : 
          # dans la première on vérifie qu'on 
          # n'a pas encore atteint le seuil précédent, ie qu'on ne compte pas deux fois les GT rehab d'un ménage
          # Deuxième condition : dans le cas où on transitionne vers A, donc on est au premier seuil, 
          # il n'y a pas de seuil_rank[2], on vérifie qu'on peut encore monter dans le classement
          # Troisième condition : dans le cas particulier de B vers A la seconde 
          # condition est fausse et la première équivaut à NA or (NA OR FALSE = NA) 
          # donc on rajoute une 3e condition qui est justement fausse quand les 
          # deux premières somment à NA. 
          
        {
          sum = 
            sum + 
            as.numeric(
              c13_2010_GT_classe %>% 
                filter(GT_rank==seuil_rank[1]-i) %>%
                select(GT_pond_rehab)
            ) /(10^6)
          # somme des montants pondérés des GT_rehab des ménages juste au dessus du seuil. 
          
          # print(seuil_rank-i)
          i=i+1
          # print(sum)
        }
        
        else {
          # Si on ne peut plus monter au dessus du seuil, soit qu'on soit arrivé au ménage classé n°1 ou que ces ménages réalisent déjà une réhabilitation on transitionne des ménages juste en dessous du seuil (qui techniquement ne peuvent pas se permettre ces travaux ...)
          sum = 
            sum +
            as.numeric(
              c13_2010_GT_classe %>% 
                filter(GT_rank==seuil_rank[1]+j) %>%
                select(GT_pond_rehab)
            ) / (10^6)
          
          # print(seuil_rank[1]+j)
          j=j+1
          # print(sum)
        }
      }
      
      # on recalcule le j_initial pour la transition dep -> arr
      j_init<-ifelse(is.na(seuil_rank[2]),1,ifelse(seuil_rank[1]-seuil_rank[2]>0 ,1,1+seuil_rank[2]-seuil_rank[1]))
      
      #  le ménage le mieux classé réhabilité au cours de cette boucle est au rang :
      seuil_haut =
        seuil_rank[1]-i+j_init
      #  le ménage le moins bien classé réhabilité au cours de cette boucle est au rang :
      #  si j==1 alors nous n'avons pas eu besoin d'aller chercher des ménages sous le seuil (sinon j=2)
      seuil_bas=
        ifelse(
          j==1,
          seuil_rank[1],
          seuil_rank[1]+j-1
        )
      
      # si on a eu besoin de passer sous le seuil alors on "baisse" le seuil à la 
      # valeur de seuil_bas pour indiquer à la prochaine boucle ne pas réhabiliter des ménages au dessus de ce seuil
      if(seuil_bas>seuil_rank[1])
      {seuil_rank[1]<-seuil_bas}
      
      #Print
      # print(paste(dep,"vers",arr,sep=" "))
      # print(seuil_haut)
      # print(seuil_rank[1])
      # print(seuil_bas)
      
      # Dans la matrice Seuils on indique que la transition de dep à arr concerne
      # les ménages entre seuil_haut et seuil_bas (inclus)
      Seuils=
        rbind(
          Seuils,
          c(dep,arr,seuil_haut,seuil_bas)
        )
      
    }
  }
  
}

colnames(Seuils)<-c("dep","arr","haut","bas")
Seuils<-as.data.frame(Seuils,stringsAsFactors=F)

# création variable REHAB, 
# TRUE si le ménage réalise des GT de réhabilitation énergétique, FALSE si ce sont des gros travaux de rénovation. 
# REHAB_M2 : prix des réhabilitations le cas échéant
c13_2010_GT<-
  c13_2010_GT %>% 
  mutate(REHAB=FALSE) %>% 
  mutate(REHAB_m2=0)

# fonction permettant de changer les valeurs de certains rangs seulement (une merveille !)
source("Code_global_ADEME/mutate_when.R")

# on parcourt tous les transitions dep->arr
for(i in 1:dim(Seuils)[1]){
  seuil<-Seuils[i,]
  seuil$bas<-as.numeric(seuil$bas)
  seuil$haut<-as.numeric(seuil$haut)
  # print(i)
  c13_2010_GT<-
    c13_2010_GT %>% 
    mutate_when(
      # condition de la mutation
      c13_2010_GT$DPE_pred==seuil$dep & c13_2010_GT$GT_rank<=seuil$bas & c13_2010_GT$GT_rank>=seuil$haut, 
      # mutation (double ici)
      list(
        REHAB=TRUE,
        REHAB_m2=as.numeric(
          Cost_m2 %>% 
            filter(classe_dep==seuil$dep) %>% 
            filter(classe_arr==seuil$arr) %>% 
            select(cost_m2))
      )
    )
}

# création variable GT_REHAB
c13_2010_GT <- c13_2010_GT %>% mutate(GT_REHAB=REHAB_m2*surfhab_d)
# si GT_REHAB > GT (quand on rehabilité des ménages sous le seuil) 
# alors les travaux de réhabiliation correspondent à toute la dépense en GT
c13_2010_GT[
  which(c13_2010_GT$GT_REHAB>c13_2010_GT$GT),
  "GT_REHAB"
  ] <-
  c13_2010_GT[
    which(c13_2010_GT$GT_REHAB>c13_2010_GT$GT),
    "GT"
    ]

# Variable GT_RENO : reste des travaux non énergétiques
c13_2010_GT <- 
  c13_2010_GT %>% 
  mutate(GT_RENO=GT-GT_REHAB)

# View(arrange(c13_2010_GT,DPE_pred,GT_rank))


c13_2010 <- c13_2010 %>% 
  left_join(c13_2010_GT %>%
              select(ident_men,GT_REHAB,GT_RENO),
            by="ident_men")

c13_2010 <- c13_2010 %>% 
  mutate_when(
    is.na(GT_REHAB),
    list(GT_REHAB=0,
         GT_RENO=c13411+c13421)
  )

Gros_travaux_2010 <-
  c13_2010 %>%
  select(ident_men,GT_REHAB,GT_RENO)

rm(c13_2010_GT,c13_2010_GT_classe,Cost_m2,menage_GT_2010,Seuils,seuil,arr,dep,i,M,L,seuil_rank)






####
#### Avons nous incorporé ces dépenses de GT_RENO dans le Rcons 2010 ? Attention à ne pas inclure BTP A05 dans ce domaine si cela est. 

## Maintenant on reventile GT_REHAB sur tous les autres postes
# source("Mise_echelle_BDFE/3_2_ventilation_rehab_ener.R")


# SUCCESS -----------------------------------------------------------------

print("1_1_desagregation_reno_rehab : SUCCESS")