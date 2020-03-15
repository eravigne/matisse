
# Iter=1

# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(animation)
library(stargazer)




# DATA --------------------------------------------------------------------
setwd("D:/CIRED/Projet_Ademe")


source("Code_global_ADEME/compute_share.R")
source("Code_global_ADEME/mutate_when.R")
source("Code_global_ADEME/compute_share_export.R")
source("Code_global_ADEME/compute_savings_rate_export.R")
source("Code_global_ADEME/verif_epargne_taux.R")
source("Code_global_ADEME/compute_Rcons_bis.R")


row<-c("share_A01",  "share_A02" ,   "share_A03"   , "share_A04" ,   "share_A05"   ,
"share_A06" ,   "share_A07" ,   "share_A08" ,   "share_A09" ,   "share_A10" ,   "share_A11",    "share_A12",    "share_A13",    "epargne")

# forme
load("2010/share_2010.RData")
A1<-share_2010%>%select(share)
rownames(A1)<-row

load("2010/menage_forme_2010.RData")
carb1<-menage_forme_2010%>%summarise(sum(pondmen*carb_lubr))

load("2010/Rcons_bis_tot_2010.RData")
R1<-sum_tot
# echelle
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/menage_echelle_export.RData",sep=""))
A2<-share_echelle
rownames(A2)<-row
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Output/menage_echelle_1.RData",sep=""))
carb2<-menage_echelle%>%summarise(sum(pondmen*carb_lubr))
R2<-compute_Rcons_bis_echelle(scenario,horizon, scenario_classement,redistribution)


# #TC_DPE_41
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_41.RData",sep=""))
A3<-rbind(t(compute_share_export(menage_echelle_41)),compute_savings_rate_export(menage_echelle_41))
rownames(A3)<-row
carb3<-menage_echelle_41%>%summarise(sum(pondmen*carb_lubr))

R3<-compute_Rcons_bis(menage_echelle_41)
# # #TC_DPE_42
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_42.RData",sep=""))
A4<-rbind(t(compute_share_export(menage_echelle_42)),compute_savings_rate_export(menage_echelle_42))
rownames(A4)<-row
carb4<-menage_echelle_42%>%summarise(sum(pondmen*carb_lubr))
R4<-compute_Rcons_bis(menage_echelle_42)


# TC_DPE_43
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_43.RData",sep=""))
A5<-rbind(t(compute_share_export(menage_echelle_43)),compute_savings_rate_export(menage_echelle_43))
rownames(A5)<-row
carb5<-menage_echelle_43%>%summarise(sum(pondmen*carb_lubr))
R5<-compute_Rcons_bis(menage_echelle_43)
# VE
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_TC_VE.RData",sep=""))
A6<-rbind(t(compute_share_export(menage_echelle_TC_VE)),compute_savings_rate_export(menage_echelle_TC_VE))
rownames(A6)<-row
carb6<-menage_echelle_TC_VE%>%summarise(sum(pondmen*carb_lubr))
R6<-compute_Rcons_bis(menage_echelle_TC_VE)
#Repond
load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
A7<-rbind(t(compute_share_export(menage_echelle)),compute_savings_rate_export(menage_echelle))
rownames(A7)<-row

carb7<-menage_echelle%>%summarise(sum(pondmen*carb_lubr))
R7<-compute_Rcons_bis(menage_echelle)

# CALCUL AGREGATS ---------------------------------------------------------

list_cat=c("agriculture",
           "elec",
           "gaz_ville",
           "autres_energies_dom",
           "BTP",
           "prod_veh",
           "carb_lubr",
           "transp_rail_air",
           "transp_routes_eau",
           "loisirs_com",
           "autres_services",
           "autres",
           "loyers",
           "veh_occasion")





# Tableau -----------------------------------------------------------------

# Tab <- as.data.frame(cbind(as.numeric(B2),as.numeric(B3),as.numeric(B4),as.numeric(B5),as.numeric(B6),as.numeric(B7)))
# colnames(Tab)<-c("2_echelle","3.1_TC_DPE_41","3.2_TC_DPE_42","3.3_TC_DPE_43","4_VE","5_Repond")
# stargazer(Tab,type="text",summary=F)



# PLOT --------------------------------------------------------------------

# DATA
dat <- data.frame(row,A1,A2,A3,A4,A5,A6,A7)
colnames(dat)<-c("share","2010","2_echelle","3.1_TC_DPE_41","3.2_TC_DPE_42","3.3_TC_DPE_43","4_VE","5_Repond")

dat2<- dat %>% gather(key=step,value=part,-c(1))
dat_2<-dat2 %>% mutate(step=factor(step,levels=c("2010","2_echelle","3.1_TC_DPE_41","3.2_TC_DPE_42","3.3_TC_DPE_43","4_VE","5_Repond")))

# dat2[,1]<-rep(c("2_echelle","3.1_TC_DPE_41","3.2_TC_DPE_42","3.3_TC_DPE_43","4_VE","5_Repond"),times=1,each=13)
# dat2 <- dat2 %>% mutate("cat"=rep(list_cat[1:13],times=6))
# dat2$step<- factor(dat2$step, levels = dat2$step[])


dat_vol <- data.frame(carb1,carb2,carb3,carb4,carb5,carb6,carb7)
colnames(dat_vol)<-c("2010","2_echelle","3.1_TC_DPE_41","3.2_TC_DPE_42","3.3_TC_DPE_43","4_VE","5_Repond")
dat_vol2<-data.frame(colnames(dat_vol),t(dat_vol))
colnames(dat_vol2)<-c("step","A07")
dat_vol2<-dat_vol2 %>% mutate(step=factor(step,levels=c("2010","2_echelle","3.1_TC_DPE_41","3.2_TC_DPE_42","3.3_TC_DPE_43","4_VE","5_Repond")))
dat_vol2$Rcons_bis<-c(R1,R2,R3,R4,R5,R6,R7)
dat_vol2<-dat_vol2 %>% gather(key=var,value=vol,-1)%>%mutate(vol=as.numeric(vol))

# Plot 12 -----------------------------------------------------------------


png(filename=paste("D:/CIRED/Projet_ADEME/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/GIF/evol_part.png",sep=""))
g12<-ggplot(dat2,aes(x=step,y=part,group=share,colour=share)) + geom_point(stat="identity",size=2.5) + geom_line(stat="identity",size=1.2) 
# +   scale_color_brewer(palette = "Paired") + theme_bw()
g12
dev.off()
# ggplot


png(filename=paste("D:/CIRED/Projet_ADEME/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/GIF/evol_A07_",horizon,".png",sep=""))
g12<-ggplot(dat2%>%filter(share=="share_A07"),aes(x=step,y=part,group=share,fill=share)) + geom_bar(stat="identity")+scale_y_continuous(limits=c(0,0.06))
# +   scale_color_brewer(palette = "Paired") + theme_bw()
g12
dev.off()
# ggplot

vol_A07<-
png(filename=paste("D:/CIRED/Projet_ADEME/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/GIF/evol_A07_volume_",horizon,".png",sep=""))
g12<-ggplot(dat_vol2,aes(x=factor(step),y=vol/10^9,group=var,fill=var))+facet_wrap(~var,scales="free_y") + geom_bar(stat="identity",position="dodge")+ggtitle("Dep Carb en milliards € courants")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
# +scale_y_continuous(limits=c(0,0.06))
# +   scale_color_brewer(palette = "Paired") + theme_bw()
g12
dev.off()


# # Plot 14 -----------------------------------------------------------------
# 
# 
# g<-ggplot(dat2,aes(x=step,y=part,group=cat,colour=cat)) + geom_point(stat="identity",size=2.5) + geom_line(stat="identity",size=1.2) +  scale_colour_hue(14) + theme_bw()
# g
# ggsave(g,file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/evol_part.png",sep=""))

# bar plot ----------------------------------------------------------------
# gif_file <- "~/evol_part_gif.gif"
#
# b<-ggplot(dat2%>% filter(step=="1_forme"),aes(x=cat,y=part,group=cat,fill=cat)) + geom_histogram(stat="identity") +  scale_colour_hue(14) + theme_bw()+theme(axis.text.x = element_text(angle=45,hjust = 1))
# b


# 
# # Epargne -----------------------------------------------------------------
# epargne=c()
# for (i in seq(2,7)){
#   A<-paste("A",i,sep="")
#   menage_echelle_2025<-get(A)
#   E<-compute_savings_rate_export(A)
# 
#   epargne=c(epargne,E)
# }


# GIF ---------------------------------------------------------------------

# 
# library(magick) # this is call to animate/read pngs
# library(tidyverse)
# setwd(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/GIF",sep=""))


# DATA


# 
# gif_file <- paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/evol_part_gif.gif",sep="")
# x_space <- 0.16
# speed <- 0.12
# tweet_list <- list()
# track_tweets <- 1
# max_value <- max(dat2$part)
# setwd("~/empyt/")
# file.remove(dir(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/GIF",sep="")))
# 
# tracker <- 1
# 
# for(i in c("2010","2_echelle","3.1_TC_DPE_41","3.2_TC_DPE_42","3.3_TC_DPE_43","4_VE","5_Repond")){
#   print(i)
#   D<-dat2%>% filter(step==i)
#   b<-ggplot(D,aes(x=share,y=part,group=share,fill=share)) + geom_histogram(stat="identity") +  scale_colour_hue(14) + theme_bw()+theme(axis.text.x = element_text(angle=45,hjust = 1))+
#     labs(x = NULL, y = NULL) +
#     labs(caption="14 secteurs, 8 étapes de micro-simulation", title = "Parts de consommation (%RDB)")+
#     labs(subtitle = "Evolution des parts à travers la micro-simulation")+
#     geom_text(x = 13, y = max_value * 0.93, label = i, size = 3.5) +
#     theme(panel.grid.major.y = element_blank())+
#     theme(panel.grid.minor.y = element_blank())+
#     theme(panel.grid.minor.x = element_blank())+
#     scale_y_continuous(limits = c(0, max_value), expand = c(0,0)) +
#     theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
#     theme(legend.position = "none")+
#     theme(plot.margin=grid::unit(c(1,1,1,1), "mm"))
#   b
#   # gg_file <- str_c(str_pad(tracker, width = 2, pad = 0), ".png")
#   gg_file <- paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/GIF/",str_c(str_pad(tracker, width = 2, pad = 0), ".png"),sep="")
#   # ggsave(gg_file, b, width = 10, height = 10 * 0.8, units = "cm")
#   ggsave(gg_file, b)
#   tracker <- tracker + 1
# }
# 
# for (i in 1:3) {
#   gg_file <- paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/GIF/",str_c(str_pad(tracker, width = 2, pad = 0), ".png"),sep="")
#   ggsave(gg_file, b)
#   tracker <- tracker + 1
# }
# 
# # Step 2: List those Plots, Read them in, and then make animation
# list.files(path = paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/GIF/",sep=""), pattern = "*.png", full.names = T) %>%
#   map(image_read) %>% # reads each path file
#   image_join() %>% # joins image
#   image_animate(fps=1) %>% # animates, can opt for number of loops
#   image_write(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/GIF/animation.gif",sep="")) # write to current dir
# 
# # file.rename("animation.gif", gif_file)
# 
# # https://ryanpeek.github.io/2016-10-19-animated-gif_maps_in_R/
# 
# 
# print("essai_gif : SUCCEED")




