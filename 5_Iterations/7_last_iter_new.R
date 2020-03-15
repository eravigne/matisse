args<-commandArgs(trailingOnly=T)
scenario=args[1]
horizon=args[2]
iter=args[3]
scenario_classement=args[4]
redistribution=args[5]
Iter<-as.numeric(iter) 
Iter_last<-Iter-1
horizon=as.numeric(horizon)

library(readxl)
library(tidyverse)
library(writexl)
source("D:/CIRED/Projet_Ademe/Code_global_ADEME/mutate_when.R")

output_macro<-read_excel(path = "D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code.xlsx",skip=1)
if(file.exists(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""))){
  file.remove(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""))}
write_xlsx(output_macro,path=paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""))

output_macro <-
  output_macro %>%
  gather(key=year_model,value=value,-c(1:3))

IMACLIM<-
  output_macro  %>%
  separate(col="year_model",into=c("year","model"),sep="_")

FC <- 
  IMACLIM %>%
  filter(year==9999) %>%
  filter(model=="Marge")%>% #Careful, model here is "Marge" and not IMACLIM or ThreeME
  filter(Variable %in% c("revact",
                         "revpat",
                         "revchom",
                         "revsoc",
                         "revetranger",
                         "rdb",
                         "tauIR",
                         "tauAID",
                         "A01",
                         "A02",
                         "A03",
                         "A04",
                         "A05",
                         "A06",
                         "A07",
                         "A08",
                         "A09",
                         "A10",
                         "A11",
                         "A12",
                         "A13",
                         "A14")) %>%
  select(Variable,value)

print(FC)
save(FC,file=paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_",Iter,"/Input/FC_2010_",as.character(horizon),".RData",sep=""))


output_micro<-read_excel(path = "D:/CIRED/Projet_Ademe/IMACLIM/Output_micro.xlsx",sheet=paste(scenario,horizon,sep=""))
if(file.exists(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/Output_micro_iter",Iter,".xlsx",sep=""))){file.remove(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/Output_micro_iter",Iter,".xlsx",sep=""))}
write_xlsx(output_micro,path=paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/Output_micro_iter",Iter,".xlsx",sep=""))


#Sauv IMACLIM
imaclim_xl<-read_excel(path = "D:/CIRED/Projet_Ademe/IMACLIM/IMACLIM 3ME.xlsm",sheet="Model")
if(file.exists(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))){
  file.remove(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))}
write_xlsx(imaclim_xl,path=paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))


#Nb iterations
Nb_iter<-read_excel(path="D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown.xlsx")
Nb_iter<-
  Nb_iter %>% 
  mutate_when(Scenario==scenario & Horizon==horizon & Scenario_classement==scenario_classement & redistribution==Redistribution, list(Iter_finale=Iter))


# sécurité pour garder trace de Iterations_countdown
if(file.exists("D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown_bis.xlsx")){
  file.remove("D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown_bis.xlsx")}
file.rename(from="D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown.xlsx",to="D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown_bis.xlsx")

write_xlsx(Nb_iter,path="D:/CIRED/Projet_Ademe/IMACLIM/Iterations_countdown.xlsx")
write_xlsx(Nb_iter,path=paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Iterations_countdown.xlsx",sep=""))        

#Results
unlink(paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,sep=""),force=T)

output_micro<-read_excel(path = "D:/CIRED/Projet_Ademe/IMACLIM/Output_micro.xlsx",sheet=paste(scenario,horizon,sep=""))

if(file.exists(paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_micro_iter",Iter,".xlsx",sep=""))){file.remove(paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_micro_iter",Iter,".xlsx",sep=""))}

if(file.exists(paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_micro.xlsx",sep=""))){file.remove(paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_micro.xlsx",sep=""))}


write_xlsx(output_micro,path=paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_micro.xlsx",sep=""))


#Sauv IMACLIM
imaclim_xl<-read_excel(path = "D:/CIRED/Projet_Ademe/IMACLIM/IMACLIM 3ME.xlsm",sheet="Model")
if(file.exists(paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))){file.remove(paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))}
if(file.exists(paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/IMACLIM_3ME.xlsx",sep=""))){file.remove(paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/IMACLIM_3ME.xlsx",sep=""))}


write_xlsx(imaclim_xl,path=paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/IMACLIM_3ME.xlsx",sep=""))

output_macro<-read_excel(path = "D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code.xlsx",skip=1)
if(file.exists(paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_macro_code_iter",Iter,".xlsx",sep=""))){file.remove(paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_macro_code_iter",Iter,".xlsx",sep=""))}
if(file.exists(paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_macro_code.xlsx",sep=""))){file.remove(paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_macro_code_iter.xlsx",sep=""))}

write_xlsx(output_macro,path=paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_macro_code.xlsx",sep=""))


# Nom fichier avec iter_
# 
# output_micro<-read_excel(path = "D:/CIRED/Projet_Ademe/IMACLIM/Output_micro.xlsx",sheet=paste(scenario,horizon,sep=""))
# if(file.exists(paste("D:/CIRED/Projet_Ademe/Results",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output/Output_micro_iter",Iter,".xlsx",sep=""))){file.remove(paste("D:/CIRED/Projet_Ademe/Results",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output/Output_micro_iter",Iter,".xlsx",sep=""))}
# write_xlsx(output_micro,path=paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_micro_iter",Iter,".xlsx",sep=""))
# 
# 
# #Sauv IMACLIM
# imaclim_xl<-read_excel(path = "D:/CIRED/Projet_Ademe/IMACLIM/IMACLIM 3ME.xlsm",sheet="Model")
# if(file.exists(paste("D:/CIRED/Projet_Ademe/Results",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))){file.remove(paste("D:/CIRED/Projet_Ademe/Results",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))}
# write_xlsx(imaclim_xl,path=paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))
# 
# output_macro<-read_excel(path = "D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code.xlsx",skip=1)
# if(file.exists(paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_macro_code_iter",Iter,".xlsx",sep=""))){file.remove(paste("D:/CIRED/Projet_Ademe/Results",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_macro_code_iter",Iter,".xlsx",sep=""))}
# write_xlsx(output_macro,path=paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_macro_code_iter",Iter,".xlsx",sep=""))


#Launch Indicateurs Inégalités
