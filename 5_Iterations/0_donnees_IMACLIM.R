# 
# lecture_IMACLIM<-function(Iter){

# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(writexl)
setwd("D:/CIRED/Projet_Ademe/")
# Iter=5
# Iter_last=Iter-1
# horizon=2025



# VARIABLES MACRO ---------------------------------------------------------

# output_macro<-read_excel(path = paste(scenario,"/",horizon,"/",scenario_classement,"/Iteration_",Iter_last,"/Output/Output_macro_code",Iter_last,".xlsx",sep="") )

output_macro<-read_excel(path = "IMACLIM/Output_macro_code.xlsx",skip=1,sheet=scenario)
if(file.exists(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""))){
file.remove(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""))}
write_xlsx(output_macro,path=paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""))

output_macro <-
  output_macro %>%
  gather(key=year_model,value=value,-c(1:3))

IMACLIM<-
  output_macro  %>%
  separate(col="year_model",into=c("year","model"),sep="_")
rm(output_macro)
save(IMACLIM,file=
       paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM.RData",sep=""))


#Sauv Output_micro
#read
output_micro<-read_excel(path = "D:/CIRED/Projet_Ademe/IMACLIM/Output_micro.xlsx",sheet=paste(scenario,horizon,sep=""))
#remove previous file
if(file.exists(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter_last,"/Output/Output_micro_iter",Iter_last,".xlsx",sep=""))){
file.remove(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter_last,"/Output/Output_micro_iter",Iter_last,".xlsx",sep=""))}
#Write
write_xlsx(output_micro,path=paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter_last,"/Output/Output_micro_iter",Iter_last,".xlsx",sep=""))


if(redistribution=="ssrec"){
  imaclim_xl<-read_excel(path = "D:/CIRED/Projet_Ademe/IMACLIM/IMACLIM 3ME_ssREC.xlsm",sheet="Model")
  
  if (file.exists(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))){
    file.remove(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))}
 
  
   write_xlsx(imaclim_xl,path=paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))
  
}else{

#Sauv IMACLIM
imaclim_xl<-read_excel(path = "D:/CIRED/Projet_Ademe/IMACLIM/IMACLIM 3ME.xlsm",sheet="Model")
if (file.exists(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))){
file.remove(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))}
write_xlsx(imaclim_xl,path=paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))
# }
}


