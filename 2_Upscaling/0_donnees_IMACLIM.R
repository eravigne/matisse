# 
# lecture_IMACLIM<-function(Iter){
#Iter =1
  
# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(readxl)
setwd("D:/CIRED/Projet_Ademe/")


# VARIABLES MACRO ---------------------------------------------------------
if(scenario_classement=="ssrec"){
  output_macro<-read_excel(path ="IMACLIM/Output_macro_code_iter_0_ssrec.xlsx",sheet=scenario,skip = 1)
} else {
  output_macro<-read_excel(path ="IMACLIM/Output_macro_code_iter_0.xlsx",sheet=scenario,skip = 1)
}
output_macro <-
  output_macro %>%
  gather(key=year_model,value=value,-c(1:3))

IMACLIM<-
  output_macro  %>%
  separate(col="year_model",into=c("year","model"),sep="_")

rm(output_macro)

save(IMACLIM,file=
       paste(scenario,"/",horizon,"/IMACLIM.RData",sep=""))

# }