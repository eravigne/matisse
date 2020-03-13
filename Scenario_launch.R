# for (scenario_classement in c("Optimiste","Pessimiste","Median")){
#   for (scenario in c("AMS","AME")){
#     for (horizon in c(2035)){
#       for (redistribution in c("niveau_vie","decile","forfait")){

for (scenario_classement in c("Optimiste")){
  for (scenario in c("AMS","AME")){
    for (horizon in c(2025,2030,2035)){
        for (redistribution in c("ssrec")){
        Iter=0
        print(paste("Scenario : ",scenario,", horizon : ", horizon, ", scenario technique :",scenario_classement, " ,scenario redistributif :",redistribution,sep=""))
        source("D:/CIRED/Projet_Ademe/Code_global_ADEME/code_global.R")
      }
    }
  }
}

# AMS et AME
# ssREC 2025, 2030, 2035 Optimiste



