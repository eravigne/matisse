# Adaptation de la fonction amort.period du package FinancialMath permettant de renvoyer un vecteur comportant le montant payé à l'année n dédié au remboursement du principal et celui des intérêts. 


library(FinancialMath)

int_princ<- function(loan,pmt=NA,n,year_purchase, horizon,i,pf=1){
  
  
  if(loan==0){return(c(0,0))}
  A<-amort.period(Loan=loan,
                  n=n,
                  pmt=NA,
                  i=i,
                  t=horizon-year_purchase+1)
  
  B<-as.data.frame(A)
  C<-cbind(rownames(B),B)
  dim(C)
  colnames(C)<-c("Var","Value")
  D<-C %>% spread(key=Var,value=Value)
  D
  D$Balance
  repayment <- D %>% select(`Int Paid`,`Princ Paid`)
  colnames(repayment) <- c("Int","Princ")
  return(repayment)
}





# # 
# P=50000
# n=144
# pmt=597.5391314
# i=0.00833333
# pf=12

# # A<-
#   
#   as.numeric(int_princ(loan=P,
#              n=144,
#              pmt=NA,
#              i=i,
#              pf=1,
#              year_purchase=2025, 
#              horizon=2025)[1])
# # 
# amort.period(Loan=P,
#             n=NA,
#             pmt=pmt,
#             i=i)[2]

# 
# B<-as.data.frame(A)
# C<-cbind(rownames(B),B)
# dim(C)
# colnames(C)<-c("Var","Value")
# D<-C %>% spread(key=Var,value=Value)
# D
# D$Balance
# D$`Int Paid`
# D$`Princ Paid`
#  