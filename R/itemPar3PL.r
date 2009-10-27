## ESTIMATION OF ITEM RESPONSE MODEL

# require ltm library

library(ltm)

# Items parameters

itemPar3PL<-function(data){
J<-ncol(data)
mod<-tpm(data)
par<-cbind(summary(mod)$coefficients[(2*J+1):(3*J),1],
summary(mod)$coefficients[(J+1):(2*J),1],
summary(mod)$coefficients[1:J,1],
sqrt(diag(summary(mod)$Var.betas))[(2*J+1):(3*J)],
sqrt(diag(summary(mod)$Var.betas))[(J+1):(2*J)],
sqrt(diag(summary(mod)$Var.betas))[1:J],
diag(summary(mod)$Var.betas[(2*J+1):(3*J),(J+1):(2*J)]),
diag(summary(mod)$Var.betas[(2*J+1):(3*J),1:J]),
diag(summary(mod)$Var.betas[(J+1):(2*J),1:J]))
colnames(par)<-c("a","b","c","se(a)","se(b)","se(c)","cov(a,b)","cov(a,c)","cov(b,c)")
if (is.null(colnames(data))==FALSE) row<-colnames(data)
else{
row<-NULL
for (i in 1:J) row<-c(row,paste("Item",i,sep=""))
}
rownames(par)<-row
return(par)}


