library(ltm)

itemPar3PLconst<-function(data,c=rep(0,ncol(data))){
J<-ncol(data)
if (length(c)==1) Guess<-rep(c,ncol(data))
else Guess<-c
cont<-cbind(1:ncol(data),rep(1,ncol(data)),Guess)
mod<-tpm(data,constraint=cont)
par<-cbind(summary(mod)$coefficients[(2*J+1):(3*J),1],
summary(mod)$coefficients[(J+1):(2*J),1],
sqrt(diag(summary(mod)$Var.betas))[(J+1):(2*J)],
sqrt(diag(summary(mod)$Var.betas))[1:J],
diag(summary(mod)$Var.betas[(J+1):(2*J),1:J]),
Guess)
colnames(par)<-c("a","b","se(a)","se(b)","cov(a,b)","c")
if (is.null(colnames(data))==FALSE) row<-colnames(data)
else{
row<-NULL
for (i in 1:J) row<-c(row,paste("Item",i,sep=""))
}
rownames(par)<-row
return(par)}