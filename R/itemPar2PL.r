library(ltm)

itemPar2PL<-function(data){
J<-ncol(data)
mod<-ltm(data~z1)
par<-cbind(mod$coefficients[,2],
mod$coefficients[,1],
sqrt(diag(vcov(mod)))[(J+1):(2*J)],
sqrt(diag(vcov(mod)))[1:J],
diag(vcov(mod)[1:J,(J+1):(2*J)]))
colnames(par)<-c("a","b","se(a)","se(b)","cov(a,b)")
if (is.null(colnames(data))==FALSE) row<-colnames(data)
else{
row<-NULL
for (i in 1:J) row<-c(row,paste("Item",i,sep=""))
}
rownames(par)<-row
return(par)}