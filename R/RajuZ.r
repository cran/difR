# RAJU'S AREA
RajuZ<-function(mR,mF){
res<-NULL
mod<-as.character(ncol(mR))
model<-switch(mod,"2"="1PL","5"="2PL","6"="3PL")
if (model=="1PL"){
bR<-mR[,1]
se.bR<-mR[,2]
bF<-mF[,1]
se.bF<-mF[,2]
H<-bF-bR
sH<-sqrt(se.bF^2+se.bR^2)
res<-cbind(H,sH,Z=H/sH)
}
else{
for (i in 1:nrow(mR)){
aF<-mF[i,1]
aR<-mR[i,1]
bF<-mF[i,2]
bR<-mR[i,2]
se.aF<-mF[i,3]
se.bF<-mF[i,4]
cov.F<-mF[i,5]
se.aR<-mR[i,3]
se.bR<-mR[i,4]
cov.R<-mR[i,5]
Y<-aF*aR*(bF-bR)/(aF-aR)
if (exp(Y)==Inf){
H<--(bF-bR)
BF<--1
BR<-1
AF<-AR<-0
}
else{
H<-2*(aF-aR)*log(1+exp(Y))/(aF*aR)-(bF-bR)
BF<-1-2*exp(Y)/(1+exp(Y))
BR<--BF
AF<-2*(Y*exp(Y)/(1+exp(Y))-log(1+exp(Y)))/aF^2
AR<--aF^2*AF/aR^2
}
sH<-BF^2*se.bF^2+BR^2*se.bR^2+AF^2*se.aF^2+AR^2*se.aR^2+2*BF*AF*cov.F+2*BR*AR*cov.R
res<-rbind(res,c(H,sqrt(sH),Z=H/sqrt(sH)))
}
if (model=="3PL"){
c<-mF[,6]
res[,1]<-res[,1]*(1-c)
res[,2]<-res[,2]*(1-c)
}
}
return(res)}


