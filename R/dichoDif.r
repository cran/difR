# DIF: COMPARING DIF STATISTICS

dichoDif<-function(Data,group,focal.name,method,alpha=0.05,correct=TRUE,thr=0.1,model="2PL",c=NULL,irtParam=NULL,same.scale=TRUE,purify=FALSE,nrIter=10){
mets<-c("MH","Std","Logistic","BD","Lord","Raju","LRT")
prov.met<-rep(0,length(method))
for (i in 1:length(method)){
if (sum(method[i]==mets)==1) prov.met[i]<-1
}
if (min(prov.met)==0){
ind<-min((1:length(method))[prov.met==0])
RES<-list(NULL,method[ind])
class(RES)<-"dichoDif"
return(RES)
}
else{
if (length(method)==1) return(selectDif(Data=Data,group=group,focal.name=focal.name,method=method,alpha=alpha,correct=correct,thr=thr,model=model,c=c,irtParam=irtParam,same.scale=same.scale,purify=purify,nrIter=nrIter))
else{
mat<-iters<-conv<-NULL
for (met in 1:length(method)){
prov<-selectDif(Data=Data,group=group,focal.name=focal.name,method=method[met],alpha=alpha,correct=correct,thr=thr,model=model,c=c,irtParam=irtParam,same.scale=same.scale,purify=purify,nrIter=nrIter)
if (method[met]=="BD") mat<-cbind(mat,rep("NoDIF",nrow(prov[[1]])))
else mat<-cbind(mat,rep("NoDIF",length(prov[[1]])))
if (is.character(prov$DIFitems)==FALSE) mat[prov$DIFitems,met]<-"DIF"
rname<-prov$names
if (purify==TRUE){
iters<-c(iters,prov$nrPur)
conv<-c(conv,prov$convergence)
} 
}
method2<-method
method2[method=="MH"]<-"M-H"
method2[method=="Std"]<-"Stand."
colnames(mat)<-method2
if (is.null(rname)==FALSE) rownames(mat)<-rname
else{
rname<-NULL
for (i in 1:nrow(mat)) rname<-c(rname,paste("Item",i,sep=""))
rownames(mat)<-rname
}
RES<-list(DIF=mat,correct=correct,alpha=alpha,thr=thr,model=model,c=c,irtParam=irtParam,same.scale=same.scale,purify=purify,nrPur=iters,convergence=conv)
class(RES)<-"dichoDif"
return(RES)}
}
}

# METHODS
print.dichoDif<-function(x,...){
res <- x
if (is.null(res[[1]])==TRUE) cat("Error: '",res[[2]],"' is not a correct method!","\n","\n",sep="")
else{
cat("Comparison of DIF detection results using",ncol(res$DIF),"methods","\n","\n")
methods<-colnames(res$DIF)
methods2<-methods
methods2[methods=="M-H"]<-"Mantel-Haenszel"
methods2[methods=="Stand."]<-"Standardization"
methods2[methods=="Logistic"]<-"Logistic regression"
methods2[methods=="BD"]<-"Breslow-Day"
methods2[methods=="Raju"]<-"Raju's area"
methods2[methods=="Lord"]<-"Lord's chi-square test"
methods2[methods=="LRT"]<-"Likelihood-Ratio Test"
met<-methods2[1]
for (i in 2:min(c(3,length(methods)))) met<-paste(met,methods2[i],sep=", ")
if (length(methods)<=3) cat("Methods used:",met,"\n")
else cat("Methods used: ",met,",","\n",sep="")
if (length(methods)>3){
met2<-methods2[4]
if (length(methods)>4){
for (i in 5:length(methods)) met2<-paste(met2,methods2[i],sep=", ")
}
cat(met2,"\n")
}
cat("\n")
cat("Parameters:","\n")
cat("Significance level: ",res$alpha,"\n",sep="")
if (sum(methods=="Stand.")==1) cat("Standardization threshold:",res$thr,"\n")
if (sum(methods=="M-H")==1){
if (res$correct==TRUE) corr<-"Yes"
else corr<-"No"
cat("Mantel-Haenszel continuity correction:",corr,"\n")
}
if (sum(methods=="Lord" | methods=="Raju")>=1) cat("Item response model:",res$model,"\n")
if (res$purify==TRUE) {
cat("Item purification: Yes","\n","\n")
cat("Item purification results:","\n","\n")
co<-rep("Yes",length(res$convergence))
co[res$convergence==FALSE]<-"No"
resConv<-data.frame(rbind(co,res$nrPur))
colnames(resConv)<-colnames(res$DIF)
rownames(resConv)<-c("Convergence","Iterations")
print(format(resConv,justify="centre"))
cat("\n")
}
else cat("Item purification: No","\n","\n")
cat("Comparison of DIF detection results:","\n","\n")
nr<-NULL
for (i in 1:nrow(res$DIF)) nr[i]<-paste(length(res$DIF[i,][res$DIF[i,]=="DIF"]),"/",ncol(res$DIF),sep="")
MAT<-cbind(res$DIF,nr)
colnames(MAT)[ncol(MAT)]<-"#DIF"
print(format(MAT,justify="centre"),quote=FALSE)
}
}

