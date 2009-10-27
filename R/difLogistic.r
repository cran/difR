# LOGISTIC REGRESSION
difLogistic<-function(Data,group,focal.name,alpha=0.05,purify=FALSE,nrIter=10){
     if (length(group) == 1) {
           if (is.numeric(group)==TRUE) {
              gr <- Data[, group]
              DATA <- Data[,(1:ncol(Data))!= group]
              colnames(DATA) <- colnames(Data)[(1:ncol(Data))!= group]
           }
           else {
              gr <- Data[, colnames(Data)==group]
              DATA <- Data[,colnames(Data)!= group]
              colnames(DATA) <- colnames(Data)[colnames(Data)!= group]
           }
    }
    else {
        gr <- group
        DATA <- Data
    }
    Group <- rep(0, nrow(DATA))
    Group[gr == focal.name] <- 1
if (purify==FALSE) {
STATS<-Logistik(DATA,Group)
if (max(STATS)<=qchisq(1-alpha,2)) DIFitems<-"No DIF item detected"
else DIFitems<-(1:ncol(DATA))[STATS>qchisq(1-alpha,2)]
RES<-list(Logistik=STATS,alpha=alpha,thr=qchisq(1-alpha,2),DIFitems=DIFitems,purification=purify,names=colnames(DATA))
}
else{
nrPur<-0
difPur<-NULL
noLoop<-FALSE
stats1<-Logistik(DATA,Group)
if (max(stats1)<=qchisq(1-alpha,2)) {
DIFitems<-"No DIF item detected"
noLoop<-TRUE
}
else{
dif<-(1:ncol(DATA))[stats1>qchisq(1-alpha,2)]
difPur<-rep(0,length(stats1))
difPur[dif]<-1
repeat{
if (nrPur>=nrIter) break
else{
nrPur<-nrPur+1
nodif<-NULL
if (is.null(dif)==TRUE) nodif<-1:ncol(DATA)
else{
for (i in 1:ncol(DATA)){
if (sum(i==dif)==0) nodif<-c(nodif,i)
}
}
stats2<-Logistik(DATA,Group,anchor=nodif)
if (max(stats2)<=qchisq(1-alpha,2)) dif2<-NULL
else dif2<-(1:ncol(DATA))[stats2>qchisq(1-alpha,2)]
difPur<-rbind(difPur,rep(0,ncol(DATA)))
difPur[nrPur+1,dif2]<-1
if (length(dif)!=length(dif2)) dif<-dif2
else{
dif<-sort(dif)
dif2<-sort(dif2)
if (sum(dif==dif2)==length(dif)){
noLoop<-TRUE
break
}
else dif<-dif2
}
}
}
stats1<-stats2
DIFitems <-(1:ncol(DATA))[stats1>qchisq(1-alpha,2)]
}
if (is.null(difPur)==FALSE){
ro<-co<-NULL
for (ir in 1:nrow(difPur)) ro[ir]<-paste("Step",ir-1,sep="")
for (ic in 1:ncol(difPur)) co[ic]<-paste("Item",ic,sep="")
rownames(difPur)<-ro
colnames(difPur)<-co
}
RES<-list(Logistik=stats1,alpha=alpha,thr=qchisq(1-alpha,2),DIFitems=DIFitems,purification=purify,nrPur=nrPur,difPur=difPur,convergence=noLoop,names=colnames(DATA))
}
class(RES)<-"Logistic"
return(RES)
}

# METHODS
plot.Logistic<-function(x,pch=8,number=TRUE,col="red", ...){
res <- x
if (number==FALSE){
plot(res$Logistik,xlab="Item",ylab="Logistic regression statistic",ylim=c(0,max(c(res$Logistik,res$thr)+1)),pch=pch,main="Logistic regression")
if (is.character(res$DIFitems)==FALSE) points(res$DIFitems,res$Logistik[res$DIFitems],pch=pch,col=col)}
else {
plot(res$Logistik,xlab="Item",ylab="Logistic regression statistic",ylim=c(0,max(c(res$Logistik,res$thr)+1)),col="white",main="Logistic regression")
text(1:length(res$Logistik),res$Logistik,1:length(res$Logistik))
if (is.character(res$DIFitems)==FALSE) text(res$DIFitems,res$Logistik[res$DIFitems],res$DIFitems,col=col)
}
abline(h=res$thr)
}


print.Logistic<-function(x, ...){
res <- x
cat("\n")
cat("Detection of Differential Item Functioning using Logistic regression method","\n")
if (res$purification==TRUE) pur<-"with "
else pur<-"without "
cat(pur, "item purification","\n","\n",sep="")
if (res$purification==TRUE){
if (res$nrPur<=1) word<-" iteration"
else word<-" iterations"
if (res$convergence==FALSE) {
 cat("WARNING: no item purification convergence after ",res$nrPur,word,"\n",sep="")
 loop<-NULL
 for (i in 1:res$nrPur) loop[i]<-sum(res$difPur[1,]==res$difPur[i+1,])
 if (max(loop)!=length(res$Logistik)) cat("(Note: no loop detected in less than ",res$nrPur,word,")","\n",sep="")
 else cat("(Note: loop of length ",min((1:res$nrPur)[loop==length(res$Logistik)])," in the item purification process)","\n",sep="")
 cat("WARNING: following results based on the last iteration of the purification","\n","\n")
}
else cat("Convergence reached after ",res$nrPur,word,"\n","\n",sep="")
}
cat("Logistic regression statistic:","\n","\n")
pval<-round(1-pchisq(res$Logistik,2),4)
symb<-symnum(pval,c(0,0.001,0.01,0.05,0.1,1),symbols=c("***","**","*",".",""))
m1<-cbind(round(res$Logistik,4),pval)
m1<-noquote(cbind(format(m1,justify="right"),symb))
if (is.null(res$names)==FALSE) rownames(m1)<-res$names
else{
rn<-NULL
for (i in 1:nrow(m1)) rn[i]<-paste("Item",i,sep="")
rownames(m1)<-rn
}
colnames(m1)<-c("Stat.","P-value","")
print(m1)
cat("\n")
cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 ","\n")
cat("\n","Detection threshold: ",round(res$thr,4)," (significance level: ",res$alpha,")","\n","\n",sep="")
if (is.character(res$DIFitems)==TRUE) cat("Items detected as DIF items:",res$DIFitems,"\n","\n")
else {
cat("Items detected as DIF items:","\n")
m2<-cbind(rownames(m1)[res$DIFitems])
rownames(m2)<-rep("",nrow(m2))
colnames(m2)<-""
print(m2,quote=FALSE)
cat("\n","\n")
}
}

