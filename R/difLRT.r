difLRT<-function(Data, group, focal.name, alpha=0.05, purify=FALSE, nrIter=10)
{
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
STATS<-LRT(DATA,Group)
if (max(STATS)<=qchisq(1-alpha,1)) DIFitems<-"No DIF item detected"
else DIFitems<-(1:ncol(DATA))[STATS>qchisq(1-alpha,1)]
RES<-list(LRT=STATS,alpha=alpha,thr=qchisq(1-alpha,1),DIFitems=DIFitems,purification=purify,names=colnames(DATA))
}
else{
nrPur<-0
noLoop<-FALSE
stats1<-LRT(DATA,Group)
if (max(stats1)<=qchisq(1-alpha,1)) {
DIFitems<-"No DIF item detected"
noLoop<-TRUE
}
else{
dif<-(1:ncol(DATA))[stats1>qchisq(1-alpha,1)]
nodif<-NULL
for (i in 1:ncol(DATA)){
if (sum(i==dif)==0) nodif<-c(nodif,i)
}
repeat{
if (nrPur>=nrIter) break
else{
nrPur<-nrPur+1
dat<-DATA[,nodif]
stats2<-LRT(dat,Group)
stats1[nodif]<-stats2
if (max(stats2)<=qchisq(1-alpha,1)) {
noLoop<-TRUE
break
}
else{
ind2<-(1:ncol(dat))[stats2>qchisq(1-alpha,1)]
noind2<-(1:ncol(dat))[stats2<=qchisq(1-alpha,1)]
dif<-c(dif,nodif[ind2])
nodif<-nodif[noind2]
}
}
}
DIFitems<-(1:ncol(DATA))[stats1>qchisq(1-alpha,1)]
}
RES<-list(LRT=stats1,alpha=alpha,thr=qchisq(1-alpha,1),DIFitems=DIFitems,purification=purify,nrPur=nrPur,convergence=noLoop,names=colnames(DATA))
}
class(RES)<-"LRT"
return(RES)
}


plot.LRT<-function(x,pch=8,number=TRUE,col="red", ...){
res <- x
if (number==FALSE) {
plot(res$LRT,xlab="Item",ylab="Likelihood Ratio statistic",ylim=c(0,max(c(res$LRT,res$thr)+1)),pch=pch,main="Likelihood Ratio Test")
points(res$DIFitems,res$MH[res$DIFitems],pch=pch,col=col)
}
else {
plot(res$LRT,xlab="Item",ylab="Likelihood Ratio statistic",ylim=c(0,max(c(res$LRT,res$thr)+1)),col="white",main="Likelihood Ratio Test")
text(1:length(res$LRT),res$LRT,1:length(res$LRT))
if (is.character(res$DIFitems)==FALSE) text(res$DIFitems,res$LRT[res$DIFitems],res$DIFitems,col=col)
}
abline(h=res$thr)
}


print.LRT<-function(x, ...){
res <- x
cat("\n")
cat("Detection of Differential Item Functioning using Likelihood Ratio Test","\n")
if (res$purification==TRUE) pur<-"with "
else pur<-"without "
cat(pur, "item purification","\n","\n",sep="")
if (res$purification==TRUE){
if (res$nrPur<=1) word<-" iteration"
else word<-" iterations"
if (res$convergence==FALSE) {
cat("WARNING: no item purification convergence after ",res$nrPur,word,"\n",sep="")
cat("WARNING: following results based on the last iteration of the purification","\n","\n")
}
else cat("Convergence reached after ",res$nrPur,word,"\n","\n",sep="")
}
cat("Likelihood Ratio statistic:","\n","\n")
pval<-round(1-pchisq(res$LRT,1),4)
symb<-symnum(pval,c(0,0.001,0.01,0.05,0.1,1),symbols=c("***","**","*",".",""))
m1<-cbind(round(res$LRT,4),pval)
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



