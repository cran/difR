difStd <-function(Data,group,focal.name,thr=0.1,purify=FALSE,nrIter=10)
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
STATS <-stdPDIF(DATA,Group)
 if (max(abs(STATS))<=thr) DIFitems<-"No DIF item detected"
 else DIFitems <-(1:ncol(DATA))[abs(STATS)>thr]
RES <-list(PDIF=STATS,thr=thr,DIFitems=DIFitems,purification=purify,names=colnames(DATA))
}
else{
nrPur<-0
difPur<-NULL
noLoop<-FALSE
stats1 <-stdPDIF(DATA,Group)
if (max(abs(stats1))<=thr) {
DIFitems<-"No DIF item detected"
noLoop<-TRUE
}
else{
dif    <-(1:ncol(DATA))[abs(stats1)>thr]
difPur<-rep(0,length(stats1))
difPur[dif]<-1
repeat{
if (nrPur>=nrIter) break
else{
nrPur<-nrPur+1
nodif  <-NULL
if (is.null(dif)==TRUE) nodif<-1:ncol(DATA)
else{
for (i in 1:ncol(DATA)){
if (sum(i==dif)==0) nodif<-c(nodif,i)
}
}
stats2 <-stdPDIF(DATA,Group,anchor=nodif)
if (max(abs(stats2))<=thr) dif2<-NULL
else dif2<-(1:ncol(DATA))[abs(stats2)>thr]
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
DIFitems <-(1:ncol(DATA))[abs(stats1)>thr]
}
if (is.null(difPur)==FALSE){
ro<-co<-NULL
for (ir in 1:nrow(difPur)) ro[ir]<-paste("Step",ir-1,sep="")
for (ic in 1:ncol(difPur)) co[ic]<-paste("Item",ic,sep="")
rownames(difPur)<-ro
colnames(difPur)<-co
}
RES<-list(PDIF=stats1,thr=thr,DIFitems=DIFitems,purification=purify,nrPur=nrPur,difPur=difPur,convergence=noLoop,names=colnames(DATA))
}
class(RES)<-"PDIF"
return(RES)
}

#METHODS
plot.PDIF <-function(x,pch=8,number=TRUE,col="red", ...){
res <- x
if (number==FALSE) {
plot(res$PDIF,xlab="Item",ylab="Standardization statistic",ylim=c(max(-1,min(c(res$PDIF,-res$thr)-0.2)),min(1,max(c(res$PDIF,res$thr)+0.2))),pch=pch,main="Standardization")
if (is.character(res$DIFitems)==FALSE) points(res$DIFitems,res$PDIF[res$DIFitems],pch=pch,col=col)
}
else {
plot(res$PDIF,xlab="Item",ylab="Standardization statistic",ylim=c(max(-1,min(c(res$PDIF,-res$thr)-0.2)),min(1,max(c(res$PDIF,res$thr)+0.2))),col="white",main="Standardization")
text(1:length(res$PDIF),res$PDIF,1:length(res$PDIF))
if (is.character(res$DIFitems)==FALSE) text(res$DIFitems,res$PDIF[res$DIFitems],res$DIFitems,col=col)
}
abline(h=res$thr)
abline(h=-res$thr)
abline(h=0,lty=2)
}


print.PDIF<-function(x, ...){
res <- x
cat("\n")
cat("Detection of Differential Item Functioning using standardization method","\n")
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
 if (max(loop)!=length(res$PDIF)) cat("(Note: no loop detected in less than ",res$nrPur,word,")","\n",sep="")
 else cat("(Note: loop of length ",min((1:res$nrPur)[loop==length(res$PDIF)])," in the item purification process)","\n",sep="")
 cat("WARNING: following results based on the last iteration of the purification","\n","\n")
}
 else cat("Convergence reached after ",res$nrPur,word,"\n","\n",sep="")
 }
cat("Standardization statistic:","\n","\n")
symb<-symnum(abs(res$PDIF),c(0,0.04,0.05,0.1,0.2,1),symbols=c("",".","*","**","***"))
m1<-cbind(round(res$PDIF,4))
m1<-noquote(cbind(format(m1,justify="right"),symb))
if (is.null(res$names)==FALSE) rownames(m1)<-res$names
else{
rn<-NULL
for (i in 1:nrow(m1)) rn[i]<-paste("Item",i,sep="")
rownames(m1)<-rn
}
colnames(m1)<-c("Stat.","")
print(m1)
cat("\n")
cat("Signif. codes (abs. values): 0 ' ' 0.04 '.' 0.05 '*' 0.1 '**' 0.2 '***' 1 ","\n")
cat("\n","Detection thresholds: ",-round(res$thr,4)," and ",round(res$thr,4),"\n","\n",sep="")
if (is.character(res$DIFitems)==TRUE) cat("Items detected as DIF items:",res$DIFitems,"\n","\n")
else {
cat("Items detected as DIF items:","\n")
m2<-cbind(rownames(m1)[res$DIFitems])
rownames(m2)<-rep("",nrow(m2))
colnames(m2)<-""
print(m2,quote=FALSE)
cat("\n","\n")
}
  cat("Effect size (Dorans, Schmitt and Bleistein scale):", "\n", "\n")
  cat("Effect size code:", "\n")
  cat(" '*': negligible effect", "\n")
  cat(" '**': moderate effect", "\n")
  cat(" '***': large effect", "\n", "\n")
  r2 <- round(res$PDIF,4)
  symb1 <- symnum(abs(r2), c(0, 0.05, 0.1, Inf), symbols = c("*", 
      "**", "***"))
  matR2<- noquote(cbind(format(r2, justify="right"), symb1))
  if (is.null(res$names) == FALSE) 
      rownames(matR2) <- res$names
  else {
      rn <- NULL
      for (i in 1:nrow(matR2)) rn[i] <- paste("Item", i, sep = "")
      rownames(matR2) <- rn
  }
  colnames(matR2) <- c("St-P-DIF", "")
  print(matR2)
  cat("\n")
  cat("Signif. codes: 0 '*' 0.05 '**' 0.10 '***'","\n")
  cat(" (for absolute values of 'St-P-DIF')","\n")
}
