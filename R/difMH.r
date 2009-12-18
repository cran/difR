# DIF MANTEL-HAENZEL
difMH<-function(Data,group,focal.name,correct=TRUE,alpha=0.05,purify=FALSE,nrIter=10)
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
PROV <- mantelHaenszel(DATA,Group,correct=correct)
STATS<-PROV$resMH
if (max(STATS)<=qchisq(1-alpha,1)) DIFitems<-"No DIF item detected"
else DIFitems <-(1:ncol(DATA))[STATS>qchisq(1-alpha,1)]
RES <-list(MH=STATS,alphaMH=PROV$resAlpha,alpha=alpha,thr=qchisq(1-alpha,1),DIFitems=DIFitems,correct=correct,purification=purify,names=colnames(DATA))
}
else{
nrPur<-0
difPur<-NULL
noLoop<-FALSE
prov1 <- mantelHaenszel(DATA,Group,correct=correct)
stats1 <- prov1$resMH
if (max(stats1)<=qchisq(1-alpha,1)) {
DIFitems<-"No DIF item detected"
noLoop<-TRUE
}
else{ 
dif  <-(1:ncol(DATA))[stats1>qchisq(1-alpha,1)]
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
prov2 <- mantelHaenszel(DATA,Group,correct=correct,anchor=nodif)
stats2 <- prov2$resMH
if (max(stats2)<=qchisq(1-alpha,1)) dif2<-NULL
else dif2<-(1:ncol(DATA))[stats2>qchisq(1-alpha,1)]
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
prov1 <- prov2
DIFitems <-(1:ncol(DATA))[stats1>qchisq(1-alpha,1)]
}
if (is.null(difPur)==FALSE){
ro<-co<-NULL
for (ir in 1:nrow(difPur)) ro[ir]<-paste("Step",ir-1,sep="")
for (ic in 1:ncol(difPur)) co[ic]<-paste("Item",ic,sep="")
rownames(difPur)<-ro
colnames(difPur)<-co
}
RES <-list(MH=stats1,alphaMH=prov1$resAlpha,alpha=alpha,thr=qchisq(1-alpha,1),DIFitems=DIFitems,correct=correct,purification=purify,nrPur=nrPur,difPur=difPur,convergence=noLoop,names=colnames(DATA))
}
class(RES) <-"MH"
return(RES)
}


# METHODS
plot.MH <- function(x, pch=8, number=TRUE, col="red", ...){
 res <- x
 if (number==FALSE) {
  plot(res$MH, xlab="Item", ylab="Mantel-Haenszel statistic", ylim=c(0,max(c(res$MH,res$thr)+1)), pch=pch, main="Mantel-Haenszel")
  if (is.character(res$DIFitems)==FALSE) points(res$DIFitems,res$MH[res$DIFitems],pch=pch,col=col)
 }
 else {
  plot(res$MH, xlab="Item", ylab="Mantel-Haenszel statistic", ylim=c(0,max(c(res$MH,res$thr)+1)), col="white", main="Mantel-Haenszel")
  text(1:length(res$MH), res$MH, 1:length(res$MH))
  if (is.character(res$DIFitems)==FALSE) text(res$DIFitems, res$MH[res$DIFitems], res$DIFitems,col=col)
 }
 abline(h=res$thr)
}


print.MH <- function(x, ...){
 res <- x
 cat("\n")
 cat("Detection of Differential Item Functioning using Mantel-Haenszel method","\n")
 if (res$correct==TRUE) corr <- "with "
 else corr <- "without "
 if (res$purification==TRUE) pur <- "with "
 else pur <- "without "
 cat(corr,"continuity correction and ", pur, "item purification","\n","\n", sep="")
 if (res$purification==TRUE){
 if (res$nrPur<=1) word<-" iteration"
 else word<-" iterations"
 if (res$convergence==FALSE) {
 cat("WARNING: no item purification convergence after ",res$nrPur,word,"\n",sep="")
 loop<-NULL
 for (i in 1:res$nrPur) loop[i]<-sum(res$difPur[1,]==res$difPur[i+1,])
 if (max(loop)!=length(res$MH)) cat("(Note: no loop detected in less than ",res$nrPur,word,")","\n",sep="")
 else cat("(Note: loop of length ",min((1:res$nrPur)[loop==length(res$MH)])," in the item purification process)","\n",sep="")
 cat("WARNING: following results based on the last iteration of the purification","\n","\n")
}
 else cat("Convergence reached after ",res$nrPur,word,"\n","\n",sep="")
 }
 cat("Mantel-Haenszel chi-square statistic:","\n","\n")
 pval <- round(1-pchisq(res$MH,1),4)
 symb <- symnum(pval,c(0,0.001,0.01,0.05,0.1,1),symbols=c("***","**","*",".",""))
 m1   <- cbind(round(res$MH,4),pval)
 m1   <- noquote(cbind(format(m1,justify="right"),symb))
 if (is.null(res$names)==FALSE) rownames(m1) <- res$names
 else {
  rn <- NULL
  for (i in 1:nrow(m1)) rn[i] <- paste("Item",i,sep="")
  rownames(m1) <- rn
 }
 colnames(m1) <- c("Stat.","P-value","")
 print(m1)
 cat("\n")
 cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 ","\n")
 cat("\n","Detection threshold: ",round(res$thr,4)," (significance level: ",res$alpha,")","\n","\n",sep="")
 if (is.character(res$DIFitems)==TRUE) cat("Items detected as DIF items:",res$DIFitems,"\n","\n")
 else {
  cat("Items detected as DIF items:","\n")
  m2           <- cbind(rownames(m1)[res$DIFitems])
  rownames(m2) <- rep("",nrow(m2))
  colnames(m2) <- ""
  print(m2, quote=FALSE)
  cat("\n","\n")
 }
  cat("Effect size (ETS Delta scale):", "\n", "\n")
  cat("Effect size code:", "\n")
  cat(" '*': negligible effect", "\n")
  cat(" '**': moderate effect", "\n")
  cat(" '***': large effect", "\n", "\n")
  r2 <- round(-2.35*log(res$alphaMH),4)
  symb1 <- symnum(abs(r2), c(0, 1, 1.5, Inf), symbols = c("*", 
      "**", "***"))
  matR2<-cbind(round(res$alphaMH,4),r2)
  matR2<- noquote(cbind(format(matR2, justify="right"), symb1))
  if (is.null(res$names) == FALSE) 
      rownames(matR2) <- res$names
  else {
      rn <- NULL
      for (i in 1:nrow(matR2)) rn[i] <- paste("Item", i, sep = "")
      rownames(matR2) <- rn
  }
  colnames(matR2) <- c("alphaMH", "deltaMH", "")
  print(matR2)
  cat("\n")
  cat("Signif. codes: 0 '*' 1.0 '**' 1.5 '***'","\n")
  cat(" (for absolute values of 'deltaMH')","\n")
}








