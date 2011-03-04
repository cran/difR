difStd <-function(Data,group,focal.name,stdWeight="focal",thrSTD=0.1,purify=FALSE,nrIter=10,save.output=FALSE, output=c("out","default")) 
{
internalSTD<-function(){
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
resProv<-stdPDIF(DATA,Group,stdWeight=stdWeight)
STATS <- resProv$resStd
ALPHA <- resProv$resAlpha
 if (max(abs(STATS))<=thrSTD) DIFitems<-"No DIF item detected"
 else DIFitems <-(1:ncol(DATA))[abs(STATS)>thrSTD]
RES <-list(PDIF=STATS,stdAlpha=ALPHA,thr=thrSTD,DIFitems=DIFitems,purification=purify,names=colnames(DATA),stdWeight=stdWeight,save.output=save.output,output=output)
}
else{
nrPur<-0
difPur<-NULL
noLoop<-FALSE
resProv<-stdPDIF(DATA,Group,stdWeight=stdWeight)
stats1 <-resProv$resStd
alpha1<-resProv$resAlpha
if (max(abs(stats1))<=thrSTD) {
DIFitems<-"No DIF item detected"
noLoop<-TRUE
}
else{
dif    <-(1:ncol(DATA))[abs(stats1)>thrSTD]
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
resProv<-stdPDIF(DATA,Group,anchor=nodif,stdWeight=stdWeight)
stats2 <-resProv$resStd
alpha2<-resProv$resAlpha
if (max(abs(stats2))<=thrSTD) dif2<-NULL
else dif2<-(1:ncol(DATA))[abs(stats2)>thrSTD]
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
alpha1<-alpha2
DIFitems <-(1:ncol(DATA))[abs(stats1)>thrSTD]
}
if (is.null(difPur)==FALSE){
ro<-co<-NULL
for (ir in 1:nrow(difPur)) ro[ir]<-paste("Step",ir-1,sep="")
for (ic in 1:ncol(difPur)) co[ic]<-paste("Item",ic,sep="")
rownames(difPur)<-ro
colnames(difPur)<-co
}
RES<-list(PDIF=stats1,stdAlpha=alpha1,thr=thrSTD,DIFitems=DIFitems,purification=purify,nrPur=nrPur,difPur=difPur,convergence=noLoop,names=colnames(DATA),stdWeight=stdWeight,save.output=save.output,output=output)
}
class(RES)<-"PDIF"
return(RES)
}
resToReturn<-internalSTD()
if (save.output==TRUE){
if (output[2]=="default") wd<-paste(getwd(),"/",sep="")
else wd<-output[2]
fileName<-paste(wd,output[1],".txt",sep="")
capture.output(resToReturn,file=fileName)
}
return(resToReturn)
}


#METHODS
plot.PDIF <-function(x,pch=8,number=TRUE,col="red",save.plot=FALSE,save.options=c("plot","default","pdf"),...) 
{
internalSTD<-function(){
res <- x
if (number==FALSE) {
plot(res$PDIF,xlab="Item",ylab="Standardization statistic",ylim=c(max(-1,min(c(res$PDIF,-res$thr)-0.2)),min(1,max(c(res$PDIF,res$thr)+0.2))),pch=pch,main="Standardization")
if (is.character(res$DIFitems)==FALSE) points(res$DIFitems,res$PDIF[res$DIFitems],pch=pch,col=col)
}
else {
plot(res$PDIF,xlab="Item",ylab="St-PDIF statistic",ylim=c(max(-1,min(c(res$PDIF,-res$thr)-0.2)),min(1,max(c(res$PDIF,res$thr)+0.2))),col="white",main="Standardization")
text(1:length(res$PDIF),res$PDIF,1:length(res$PDIF))
if (is.character(res$DIFitems)==FALSE) text(res$DIFitems,res$PDIF[res$DIFitems],res$DIFitems,col=col)
}
abline(h=res$thr)
abline(h=-res$thr)
abline(h=0,lty=2)
}
internalSTD()
if (save.plot==TRUE){
plotype<-NULL
if (save.options[3]=="pdf") plotype<-1
if (save.options[3]=="jpeg") plotype<-2
if (is.null(plotype)==TRUE) cat("Invalid plot type (should be either 'pdf' or 'jpeg').","\n","The plot was not captured!","\n")
else {
if (save.options[2]=="default") wd<-paste(getwd(),"/",sep="")
else wd<-save.options[2]
fileName<-paste(wd,save.options[1],switch(plotype,'1'=".pdf",'2'=".jpg"),sep="")
if (plotype==1){
{
pdf(file=fileName)
internalSTD()
}
dev.off()
}
if (plotype==2){
{
jpeg(file=fileName)
internalSTD()
}
dev.off()
}
cat("The plot was captured and saved into","\n"," '",fileName,"'","\n","\n",sep="")
}
}
else cat("The plot was not captured!","\n",sep="")
}


print.PDIF<-function(x, ...){
res <- x
cat("\n")
cat("Detection of Differential Item Functioning using standardization method","\n")
if (res$purification==TRUE) pur<-"with "
else pur<-"without "
cat(pur, "item purification","\n",sep="")
if (res$stdWeight=="total") wt<-"both groups (the total group)"
else wt<-paste("the ",res$stdWeight," group",sep="")
cat("and with weights based on",wt,"\n" ,"\n")
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
cat("Standardized P-DIF statistic:","\n","\n")
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
  cat("Effect sizes:", "\n", "\n")
  cat("Effect size code:", "\n")
  cat(" 'A': negligible effect", "\n")
  cat(" 'B': moderate effect", "\n")
  cat(" 'C': large effect", "\n", "\n")
  r2 <- round(-2.35*log(res$stdAlpha),4)
  symb1 <- symnum(abs(res$PDIF), c(0, 0.05, 0.1, Inf), symbols = c("A", 
      "B", "C"))
  symb2 <- symnum(abs(r2), c(0, 1, 1.5, Inf), symbols = c("A", 
      "B", "C"))
  matR2<-cbind(round(res$PDIF,4),round(res$stdAlpha,4),r2)
  matR2<- noquote(cbind(format(matR2, justify="right"), symb1, symb2))
  if (is.null(res$names) == FALSE) 
      rownames(matR2) <- res$names
  else {
      rn <- NULL
      for (i in 1:nrow(matR2)) rn[i] <- paste("Item", i, sep = "")
      rownames(matR2) <- rn
  }
  colnames(matR2) <- c("St-P-DIF","alphaStd","deltaStd","DSB","ETS")
  print(matR2)
  cat("\n")
  cat("Effect size codes:", "\n")
  cat(" Dorans, Schmitt & Bleistein (DSB): 0 'A' 0.05 'B' 0.10 'C'","\n")
  cat("  (for absolute values of 'St-P-DIF')","\n")
  cat(" ETS Delta Scale (ETS): 0 'A' 1 'B' 1.5 'C'","\n")
  cat("  (for absolute values of 'deltaStd')","\n")
    if (x$save.output==FALSE) cat("\n","Output was not captured!","\n")
    else {
if (x$output[2]=="default") wd<-paste(getwd(),"/",sep="")
else wd<-x$output[2]
fileName<-paste(wd,x$output[1],".txt",sep="")
cat("\n","Output was captured and saved into file","\n"," '",fileName,"'","\n","\n",sep="")
}
}


