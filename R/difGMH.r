difGMH<-function(Data,group,focal.names,alpha=0.05,purify=FALSE,nrIter=10,save.output=FALSE, output=c("out","default")) 
{
internalGMH<-function(){
if (length(focal.names)==1) return(difMH(Data=Data,group=group,focal.name=focal.names,alpha=alpha,purify=purify,nrIter=nrIter,correct=FALSE))
else{
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
    DF<-length(focal.names)
    for (i in 1:DF) Group[gr == focal.names[i]] <- i
if (purify==FALSE) {
STATS<-genMantelHaenszel(DATA,Group)
if (max(STATS)<=qchisq(1-alpha,DF)) DIFitems<-"No DIF item detected"
else DIFitems <-(1:ncol(DATA))[STATS>qchisq(1-alpha,DF)]
RES <-list(GMH=STATS,alpha=alpha,thr=qchisq(1-alpha,DF),DIFitems=DIFitems,purification=purify,names=colnames(DATA),focal.names=focal.names,save.output=save.output,output=output)
}
else{
nrPur<-0
difPur<-NULL
noLoop<-FALSE
stats1 <-genMantelHaenszel(DATA,Group)
if (max(stats1)<=qchisq(1-alpha,DF)) {
DIFitems<-"No DIF item detected"
noLoop<-TRUE
}
else{
dif  <-(1:ncol(DATA))[stats1>qchisq(1-alpha,DF)]
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
stats2 <-genMantelHaenszel(DATA,Group,anchor=nodif)
if (max(stats2)<=qchisq(1-alpha,DF)) dif2<-NULL
else dif2<-(1:ncol(DATA))[stats2>qchisq(1-alpha,DF)]
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
DIFitems <-(1:ncol(DATA))[stats1>qchisq(1-alpha,DF)]
}
if (is.null(difPur)==FALSE){
ro<-co<-NULL
for (ir in 1:nrow(difPur)) ro[ir]<-paste("Step",ir-1,sep="")
for (ic in 1:ncol(difPur)) co[ic]<-paste("Item",ic,sep="")
rownames(difPur)<-ro
colnames(difPur)<-co
}
RES <-list(GMH=stats1,alpha=alpha,thr=qchisq(1-alpha,DF),DIFitems=DIFitems,purification=purify,nrPur=nrPur,difPur=difPur,convergence=noLoop,names=colnames(DATA),focal.names=focal.names,save.output=save.output,output=output)
}
class(RES) <-"GMH"
return(RES)
}
}
resToReturn<-internalGMH()
if (save.output==TRUE){
if (output[2]=="default") wd<-paste(getwd(),"/",sep="")
else wd<-output[2]
fileName<-paste(wd,output[1],".txt",sep="")
capture.output(resToReturn,file=fileName)
}
return(resToReturn)
}

# METHODS
plot.GMH <-function(x,pch=8,number=TRUE,col="red", save.plot=FALSE,save.options=c("plot","default","pdf"),...) 
{
internalGMH<-function(){
res <- x
if (number==FALSE) {
plot(res$GMH,xlab="Item",ylab="Generalized Mantel-Haenszel statistic",ylim=c(0,max(c(res$GMH,res$thr)+1)),pch=pch,main="Generalized Mantel-Haenszel")
if (is.character(res$DIFitems)==FALSE) points(res$DIFitems,res$GMH[res$DIFitems],pch=pch,col=col)
}
else {
plot(res$GMH,xlab="Item",ylab="Generalized Mantel-Haenszel statistic",ylim=c(0,max(c(res$GMH,res$thr)+1)),col="white",main=" Generalized Mantel-Haenszel")
text(1:length(res$GMH),res$GMH,1:length(res$GMH))
if (is.character(res$DIFitems)==FALSE) text(res$DIFitems,res$GMH[res$DIFitems],res$DIFitems,col=col)
}
abline(h=res$thr)
}
internalGMH()
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
internalGMH()
}
dev.off()
}
if (plotype==2){
{
jpeg(filename=fileName)
internalGMH()
}
dev.off()
}
cat("The plot was captured and saved into","\n"," '",fileName,"'","\n","\n",sep="")
}
}
else cat("The plot was not captured!","\n",sep="")
}



print.GMH<- function(x,...){
res <- x
cat("\n")
cat("Detection of Differential Item Functioning using Generalized Mantel-Haenszel","\n")
if (res$purification==TRUE) pur<-"with "
else pur<-"without "
cat("method, ",pur, "item purification and with ",length(res$focal.names)," focal groups","\n","\n",sep="")
if (is.character(res$focal.names)==TRUE | is.factor(res$focal.names)==TRUE){
cat("Focal groups:","\n")
nagr<-cbind(res$focal.names)
rownames(nagr)<-rep("",nrow(nagr))
colnames(nagr)<-""
print(nagr,quote=FALSE)
cat("\n")
}
 if (res$purification==TRUE){
if (res$nrPur<=1) word<-" iteration"
else word<-" iterations"
 if (res$convergence==FALSE) {
 cat("WARNING: no item purification convergence after ",res$nrPur,word,"\n",sep="")
 loop<-NULL
 for (i in 1:res$nrPur) loop[i]<-sum(res$difPur[1,]==res$difPur[i+1,])
 if (max(loop)!=length(res$GMH)) cat("(Note: no loop detected in less than ",res$nrPur,word,")","\n",sep="")
 else cat("(Note: loop of length ",min((1:res$nrPur)[loop==length(res$GMH)])," in the item purification process)","\n",sep="")
 cat("WARNING: following results based on the last iteration of the purification","\n","\n")
}
 else cat("Convergence reached after ",res$nrPur,word,"\n","\n",sep="")
 }
cat("Generalized Mantel-Haenszel chi-square statistic:","\n","\n")
pval<-round(1-pchisq(res$GMH,length(res$focal.names)),4)
symb<-symnum(pval,c(0,0.001,0.01,0.05,0.1,1),symbols=c("***","**","*",".",""))
m1<-cbind(round(res$GMH,4),pval)
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
    if (x$save.output==FALSE) cat("Output was not captured!","\n")
    else {
if (x$output[2]=="default") wd<-paste(getwd(),"/",sep="")
else wd<-x$output[2]
fileName<-paste(wd,x$output[1],".txt",sep="")
cat("Output was captured and saved into file","\n"," '",fileName,"'","\n","\n",sep="")
}
}