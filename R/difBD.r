difBD<-function(Data,group,focal.name,BDstat="BD",alpha=0.05,purify=FALSE, nrIter=10,save.output=FALSE, output=c("out","default")) 
{
internalBD<-function(){
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
STATS<-breslowDay(DATA,Group,BDstat=BDstat)$res
if (min(STATS[,3])>=alpha) DIFitems<-"No DIF item detected"
else DIFitems<-(1:nrow(STATS))[STATS[,3]<alpha]
RES<-list(BD=STATS,alpha=alpha,DIFitems=DIFitems,BDstat=BDstat,purification=purify,names=colnames(DATA),save.output=save.output,output=output)
}
else{
nrPur<-0
difPur<-NULL
noLoop<-FALSE
stats1<-breslowDay(DATA,Group,BDstat=BDstat)$res
if (min(stats1[,3])>=alpha) {
DIFitems<-"No DIF item detected"
noLoop<-TRUE
}
else{
dif<-(1:nrow(stats1))[stats1[,3]<alpha]
difPur<-rep(0,nrow(stats1))
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
stats2<-breslowDay(DATA,Group,anchor=nodif,BDstat=BDstat)$res
if (min(stats2[,3])>=alpha) dif2<-NULL
else dif2<-(1:ncol(DATA))[stats2[,3]<alpha]
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
DIFitems <-(1:ncol(DATA))[stats1[,3]<alpha]
}
if (is.null(difPur)==FALSE){
ro<-co<-NULL
for (ir in 1:nrow(difPur)) ro[ir]<-paste("Step",ir-1,sep="")
for (ic in 1:ncol(difPur)) co[ic]<-paste("Item",ic,sep="")
rownames(difPur)<-ro
colnames(difPur)<-co
}
RES<-list(BD=stats1,alpha=alpha,DIFitems=DIFitems,BDstat=BDstat,purification=purify,nrPur=nrPur,difPur=difPur,convergence=noLoop,names=colnames(DATA),save.output=save.output,output=output)
}
class(RES)<-"BD"
return(RES)
}
resToReturn<-internalBD()
if (save.output==TRUE){
if (output[2]=="default") wd<-paste(getwd(),"/",sep="")
else wd<-output[2]
fileName<-paste(wd,output[1],".txt",sep="")
capture.output(resToReturn,file=fileName)
}
return(resToReturn)
}


# METHODS
plot.BD<-function(x,pch=8,number=TRUE,col="red", save.plot=FALSE,save.options=c("plot","default","pdf"),...){
internalBD<-function(){
res <- x
upper<-qchisq(1-res$alpha,res$BD[,2])
if (number==FALSE) {
plot(res$BD[,1],xlab="Item",ylab="Breslow-Day statistic",ylim=c(0,max(c(res$BD[,1],upper)+1)),pch=pch,main="Breslow-Day")
if (is.character(res$DIFitems)==FALSE) points(res$DIFitems,res$BD[res$DIFitems,1],pch=pch,col=col)
}
else {
plot(res$BD[,1],xlab="Item",ylab="Breslow-Day statistic",ylim=c(0,max(c(res$BD[,1],upper)+1)),col="white",main="Breslow-Day")
text(1:nrow(res$BD),res$BD[,1],1:nrow(res$BD))
if (is.character(res$DIFitems)==FALSE) text(res$DIFitems,res$BD[res$DIFitems,1],res$DIFitems,col=col)
}
s<-seq(0.5,nrow(res$BD)+0.5,1)
for (i in 1:nrow(res$BD)) lines(s[i:(i+1)],rep(upper[i],2))
for (i in 1:(nrow(res$BD)-1)) lines(rep(s[i+1],2),upper[i:(i+1)],lty=2)
}
internalBD()
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
internalBD()
}
dev.off()
}
if (plotype==2){
{
jpeg(file=fileName)
internalBD()
}
dev.off()
}
cat("The plot was captured and saved into","\n"," '",fileName,"'","\n","\n",sep="")
}
}
else cat("The plot was not captured!","\n",sep="")
}



print.BD <- function(x, ...){
 res <- x
cat("\n")
cat("Detection of Differential Item Functioning using Breslow-Day method","\n")
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
if (max(loop)!=nrow(res$BD)) cat("(Note: no loop detected in less than ",res$nrPur,word,")","\n",sep="")
else cat("(Note: loop of length ",min((1:res$nrPur)[loop==nrow(res$BD)])," in the item purification process)","\n",sep="")
cat("WARNING: following results based on the last iteration of the purification","\n","\n")
}
else cat("Convergence reached after ",res$nrPur,word,"\n","\n",sep="")
}
if (res$BDstat=="BD") cat("Breslow-Day statistic:","\n","\n")
else cat("Breslow-Day trend test statistic:","\n","\n")
pval<-res$BD[,3]
symb<-symnum(pval,c(0,0.001,0.01,0.05,0.1,1),symbols=c("***","**","*",".",""))
if (res$BDstat=="BD") m1<-cbind(round(res$BD[,1],4),res$BD[,2],pval)
else m1<-cbind(round(res$BD[,1],4),pval)
m1<-noquote(cbind(format(m1,justify="right"),symb))
if (is.null(res$names)==FALSE) rownames(m1)<-res$names
else{
rn<-NULL
for (i in 1:nrow(m1)) rn[i]<-paste("Item",i,sep="")
rownames(m1)<-rn
}
if (res$BDstat=="BD") colnames(m1)<-c("Stat.","df","P-value","")
else colnames(m1)<-c("Stat.","P-value","")
print(m1)
cat("\n")
cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 ","\n")
if (res$BDstat=="BD") cat("\n","Significance level: ",res$alpha,"\n","\n",sep="")
else cat("\n","Detection threshold: ",round(qchisq(1-res$alpha,1),4)," (significance level: ",res$alpha,")","\n","\n",sep="")
if (is.character(res$DIFitems)==TRUE) cat("Items detected as DIF items:",res$DIFitems,"\n","\n")
else {
cat("Items detected as DIF items:","\n")
m2<-cbind(rownames(m1)[res$DIFitems])
rownames(m2)<-rep("",nrow(m2))
colnames(m2)<-""
print(m2,quote=FALSE)
cat("\n")
}
if (x$save.output==FALSE) cat("Output was not captured!","\n")
    else {
if (x$output[2]=="default") wd<-paste(getwd(),"/",sep="")
else wd<-x$output[2]
fileName<-paste(wd,x$output[1],".txt",sep="")
cat("Output was captured and saved into file","\n"," '",fileName,"'","\n","\n",sep="")
}
}



