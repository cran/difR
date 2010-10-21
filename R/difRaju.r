difRaju<-function(Data, group, focal.name, model, c=NULL, engine="ltm", irtParam=NULL, same.scale=TRUE, alpha=0.05, purify=FALSE, nrIter=10,save.output=FALSE, output=c("out","default"))
{
internalRaju<-function(){
if (is.null(irtParam)==FALSE){
nrItems<-nrow(irtParam)/2
m0<-irtParam[1:nrItems,]
m1<-irtParam[(nrItems+1):(2*nrItems),]
if (same.scale==TRUE) m1p<-m1
else m1p<-itemRescale(m0,m1)
mod<-as.character(ncol(irtParam))
model=switch(mod,"2"="1PL","5"="2PL","6"="3PL")
dataName=rownames(m0)
if (ncol(irtParam)!=6) Guess<-NULL
else{
Guess<-irtParam[1:nrItems,6]
if (length(unique(round(Guess,4)))==1) Guess<-unique(round(Guess,4))
}
itemParInit<-irtParam
estPar<-FALSE
}
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
    Group[gr == focal.name] <- 1  

nr1<-sum(Group)
nr0<-length(Group)-nr1

d0<-matrix(NA,nr0,ncol(DATA))
d1<-matrix(NA,nr1,ncol(DATA))

c0<-c1<-0
for (i in 1:length(Group)){
if (Group[i]==0){
c0<-c0+1
d0[c0,]<-as.numeric(DATA[i,])
}
else {
c1<-c1+1
d1[c1,]<-as.numeric(DATA[i,])
}
}

Guess<-c
if (is.null(Guess)==TRUE){
m0<-switch(model,"1PL"=itemParEst(d0,model="1PL",engine=engine),"2PL"=itemParEst(d0,model="2PL"))
m1<-switch(model,"1PL"=itemParEst(d1,model="1PL",engine=engine),"2PL"=itemParEst(d1,model="2PL"))
}
else{
m0<-itemParEst(d0,model="3PL",c=Guess)
m1<-itemParEst(d1,model="3PL",c=Guess)
}
m1p<-itemRescale(m0,m1)
dataName<-colnames(DATA)
nrItems<-ncol(DATA)
itemParInit<-rbind(m0,m1)
estPar<-TRUE
}

if (purify==FALSE) {
STATS<-RajuZ(m0,m1p)[,3]
if (max(abs(STATS))<=qnorm(1-alpha/2)) DIFitems<-"No DIF item detected"
else DIFitems<-(1:nrItems)[abs(STATS)>qnorm(1-alpha/2)]
RES<-list(RajuZ=STATS,alpha=alpha,thr=qnorm(1-alpha/2),DIFitems=DIFitems,purification=purify,model=model,c=Guess,engine=engine,itemParInit=itemParInit,estPar=estPar,names=dataName,save.output=save.output,output=output)
}
else{
nrPur<-0
difPur<-NULL
noLoop<-FALSE
stats1<-RajuZ(m0,m1p)[,3]
if (max(abs(stats1))<=qnorm(1-alpha/2)){
DIFitems<-"No DIF item detected"
noLoop<-TRUE
itemParFinal=rbind(m0,m1p)
RES<-list(RajuZ=stats1,alpha=alpha,thr=qnorm(1-alpha/2),DIFitems=DIFitems,purification=purify,nrPur=nrPur,difPur=difPur,convergence=noLoop,model=model,c=Guess,engine=engine,itemParInit=itemParInit,itemParFinal=itemParFinal,estPar=estPar,names=dataName,save.output=save.output,output=output)
}
else{ 
dif<-(1:nrItems)[abs(stats1)>qnorm(1-alpha/2)]
difPur<-rep(0,length(stats1))
difPur[dif]<-1
repeat{
if (nrPur>=nrIter) {
itemParFinal<-rbind(m0,itemRescale(m0,m1,items=nodif))
break
}
else{
nrPur<-nrPur+1
nodif<-NULL
if (is.null(dif)==TRUE) nodif<-1:nrItems
else{
for (i in 1:nrItems){
if (sum(i==dif)==0) nodif<-c(nodif,i)
}
}
stats2<-RajuZ(m0,itemRescale(m0,m1,items=nodif))[,3]
if (max(abs(stats2))<=qnorm(1-alpha/2)) dif2<-NULL
else dif2 <- (1:nrItems)[abs(stats2)>qnorm(1-alpha/2)]
difPur<-rbind(difPur,rep(0,nrItems))
difPur[nrPur+1,dif2]<-1
if (length(dif)!=length(dif2)) dif<-dif2
else{
dif<-sort(dif)
dif2<-sort(dif2)
if (sum(dif==dif2)==length(dif)){
noLoop<-TRUE
itemParFinal<-rbind(m0,itemRescale(m0,m1,items=nodif))
break
}
else dif<-dif2
}
}
}
if (is.null(difPur)==FALSE){
ro<-co<-NULL
for (ir in 1:nrow(difPur)) ro[ir]<-paste("Step",ir-1,sep="")
for (ic in 1:ncol(difPur)) co[ic]<-paste("Item",ic,sep="")
rownames(difPur)<-ro
colnames(difPur)<-co
}
RES<-list(RajuZ=stats2,alpha=alpha,thr=qnorm(1-alpha/2),DIFitems=dif2,purification=purify,nrPur=nrPur,difPur=difPur,convergence=noLoop,model=model,c=Guess,engine=engine,itemParInit=itemParInit,itemParFinal=itemParFinal,estPar=estPar,names=dataName,save.output=save.output,output=output)
}
}
class(RES)<-"Raj"
return(RES)
}
resToReturn<-internalRaju()
if (save.output==TRUE){
if (output[2]=="default") wd<-paste(getwd(),"/",sep="")
else wd<-output[2]
fileName<-paste(wd,output[1],".txt",sep="")
capture.output(resToReturn,file=fileName)
}
return(resToReturn)
}


# METHODS
plot.Raj<-function(x,pch=8,number=TRUE,col="red", save.plot=FALSE,save.options=c("plot","default","pdf"),...) 
{
internalRaju<-function(){
res <- x
title<-paste("Raju's method (",res$model,")",sep="")
if (number==FALSE) {
plot(res$RajuZ,xlab="Item",ylab="Raju's statistic",ylim=c(min(c(res$RajuZ,-res$thr)-1),max(c(res$RajuZ,res$thr)+1)),pch=pch,main=title)
if (is.character(res$DIFitems)==FALSE) points(res$DIFitems,res$RajuZ[res$DIFitems],pch=pch,col=col)
}
else {
plot(res$RajuZ,xlab="Item",ylab="Raju's statistic",ylim=c(min(c(res$RajuZ,-res$thr)-1),max(c(res$RajuZ,res$thr)+1)),col="white",main=title)
text(1:length(res$RajuZ),res$RajuZ,1:length(res$RajuZ))
if (is.character(res$DIFitems)==FALSE) text(res$DIFitems,res$RajuZ[res$DIFitems],res$DIFitems,col=col)
}
abline(h=res$thr)
abline(h=-res$thr)
abline(h=0,lty=2)
}
internalRaju()
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
internalRaju()
}
dev.off()
}
if (plotype==2){
{
jpeg(file=fileName)
internalRaju()
}
dev.off()
}
cat("The plot was captured and saved into","\n"," '",fileName,"'","\n","\n",sep="")
}
}
else cat("The plot was not captured!","\n",sep="")
}



print.Raj<-function(x,...){
res <- x
if (is.character(res)==TRUE) cat("Error: unappropriate model specification!","\n")
else{
cat("\n")
cat("Detection of Differential Item Functioning using Raju's method","\n")
if (res$purification==TRUE) pur<-"with "
else pur<-"without "
cat("with ",res$model," model and ",pur, "item purification","\n","\n",sep="")
if (res$estPar==TRUE){
if (res$model!="1PL" | res$engine=="ltm") cat("Engine 'ltm' for item parameter estimation","\n","\n")
else cat("Engine 'lme4' for item parameter estimation","\n","\n")
}
if (is.null(res$c)==FALSE){
if (length(res$c)==1) cat("Common pseudo-guessing value:",res$c,"\n","\n")
else 
{
mat<-cbind(res$c)
if (is.null(res$names)==FALSE) rownames(mat)<-res$names
else{
rn<-NULL
for (i in 1:nrow(mat)) rn[i]<-paste("Item",i,sep="")
rownames(mat)<-rn
}
colnames(mat)<-"c"
cat("Common pseudo-guessing values:","\n","\n")
print(mat)
cat("\n")
}
}
if (res$purification==TRUE){
if (res$nrPur<=1) word<-" iteration"
else word<-" iterations"
if (res$convergence==FALSE) {
cat("WARNING: no item purification convergence after ",res$nrPur,word,"\n",sep="")
loop<-NULL
for (i in 1:res$nrPur) loop[i]<-sum(res$difPur[1,]==res$difPur[i+1,])
if (max(loop)!=length(res$RajuZ)) cat("(Note: no loop detected in less than ",res$nrPur,word,")","\n",sep="")
else cat("(Note: loop of length ",min((1:res$nrPur)[loop==length(res$RajuZ)])," in the item purification process)","\n",sep="")
cat("WARNING: following results based on the last iteration of the purification","\n","\n")
}
else cat("Convergence reached after ",res$nrPur,word,"\n","\n",sep="")
}
cat("Raju's statistic:","\n","\n")
pval<-round(2*(1-pnorm(abs(res$RajuZ))),4)
symb<-symnum(pval,c(0,0.001,0.01,0.05,0.1,1),symbols=c("***","**","*",".",""))
m1<-cbind(round(res$RajuZ,4),pval)
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
cat("\n","Detection thresholds: ",-round(res$thr,4)," and ",round(res$thr,4)," (significance level: ",res$alpha,")","\n","\n",sep="")
if (is.character(res$DIFitems)==TRUE) cat("Items detected as DIF items:",res$DIFitems,"\n","\n")
else {
cat("Items detected as DIF items:","\n")
m2<-cbind(rownames(m1)[res$DIFitems])
rownames(m2)<-rep("",nrow(m2))
colnames(m2)<-""
print(m2,quote=FALSE)
cat("\n")
}
}
    if (x$save.output==FALSE) cat("Output was not captured!","\n")
    else {
if (x$output[2]=="default") wd<-paste(getwd(),"/",sep="")
else wd<-x$output[2]
fileName<-paste(wd,x$output[1],".txt",sep="")
cat("Output was captured and saved into file","\n"," '",fileName,"'","\n","\n",sep="")
}
}

