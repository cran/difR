difGenLord<-function(Data, group, focal.names, model, c=NULL, engine="ltm", irtParam=NULL, nrFocal=2, same.scale=TRUE, alpha=0.05, purify=FALSE, nrIter=10)
{
if (is.null(irtParam)==FALSE){
nrItems<-nrow(irtParam)/(nrFocal+1)
if (same.scale==FALSE){
prov<-vector("list",nrFocal+1)
for (i in 1:(nrFocal+1)) prov[[i]]<-irtParam[((i-1)*nrItems+1):(i*nrItems),]
irtParam<-prov[[1]]
for (gr in 1:nrFocal) irtParam<-rbind(irtParam,itemRescale(prov[[1]],prov[[gr+1]]))
}
mod<-as.character(ncol(irtParam))
model<-switch(mod,"2"="1PL","5"="2PL","6"="3PL","9"="3PL")
nPar<-switch(mod,"2"=1,"5"=2,"6"=2,"9"=3)
if (ncol(irtParam)!=6) Guess<-NULL
else {
Guess<-irtParam[1:nrItems,6]
if (length(unique(round(Guess,4)))==1) Guess<-unique(round(Guess,4))
}
Q<-qchisq(1-alpha,nPar*nrFocal)
dataName<-rownames(irtParam[1:nrItems,])
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
nrFocal<-length(focal.names)
for (i in 1:nrFocal) Group[gr == focal.names[i]] <- i
nrItems<-ncol(DATA)
irtParam<-NULL
GROUP<-0:nrFocal
for (indic in 1:length(GROUP)){
nr<-length(Group[Group==GROUP[indic]])
d0<-matrix(NA,nr,nrItems)
c0<-0
for (i in 1:length(Group)){
if (Group[i]==GROUP[indic]){
c0<-c0+1
d0[c0,]<-as.numeric(DATA[i,])
}
}
Guess<-c
if (is.null(Guess)==TRUE) m0<-switch(model,"1PL"=itemParEst(d0,model="1PL",engine=engine),"2PL"=itemParEst(d0,model="2PL"),"3PL"=itemParEst(d0,model="3PL"))
else m0<-itemParEst(d0,model="3PL",c=Guess)
if (indic==1) irtParam<-m0
else irtParam<-rbind(irtParam,itemRescale(irtParam[1:nrItems,],m0))
}
if (is.null(Guess)==TRUE) nPar<-switch(model,"1PL"=1,"2PL"=2,"3PL"=3)
else nPar<-2
Q<-qchisq(1-alpha,nPar*nrFocal)
dataName<-colnames(DATA)
itemParInit<-irtParam
estPar<-TRUE
}
if (purify==FALSE) {
STATS<-genLordChi2(irtParam,nrFocal)
if ((max(STATS))<=Q) DIFitems<-"No DIF item detected"
else DIFitems<-(1:nrItems)[STATS>Q]
RES<-list(genLordChi=STATS,alpha=alpha,thr=Q,df=nPar*nrFocal,DIFitems=DIFitems,purification=purify,model=model,c=Guess,engine=engine,itemParinit=itemParInit,estPar=estPar,names=dataName)
}
else{
nrPur<-0
difPur<-NULL
noLoop<-FALSE
stats1<-genLordChi2(irtParam,nrFocal)
if (max(stats1)<=Q){
DIFitems<-"No DIF item detected"
noLoop<-TRUE
itemParFinal=irtParam
RES<-list(genLordChi=stats1,alpha=alpha,thr=Q,df=nPar*nrFocal,DIFitems=DIFitems,purification=purify,nrPur=nrPur,difPur=difPur,convergence=noLoop,model=model,c=Guess,engine=engine,itemParinit=itemParInit,itemParFinal=itemParFinal,estPar=estPar,names=dataName)
}
else{
dif<-(1:nrItems)[stats1>Q]
difPur<-rep(0,length(stats1))
difPur[dif]<-1
repeat{
if (nrPur>=nrIter) {
itemParFinal<-irtParam
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
prov<-vector("list",nrFocal+1)
for (i in 1:(nrFocal+1)) prov[[i]]<-irtParam[((i-1)*nrItems+1):(i*nrItems),]
irtParam<-prov[[1]]
for (gr in 1:nrFocal) irtParam<-rbind(irtParam,itemRescale(prov[[1]],prov[[gr+1]],items=nodif))
stats2<-genLordChi2(irtParam,nrFocal) 
if (max(stats2)<=Q) dif2<-NULL
else dif2 <- (1:nrItems)[stats2>Q]
difPur<-rbind(difPur,rep(0,nrItems))
difPur[nrPur+1,dif2]<-1
if (length(dif)!=length(dif2)) dif<-dif2
else{
dif<-sort(dif)
dif2<-sort(dif2)
if (sum(dif==dif2)==length(dif)) {
noLoop<-TRUE
itemParFinal<-irtParam
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
RES<-list(genLordChi=stats2,alpha=alpha,thr=Q,df=nPar*nrFocal,DIFitems=dif2,purification=purify,nrPur=nrPur,difPur=difPur,convergence=noLoop,model=model,c=Guess,engine=engine,itemParinit=itemParInit,itemParFinal=itemParFinal,estPar=estPar,names=dataName)
}
}
class(RES)<-"GenLord"
return(RES)
}



# METHODS
plot.GenLord<-function(x,pch=8,number=TRUE,col="red",...){
res<-x
title<-expression(paste("Generalized Lord's ",chi^2))
if (number==FALSE) {
plot(res$genLordChi,xlab="Item",ylab=expression(paste("Generalized Lord's ",chi^2," statistic")),ylim=c(0,max(c(res$genLordChi,res$thr)+1)),pch=pch,main=title)
if (is.character(res$DIFitems)==FALSE) points(res$DIFitems,res$genLordChi[res$DIFitems],pch=pch,col=col)
}
else {
plot(res$genLordChi,xlab="Item",ylab=expression(paste("Generalized Lord's ",chi^2," statistic")),ylim=c(0,max(c(res$genLordChi,res$thr)+1)),col="white",main=title)
text(1:length(res$genLordChi),res$genLordChi,1:length(res$genLordChi))
if (is.character(res$DIFitems)==FALSE) text(res$DIFitems,res$genLordChi[res$DIFitems],res$DIFitems,col=col)
}
abline(h=res$thr)
}


print.GenLord<-function(x,...){
res<-x
cat("\n")
cat("Detection of Differential Item Functioning using generalized Lord's method","\n")
if (res$purification==TRUE) pur<-"with "
else pur<-"without "
if (is.null(res$c)==TRUE) {
mod<-res$model
nrFocal<-res$df/switch(res$model,"1PL"=1,"2PL"=2,"3PL"=3)
}
else {
mod<-"constrained 3PL"
nrFocal<-res$df/2
}
cat("(",nrFocal," focal groups), with ",mod," model and ",pur, "item purification","\n","\n",sep="")
if (res$estPar==TRUE){
if (res$model!="1PL" | res$engine=="ltm") cat("Engine 'ltm' for item parameter estimation","\n","\n")
else cat("Engine 'lme4' for item parameter estimation","\n","\n")
}
if (is.null(res$c)==FALSE){
if (length(res$c)==1) cat("Common pseudo-guessing value: ",res$c,"\n","\n",sep="")
else {
pg<-cbind(res$c)
rownames(pg)<-res$names
colnames(pg)<-"c"
cat("Common pseudo-guessing values:","\n","\n")
print(pg)
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
if (max(loop)!=length(res$genLordChi)) cat("(Note: no loop detected in less than ",res$nrPur,word,")","\n",sep="")
else cat("(Note: loop of length ",min((1:res$nrPur)[loop==length(res$genLordChi)])," in the item purification process)","\n",sep="")
cat("WARNING: following results based on the last iteration of the purification","\n","\n")
}
else cat("Convergence reached after ",res$nrPur,word,"\n","\n",sep="")
}
cat("Generalized Lord's chi-square statistic:","\n","\n")
it<-rep("",length(res$genLordChi))
pval<-round(1-pchisq(res$genLordChi,res$df),4)
symb<-symnum(pval,c(0,0.001,0.01,0.05,0.1,1),symbols=c("***","**","*",".",""))
m1<-cbind(round(res$genLordChi,4),pval)
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

