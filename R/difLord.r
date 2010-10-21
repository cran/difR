difLord<-function(Data, group, focal.name, model, c=NULL, engine="ltm", irtParam=NULL, same.scale=TRUE, alpha=0.05, purify=FALSE, nrIter=10, save.output=FALSE, output=c("out","default"))
{
internalLord<-function(){
if (is.null(irtParam)==FALSE){
nrItems<-nrow(irtParam)/2
m0<-irtParam[1:nrItems,]
m1<-irtParam[(nrItems+1):(2*nrItems),]
if (same.scale==TRUE) m1p<-m1
else m1p<-itemRescale(m0,m1)
mod<-as.character(ncol(irtParam))
model<-switch(mod,"2"="1PL","5"="2PL","6"="3PL","9"="3PL")
if (ncol(irtParam)!=6) Guess<-NULL
else {
Guess<-irtParam[1:nrItems,6]
if (length(unique(round(Guess,4)))==1) Guess<-unique(round(Guess,4))
}
if (is.null(Guess)==TRUE) Q<-switch(model,"1PL"=qchisq(1-alpha,1),"2PL"=qchisq(1-alpha,2),"3PL"=qchisq(1-alpha,3))
else Q<-qchisq(1-alpha,2)
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
if (is.null(Guess)==TRUE) {
Q<-switch(model,"1PL"=qchisq(1-alpha,1),"2PL"=qchisq(1-alpha,2),"3PL"=qchisq(1-alpha,3))
m0<-switch(model,"1PL"=itemParEst(d0,model="1PL",engine=engine),"2PL"=itemParEst(d0,model="2PL"),"3PL"=itemParEst(d0,model="3PL"))
m1<-switch(model,"1PL"=itemParEst(d1,model="1PL",engine=engine),"2PL"=itemParEst(d1,model="2PL"),"3PL"=itemParEst(d1,model="3PL"))
}
else{
Q<-qchisq(1-alpha,2)
m0<-itemParEst(d0,model="3PL",c=Guess)
m1<-itemParEst(d1,model="3PL",c=Guess)
}
nrItems<-ncol(DATA)
m1p<-itemRescale(m0,m1,items=1:nrItems)
irtParam<-rbind(m0,m1p)
same.scale<-TRUE
dataName<-colnames(DATA)
itemParInit<-rbind(m0,m1)
estPar<-TRUE
}
if (purify==FALSE) {
STATS<-LordChi2(m0,m1p)
if ((max(STATS))<=Q) DIFitems<-"No DIF item detected"
else DIFitems<-(1:nrItems)[STATS>Q]
RES<-list(LordChi=STATS,alpha=alpha,thr=Q,DIFitems=DIFitems,purification=purify,model=model,c=Guess,engine=engine,itemParInit=itemParInit,estPar=estPar,names=dataName,save.output=save.output,output=output)
}
else{
nrPur<-0
difPur<-NULL
noLoop<-FALSE
stats1<-LordChi2(m0,m1p)
if (max(stats1)<=Q){
DIFitems<-"No DIF item detected"
noLoop<-TRUE
itemParFinal=rbind(m0,m1p)
RES<-list(LordChi=stats1,alpha=alpha,thr=Q,DIFitems=DIFitems,purification=purify,nrPur=nrPur,difPur=difPur,convergence=noLoop,model=model,c=Guess,engine=engine,itemParInit=itemParInit,itemParFinal=itemParFinal,estPar=estPar,names=dataName,save.output=save.output,output=output)
}
else{
dif<-(1:nrItems)[stats1>Q]
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
stats2<-LordChi2(m0,itemRescale(m0,m1,items=nodif))
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
RES<-list(LordChi=stats2,alpha=alpha,thr=Q,DIFitems=dif2,purification=purify,nrPur=nrPur,difPur=difPur,convergence=noLoop,model=model,c=Guess,engine=engine,itemParInit=itemParInit,itemParFinal=itemParFinal,estPar=estPar,names=dataName,save.output=save.output,output=output)
}
}
class(RES)<-"Lord"
return(RES)
}
resToReturn<-internalLord()
if (save.output==TRUE){
if (output[2]=="default") wd<-paste(getwd(),"/",sep="")
else wd<-output[2]
fileName<-paste(wd,output[1],".txt",sep="")
capture.output(resToReturn,file=fileName)
}
return(resToReturn)
}


# METHODS

plot.Lord<-function (x, plot = "lordStat", item = 1, pch = 8, number = TRUE, 
    col = "red", colIC = rep("black", 2), ltyIC = c(1, 2), save.plot=FALSE,save.options=c("plot","default","pdf"),...) 
{
internalLord<-function(){
    res <- x
    title <- expression(paste("Lord's ", chi^2))
    plotType <- switch(plot, lordStat = 1, itemCurve = 2)
    if (is.null(plotType) == TRUE) 
        return("Error: misspecified 'type' argument")
    else {
       if (plotType == 1) {
    		if (number == FALSE) {
       	 plot(res$LordChi, xlab = "Item", ylab = expression(paste(chi^2, 
         	   " statistic")), ylim = c(0, max(c(res$LordChi, res$thr) + 
       	     1)), pch = pch, main = title)
      	  if (is.character(res$DIFitems) == FALSE) 
       	     points(res$DIFitems, res$LordChi[res$DIFitems], pch = pch, 
           	     col = col)
   	 	}
   		else {
     	   	plot(res$LordChi, xlab = "Item", ylab = expression(paste(chi^2, 
      	      " statistic")), ylim = c(0, max(c(res$LordChi, res$thr) + 
      	      1)), col = "white", main = title)
      	  text(1:length(res$LordChi), res$LordChi, 1:length(res$LordChi))
      	  if (is.character(res$DIFitems) == FALSE) 
      	      text(res$DIFitems, res$LordChi[res$DIFitems], res$DIFitems, 
       	         col = col)
       	}
   	 abline(h = res$thr)
	}
	else{
            it <- ifelse(is.character(item) | is.factor(item), 
                (1:length(res$names))[res$names == item], item)
		J<-length(res$LordChi)
		if (res$purification==TRUE) matPar<-res$itemParFinal
		else matPar<-rbind(res$itemParInit[1:J,],
				   itemRescale(res$itemParInit[1:J,],
						res$itemParInit[(J+1):(2*J),]))
		nrpar<-ncol(matPar)
		nrpar<-paste("N",nrpar,sep="")
		parRef<-switch(nrpar,N2=c(1,matPar[it,1],0),
					   N5=c(matPar[it,1:2],0),
					   N6=matPar[it,c(1,2,6)],
					   N9=matPar[it,1:3])
		parFoc<-switch(nrpar,N2=c(1,matPar[J+it,1],0),
					   N5=c(matPar[J+it,1:2],0),
					   N6=matPar[J+it,c(1,2,6)],
					   N9=matPar[J+it,1:3])
            seq <- seq(-4,4, 0.1)
            mod <- function(t,s) t[3]+(1-t[3])*exp(t[1]*(s-t[2]))/(1+exp(t[1]*(s-t[2])))
            mainName <- ifelse(is.character(res$names[it]), res$names[it], 
                paste("Item ", it, sep = ""))
            plot(seq, mod(parRef,seq), col = colIC[1], 
                type = "l", lty = ltyIC[1], ylim = c(0, 1), xlab = expression(theta), 
                ylab = "Probability", main = mainName)
            lines(seq, mod(parFoc,seq), col = colIC[2], 
                  lty = ltyIC[2])
            legend(-4, 1, c("Reference", "Focal"), col = colIC, 
                  lty = ltyIC, bty = "n")
      }
   }
}
internalLord()
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
internalLord()
}
dev.off()
}
if (plotype==2){
{
jpeg(file=fileName)
internalLord()
}
dev.off()
}
cat("The plot was captured and saved into","\n"," '",fileName,"'","\n","\n",sep="")
}
}
else cat("The plot was not captured!","\n",sep="")
}


print.Lord<-function(x,...){
res<-x
cat("\n")
cat("Detection of Differential Item Functioning using Lord's method","\n")
if (res$purification==TRUE) pur<-"with "
else pur<-"without "
if (is.null(res$c)==TRUE) mod<-res$model
else mod<-"constrained 3PL"
cat("with ",mod," model and ",pur, "item purification","\n","\n",sep="")
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
if (max(loop)!=length(res$LordChi)) cat("(Note: no loop detected in less than ",res$nrPur,word,")","\n",sep="")
else cat("(Note: loop of length ",min((1:res$nrPur)[loop==length(res$LordChi)])," in the item purification process)","\n",sep="")
cat("WARNING: following results based on the last iteration of the purification","\n","\n")
}
else cat("Convergence reached after ",res$nrPur,word,"\n","\n",sep="")
}
cat("Lord's chi-square statistic:","\n","\n")
it<-rep("",length(res$LordChi))
df<-switch(res$model,"1PL"=1,"2PL"=2,"3PL"=3)
pval<-round(1-pchisq(res$LordChi,df),4)
symb<-symnum(pval,c(0,0.001,0.01,0.05,0.1,1),symbols=c("***","**","*",".",""))
m1<-cbind(round(res$LordChi,4),pval)
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