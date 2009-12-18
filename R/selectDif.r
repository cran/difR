selectDif<-function(Data,group,focal.name,method,alpha=0.05,correct=TRUE,thr=0.1,type="both",model="2PL",c=NULL,engine="ltm",irtParam=NULL,same.scale=TRUE,purify=FALSE,nrIter=10){
res<-switch(method,
MH=difMH(Data=Data,group=group,focal.name=focal.name,correct=correct,alpha=alpha,purify=purify,nrIter=nrIter),
Std=difStd(Data=Data,group=group,focal.name=focal.name,thr=thr,purify=purify,nrIter=nrIter),
Logistic=difLogistic(Data=Data,group=group,focal.name=focal.name,alpha=alpha,purify=purify,nrIter=nrIter),
BD=difBD(Data=Data,group=group,focal.name=focal.name,alpha=alpha,purify=purify,nrIter=nrIter),
LRT=difLRT(Data=Data,group=group,focal.name=focal.name,alpha=alpha,purify=purify,nrIter=nrIter),
Raju=difRaju(Data=Data,group=group,focal.name=focal.name,model=model,c=c,engine=engine,alpha=alpha,purify=purify,nrIter=nrIter),
Lord=difLord(Data=Data,group=group,focal.name=focal.name,model=model,c=c,engine=engine,alpha=alpha,purify=purify,nrIter=nrIter))
return(res)
}

