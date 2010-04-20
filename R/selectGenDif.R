selectGenDif<-function(Data,group,focal.names,method,type="both",criterion="LRT",alpha=0.05,model="2PL",c=NULL,engine="ltm",irtParam=NULL,nrFocal=2,same.scale=TRUE,purify=FALSE,nrIter=10){
res<-switch(method,
GMH=difGMH(Data=Data,group=group,focal.names=focal.names,alpha=alpha,purify=purify,nrIter=nrIter),
genLogistic=difGenLogistic(Data=Data,group=group,focal.names=focal.names,type=type,criterion=criterion,alpha=alpha,purify=purify,nrIter=nrIter),
genLord=difGenLord(Data=Data,group=group,focal.names=focal.names,model=model,c=c,engine=engine,irtParam=irtParam,nrFocal=nrFocal,same.scale=same.scale,alpha=alpha,purify=purify,nrIter=nrIter))
return(res)
}

