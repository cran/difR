selectDif<-function(Data,group,focal.name,method,alpha=0.05,MHstat="MHChisq",correct=TRUE,stdWeight="focal",thr=0.1,BDstat="BD",type="both",criterion="LRT",model="2PL",c=NULL,engine="ltm",irtParam=NULL,same.scale=TRUE,purify=FALSE,nrIter=10,save.output=FALSE, output=c("out","default")) 
{
res<-switch(method,
MH=difMH(Data=Data,group=group,focal.name=focal.name,MHstat=MHstat,correct=correct,alpha=alpha,purify=purify,nrIter=nrIter,save.output=save.output,output=output),
Std=difStd(Data=Data,group=group,focal.name=focal.name,stdWeight=stdWeight,thr=thr,purify=purify,nrIter=nrIter,save.output=save.output,output=output),
Logistic=difLogistic(Data=Data,group=group,focal.name=focal.name,type=type,criterion=criterion,alpha=alpha,purify=purify,nrIter=nrIter,save.output=save.output,output=output),
BD=difBD(Data=Data,group=group,focal.name=focal.name,BDstat=BDstat,alpha=alpha,purify=purify,nrIter=nrIter,save.output=save.output,output=output),
LRT=difLRT(Data=Data,group=group,focal.name=focal.name,alpha=alpha,purify=purify,nrIter=nrIter,save.output=save.output,output=output),
Raju=difRaju(Data=Data,group=group,focal.name=focal.name,model=model,c=c,engine=engine,alpha=alpha,purify=purify,nrIter=nrIter,save.output=save.output,output=output),
Lord=difLord(Data=Data,group=group,focal.name=focal.name,model=model,c=c,engine=engine,alpha=alpha,purify=purify,nrIter=nrIter,save.output=save.output,output=output))
return(res)
}

