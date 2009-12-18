# BRESLOW-DAY
breslowDay<-function(data,member,anchor=1:ncol(data))
{
      res<-NULL
      for (item in 1:ncol(data))
      {
      data2<-data[,anchor]
      if (sum(anchor==item)==0) data2<-cbind(data2,data[,item])
      xj<-rowSums(data2)
      scores<-sort(unique(xj))
      ind<-1:nrow(data)
      prov<-NULL
        for (j in 1:length(scores))
        {
        if (scores[j]!=0 & scores[j]!=ncol(data2)){
        Aj<-length(ind[xj==scores[j] & member==0 & data[,item]==1])
        Bj<-length(ind[xj==scores[j] & member==0 & data[,item]==0])
        Cj<-length(ind[xj==scores[j] & member==1 & data[,item]==1])
        Dj<-length(ind[xj==scores[j] & member==1 & data[,item]==0])
        nrj<-length(ind[xj==scores[j] & member==0])
        nfj<-length(ind[xj==scores[j] & member==1])
        m1j<-length(ind[xj==scores[j] & data[,item]==1])
        m0j<-length(ind[xj==scores[j] & data[,item]==0])
        Tj<-length(ind[xj==scores[j]])
        if (nrj>0 & nfj>0 & m1j>0 & m0j>0) prov<-rbind(prov,c(Aj,Bj,Cj,Dj,nrj,nfj,m1j,Tj))
        }}
      if (sum(prov[,2]*prov[,3]/prov[,8])==0) res<-"Error: common M-H alpha cannot be computed!"
      else{
        alpha<-sum(prov[,1]*prov[,4]/prov[,8])/sum(prov[,2]*prov[,3]/prov[,8])
        E1<-(alpha*(prov[,5]+prov[,7])+(prov[,6]-prov[,7])-sqrt((alpha*(prov[,5]+prov[,7])+(prov[,6]-prov[,7]))^2-4*alpha*(alpha-1)*prov[,5]*prov[,7]))/(2*(alpha-1))
        E2<-(alpha*(prov[,5]+prov[,7])+(prov[,6]-prov[,7])+sqrt((alpha*(prov[,5]+prov[,7])+(prov[,6]-prov[,7]))^2-4*alpha*(alpha-1)*prov[,5]*prov[,7]))/(2*(alpha-1))
        E<-NULL
        for (ii in 1:length(E1))
           {
           if (E1[ii]>0 & E1[ii]<prov[ii,5] & E1[ii]<prov[ii,7]) E[ii]<-E1[ii]
           else E[ii]<-E2[ii]
           }
        V<-1/(1/E+1/(prov[,5]-E)+1/(prov[,7]-E)+1/(prov[,6]-prov[,7]+E))
        RES<-NULL
        for (ii in 1:length(V))
           {
        if (V[ii]>0) RES<-rbind(RES,c(prov[ii,1],E[ii],V[ii]))
           }
        df<-nrow(RES)-1
        STAT<-sum((RES[,1]-RES[,2])^2/RES[,3])
        res<-rbind(res,c(round(STAT,4),df,round(1-pchisq(STAT,df),4)))
           } 
        }
return(res)
}






