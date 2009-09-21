# DIF: STANDARDIZATION

stdPDIF<-function(data,member,anchor=1:ncol(data))
{
res <-NULL
for (item in 1:ncol(data))
{
data2<-data[,anchor]
if (sum(anchor==item)==0) data2<-cbind(data2,data[,item])
xj     <-rowSums(data2)
scores <-sort(unique(xj))

ind <-1:nrow(data)

prov<-NULL
 for (j in 1:length(scores))
{
Prs   <-length(ind[xj==scores[j] & member==0 & data[,item]==1])/length(ind[xj==scores[j] & member==0])
Pfs   <-length(ind[xj==scores[j] & member==1 & data[,item]==1])/length(ind[xj==scores[j] & member==1])
Ks    <-length(ind[xj==scores[j] & member==1])
prov  <-rbind(prov,c(scores[j],Prs,Pfs,Ks))
}
num<-den<-0
 for (i in 1:nrow(prov))
{
 if (is.na(prov[i,2])==FALSE & is.na(prov[i,3])==FALSE) 
{
num  <-num+prov[i,4]*(prov[i,3]-prov[i,2])
den  <-den+prov[i,4]
}
}
res[item]<-num/den
}
return(res)
}




