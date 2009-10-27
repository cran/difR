# MANTEL-HAENSZEL
mantelHaenszel<-function(data,member,correct=TRUE,anchor=1:ncol(data))
{
res<-NULL
for (item in 1:ncol(data)){
data2<-data[,anchor]
if (sum(anchor==item)==0) data2<-cbind(data2,data[,item])
xj<-rowSums(data2)
scores<-sort(unique(xj))
prov  <-NULL
ind   <-1:nrow(data)
for (j in 1:length(scores))
{
Aj    <-length(ind[xj==scores[j] & member==0 & data[,item]==1])
nrj   <-length(ind[xj==scores[j] & member==0])
nfj   <-length(ind[xj==scores[j] & member==1])
m1j   <-length(ind[xj==scores[j] & data[,item]==1])
m0j   <-length(ind[xj==scores[j] & data[,item]==0])
Tj<-length(ind[xj==scores[j]])
if (Tj>1) prov<-rbind(prov,c(Aj,nrj*m1j/Tj,nrj*nfj*m1j*m0j/(Tj^2*(Tj-1)),scores[j]))
}
if (correct==TRUE) res[item] <-(abs(sum(prov[,1]-prov[,2]))-0.5)^2/sum(prov[,3])
else res[item] <-(abs(sum(prov[,1]-prov[,2])))^2/sum(prov[,3])
}
return(res)
}

