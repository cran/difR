# DIF: LOGISTIC REGRESSION
Logistik<-function(data,member,anchor=1:ncol(data))
{
   res<-NULL
   for (item in 1:ncol(data))
   {
   data2<-data[,anchor]
   if (sum(anchor==item)==0) data2<-cbind(data2,data[,item])
   score<-rowSums(data2)
   Scores<-sort(unique(score))
   SCORES<-rep(Scores,2)
   GE<-unique(member)
   GROUP<-c(rep(GE[1],length(Scores)),rep(GE[2],length(Scores)))
   success<-failure<-NULL
   for (i in 1:length(Scores))
      {
      success<-c(success,length(data[,item][data[,item]==1 & member==GE[1] & score==Scores[i]]))
      failure<-c(failure,length(data[,item][data[,item]==0 & member==GE[1] & score==Scores[i]]))
      }
   for (i in 1:length(Scores))
      {
      success<-c(success,length(data[,item][data[,item]==1 & member==GE[2] & score==Scores[i]]))
      failure<-c(failure,length(data[,item][data[,item]==0 & member==GE[2] & score==Scores[i]]))
      }
   GROUP<-as.factor(GROUP)
   m0<-glm(cbind(success,failure)~GROUP*SCORES,family="binomial")
   m1<-glm(cbind(success,failure)~SCORES,family="binomial")
   res[item]<-deviance(m1)-deviance(m0)
   }
   return(res)
}


 