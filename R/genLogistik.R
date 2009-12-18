# DIF: GENERALIZED LOGISTIC REGRESSION
genLogistik<-function (data, member, anchor = 1:ncol(data), type="both") 
{
    R2<-function(m,n) 1-(exp(-m$null.deviance/2)/exp(-m$deviance/2))^(2/n)
    R2max<-function(m,n) 1-(exp(-m$null.deviance/2))^(2/n)
    R2DIF<-function(m,n) R2(m,n)/R2max(m,n)
    dev <-  deltaR<- NULL
    nGroup <- length(unique(member))-1
    mFull <- mSimple <- matrix(0,ncol(data),2+2*nGroup)
    for (item in 1:ncol(data)) {
        data2 <- data[, anchor]
        if (sum(anchor == item) == 0) 
            data2 <- cbind(data2, data[, item])
        score <- rowSums(data2)
        Scores <- sort(unique(score))
        SCORES <- rep(Scores, nGroup+1)
        GE <- sort(unique(member))
        GROUP <- NULL
        for (i in 1:length(GE)) GROUP <- c(GROUP, rep(GE[i],length(Scores)))
        success <- failure <- NULL
	  for (t in 1:length(GE)){
          for (i in 1:length(Scores)) {
            success <- c(success, length(data[, item][data[, 
                item] == 1 & member == GE[t] & score == Scores[i]]))
            failure <- c(failure, length(data[, item][data[, 
                item] == 0 & member == GE[t] & score == Scores[i]]))
          }
	  }
        GROUP <- as.factor(GROUP)
        m0 <- switch(type,
         both=glm(cbind(success, failure) ~ SCORES * GROUP, family = "binomial"),
         udif=glm(cbind(success, failure) ~ SCORES + GROUP, family = "binomial"),
	   nudif=glm(cbind(success, failure) ~ SCORES * GROUP, family = "binomial"))
        m1 <- switch(type,
         both=glm(cbind(success, failure) ~ SCORES, family = "binomial"),
	   udif=glm(cbind(success, failure) ~ SCORES, family = "binomial"),
	   nudif=glm(cbind(success, failure) ~ SCORES + GROUP, family = "binomial"))
        dev[item] <- deviance(m1) - deviance(m0)
	  deltaR[item] <- R2DIF(m0,nrow(data))-R2DIF(m1,nrow(data))
        mFull[item,1:length(m0$coefficients)]<-m0$coefficients
        mSimple[item,1:length(m1$coefficients)]<-m1$coefficients
    }
    names <- c("(Intercept)","SCORE") 
    for (i in 2:length(GE)) names <- c(names, paste("GROUP",GE[i],sep=""))
    for (i in 2:length(GE)) names <- c(names, paste("SCORE:GROUP",GE[i],sep=""))
    colnames(mFull)<-colnames(mSimple) <- names
    res <- list(deviance=dev,deltaR2=deltaR,parM0 = mFull, parM1 = mSimple)
    return(res)
}
