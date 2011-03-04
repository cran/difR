# DIF: LOGISTIC REGRESSION
Logistik<-function (data, member, anchor = 1:ncol(data), type = "both", criterion="LRT") 
{
    R2 <- function(m, n) 1 - (exp(-m$null.deviance/2)/exp(-m$deviance/2))^(2/n)
    R2max <- function(m, n) 1 - (exp(-m$null.deviance/2))^(2/n)
    R2DIF <- function(m, n) R2(m, n)/R2max(m, n)
    dev <- deltaR <- NULL
    mFull <- mSimple <- matrix(0, ncol(data), 4)
    for (item in 1:ncol(data)) {
        data2 <- data[, anchor]
        if (sum(anchor == item) == 0) 
            data2 <- cbind(data2, data[, item])
        score <- rowSums(data2, na.rm=TRUE)
        Scores <- sort(unique(score))
        SCORES <- rep(Scores, 2)
        GE <- c(1, 0)
        GROUP <- c(rep(GE[1], length(Scores)), rep(GE[2], length(Scores)))
        success <- failure <- NULL
        for (i in 1:length(Scores)) {
            success <- c(success, length(data[, item][data[, 
                item] == 1 & member == GE[1] & score == Scores[i]]))
            failure <- c(failure, length(data[, item][data[, 
                item] == 0 & member == GE[1] & score == Scores[i]]))
        }
        for (i in 1:length(Scores)) {
            success <- c(success, length(data[, item][data[, 
                item] == 1 & member == GE[2] & score == Scores[i]]))
            failure <- c(failure, length(data[, item][data[, 
                item] == 0 & member == GE[2] & score == Scores[i]]))
        }
        GROUP <- as.factor(GROUP)
        m0 <- switch(type, both = glm(cbind(success, failure) ~ 
            SCORES * GROUP, family = "binomial"), udif = glm(cbind(success, 
            failure) ~ SCORES + GROUP, family = "binomial"), 
            nudif = glm(cbind(success, failure) ~ SCORES * GROUP, 
                family = "binomial"))
        m1 <- switch(type, both = glm(cbind(success, failure) ~ 
            SCORES, family = "binomial"), udif = glm(cbind(success, 
            failure) ~ SCORES, family = "binomial"), nudif = glm(cbind(success, 
            failure) ~ SCORES + GROUP, family = "binomial"))
        if (criterion=="LRT") dev[item] <- deviance(m1) - deviance(m0)
        else{
	  if (criterion != "Wald") stop("'criterion' must be either 'LRT' or Wald'",call.=FALSE)
        else{
	  coeff<-as.numeric(coefficients(m0))
        covMat<-summary(m0)$cov.scaled
        if (type=="udif") C<-rbind(c(0,0,1))
        else{
	  if (type=="nudif") C<-rbind(c(0,0,0,1))
	  else C<-rbind(c(0,0,1,0),c(0,0,0,1))
	  }
        dev[item]<-t(C%*%coeff)%*%solve(C%*%covMat%*%t(C))%*%C%*%coeff
        }
	  }
        deltaR[item] <- R2DIF(m0, nrow(data)) - R2DIF(m1, nrow(data))
        mFull[item, 1:length(m0$coefficients)] <- m0$coefficients
        mSimple[item, 1:length(m1$coefficients)] <- m1$coefficients
    }
    colnames(mFull) <- colnames(mSimple) <- c("(Intercept)", 
        "SCORE", "GROUP", "SCORE:GROUP")
    res <- list(stat = dev, deltaR2 = deltaR, parM0 = mFull, 
        parM1 = mSimple, criterion=criterion)
    return(res)
}
