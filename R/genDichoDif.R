# DIF: COMPARING DIF STATISTICS

genDichoDif <- function (Data, group, focal.names, method, type=  "both",
    alpha = 0.05, model = "2PL", c = NULL, engine="ltm", irtParam = NULL, 
    nrFocal = 2, same.scale = TRUE, purify = FALSE, nrIter = 10) 
{
    mets <- c("GMH", "genLogistic", "genLord")
    prov.met <- rep(0, length(method))
    for (i in 1:length(method)) {
        if (sum(method[i] == mets) == 1) 
            prov.met[i] <- 1
    }
    if (min(prov.met) == 0) {
        ind <- min((1:length(method))[prov.met == 0])
        RES <- list(NULL, method[ind])
        class(RES) <- "genDichoDif"
        return(RES)
    }
    else {
        if (length(method) == 1) 
            return(selectGenDif(Data = Data, group = group, focal.names = focal.names, 
                method = method, alpha = alpha, model = model, c = c, irtParam = irtParam, 
                same.scale = same.scale, purify = purify, nrIter = nrIter))
        else {
            mat <- iters <- conv <- NULL
            for (met in 1:length(method)) {
                prov <- selectGenDif(Data = Data, group = group, 
                  focal.names = focal.names, method = method[met], 
                  alpha = alpha, model = model, c = c, irtParam = irtParam, 
                  same.scale = same.scale, purify = purify, nrIter = nrIter)
                mat <- cbind(mat, rep("NoDIF", length(prov[[1]])))
                if (is.character(prov$DIFitems) == FALSE) 
                  mat[prov$DIFitems, met] <- "DIF"
                rname <- prov$names
                if (purify == TRUE) {
                  iters <- c(iters, prov$nrPur)
                  conv <- c(conv, prov$convergence)
                }
            }
            method2 <- method
            method2[method == "GMH"] <- "M.-H."
            method2[method == "genLogistic"] <- "Logistic"
            method2[method == "genLord"] <- "Lord"
            colnames(mat) <- method2
            if (is.null(rname) == FALSE) 
                rownames(mat) <- rname
            else {
                rname <- NULL
                for (i in 1:nrow(mat)) rname <- c(rname, paste("Item", 
                  i, sep = ""))
                rownames(mat) <- rname
            }
            RES <- list(DIF = mat, alpha = alpha, method = method,
                type = type, model = model, c = c, irtParam = irtParam, 
		    same.scale = same.scale, purification = purify, nrPur = iters, 
                convergence = conv)
            class(RES) <- "genDichoDif"
            return(RES)
        }
    }
}



# METHODS
print.genDichoDif<-function(x,...){
res <- x
if (is.null(res[[1]])==TRUE) cat("Error: '",res[[2]],"' is not a correct method!","\n","\n",sep="")
else{
cat("Comparison of DIF detection among multiple groups, using",ncol(res$DIF),"methods","\n","\n")
methods<-colnames(res$DIF)
methods2<-methods
methods2[methods=="M.-H."]<-"Mantel-Haenszel"
methods2[methods=="Logistic"]<-"Logistic regression"
methods2[methods=="Lord"]<-"Lord's chi-square test"
met<-methods2[1]
for (i in 2:min(c(2,length(methods)))) met<-paste(met,methods2[i],sep=", ")
if (length(methods)<=2) cat("Methods used:",met,"\n")
else cat("Generalized methods used: ",met,",","\n",sep="")
if (length(methods)>2){
met2<-methods2[3]
if (length(methods)>3){
for (i in 4:length(methods)) met2<-paste(met2,methods2[i],sep=", ")
}
cat(met2,"\n")
}
cat("\n")
cat("Parameters:","\n")
cat("Significance level: ",res$alpha,"\n",sep="")
if (sum(methods=="genLogistic")==1){
if (res$type=="both") cat("DIF effects tested by logistic regression: ",
				   res$type," effects","\n",sep="")
else cat("DIF effects tested by logistic regression: ",res$type,
         " DIF effect","\n",sep="")
}
if (sum(methods=="genLord")==1) cat("Item response model:",res$model,"\n")
if (res$purification==TRUE) {
cat("Item purification: Yes","\n","\n")
cat("Item purification results:","\n","\n")
co<-rep("Yes",length(res$convergence))
co[res$convergence==FALSE]<-"No"
resConv<-data.frame(rbind(co,res$nrPur))
colnames(resConv)<-colnames(res$DIF)
rownames(resConv)<-c("Convergence","Iterations")
print(format(resConv,justify="centre"))
cat("\n")
}
else cat("Item purification: No","\n","\n")
cat("Comparison of DIF detection results:","\n","\n")
nr<-NULL
for (i in 1:nrow(res$DIF)) nr[i]<-paste(length(res$DIF[i,][res$DIF[i,]=="DIF"]),"/",ncol(res$DIF),sep="")
MAT<-cbind(res$DIF,nr)
colnames(MAT)[ncol(MAT)]<-"#DIF"
print(format(MAT,justify="centre"),quote=FALSE)
}
}

