# LOGISTIC REGRESSION
difLogistic<-function (Data, group, focal.name, type = "both", criterion = "LRT", 
    alpha = 0.05, purify = FALSE, nrIter = 10, save.output = FALSE, 
    output = c("out", "default")) 
{
    internalLog <- function() {
        if (length(group) == 1) {
            if (is.numeric(group) == TRUE) {
                gr <- Data[, group]
                DATA <- Data[, (1:ncol(Data)) != group]
                colnames(DATA) <- colnames(Data)[(1:ncol(Data)) != 
                  group]
            }
            else {
                gr <- Data[, colnames(Data) == group]
                DATA <- Data[, colnames(Data) != group]
                colnames(DATA) <- colnames(Data)[colnames(Data) != 
                  group]
            }
        }
        else {
            gr <- group
            DATA <- Data
        }
        Group <- rep(0, nrow(DATA))
        Group[gr == focal.name] <- 1
        Q <- switch(type, both = qchisq(1 - alpha, 2), udif = qchisq(1 - 
            alpha, 1), nudif = qchisq(1 - alpha, 1))
        if (purify == FALSE) {
            PROV <- Logistik(DATA, Group, type = type, criterion = criterion)
            STATS <- PROV$stat
            deltaR2 <- PROV$deltaR2
            if (max(STATS) <= Q) {
                DIFitems <- "No DIF item detected"
                logitPar <- PROV$parM1
            }
            else {
                DIFitems <- (1:ncol(DATA))[STATS > Q]
                logitPar <- PROV$parM1
                for (idif in 1:length(DIFitems)) logitPar[DIFitems[idif], 
                  ] <- PROV$parM0[DIFitems[idif], ]
            }
            RES <- list(Logistik = STATS, logitPar = logitPar, parM0 = PROV$parM0,
                deltaR2 = deltaR2, alpha = alpha, thr = Q, DIFitems = DIFitems, 
                type = type, purification = purify, names = colnames(DATA), 
                criterion = criterion, save.output = save.output, 
                output = output)
        }
        else {
            nrPur <- 0
            difPur <- NULL
            noLoop <- FALSE
            prov1 <- Logistik(DATA, Group, type = type, criterion = criterion)
            stats1 <- prov1$stat
            deltaR2 <- prov1$deltaR2
            if (max(stats1) <= Q) {
                DIFitems <- "No DIF item detected"
                logitPar <- prov1$parM1
                noLoop <- TRUE
            }
            else {
                dif <- (1:ncol(DATA))[stats1 > Q]
                difPur <- rep(0, length(stats1))
                difPur[dif] <- 1
                repeat {
                  if (nrPur >= nrIter) 
                    break
                  else {
                    nrPur <- nrPur + 1
                    nodif <- NULL
                    if (is.null(dif) == TRUE) 
                      nodif <- 1:ncol(DATA)
                    else {
                      for (i in 1:ncol(DATA)) {
                        if (sum(i == dif) == 0) 
                          nodif <- c(nodif, i)
                      }
                    }
                    prov2 <- Logistik(DATA, Group, anchor = nodif, 
                      type = type, criterion = criterion)
                    stats2 <- prov2$stat
                    deltaR2 <- prov2$deltaR2
                    if (max(stats2) <= Q) 
                      dif2 <- NULL
                    else dif2 <- (1:ncol(DATA))[stats2 > Q]
                    difPur <- rbind(difPur, rep(0, ncol(DATA)))
                    difPur[nrPur + 1, dif2] <- 1
                    if (length(dif) != length(dif2)) 
                      dif <- dif2
                    else {
                      dif <- sort(dif)
                      dif2 <- sort(dif2)
                      if (sum(dif == dif2) == length(dif)) {
                        noLoop <- TRUE
                        break
                      }
                      else dif <- dif2
                    }
                  }
                }
                prov1 <- prov2
                stats1 <- stats2
                deltaR2 <- deltaR2
                DIFitems <- (1:ncol(DATA))[stats1 > Q]
                logitPar <- prov1$parM1
                for (idif in 1:length(DIFitems)) logitPar[DIFitems[idif], 
                  ] <- prov1$parM0[DIFitems[idif], ]
            }
            if (is.null(difPur) == FALSE) {
                ro <- co <- NULL
                for (ir in 1:nrow(difPur)) ro[ir] <- paste("Step", 
                  ir - 1, sep = "")
                for (ic in 1:ncol(difPur)) co[ic] <- paste("Item", 
                  ic, sep = "")
                rownames(difPur) <- ro
                colnames(difPur) <- co
            }
            RES <- list(Logistik = stats1, logitPar = logitPar, parM0 = prov1$parM0,
                deltaR2 = deltaR2, alpha = alpha, thr = Q, DIFitems = DIFitems, 
                type = type, purification = purify, nrPur = nrPur, 
                difPur = difPur, convergence = noLoop, names = colnames(DATA), 
                criterion = criterion, save.output = save.output, 
                output = output)
        }
        class(RES) <- "Logistic"
        return(RES)
    }
    resToReturn <- internalLog()
    if (save.output == TRUE) {
        if (output[2] == "default") 
            wd <- paste(getwd(), "/", sep = "")
        else wd <- output[2]
        fileName <- paste(wd, output[1], ".txt", sep = "")
        capture.output(resToReturn, file = fileName)
    }
    return(resToReturn)
}




# METHODS
plot.Logistic<-function (x, plot = "lrStat", item = 1, itemFit="best", pch = 8, number = TRUE, 
    col = "red", colIC = rep("black", 2), ltyIC = c(1, 2), save.plot = FALSE, 
    save.options = c("plot", "default", "pdf"), group.names=NULL, ...) 
{
    internalLog <- function() {
        res <- x
        plotType <- switch(plot, lrStat = 1, itemCurve = 2)
        if (is.null(plotType) == TRUE) 
            return("Error: misspecified 'type' argument")
        else {
            if (plotType == 1) {
                if (number == FALSE) {
                  plot(res$Logistik, xlab = "Item", ylab = paste(x$criterion, 
                    " statistic", sep = ""), ylim = c(0, max(c(res$Logistik, 
                    res$thr) + 1)), pch = pch, main = paste("Logistic regression (", 
                    x$criterion, "statistic)", sep = ""))
                  if (is.character(res$DIFitems) == FALSE) 
                    points(res$DIFitems, res$Logistik[res$DIFitems], 
                      pch = pch, col = col)
                }
                else {
                  plot(res$Logistik, xlab = "Item", ylab = paste(x$criterion, 
                    " statistic", sep = ""), ylim = c(0, max(c(res$Logistik, 
                    res$thr) + 1)), col = "white", main = paste("Logistic regression (", 
                    x$criterion, " statistic)", sep = ""))
                  text(1:length(res$Logistik), res$Logistik, 
                    1:length(res$Logistik))
                  if (is.character(res$DIFitems) == FALSE) 
                    text(res$DIFitems, res$Logistik[res$DIFitems], 
                      res$DIFitems, col = col)
                }
                abline(h = res$thr)
            }
            else {
                it <- ifelse(is.character(item) | is.factor(item), 
                  (1:length(res$names))[res$names == item], item)
                if (itemFit=="best") logitPar <- res$logitPar[it, ]
                else logitPar <- res$parM0[it,]
                s <- seq(0, length(res$Logistik), 0.1)
                expit <- function(t) exp(t)/(1 + exp(t))
                mainName <- ifelse(is.character(res$names[it]), 
                  res$names[it], paste("Item ", it, sep = ""))
                plot(s, expit(logitPar[1] + logitPar[2] * s), 
                  col = colIC[1], type = "l", lty = ltyIC[1], 
                  ylim = c(0, 1), xlab = "Score", ylab = "Probability", 
                  main = mainName)
                if (itemFit == "null" | (itemFit=="best" & 
                  is.character(res$DIFitems) == FALSE & sum(res$DIFitems == 
                  it) == 1)) {
                  lines(s, expit(logitPar[1] + logitPar[2] * 
                    s + logitPar[3] + logitPar[4] * s), col = colIC[2], 
                    lty = ltyIC[2])
                if (is.null(group.names)) legnames<-c("Reference", "Focal")
                else legnames<-group.names
                  legend(0, 1, legnames, col = colIC, 
                    lty = ltyIC, bty = "n")
                }
            }
        }
    }
    internalLog()
    if (save.plot == TRUE) {
        plotype <- NULL
        if (save.options[3] == "pdf") 
            plotype <- 1
        if (save.options[3] == "jpeg") 
            plotype <- 2
        if (is.null(plotype) == TRUE) 
            cat("Invalid plot type (should be either 'pdf' or 'jpeg').", 
                "\n", "The plot was not captured!", "\n")
        else {
            if (save.options[2] == "default") 
                wd <- paste(getwd(), "/", sep = "")
            else wd <- save.options[2]
            fileName <- paste(wd, save.options[1], switch(plotype, 
                `1` = ".pdf", `2` = ".jpg"), sep = "")
            if (plotype == 1) {
                {
                  pdf(file = fileName)
                  internalLog()
                }
                dev.off()
            }
            if (plotype == 2) {
                {
                  jpeg(file = fileName)
                  internalLog()
                }
                dev.off()
            }
            cat("The plot was captured and saved into", "\n", 
                " '", fileName, "'", "\n", "\n", sep = "")
        }
    }
    else cat("The plot was not captured!", "\n", sep = "")
}





print.Logistic <- function (x, ...) 
{
    res <- x
    cat("\n")
    mess1 <- switch(res$type,
			  both = " both types of ",
			  nudif = " nonuniform ",
			  udif = " uniform ")
    cat("Detection of", mess1, "Differential Item Functioning", "\n", 
    "using Logistic regression method, ", sep = "")
    if (res$purification == TRUE) 
        pur <- "with "
    else pur <- "without "
    cat(pur, "item purification", "\n", sep = "")
    cat("and with ",res$criterion," DIF statistic","\n", "\n", sep = "")
    if (res$purification == TRUE) {
        if (res$nrPur <= 1) 
            word <- " iteration"
        else word <- " iterations"
        if (res$convergence == FALSE) {
            cat("WARNING: no item purification convergence after ", 
                res$nrPur, word, "\n", sep = "")
            loop <- NULL
            for (i in 1:res$nrPur) loop[i] <- sum(res$difPur[1, 
                ] == res$difPur[i + 1, ])
            if (max(loop) != length(res$Logistik)) 
                cat("(Note: no loop detected in less than ", 
                  res$nrPur, word, ")", "\n", sep = "")
            else cat("(Note: loop of length ", min((1:res$nrPur)[loop == 
                length(res$Logistik)]), " in the item purification process)", 
                "\n", sep = "")
            cat("WARNING: following results based on the last iteration of the purification", 
                "\n", "\n")
        }
        else cat("Convergence reached after ", res$nrPur, word, 
            "\n", "\n", sep = "")
    }
    cat("Logistic regression DIF statistic:", "\n", "\n")
    df <- switch(res$type, both = 2, udif = 1, nudif = 1)
    pval <- round(1 - pchisq(res$Logistik, df), 4)
    symb <- symnum(pval, c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", 
        "**", "*", ".", ""))
    m1 <- cbind(round(res$Logistik, 4), pval)
    m1 <- noquote(cbind(format(m1, justify = "right"), symb))
    if (is.null(res$names) == FALSE) 
        rownames(m1) <- res$names
    else {
        rn <- NULL
        for (i in 1:nrow(m1)) rn[i] <- paste("Item", i, sep = "")
        rownames(m1) <- rn
    }
    colnames(m1) <- c("Stat.", "P-value", "")
    print(m1)
    cat("\n")
    cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 ", 
        "\n")
    cat("\n", "Detection threshold: ", round(res$thr, 4), " (significance level: ", 
        res$alpha, ")", "\n", "\n", sep = "")
    if (is.character(res$DIFitems) == TRUE) 
        cat("Items detected as DIF items:", res$DIFitems, "\n", 
            "\n")
    else {
	  mess2 <- switch(res$type,
			  both = " ",
			  nudif = " nonuniform ",
			  udif = " uniform ")
        cat("Items detected as", mess2, "DIF items:", "\n", sep = "")
        m2 <- cbind(rownames(m1)[res$DIFitems])
        rownames(m2) <- rep("", nrow(m2))
        colnames(m2) <- ""
        print(m2, quote = FALSE)
        cat("\n", "\n")
    }
    cat("Effect size (Nagelkerke's R^2):", "\n", "\n")
    cat("Effect size code:", "\n")
    cat(" 'A': negligible effect", "\n")
    cat(" 'B': moderate effect", "\n")
    cat(" 'C': large effect", "\n", "\n")
    r2 <- round(res$deltaR2,4)
    symb1 <- symnum(r2, c(0, 0.13, 0.26, 1), symbols = c("A", 
        "B", "C"))
    symb2 <- symnum(r2, c(0, 0.035, 0.07, 1), symbols = c("A", 
        "B", "C"))
    matR2<-cbind(r2)
    matR2<- noquote(cbind(format(r2, justify="right"), symb1, symb2))
    if (is.null(res$names) == FALSE) 
        rownames(matR2) <- res$names
    else {
        rn <- NULL
        for (i in 1:nrow(matR2)) rn[i] <- paste("Item", i, sep = "")
        rownames(matR2) <- rn
    }
    colnames(matR2) <- c("R^2", "ZT", "JG")
    print(matR2)
    cat("\n")
    cat("Effect size codes:", "\n")
    cat(" Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1","\n")
    cat(" Jodoign & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1","\n")
if (x$save.output==FALSE) cat("\n","Output was not captured!","\n")
    else {
if (x$output[2]=="default") wd<-paste(getwd(),"/",sep="")
else wd<-x$output[2]
fileName<-paste(wd,x$output[1],".txt",sep="")
cat("\n","Output was captured and saved into file","\n"," '",fileName,"'","\n","\n",sep="")
}
}

