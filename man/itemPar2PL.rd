\name{itemPar2PL}
\alias{itemPar2PL}

\title{Item parameter estimation for DIF detection using 2PL model}

\description{
 Fits the 2PL model and returns related item parameter estimates.
 }

\usage{
 itemPar2PL(data)
 }

\arguments{
 \item{data}{numeric: the data matrix.}
}

\value{
 A matrix with one row per item and five columns: the estimates of item discrimination \emph{a} and difficulty \emph{b} parameters, the 
 related standard errors \emph{se(a)} and \emph{se(b)}, and the covariances \emph{cov(a,b)}, in this order.
}
 
\details{
 \code{itemPar2PL} permits to get item parameter estimates from the 2PL model. The output is ordered such that it can be directly used
 with the general \code{\link{itemParEst}} command, as well as the methods of Lord (\code{\link{difLord}}) and Raju (\code{\link{difRaju}}) 
 and Generalized Lord's (\code{\link{difGenLord}}) to detect differential item functioning.

 The \code{data} is a matrix whose rows correspond to the subjects and columns to the items. Missing values are not allowed.
  
 The 2PL model is fitted using marginal maximum likelihood by means of the functions from the \code{ltm} package (Rizopoulos, 2006).
 }

\references{ 
 Rizopoulos, D. (2006). ltm: An R package for latent variable modelling and item response theory analyses. \emph{Journal of Statistical Software, 17}, 
 1-25. URL: http://www.jstatsoft.org/v17/i05/
 }

\author{
    Sebastien Beland \cr
    Centre sur les Applications des Modeles de Reponses aux Items (CAMRI) \cr
    Universite du Quebec a Montreal \cr
    \email{sebastien.beland.1@hotmail.com} \cr
    David Magis \cr
    Research Group of Quantitative Psychology and Individual Differences \cr
    Katholieke Universiteit Leuven \cr
    \email{David.Magis@psy.kuleuven.be}, \url{http://ppw.kuleuven.be/okp/home/}
    Gilles Raiche \cr
    Centre sur les Applications des Modeles de Reponses aux Items (CAMRI) \cr
    Universite du Quebec a Montreal \cr
    \email{raiche.gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/} \cr 
    }

\seealso{
 \code{\link{itemPar1PL}}, \code{\link{itemPar3PL}}, \code{\link{itemPar3PLconst}}, \code{\link{itemParEst}}, \code{\link{difLord}}, 
 \code{\link{difRaju}}, \code{\link{difGenLord}}
 }

\examples{
# Loading of the verbal data
data(verbal)

# Getting item parameter estimates 
itemPar2PL(verbal[,1:24])
}

