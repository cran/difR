\name{itemPar3PL}
\alias{itemPar3PL}

\title{Item parameter estimation for DIF detection using 3PL model}

\description{
 Fits the 3PL model and returns related item parameter estimates.
}

\usage{
 itemPar3PL(data)
}

\arguments{
 \item{data}{numeric: the data matrix.}
}

\value{
 A matrix with one row per item and nine columns. See \bold{Details}.}
 
\details{
 \code{itemPar3PL} permits to get item parameter estimates from the 3PL model. The output is ordered such that it can be directly used
 with the general \code{\link{itemParEst}} command, as well as the methods of Lord (\code{\link{difLord}}) and Raju (\code{\link{difRaju}}) 
 and Generalized Lord's (\code{\link{difGenLord}}) to detect differential item functioning. 

 The output consists of nine columns which are displayed in the following order. The first three columns hold the estimates of item discrimination \emph{a},
 difficulty \emph{b} and pseudo-guessing \emph{c} parameters. In the next three columns one can find the related standard errors \emph{se(a)}, \emph{se(b)} 
 and \emph{se(c)}. Eventually, the last three columns contain the covariances between item parameters, respectively \emph{cov(a,b)}, \emph{cov(a,c)} and 
 \emph{cov(b,c)}.

 The \code{data} is a matrix whose rows correspond to the subjects and columns to the items. Missing values are not allowed.
  
 The 3PL model is fitted using marginal maximum likelihood by means of the functions from the \code{ltm} package (Rizopoulos, 2006).
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
    \email{David.Magis@psy.kuleuven.be}, \url{http://ppw.kuleuven.be/okp/home/} \cr
    Gilles Raiche \cr
    Centre sur les Applications des Modeles de Reponses aux Items (CAMRI) \cr
    Universite du Quebec a Montreal \cr
    \email{raiche.gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/} \cr 
    }
     
\seealso{
 \code{\link{itemPar1PL}}, \code{\link{itemPar2PL}}, \code{\link{itemPar3PLconst}}, \code{\link{itemParEst}}, \code{\link{difLord}}, 
 \code{\link{difRaju}}, \code{\link{difGenLord}}
 }

\examples{
 \dontrun{
 # Loading of the verbal data
 data(verbal)

 # Getting item parameter estimates
 # itemPar3PL(verbal[,1:24])
 }
 }
