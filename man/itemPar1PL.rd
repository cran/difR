\name{itemPar1PL}
\alias{itemPar1PL}

\title{Item parameter estimation for DIF detection using Rasch (1PL) model}

\description{
 Fits the Rasch (1PL) model and returns related item parameter estimates.
 }

\usage{
 itemPar1PL(data, engine="ltm")
 }

\arguments{
 \item{data}{numeric: the data matrix.}
 \item{engine}{character: the engine for estimating the 1PL model, either \code{"ltm"} (default) or \code{"lme4"}.}
 }

\value{
 A matrix with one row per item and two columns, the first one with item parameter estimates and the second one with the related standard errors.
 }
 
\details{
 \code{itemPar1PL} permits to get item parameter estimates from the Rasch or 1PL model. The output is ordered such that it can be directly used
 with the general \code{\link{itemParEst}} command, as well as the methods of Lord (\code{\link{difLord}}) and Raju (\code{\link{difRaju}}) and
 Generalized Lord's (\code{\link{difGenLord}}) to detect differential item functioning.

 The \code{data} is a matrix whose rows correspond to the subjects and columns to the items. Missing values are not allowed.
  
 The estimation engine is set by the \code{engine} argument. By default (\code{engine="ltm"}), the Rasch model is fitted using marginal maximum likelihood, by means of 
 the function \code{rasch} from the \code{ltm} package (Rizopoulos, 2006). The other option, \code{engine="lme4"}, permits to fit the Rasch model as a generalized 
 linear mixed model, by means of the \code{glmer} function of the \code{lme4} package (Bates and Maechler, 2009).
 }

\references{ 
 Bates, D. and Maechler, M. (2009). lme4: Linear mixed-effects models using S4 classes. R package version 0.999375-31. http://CRAN.R-project.org/package=lme4
 
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
 \code{\link{itemPar2PL}}, \code{\link{itemPar3PL}}, \code{\link{itemPar3PLconst}}, \code{\link{itemParEst}}, \code{\link{difLord}}, 
 \code{\link{difRaju}}, \code{\link{difGenLord}}
 }

\examples{
# Loading of the verbal data
data(verbal)
 
# Getting item parameter estimates ('ltm' engine)
itemPar1PL(verbal[,1:24])

# Getting item parameter estimates ('lme4' engine)
itemPar1PL(verbal[,1:24], engine="lme4")
}

