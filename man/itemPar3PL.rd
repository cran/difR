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

 The \code{data} is a matrix whose rows correspond to the subjects and columns to the items. 

 Missing values are allowed but must be coded as \code{NA} values. They are discarded for item parameter estimation.
  
 The 3PL model is fitted using marginal maximum likelihood by means of the functions from the \code{ltm} package (Rizopoulos, 2006).
}

\note{
 The 3PL model is fitted under the linear parametrization in \code{tpm}, the covariance matrix is extracted with the \code{vcov()} function, and final standard errors and covariances are derived by the Delta method. See Rizopoulos (2006) for further details, and the \code{Note.pdf} document in the \code{difR} package for mathematical details.
}

\references{ 
 Magis, D., Beland, S., Tuerlinckx, F. and De Boeck, P. (2010). A general framework and an R package for the detection
 of dichotomous differential item functioning. \emph{Behavior Research Methods, 42}, 847-862.
 
 Rizopoulos, D. (2006). ltm: An R package for latent variable modelling and item response theory analyses. \emph{Journal of Statistical Software, 17}, 
 1-25. URL: http://www.jstatsoft.org/v17/i05/
}

\author{
    Sebastien Beland \cr
    Collectif pour le Developpement et les Applications en Mesure et Evaluation (Cdame) \cr
    Universite du Quebec a Montreal \cr
    \email{sebastien.beland.1@hotmail.com}, \url{http://www.cdame.uqam.ca/} \cr
    David Magis \cr
    Department of Education, University of Liege \cr
    Research Group of Quantitative Psychology and Individual Differences, KU Leuven \cr
    \email{David.Magis@ulg.ac.be}, \url{http://ppw.kuleuven.be/okp/home/} \cr
    Gilles Raiche \cr
    Collectif pour le Developpement et les Applications en Mesure et Evaluation (Cdame) \cr
    Universite du Quebec a Montreal \cr
    \email{raiche.gilles@uqam.ca}, \url{http://www.cdame.uqam.ca/} \cr 
 }

     
\seealso{
 \code{\link{itemPar1PL}}, \code{\link{itemPar2PL}}, \code{\link{itemPar3PLconst}}, \code{\link{itemParEst}}, \code{\link{difLord}}, \code{\link{difRaju}}, 

 \code{\link{difGenLord}}
 }

\examples{
\dontrun{

 # Loading of the verbal data
 data(verbal)

 # Getting item parameter estimates
 itemPar3PL(verbal[,1:24])
 }
 }
