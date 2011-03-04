\name{RajuZ}
\alias{RajuZ}

\title{Raju's area DIF statistic}

\description{
 Calculates the Raju's statistics for DIF detection. 
 }

\usage{
RajuZ(mR, mF)
 }

\arguments{
 \item{mR}{numeric: the matrix of item parameter estimates (one row per item) for the reference group. See \bold{Details}.}
 \item{mF}{numeric: the matrix of item parameter estimates (one row per item) for the focal group. See \bold{Details}.}
}

\value{
 A matrix with one row per item and three columns, holding respectively Raju's area between the two item characteristic curves, its 
 standard error and the Raju DIF statistic (the latter being the ratio of the first two columns).
}
 
\details{
 This command computes the Raju's area statistic (Raju, 1988, 1990) in the specific framework of differential item functioning. It forms the basic command 
 of \code{\link{difRaju}} and is specifically designed for this call.

 The matrices \code{mR} and \code{mF} must have the same format as the output of the command \code{\link{itemParEst}} and one the possible models (1PL, 2PL
 or constrained 3PL). The number of columns therefore equals two, five or six, respectively. Note that the unconstrained 3PL model cannot be used in this 
 method: all pseudo-guessing parameters must be equal in both groups of subjects. Moreover, item parameters of the focal must be on the  same scale of that 
 of the reference group. If not, make use of e.g. equal means anchoring (Cook and Eignor, 1991) and \code{\link{itemRescale}} to transform them adequately. 
}

\references{
 Cook, L. L. and Eignor, D. R. (1991). An NCME instructional module on IRT equating methods. \emph{Educational Measurement: Issues and Practice, 10}, 37-45.
 
 Magis, D., Beland, S., Tuerlinckx, F. and De Boeck, P. (2010). A general framework and an R package for the detection
 of dichotomous differential item functioning. \emph{Behavior Research Methods, 42}, 847-862.

 Raju, N.S. (1988). The area between two item characteristic curves. \emph{Psychometrika, 53}, 495-502. 

 Raju, N. S. (1990). Determining the significance of estimated signed and unsigned areas between two item response functions. \emph{Applied Psychological Measurement, 14}, 197-207.
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
 \code{\link{itemParEst}}, \code{\link{itemRescale}}, \code{\link{difRaju}}, \code{\link{dichoDif}}
}

\examples{
\dontrun{

 # Loading of the verbal data
 data(verbal)
 attach(verbal)

 # Splitting the data into reference and focal groups
 nF<-sum(Gender)
 nR<-nrow(verbal)-nF
 data.ref<-verbal[,1:24][order(Gender),][1:nR,]
 data.focal<-verbal[,1:24][order(Gender),][(nR+1):(nR+nF),]

 # Pre-estimation of the item parameters (1PL model)
 mR<-itemParEst(data.ref,model="1PL")
 mF<-itemParEst(data.focal,model="1PL")
 mF<-itemRescale(mR, mF)
 RajuZ(mR, mF)

 # Pre-estimation of the item parameters (2PL model)
 mR<-itemParEst(data.ref,model="2PL")
 mF<-itemParEst(data.focal,model="2PL")
 mF<-itemRescale(mR, mF)
 RajuZ(mR, mF)

 # Pre-estimation of the item parameters (constrained 3PL model)
 mR<-itemParEst(data.ref,model="3PL",c=0.05)
 mF<-itemParEst(data.focal,model="3PL",c=0.05)
 mF<-itemRescale(mR, mF)
 RajuZ(mR, mF)
 }
 }
