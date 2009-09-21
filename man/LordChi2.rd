\name{LordChi2}
\alias{LordChi2}

\title{Lord's chi-square DIF statistic}

\description{
 Calculates the Lord's chi-square statistics for DIF detection. 
 }

\usage{
 LordChi2(mR, mF)
 }

\arguments{
 \item{mR}{numeric: the matrix of item parameter estimates (one row per item) for the reference group. See \bold{Details}.}
 \item{mF}{numeric: the matrix of item parameter estimates (one row per item) for the focal group. See \bold{Details}.}
}

\value{
 A vector with the values of the Lord's chi-square DIF statistics.
 }
 
\details{
 This command computes the Lord's chi-square statistic (Lord, 1980) in the specific framework of differential item functioning. It forms the basic command 
 of \code{\link{difLord}} and is specifically designed for this call.

 The matrices \code{mR} and \code{mF} must have the same format as the output of the command \code{\link{itemParEst}} with one the possible models (1PL, 2PL,
 3PL or constrained 3PL). The number of columns therefore equals two, five, nine or six, respectively. Moreover, item parameters of the focal must be on the
 same scale of that of the reference group. If not, make use of e.g. equal means anchoring (Cook and Eignor, 1991) and \code{\link{itemRescale}} to transform 
 them adequately. 
}

\references{
 Cook, L. L. and Eignor, D. R. (1991). An NCME instructional module on IRT equating methods. \emph{Educational Measurement: Issues and Practice, 10}, 37-45.
 
 Lord, F. (1980). \emph{Applications of item response theory to practical testing problems}. Hillsdale, NJ: Lawrence Erlbaum Associates. 
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
 \code{\link{itemParEst}}, \code{\link{itemRescale}}, \code{\link{difLord}}, \code{\link{dichoDif}}
}

\examples{
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
LordChi2(mR, mF)

# Pre-estimation of the item parameters (2PL model)
mR<-itemParEst(data.ref,model="2PL")
mF<-itemParEst(data.focal,model="2PL")
mF<-itemRescale(mR, mF)
LordChi2(mR, mF)

# Pre-estimation of the item parameters (constrained 3PL model)
mR<-itemParEst(data.ref,model="3PL",c=0.05)
mF<-itemParEst(data.focal,model="3PL",c=0.05)
mF<-itemRescale(mR, mF)
LordChi2(mR, mF)
}
