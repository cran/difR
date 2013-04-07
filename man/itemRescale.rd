\name{itemRescale}
\alias{itemRescale}

\title{Rescaling item parameters by equal means anchoring}

\description{
 Rescale the item parameters from one data set to the scale of the parameters from another data set, using equal means anchoring.}

\usage{
itemRescale(mR, mF, items=1:nrow(mR))
 }

\arguments{
 \item{mR}{numeric: a matrix of item parameter estimates (one row per item) which constitutes the reference scale. See \bold{Details}.}
 \item{mF}{numeric: a matrix of item parameter estimates (one row per item) which have to be rescaled. See \bold{Details}.}
 \item{items}{a numeric vector of integer values specifying which items are used for equal means anchoring. See \bold{Details}.}
 }

\value{
 A matrix of the same format as \code{mF} with the rescaled item parameters.
}
 
\details{
 The matrices \code{mR} and \code{mF} must have the same format as the output of the command \code{\link{itemParEst}} and one the possible models (1PL, 2PL,
 3PL or constrained 3PL). The number of columns therefore equals two, five, nine or six, respectively.

 Rescaling is performed by equal means anchoring (Cook and Eignor, 1991). The items involved in the anchoring process are specified by means of their row
 number in either \code{mR} or \code{mF}, and are passed through the \code{items} argument.

 \code{itemRescale} primarily serves as a routine for item purification in Lord (\code{\link{difLord}}) and Raju (\code{\link{difRaju}}) 
 Generalized Lord's (\code{\link{difGenLord}}) methods of DIF identification (Candell and Drasgow, 1988).
}

\references{
 Candell, G.L. and Drasgow, F. (1988). An iterative procedure for linking metrics and assessing item bias in item response theory. 
 \emph{Applied Psychological Measurement, 12}, 253-260. 

 Cook, L. L. and Eignor, D. R. (1991). An NCME instructional module on IRT equating methods. \emph{Educational Measurement: Issues and Practice, 10}, 37-45.

 Magis, D., Beland, S., Tuerlinckx, F. and De Boeck, P. (2010). A general framework and an R package for the detection
 of dichotomous differential item functioning. \emph{Behavior Research Methods, 42}, 847-862.
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
    \email{raiche.gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/} \cr 
 }


\seealso{
 \code{\link{itemPar1PL}}, \code{\link{itemPar2PL}},  \code{\link{itemPar3PL}}, \code{\link{itemPar3PLconst}}, \code{\link{difLord}}, \code{\link{difRaju}}, 

 \code{\link{difGenLord}} 
}

\examples{
\dontrun{

 # Loading of the verbal data
 data(verbal)
 attach(verbal)

 # Splitting the data set into reference and focal groups
 nF<-sum(Gender)
 nR<-nrow(verbal)-nF
 data.ref<-verbal[,1:24][order(Gender),][1:nR,]
 data.focal<-verbal[,1:24][order(Gender),][(nR+1):(nR+nF),]

 # Estimating item parameters in each data set with 1PL model
 mR<-itemPar1PL(data.ref)
 mF<-itemPar1PL(data.focal)

 # Rescaling focal group item parameters, using all items for anchoring
 itemRescale(mR, mF)

 # Rescaling focal group item parameters, using the first 10 items for
 # anchoring
 itemRescale(mR, mF, items=1:10)

 # Estimating item parameters in each data set with 2PL model
 mR<-itemPar2PL(data.ref)
 mF<-itemPar2PL(data.focal)

 # Rescaling focal group item parameters, using all items for anchoring
 itemRescale(mR, mF)
 }
 }
