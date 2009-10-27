\name{stdPDIF}
\alias{stdPDIF}

\title{Standardization DIF statistic}

\description{
  Calculates standardized P-difference statistics for DIF detection. 
 }

\usage{
 stdPDIF(data, member, anchor=1:ncol(data))
 }
 
\arguments{
 \item{data}{numeric: the data matrix (one row per subject, one column per item).}
 \item{member}{numeric: the vector of group membership with zero and one entries only. See \bold{Details}.}
 \item{anchor}{a vector of integer values specifying which items (all by default) are currently considered as anchor (DIF free) items. See \bold{Details}.}
}

\value{
A vector with the values of the standardization DIF statistics.
 }
 
\details{
 This command computes the standardization statistic in the specific framework of differential item functioning (Dorans and Kullick, 1986). It forms the basic command 
 of \code{\link{difStd}} and is specifically designed for this call.
 
 The data are passed through the \code{data} argument, with one row per subject and one column per item. Missing values are not allowed.
  
 The vector of group membership, specified with \code{member} argument, must hold only zeros and ones, a value of zero corresponding to the
 reference group and a value of one to the focal group.
 
 Option \code{anchor} sets the items which are considered as anchor items for computing standardized P-DIF statistics. Items other than the anchor items and the tested item 
 are discarded. \code{anchor} must hold integer values specifying the column numbers of the corresponding anchor items. It is mainly designed to perform item purification.
}

\references{
 Dorans, N. J. and Kullick, E. (1986). Demonstrating the utility of the standardization approach to assessing unexpected differential item performance on the Scholastic Aptitude Test. \emph{Journal of Educational Measurement, 23}, 355-368.
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
 \code{\link{difStd}}, \code{\link{dichoDif}}
 }

\examples{
# Loading of the verbal data
data(verbal)

# All items as anchor items
stdPDIF(verbal[,1:24], verbal[,26])

# Removing item 6 from the set of anchor items
stdPDIF(verbal[,1:24], verbal[,26], anchor=c(1:5,7:24))
}
