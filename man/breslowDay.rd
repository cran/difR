\name{breslowDay}
\alias{breslowDay}


\title{Breslow-Day DIF statistic}

\description{
 Computes Breslow-Day statistics for DIF detection. 
 }

\usage{
breslowDay(data, member, anchor=1:ncol(data))
 }

\arguments{
 \item{data}{numeric: the data matrix (one row per subject, one column per item).}
 \item{member}{numeric: the vector of group membership with zero and one entries only. See \bold{Details}.}
 \item{anchor}{a vector of integer values specifying which items (all by default) are currently considered as anchor (DIF free) items. See \bold{Details}.}
 }
 
\value{
 A matrix with one row per item and three columns: the first one contains the Breslow-Day statistic values, the second column indicates 
 the degrees of freedom, and the last column displays the asymptotic \emph{p}-values.}


\details{
 \code{breslowDay} computes the Breslow-Day statistic (1980) in the specific framework of differential item functioning. It forms the basic command 
 of \code{\link{difBD}} and is specifically designed for this call.
 
 The data are supplied by the \code{data} argument, with one row per subject and one column per item. Missing values are not allowed.
  
 The vector of group membership, specified by the \code{member} argument,�must hold only zeros and ones, a value of zero corresponding to the
 reference group and a value of one to the focal group.

 Option \code{anchor} sets the items which are considered as anchor items for computing Breslow-Day DIF statistics. Items other than the anchor items and
 the tested item are discarded. \code{anchor} must hold integer values specifying the column numbers of the corresponding anchor items. It is 
 primarily designed to perform item purification.
}

\references{
Breslow, N.E. and Day, N.E. (1980). \emph{Statistical methods in cancer research, vol. I: The analysis of case-control studies}. Scientific Publication N� 32. International Agency for Research on Cancer, Lyon, France.
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
\code{\link{difBD}}, \code{\link{dichoDif}}
 }

\examples{
# Loading of the verbal data
data(verbal)

# With all items as anchor items
breslowDay(verbal[,1:24], verbal[,26])

# Removing item 6 from the set of anchor items
breslowDay(verbal[,1:24], verbal[,26], anchor=c(1:5,7:24))
}

