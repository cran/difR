\name{genMantelHaenszel}
\alias{genMantelHaenszel}

\title{Generalized Mantel-Haenszel DIF statistic}

\description{
 Calculates the generalized Mantel-Haenszel statistics for DIF detection among multiple groups. 
}

\usage{
 genMantelHaenszel(data, member, anchor=1:ncol(data))
}

\arguments{
 \item{data}{numeric: the data matrix (one row per subject, one column per item).}
 \item{member}{numeric: the vector of group membership with zero and positive integer entries only. See \bold{Details}.}
 \item{anchor}{a vector of integer values specifying which items (all by default) are currently considered as anchor (DIF free) items. See \bold{Details}.}
}

\value{
A vector with the values of the generalized Mantel-Haenszel DIF statistics.
 }
 
\details{
 This command computes the generalized Mantel-Haenszel statistic (Somes, 1986) in the specific framework of differential item functioning. It forms the basic command
 of \code{\link{difGMH}} and is specifically designed for this call.

 The data are passed through the \code{data} argument, with one row per subject and one column per item. Missing values are not allowed.

 The vector of group membership, specified with \code{member} argument, must hold only zeros and positive integers. The value zero corresponds to the reference group,
 and each positive integer value corresponds to one focal group. At least two different positive integers must be supplied.

 Option \code{anchor} sets the items which are considered as anchor items for computing generalized Mantel-Haenszel statistics. Items other than the anchor items and
 the tested item are discarded. \code{anchor} must hold integer values specifying the column numbers of the corresponding anchor items. It is primarily designed to perform 
 item purification.
}

\references{
 Penfield, R. D. (2001). Assessing differential item functioning among multiple groups: a comparison of three Mantel-Haenszel procedures.
 \emph{Applied Measurement in Education, 14}, 235-259.

 Somes, G. W. (1986). The generalized Mantel-Haenszel statistic. \emph{The American Statistician, 40}, 106-108.
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
 \code{\link{difGMH}}
}

\examples{
# Loading of the verbal data
data(verbal)
attach(verbal)

# Creating four groups according to gender (0 or 1) and trait anger score
# ("Low" or "High")
# Reference group: women with low trait anger score (<=20)

group<-rep(0,nrow(verbal))
group[Anger>20 & Gender==0]<-1
group[Anger<=20 & Gender==1]<-2
group[Anger>20 & Gender==1]<-3

# Without continuity correction
genMantelHaenszel(verbal[,1:24], group)

# Removing item 6 from the set of anchor items
genMantelHaenszel(verbal[,1:24], group, anchor=c(1:5,7:24))
}




