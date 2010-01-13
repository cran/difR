\name{difGMH}
\alias{difGMH}
\alias{print.GMH}
\alias{plot.GMH}

\title{Generalized Mantel-Haenszel DIF method}

\description{
 Performs DIF detection among multiple groups using the generalized Mantel-Haenszel method. 
}

\usage{
 difGMH(Data, group, focal.names, alpha=0.05, purify=FALSE, 
 nrIter=10)
 \method{print}{GMH}(x, ...)
 \method{plot}{GMH}(x, pch=8, number=TRUE, col="red", ...)
}

\arguments{
 \item{Data}{numeric: either the data matrix only, or the data matrix plus the vector of group membership. See \bold{Details}.}
 \item{group}{numeric or character: either the vector of group membership or the column indicator (within \code{Data}) of group membership. See \bold{Details}.}
 \item{focal.names}{numeric or character vector indicating the levels of \code{group} which correspond to the focal groups.}
 \item{alpha}{numeric: significance level (default is 0.05).}
 \item{purify}{logical: should the method be used iteratively to purify the set of anchor items? (default is FALSE).}
 \item{nrIter}{numeric: the maximal number of iterations in the item purification process. Default is 10.}
 \item{x}{the result from a \code{GMH} class object.}
 \item{pch, col}{type of usual \code{pch} and \code{col} graphical options.}
 \item{number}{logical: should the item number identification be printed (default is \code{TRUE}).}
 \item{...}{other generic parameters for the \code{plot} or the \code{print} functions.}
}

\value{
 A list of class "GMH" with the following arguments:
  \item{GMH}{the values of the generalized Mantel-Haenszel statistics.}
  \item{alpha}{the value of \code{alpha} argument.}
  \item{thr}{the threshold (cut-score) for DIF detection.}
  \item{DIFitems}{either the items which were detected as DIF items, or "No DIF item detected".}
  \item{purification}{the value of \code{purify} option.} 
  \item{nrPur}{the number of iterations in the item purification process. Returned only if \code{purify} is \code{TRUE}.}
  \item{difPur}{a binary matrix with one row per iteration in the item purification process and one column per item. Zeros and ones in the \emph{i}-th 
   row refer to items which were classified respectively as non-DIF and DIF items at the (\emph{i}-1)-th step. The first row corresponds to the initial
   classification of the items. Returned only if \code{purify} is \code{TRUE}.}
  \item{convergence}{logical indicating whether the iterative item purification process stopped before the maximal number \code{nrIter} of allowed iterations. 
   Returned only if \code{purify} is \code{TRUE}.}
  \item{names}{the names of the items.}
  \item{focal.names}{the value of \code{focal.names} argument.}
 }
 
\details{
 The generalized Mantel-Haenszel statistic (Somes, 1986) can be used to detect uniform differential item functioning among multiple groups,
 without requiring an item response model approach (Penfield, 2001).
 
 The \code{Data} is a matrix whose rows correspond to the subjects and columns to the items. Missing values are not allowed.
 In addition, \code{Data} can hold the vector of group membership. If so, \code{group} indicates the column of \code{Data} which 
 corresponds to the group membership, either by specifying its name or by giving the column number. Otherwise, \code{group} must be
 a vector of same length as \code{nrow(Data)}.
 
 The vector of group membership must hold at least three value, either as numeric or character. The focal groups are defined by the values of the argument \code{focal.names}. 
 If there is a unique focal group, then \code{difGMH} returns the output of \code{\link{difMH}} (\bold{without} continuity correction).

 The threshold (or cut-score) for classifying items as DIF is computed as the quantile of the chi-square distribution with lower-tail
 probability of one minus \code{alpha} and with as many degrees of freedom as the number of focal groups.
 
 Item purification can be performed by setting \code{purify} to \code{TRUE}. Purification works as follows: if at least one item detected as functioning 
 differently at the first step of the process, then the data set of the next step consists in all items that are currently anchor (DIF free) items, plus the 
 tested item (if necessary). The process stops when either two successive applications of the method yield the same classifications of the items (Clauser and Mazor, 1998),
 or when \code{nrIter} iterations are run without obtaining two successive identical classifications. In the latter case a warning message is printed. 
}


\references{
 Clauser, B. E. and Mazor, K. M. (1998). Using statistical procedures to identify differential item functioning test items. \emph{Educational Measurement: Issues and Practice, 17}, 31-44.

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
 \code{\link{difGMH}}, \code{\link{difMH}}
 }

\examples{
 \dontrun{
 # Loading of the verbal data
 data(verbal)
 attach(verbal)

 # Creating four groups according to gender ("Man" or "Woman") and
 # trait anger score ("Low" or "High")
 group<-rep("WomanLow",nrow(verbal))
 group[Anger>20 & Gender==0]<-"WomanHigh"
 group[Anger<=20 & Gender==1]<-"ManLow"
 group[Anger>20 & Gender==1]<-"ManHigh"

 # New data set
 Verbal<-cbind(verbal[,1:24],group)

 # Reference group: "WomanLow"
 names<-c("WomanHigh","ManLow","ManHigh")

 # Three equivalent settings of the data matrix and the group membership
 difGMH(Verbal, group=25, focal.names=names)
 difGMH(Verbal, group="group", focal.name=names)
 difGMH(Verbal[,1:24], group=Verbal[,25], focal.names=names)

 # With item purification 
 difGMH(Verbal, group=25, focal.names=names, purify=TRUE)
 difGMH(Verbal, group=25, focal.names=names, purify=TRUE, nrIter=5)
 }
 }

