\name{mantelHaenszel}
\alias{mantelHaenszel}

\title{Mantel-Haenszel DIF statistic}

\description{
 Calculates the Mantel-Haenszel statistics for DIF detection. 
 }

\usage{
 mantelHaenszel(data, member, correct=TRUE, anchor=1:ncol(data))
 }

\arguments{
 \item{data}{numeric: the data matrix (one row per subject, one column per item).}
 \item{member}{numeric: the vector of group membership with zero and one entries only. See \bold{Details}.}
 \item{correct}{logical: should the continuity correction be used? (default is TRUE).}
 \item{anchor}{a vector of integer values specifying which items (all by default) are currently considered as anchor (DIF free) items. See \bold{Details}.}
 }

\value{
 A list with wto arguments:
 \item{resMH}{the vector of the Mantel-Haenszel DIF statistics.}
 \item{resAlpha}{the vector of the Mantel-Haenszel estimates of the common odds ratios.}
 }
 
\details{
 This command computes the Mantel-Haenszel (1959) statistic in the specific framework of differential item functioning. It forms the basic command
 of \code{\link{difMH}} and is specifically designed for this call.
 
 The data are passed through the \code{data} argument, with one row per subject and one column per item. Missing values are not allowed.
  
 The vector of group membership, specified with \code{member} argument, must hold only zeros and ones, a value of zero corresponding to the
 reference group and a value of one to the focal group.
 
 By default, the continuity correction factor -0.5 is used (Holland and Thayer, 1988). One can nevertheless remove it by specifying \code{correct=FALSE}.

 Option \code{anchor} sets the items which are considered as anchor items for computing Mantel-Haenszel statistics. Items other than the anchor items and the tested item 
 are discarded. \code{anchor} must hold integer values specifying the column numbers of the corresponding anchor items. It is primarily designed to perform item purification.
 
 In addition to the Mantel-Haenszel statistics to identify DIF items, \code{mantelHaenszel} computes the estimates of the common odds ratio \eqn{\alpha_{MH}} which are used
 for measuring the effect size of the items (Holland and Thayer, 1985, 1988).
}

\references{
 Holland, P. W. and Thayer, D. T. (1985). An alternative definition of the ETS delta scale of item difficulty. \emph{Research Report RR-85-43}. Princeton, New-Jersey:
 Educational Testing Service.

 Holland, P. W. and Thayer, D. T. (1988). Differential item performance and the Mantel-Haenszel procedure. In H. Wainer and H. I. Braun (Ed.), \emph{Test validity}. Hillsdale, New Jersey: Lawrence Erlbaum Associates.
 
 Magis, D., Beland, S., Tuerlinckx, F. and De Boeck, P. (in press). A general framework and an R package for the detection
 of dichotomous differential item functioning. \emph{Behavior Research Methods}.

 Mantel, N. and Haenszel, W. (1959). Statistical aspects of the analysis of data from retrospective studies of disease. \emph{Journal of the National Cancer Institute, 22}, 719-748.
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
 \code{\link{difMH}}, \code{\link{dichoDif}}
 }

\examples{
\dontrun{

 # Loading of the verbal data
 data(verbal)

 # With and without continuity correction
 mantelHaenszel(verbal[,1:24], verbal[,26])
 mantelHaenszel(verbal[,1:24], verbal[,26],correct=FALSE)
 
 # Removing item 6 from the set of anchor items
 mantelHaenszel(verbal[,1:24], verbal[,26], anchor=c(1:5,7:24))
 }
 }



