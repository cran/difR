\name{trItemDiff}
\alias{trItemDiff}

\title{Transformed Item Difficulties Perpendicular Distances}

\description{
 Computes the perpendicular distances for DIF detection with Transformed Item Difficulties (TID) approach. 
 }

\usage{
trItemDiff(prop, anchor = 1:nrow(prop))
 }

\arguments{
 \item{prop}{numeric: a matrix with one row per subject and two columns: the first column with proportions of success in the reference group, 
            the second column with proportions of success in the focal group.}
 \item{anchor}{a vector of integer values specifying which items (all by default) are currently considered as anchor (DIF free) items. See \bold{Details}.}
 }


\value{
 A list with four arguments:
 \item{prop}{the value of the \code{prop} argument.}
 \item{delta}{the matrix of Delta scores, in the same format as \code{prop}.}
 \item{pars}{a vector of length two with the intercept and slope parameters of the major axis of Delta points.}
 \item{dist}{a numeric vector of perpendicular distances.}
 }
 
\details{
 This command basically computes the perpendicular distances from each Delta point to the major axis (Angoff, 1982; Angoff and Ford, 1973). It forms the basic command
 of \code{\link{difTID}} and is specifically designed for this call.
 
 The data are passed through the \code{prop} argument, a matrix with one row per item and two columns. The first column holds the proportions of success (correct
 responses) for each item in the reference group, and the second column provides the same information but for the focal group. Missing values are not allowed.
 Moreover, these proportions are internally constrained into the interval [0.001; 0.999] to ensure valid computation of Delta scores.

 The computation is made in three steps: (a) the proportions of success are transformed into Delta scores; (b) the major axis of the ellipse made by the Delta points
 is determined and its intercept and slope parameters are obtained; (c) the perpendicular distances (between each Delta point and the major axis) are computed. See
 Angoff (1982) and Angoff and Ford (1973) for further details. 

 Option \code{anchor} sets the items which are considered as anchor items for computing the perpndicular distances. Only anchor items are used to compute the 
 intercept and slope parameters of the major axis. \code{anchor} must hold integer values specifying the column numbers of the corresponding anchor items. 
 It is primarily designed to perform item purification.
 }

\references{
 Angoff, W. H. (1982). Use of difficulty and discrimination indices for detecting item bias. In R. A. Berck (Ed.), \emph{Handbook of methods for detecting
 item bias} (pp. 96-116). Baltimore, MD: Johns Hopkins University Press.

 Angoff, W. H., and Ford, S. F. (1973). Item-race interaction on a test of scholastic aptitude. \emph{Journal of Educational Measurement, 2}, 95-106.
 }


\author{
    David Magis \cr
    Department of Education, University of Liege \cr
    Research Group of Quantitative Psychology and Individual Differences, KU Leuven \cr
    \email{David.Magis@ulg.ac.be}, \url{http://ppw.kuleuven.be/okp/home/} \cr
 }


\seealso{
 \code{\link{difTID}}, \code{\link{dichoDif}}
 }

\examples{
\dontrun{

 # Loading of the verbal data
 data(verbal)

 # Computing proportions of success
 props <- cbind(colMeans(verbal[verbal[,26]==0,1:24]),
   colMeans(verbal[verbal[,26]==1,1:24]) )

 # Perpendicular distances 
 trItemDiff(props)

 # Removing item 6 from the set of anchor items
 trItemDiff(props, anchor = c(1:5,7:24))
 }
 }
