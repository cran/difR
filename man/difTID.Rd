\name{difTID}
\alias{difTID}
\alias{print.TID}
\alias{plot.TID}

\title{Transformed Item Difficulties (TID) DIF method}

\description{
 Performs DIF detection using Transformed Item Difficulties (TID) method. 
 }

\usage{
difTID(Data, group, focal.name, props = NULL, 
  	thrTID = 1.5, purify = FALSE, nrIter = 10, 
  	save.output=FALSE, output=c("out","default")) 
\method{print}{TID}(x, ...)
\method{plot}{TID}(x, plot="dist", pch=8, number=TRUE, col="red", 
  	save.plot=FALSE, save.options=c("plot","default","pdf"), ...)
 }

\arguments{
 \item{Data}{numeric: either the data matrix only, or the data matrix plus the vector of group membership. See \bold{Details}.}
 \item{group}{numeric or character: either the vector of group membership or the column indicator (within \code{data}) of group membership. See \bold{Details}.}
 \item{focal.name}{numeric or character indicating the level of \code{group} which corresponds to the focal group.}
 \item{props}{either \code{NULL} (default) or a two-column matrix with proportions of success in the reference group and the focal group. See \bold{Details }.}
 \item{thrTID}{numeric: the threshold for detecting DIF items (default is 1.5).}
 \item{purify}{logical: should the method be used iteratively to purify the set of anchor items? (default is FALSE).}
 \item{nrIter}{numeric: the maximal number of iterations in the item purification process (default is 10).}
 \item{save.output}{logical: should the output be saved into a text file? (Default is \code{FALSE}).}
 \item{output}{character: a vector of two components. The first component is the name of the output file, the second component is either the file path or 
              \code{"default"} (default value). See \bold{Details}.}
 \item{x}{the result from a \code{TID} class object.}
 \item{plot}{character: either \code{"dist"} (default) to display the perpendicular distances, or \code{"delta"} for the Delta plot. See \bold{Details}.}
 \item{pch, col}{type of usual \code{pch} and \code{col} graphical options.}
 \item{number}{logical: should the item number identification be printed (default is \code{TRUE}).}
 \item{save.plot}{logical: should the plot be saved into a separate file? (default is \code{FALSE}).}
 \item{save.options}{character: a vector of three components. The first component is the name of the output file, the second component is either the file path 
                     or \code{"default"} (default value), and the third component is the file extension, either \code{"pdf"} (default) or \code{"jpeg"}.
                     See \bold{Details}.}
 \item{...}{other generic parameters for the \code{plot} or the \code{print} functions.}
 }

\value{
A list of class "TID" with the following arguments:
  \item{Dj}{the values of the perpendicular distances.}
  \item{prop}{the matrix of proportions of success.}
  \item{delta}{the matrix of Delta scores, in the same format as the \code{prop} matrix.}
  \item{axisPar}{a vector of length two with the intercept and slope parameters of the major axis of Delta points.}
  \item{thr}{the threshold (cut-score) for DIF detection.}
  \item{DIFitems}{either the column indicators of the items which were detected as DIF items, or "No DIF item detected".}
  \item{purification}{the value of \code{purify} option.} 
  \item{nrPur}{the number of iterations in the item purification process. Returned only if \code{purify} is \code{TRUE}.}
  \item{difPur}{a binary matrix with one row per iteration in the item purification process and one column per item. Zeros and ones in the \emph{i}-th 
   row refer to items which were classified respectively as non-DIF and DIF items at the (\emph{i}-1)-th step. The first row corresponds to the initial
   classification of the items. Returned only if \code{purify} is \code{TRUE}.}
  \item{convergence}{logical indicating whether the iterative item purification process stopped before the maximal number \code{nrIter} of allowed iterations. 
  Returned only if \code{purify} is \code{TRUE}.}
  \item{names}{the names of the items.}
  \item{save.output}{the value of the \code{save.output} argument.}
  \item{output}{the value of the \code{output} argument.}
 }
 

\details{
 The Transformed Item Difficulties (TID) method, also known as Angoff's Delta method (Angoff, 1982; Angoff and Ford, 1973) allows for detecting uniform 
 differential item functioning without requiring an item response model approach. 
 
 The \code{Data} is a matrix whose rows correspond to the subjects and columns to the items. 
 In addition, \code{Data} can hold the vector of group membership. If so, \code{group} indicates the column of \code{Data} which corresponds to the group
 membership, either by specifying its name or by giving the column number. Otherwise, \code{group} must be a vector of same length as \code{nrow(Data)}.
 
 Missing values are allowed for item responses (not for group membership) but must be coded as \code{NA} values. They are discarded from the computation of
 proportions of success.

 The vector of group membership must hold only two different values, either as numeric or character. The focal group is defined by
 the value of the argument \code{focal.name}. 

 Alternatively, one can provide the matrix of proportions of success in for each item in each group. This matrix must have the same format as that provided to
 the \code{\link{trItemDiff}} function; see the corresponding help file for further details.
  
 The threshold (or cut-score) for classifying items as DIF must be supplied through the \code{thrTID} argument. The default value is 1.5, as being one of the most
 commonbly used values (e.g. Facon and Nuchadee, 2010; Muniz, Hambleton, and Xing, 2001; Robin, Sirecci, and Hambleton, 2003). Other values can be specified instead.
 
 Item purification can be performed by setting \code{purify} to \code{TRUE}. Purification works as follows: if at least one item was detected as functioning 
 differently at some step of the process, then the intercept and slope parameters of the major axis are re-computed by discarding all items previously flagged as
 DIF. All perpendicular distances are then re-computed for all items. The process stops when either two successive applications of the method yield the same
 classifications of the items, or when \code{nrIter} iterations are run without obtaining two successive identical classifications. In the latter case a warning
 message is printed. 

 The output of the \code{difTID}, as displayed by the \code{print.TID} function, can be stored in a text file provided that \code{save.output} is set to \code{TRUE} 
 (the default value \code{FALSE} does not execute the storage). In this case, the name of the text file must be given as a character string into the first component
 of the \code{output} argument (default name is \code{"out"}), and the path for saving the text file can be given through the second component of \code{output}. The
 default value is \code{"default"}, meaning that the file will be saved in the current working directory. Any other path can be specified as a character string: 
 see the \bold{Examples} section for an illustration.

 Two types of plots are available through the \code{plot.TID} function. If the argument \code{plot} is set to \code{"dist"} (the default value), then the 
 perpendicular distances are represented on the Y axis of a scatter plot, with each item on the X axis. If \code{plot} is set to \code{"delta"}, the Delta plot is
 returned, that is, the scatter plot of pairs of Delta scores for each item, with the reference group on the X axis and the focal group on the Y axis. The type of
 point and the color are fixed by the usual \code{pch} and \code{col} arguments. Option \code{number} permits to display the item numbers instead. Detection 
 thresholds are also printed. Also, the plot can be stored in a figure file, either in PDF or JPEG format. Fixing \code{save.plot} to \code{TRUE} allows this
 process. The figure is defined through the components of \code{save.options}. The first two components perform similarly as those of the \code{output} argument.
 The third component is the figure format, with allowed values \code{"pdf"} (default) for PDF file and \code{"jpeg"} for JPEG file.
 }

\references{
 Angoff, W. H. (1982). Use of difficulty and discrimination indices for detecting item bias. In R. A. Berck (Ed.), \emph{Handbook of methods for detecting
 item bias} (pp. 96-116). Baltimore, MD: Johns Hopkins University Press.

 Angoff, W. H., and Ford, S. F. (1973). Item-race interaction on a test of scholastic aptitude. \emph{Journal of Educational Measurement, 2}, 95-106.

 Facon, B., and Nuchadee, M.-L. (2010). An item analysis of Raven's Colored Progressive Matrices among participants with Down syndrome. \emph{Research in
 Deevelopmental Disabilities, 31}, 243-249.

 Muniz, J., Hambleton, R. K., and Xing, D. (2001). Small sample studies to detect flaws in item translations. \emph{International Journal of Testing, 1}, 115-135.

 Robin, F., Sirecci, S. G., and Hambleton, R. K. (2003). Evaluating the equivalence of different language versions of a credentialing exam. \emph{International
 Journal of Testing, 3}, 1-20.
 }
 

\author{
    David Magis \cr
    Department of Education, University of Liege \cr
    Research Group of Quantitative Psychology and Individual Differences, KU Leuven \cr
    \email{David.Magis@ulg.ac.be}, \url{http://ppw.kuleuven.be/okp/home/} \cr
 }


\seealso{
 \code{\link{trItemDiff}}, \code{\link{dichoDif}}
 }

\examples{
\dontrun{

 # Loading of the verbal data
 data(verbal)

 # Excluding the "Anger" variable
 verbal <- verbal[colnames(verbal)!="Anger"]

 # Three equivalent settings of the data matrix and the group membership
 r <- difTID(verbal, group=25, focal.name=1)
 difTID(verbal, group="Gender", focal.name=1)
 difTID(verbal[,1:24], group=verbal[,25], focal.name=1)

 # With item purification and threshold 1
 r2 <- difTID(verbal, group="Gender", focal.name=1, purify=TRUE, thrTID=1)

 # Saving the output into the "TIDresults.txt" file (and default path)
 difTID(verbal, group=25, focal.name=1, save.output = TRUE, 
   output = c("TIDresults","default"))

 # Providing the proportions of success only
 props <- cbind(colMeans(verbal[verbal[,25]==0,1:24]),
   colMeans(verbal[verbal[,25]==1,1:24]) )
 difTID(prop=props)

 # Graphical devices
 plot(r2)
 plot(r2, plot="delta")

 # Plotting results and saving it in a PDF figure
 plot(r2, save.plot = TRUE, save.options = c("plot", "default", "pdf"))

 # Changing the path, JPEG figure
 path <- "c:/Program Files/"
 plot(r2, save.plot = TRUE, save.options = c("plot", path, "jpeg"))
}
 }



