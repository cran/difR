\name{difMH}
\alias{difMH}
\alias{print.MH}
\alias{plot.MH}

\title{Mantel-Haenszel DIF method}

\description{
 Performs DIF detection using Mantel-Haenszel method. 
 }

\usage{
difMH(Data, group, focal.name , MHstat="MHChisq", correct=TRUE, exact=FALSE, 
  	alpha=0.05, purify=FALSE, nrIter=10, save.output=FALSE, 
  	output=c("out","default")) 
\method{print}{MH}(x, ...)
\method{plot}{MH}(x, pch=8, number=TRUE, col="red", save.plot=FALSE, 
  	save.options=c("plot","default","pdf"), ...)
 }

\arguments{
 \item{Data}{numeric: either the data matrix only, or the data matrix plus the vector of group membership. See \bold{Details}.}
 \item{group}{numeric or character: either the vector of group membership or the column indicator (within \code{data}) of group membership. See \bold{Details}.}
 \item{focal.name}{numeric or character indicating the level of \code{group} which corresponds to the focal group.}
 \item{MHstat}{character: specifies the DIF statistic to be used for DIF identification. Possible values are \code{"MHChisq"} (default) and \code{"logOR"}. 
               See \bold{Details }.}
 \item{correct}{logical: should the continuity correction be used? (default is \code{TRUE})}
 \item{exact}{logical: should an exact test be computed? (default is \code{FALSE}).}
 \item{alpha}{numeric: significance level (default is 0.05).}
 \item{purify}{logical: should the method be used iteratively to purify the set of anchor items? (default is FALSE).}
 \item{nrIter}{numeric: the maximal number of iterations in the item purification process (default is 10).}
 \item{save.output}{logical: should the output be saved into a text file? (Default is \code{FALSE}).}
 \item{output}{character: a vector of two components. The first component is the name of the output file, the second component is either the file path or 
              \code{"default"} (default value). See \bold{Details}.}
 \item{x}{the result from a \code{MH} class object.}
 \item{pch, col}{type of usual \code{pch} and \code{col} graphical options.}
 \item{number}{logical: should the item number identification be printed (default is \code{TRUE}).}
 \item{save.plot}{logical: should the plot be saved into a separate file? (default is \code{FALSE}).}
 \item{save.options}{character: a vector of three components. The first component is the name of the output file, the second component is either the file path or
                    \code{"default"} (default value), and the third component is the file extension, either \code{"pdf"} (default) or \code{"jpeg"}. 
                    See \bold{Details}.}
 \item{...}{other generic parameters for the \code{plot} or the \code{print} functions.}
 }

\value{
A list of class "MH" with the following arguments:
  \item{MH}{the values of the Mantel-Haenszel DIF statistics (either exact or asymptotic).}
  \item{alphaMH}{the values of the mantel-Haenszel estimates of common odds ratios. Returned only if \code{exact} is \code{FALSE}.}
  \item{varLambda}{the values of the variances of the log odds-ratio statistics. Returned only if \code{exact} is \code{FALSE}.}
  \item{Pval}{the exact P-values of the exact MH test. Returned only if \code{exact} is \code{TRUE}.}
  \item{MHstat}{the value of the \code{MHstat} argument. Returned only if \code{exact} is \code{FALSE}.}
  \item{alpha}{the value of \code{alpha} argument.}
  \item{thr}{the threshold (cut-score) for DIF detection. Returned only if \code{exact} is \code{FALSE}.}
  \item{DIFitems}{either the column indicators of the items which were detected as DIF items, or "No DIF item detected".}
  \item{correct}{the value of \code{correct} option.}
  \item{exact}{the value of \code{exact} option.}
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
 The method of Mantel-Haenszel (1959) allows for detecting uniform differential item functioning without requiring an item response model approach. 
 
 The \code{Data} is a matrix whose rows correspond to the subjects and columns to the items. In addition, \code{Data} can hold the vector of group membership. 
 If so, \code{group} indicates the column of \code{Data} which corresponds to the group membership, either by specifying its name or by giving the column number.
 Otherwise, \code{group} must be a vector of same length as \code{nrow(Data)}.
 
 Missing values are allowed for item responses (not for group membership) but must be coded as \code{NA} values. They are discarded from sum-score computation.

 The vector of group membership must hold only two different values, either as numeric or character. The focal group is defined by the value of the argument 
 \code{focal.name}. 
 
 The DIF statistic is specified by the \code{MHstat} argument. By default, \code{MHstat} takes the value \code{"MHChisq"} and the Mantel-Haenszel chi-square
 statistic is used. The other optional value is \code{"logOR"}, and the log odds-ratio statistic (that is, the log of \code{alphaMH} divided by the square root
 of \code{varLambda}) is used. See Penfield and Camilli (2007), Philips and Holland (1987) and \code{\link{mantelHaenszel}} help file.
 
 By default, the asymptotic Mantel-Haenszel statistic is computed. However, the exact statistics and related P-values can
 be obtained by specifying the logical argument \code{exact} to \code{TRUE}. See Agresti (1990, 1992) for further 
 details about exact inference.

 The threshold (or cut-score) for classifying items as DIF depends on the DIF statistic. With the Mantel-Haenszel chi-squared statistic (\code{MHstat=="MHChisq"}),
 it is computed as the quantile of the chi-square distribution with lower-tail probability of one minus \code{alpha} and with one degree of freedom. With 
 the log odds-ratio statistic (\code{MHstat=="logOR"}), it is computed as the quantile of the standard normal distribution with lower-tail probability of
 1-\code{alpha}/2. WIth exact inference, it is simply the \code{alpha} level since exact P-values are returned.
 
 By default, the continuity correction factor -0.5 is used (Holland and Thayer, 1988). One can nevertheless remove it by specifying \code{correct=FALSE}.
 
 In addition, the Mantel-Haenszel estimates of the common odds ratios \eqn{\alpha_{MH}} are used to measure the effect sizes of the items. These are obtained by
 \eqn{\Delta_{MH} = -2.35 \log \alpha_{MH}} (Holland and Thayer, 1985). According to the ETS delta scale, the effect size of an item is classified as negligible
 if \eqn{|\Delta_{MH}| \leq 1}, moderate  if \eqn{1 \leq |\Delta_{MH}| \leq 1.5}, and large if \eqn{|\Delta_{MH}| \geq 1.5}. The values of the effect sizes, 
 together with the ETS classification, are printed with the output. Note that this is returned only for asymptotic tests, i.e.
 when \code{exact} is \code{FALSE}.

 Item purification can be performed by setting \code{purify} to \code{TRUE}. Purification works as follows: if at least one item was detected as functioning 
 differently at some step of the process, then the data set of the next step consists in all items that are currently anchor (DIF free) items, plus the 
 tested item (if necessary). The process stops when either two successive applications of the method yield the same classifications of the items (Clauser and 
 Mazor, 1998), or when \code{nrIter} iterations are run without obtaining two successive identical classifications. In the latter case a warning message is printed. 

 The output of the \code{difMH}, as displayed by the \code{print.MH} function, can be stored in a text file provided that \code{save.output} is set to \code{TRUE} 
 (the default value \code{FALSE} does not execute the storage). In this case, the name of the text file must be given as a character string into the first component
 of the \code{output} argument (default name is \code{"out"}), and the path for saving the text file can be given through the second component of \code{output}. The
 default value is \code{"default"}, meaning that the file will be saved in the current working directory. Any other path can be specified as a character string: 
 see the \bold{Examples} section for an illustration.

 The \code{plot.MH} function displays the DIF statistics in a plot, with each item on the X axis. The type of point and the color are fixed by the usual \code{pch}
 and \code{col} arguments. Option \code{number} permits to display the item numbers instead. Also, the plot can be stored in a figure file, either in PDF or JPEG
 format. Fixing \code{save.plot} to \code{TRUE} allows this process. The figure is defined through the components of \code{save.options}. The first two components
 perform similarly as those of the \code{output} argument. The third component is the figure format, with allowed values \code{"pdf"} (default) for PDF file and
 \code{"jpeg"} for JPEG file. Note that no plot is returned for exact inference.
}


\references{
 Agresti, A. (1990). \emph{Categorical data analysis}. New York: Wiley.

 Agresti, A. (1992). A survey of exact inference for contingency tables. \emph{Statistical Science, 7}, 131-177.

 Holland, P. W. and Thayer, D. T. (1985). An alternative definition of the ETS delta scale of item difficulty. \emph{Research Report RR-85-43}. Princeton, NJ: 
 Educational Testing Service.

 Holland, P. W. and Thayer, D. T. (1988). Differential item performance and the Mantel-Haenszel procedure. In H. Wainer and H. I. Braun (Ed.), \emph{Test validity}.
 Hillsdale, NJ: Lawrence Erlbaum Associates.
 
 Magis, D., Beland, S., Tuerlinckx, F. and De Boeck, P. (2010). A general framework and an R package for the detection
 of dichotomous differential item functioning. \emph{Behavior Research Methods, 42}, 847-862.

 Mantel, N. and Haenszel, W. (1959). Statistical aspects of the analysis of data from retrospective studies of disease. \emph{Journal of the National Cancer 
 Institute, 22}, 719-748.
 
 Penfield, R. D., and Camilli, G. (2007). Differential item functioning and item bias. In C. R. Rao and S. Sinharray (Eds.), \emph{Handbook of Statistics 26: 
 Psychometrics} (pp. 125-167). Amsterdam, The Netherlands: Elsevier.

 Philips, A., and Holland, P. W. (1987). Estimators of the Mantel-Haenszel log odds-ratio estimate. \emph{Biometrics, 43}, 425-431.

 Raju, N. S., Bode, R. K. and Larsen, V. S. (1989). An empirical assessment of the Mantel-Haenszel statistic to detect differential item functioning. 
 \emph{Applied Measurement in Education, 2}, 1-13.
 
 Uttaro, T. and Millsap, R. E. (1994). Factors influencing the Mantel-Haenszel procedure in the detection of differential item functioning. \emph{Applied 
 Psychological Measurement, 18}, 15-25.
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
 \code{\link{mantelHaenszel}}, \code{\link{dichoDif}}
 }

\examples{
\dontrun{

 # Loading of the verbal data
 data(verbal)

 # Excluding the "Anger" variable
 verbal <- verbal[colnames(verbal)!="Anger"]

 # Three equivalent settings of the data matrix and the group membership
 r <- difMH(verbal, group=25, focal.name=1)
 difMH(verbal, group="Gender", focal.name=1)
 difMH(verbal[,1:24], group=verbal[,25], focal.name=1)

 # With log odds-ratio statistic
 r2 <- difMH(verbal, group=25, focal.name=1, MHstat="logOR")

 # With exact inference
 difMH(verbal, group=25, focal.name=1, exact=TRUE)

 # With item purification
 difMH(verbal, group="Gender", focal.name=1, purify=TRUE)
 difMH(verbal, group="Gender", focal.name=1, purify=TRUE, nrIter=5)

 # Without continuity correction and with 0.01 significance level
 difMH(verbal, group="Gender", focal.name=1, alpha=0.01, correct=FALSE)

 # Saving the output into the "MHresults.txt" file (and default path)
 r <- difMH(verbal, group=25, focal.name=1, save.output = TRUE, 
            output = c("MHresults","default"))

 # Graphical devices
 plot(r)
 plot(r2)

 # Plotting results and saving it in a PDF figure
 plot(r, save.plot = TRUE, save.options = c("plot", "default", "pdf"))

 # Changing the path, JPEG figure
 path <- "c:/Program Files/"
 plot(r, save.plot = TRUE, save.options = c("plot", path, "jpeg"))
}
 }



