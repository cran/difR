\name{LRT}
\alias{LRT}

\title{Likelihood-Ratio Test DIF statistic}

\description{
 Calulates Likelihoo-Ratio Test (LRT) statistics for DIF detection.
}

\usage{
LRT(data, member)
}

\arguments{
 \item{data}{numeric: the data matrix (one row per subject, one column per item).}
 \item{member}{numeric: the vector of group membership with zero and one entries only. See \bold{Details}.}
}

\value{
A vector with the values of the LRT DIF statistics. 
}

\details{
 This command computes the likelihood-ratio test statistic (Thissen, Steinberg and Wainer, 1988) in the specific framework of differnetial item functioning. 
 It forms the basic command of \code{\link{difLRT}} and is specifically designed for this call.
 
 The data are passed through the \code{data} argument, with one row per subject and one column per item. Missing values are not allowed.
  
 The vector of group membership, specified with \code{member} argument, must hold only zeros and ones, a value of zero corresponding to the
 reference group and a value of one to the focal group.
 
 The LRT DIF statistic is computed for each item separately, using all other items as anchor items.
}

\note{
 Because of the fitting of the modified Rasch model with \code{glmer} the process can be very time consuming (see the \bold{Details} section of \code{\link{difLRT}}). 
}

\references{
 Bates, D. and Maechler, M. (2009). lme4: Linear mixed-effects models using S4 classes. R package version 0.999375-31. http://CRAN.R-project.org/package=lme4
 
 Magis, D., Beland, S., Tuerlinckx, F. and De Boeck, P. (2010). A general framework and an R package for the detection
 of dichotomous differential item functioning. \emph{Behavior Research Methods, 42}, 847-862.

 Thissen, D., Steinberg, L. and Wainer, H. (1988). Use of item response theory in the study of group difference in trace lines. 
 In H. Wainer and H. Braun (Eds.), \emph{Test validity}. Hillsdale, NJ: Lawrence Erlbaum Associates.
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
 \code{\link{difLRT}}, \code{\link{dichoDif}}
 }

\examples{
\dontrun{

 # Loading of the verbal data
 data(verbal)
 attach(verbal)

 # Excluding the "Anger" variable
 verbal<-verbal[colnames(verbal)!="Anger"]

 # Keeping the first 5 items and the first 50 subjects
 # (this is an artificial simplification to reduce the computational time)
 # Sixth column holds the group membership
 verbal<-verbal[1:50,c(1:5,25)]

 # Likelihood-ratio statistics
 LRT(verbal[,1:5],verbal[,6])
 }
 }

