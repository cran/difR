\name{difR-package}
\alias{difR-package}
\alias{difR}
\docType{package}
\title{Collection of methods to detect dichotomous differential item functioning (DIF) in psychometrics}


\description{
 The difR package contains several traditional methods to detect DIF in dichotomously scored items.
 Both uniform and non-uniform DIF effects can be detected, with methods relying upon item response
 models or not. Some methods deal with more than one focal group.

 Methods currently available are:
  \enumerate{
  \item{Transformed Item Difficulties (TID) method (Angoff and Ford, 1973)}
  \item{Mantel-Haenszel (Holland and Thayer, 1988)}
  \item{Standardization (Dorans and Kullick, 1986)}
  \item{Breslow-Day (Aguerri et al., 2009; Penfield, 2003)}
  \item{Logistic regression (Swaminathan and Rogers, 1990)}
  \item{Lord's chi-square test (Lord, 1980)}
  \item{Raju's area (Raju, 1990)}
  \item{Likelihood-ratio test (Thissen, Steinberg and Wainer, 1988)}
  \item{Generalized Mantel-Haenszel (Penfield, 2001)}
  \item{Generalized logistic regression (Magis, Raiche, Beland and Gerard, 2011)}
  \item{Generalized Lord's chi-square test (Kim, Cohen and Park, 1995).}
    }

 The difR package is further described in Magis, Beland, Tuerlinckx and De Boeck (2010).
 }

\details{
 \tabular{ll}{
  Package: \tab difR \cr
  Type:    \tab Package \cr
  Version: \tab 4.2 \cr
  Date:    \tab 2012-03-29\cr
  Depends: \tab R (>= 2.8.0), lme4, ltm \cr
  License: \tab see file LICENSE \cr
  }
 }


\references{
 Aguerri, M.E., Galibert, M.S., Attorresi, H.F. and Maranon, P.P. (2009). Erroneous detection of nonuniform DIF using the Breslow-Day test in a short test. \emph{Quality and Quantity, 43}, 35-44. 

 Angoff, W. H., and Ford, S. F. (1973). Item-race interaction on a test of scholastic aptitude. \emph{}Journal of Educational Measurement, 95-106.

 Dorans, N. J. and Kullick, E. (1986). Demonstrating the utility of the standardization approach to assessing unexpected differential item performance on the Scholastic Aptitude Test. \emph{Journal of Educational Measurement, 23}, 355-368.

 Holland, P. W. and Thayer, D. T. (1988). Differential item performance and the Mantel-Haenszel procedure. In H. Wainer and H. I. Braun (Dirs.), \emph{Test validity}. Hillsdale, New Jersey: Lawrence Erlbaum Associates.

 Kim, S.-H., Cohen, A.S. and Park, T.-H. (1995). Detection of differential item functioning in multiple groups. \emph{Journal of Educational Measurement, 32}, 261-276. 
 
 Lord, F. (1980). \emph{Applications of item response theory to practical testing problems}. Hillsdale, NJ: Lawrence Erlbaum Associates.

 Magis, D., Beland, S., Tuerlinckx, F. and De Boeck, P. (2010). A general framework and an R package for the detection
 of dichotomous differential item functioning. \emph{Behavior Research Methods, 42}, 847-862.

 Magis, D., Raiche, G., Beland, S. and Gerard, P. (2011). A logistic regression procedure to detect differential item functioning among multiple groups. Unpublished 
 manuscript.

 Penfield, R. D. (2001). Assessing differential item functioning among multiple groups: a comparison of three Mantel-Haenszel procedures. \emph{Applied Measurement in Education, 14}, 235-259. 

 Penfield, R.D. (2003). Application of the Breslow-Day test of trend in odds ratio heterogeneity to the detection of nonuniform DIF. \emph{Alberta Journal of Educational Research, 49}, 231-243.

 Raju, N. S. (1990). Determining the significance of estimated signed and unsigned areas between two item response functions. \emph{Applied Psychological Measurement, 14}, 197-207.
 
 Swaminathan, H. and Rogers, H. J. (1990). Detecting differential item functioning using logistic regression procedures. \emph{Journal of Educational Measurement, 27}, 361-370.

 Thissen, D., Steinberg, L. and Wainer, H. (1988). Use of item response theory in the study of group difference in trace lines. 
 In H. Wainer and H. Braun (Eds.), \emph{Test validity}. Hillsdale, NJ: Lawrence Erlbaum Associates.
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
 Other useful packages can be found in the \emph{R} \emph{Psychometric} task view.
 See \url{http://cran.stat.sfu.ca/web/views/Psychometrics.html} for further details.
 }


\keyword{package}



