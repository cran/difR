\name{contrastMatrix}
\alias{contrastMatrix}

\title{Contrast matrix for computing generalized Lord's chi-square DIF statistic}

\description{
 This command sets the appropriate contrast matrix C for computing the generalized Lord's chi-square statistics in the framework of DIF detection among multiple groups. 
 }

\usage{
 contrastMatrix(nrFocal, model)
 }

\arguments{
 \item{nrFocal}{numeric: the number of focal groups.}
 \item{model}{character: the logistic model to be fitted (either \code{"1PL"}, \code{"2PL"}, \code{"3PL"} or \code{"3PLc"}). See \bold{Details}.}
}

\value{
 A contrast matrix designed to test equality of item parameter estimates from the specified \code{model} and with \code{nrFocal} focal groups.The output matrix has 
 a number of rows equal to \code{nrFocal} times the number of tested parameters (one for 1PL model, two for 2PL and constrained 3PL models, three for 3PL model). The 
 number of columns is equal to (\code{nrFocal}+1) times the number of tested parameters. See Kim, Cohen and Park (1995) for further details.

 }
 
\details{
 The contrast matrix C is necessary to calculate the generalized Lord's chi-square statistic. It is designed to perform accurate tests of equality of item parameters 
 accross the groups of examinees (see Kim, Cohen and Park, 1995). This is a subroutine for the command \code{\link{genLordChi2}} which returns the DIF statistics. 

 The number of focal groups has to be specified by the argument \code{nrFocal}. Moreover, four logistic IRT models can be considered: the 1PL, 2PL and 3PL models 
 can be set by using their accronym (e.g. \code{"1PL"} for 1PL model, and so on). It is also possible to consider the constrained 3PL model, where all
 pseudo-guessing values are equal accross the groups of examinees and take some predefined values which do not not need to be supplied here. This model is specified by 
 the value \code{"3PLc"} for argument \code{model}.
 }

\references{
 Kim, S.-H., Cohen, A.S. and Park, T.-H. (1995). Detection of differential item functioning in multiple groups. \emph{Journal of Educational Measurement, 32}, 261-276. 

 Magis, D., Beland, S., Tuerlinckx, F. and De Boeck, P. (2010). A general framework and an R package for the detection
 of dichotomous differential item functioning. \emph{Behavior Research Methods, 42}, 847-862.
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
 \code{\link{genLordChi2}}, \code{\link{difGenLord}}
}

\examples{
\dontrun{

 # Contrast matrices with 1PL model and several focal groups
 contrastMatrix(2, "1PL")
 contrastMatrix(3, "1PL")
 contrastMatrix(4, "1PL")

 # Contrast matrices with 2PL, constrained and unconstrained 3PL models,
 # and three focal groups
 contrastMatrix(3, "2PL")
 contrastMatrix(3, "3PLc")
 contrastMatrix(3, "3PL")
}
}
