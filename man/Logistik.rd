\name{Logistik}
\alias{Logistik}

\title{Logistic regression DIF statistic}

\description{
 Calculates the "logistic regression" likelihood-ratio statistics and effect sizes
 for DIF detection. 
 }

\usage{
Logistik(data, member, anchor=1:ncol(data), type="both",
 	criterion="LRT")
 }
 
\arguments{
 \item{data}{numeric: the data matrix (one row per subject, one column per item).}
 \item{member}{numeric: the vector of group membership with zero and one entries only. See \bold{Details}.}
 \item{anchor}{a vector of integer values specifying which items (all by default) are currently considered as anchor (DIF free) items. See \bold{Details}.}
 \item{type}{a character string specifying which DIF effects must be tested. Possible values are \code{"both"} (default), \code{"udif"} and \code{"nudif"}. See \bold{Details}.}
 \item{criterion}{a character string specifying which DIF statistic is computed. Possible values are \code{"LRT"} (default) or \code{"Wald"}. See \bold{Details}.}
}


\value{ 
 A list with four components:
 \item{stat}{the values of the logistic regression DIF statistics.}
 \item{deltaR2}{the differences between Nagelkerke's \eqn{R^2} coefficients of the tested models. See \bold{Details}.}
 \item{parM0}{a matrix with one row per item and four columns, holding successively the fitted parameters \eqn{\hat{\alpha}}, \eqn{\hat{\beta}}, \eqn{\hat{\gamma}_1}
               and \eqn{\hat{\delta}_1} of the "full" model (\eqn{M_0} if \code{type="both"} or \code{type="nudif"}, \eqn{M_1} if \code{type="udif"}).}
 \item{parM1}{the same matrix as \code{parM0} but with fitted parameters for the "simpler" model (\eqn{M_1} if \code{type="nudif"}, \eqn{M_2} if \code{type="both"}
 		   or \code{type="udif"}).}
 \item{criterion}{the value of the \code{criterion} argument.}
 }


\details{
 This command computes the logistic regression statistic (Swaminathan and Rogers, 1990) in the specific framework of differential item functioning. 
 It forms the basic command of \code{\link{difLogistic}} and is specifically designed for this call.
 
 The three possible models to be fitted are:

 \deqn{M_0: logit (\pi_g) = \alpha + \beta X + \gamma_g + \delta_g X}
 \deqn{M_1: logit (\pi_g) = \alpha + \beta X + \gamma_g}
 \deqn{M_2: logit (\pi_g) = \alpha + \beta X}

 where \eqn{\pi_g} is the probability of answering correctly the item in group \emph{g} and \eqn{X} is the sum score. Parameters \eqn{\alpha} and 
 \eqn{\beta} are the intercept and the slope of the logistic curves (common to all groups), while \eqn{\gamma_g} and \eqn{\delta_g} are group-specific
 parameters. For identification reasons the parameters \eqn{\gamma_0} and \eqn{\delta_0} for reference group (\eqn{g=0}) are set to zero. The parameter
 \eqn{\gamma_1} of the focal group (\eqn{g=1}) represents the uniform DIF effect, and the parameter \eqn{\delta_1} is used to model nonuniform DIF 
 effect. The models are fitted with the \code{\link{glm}} function.

 Two types of DIF statistics can be computed: the likelihood ratio test statistics, obtained by comparing the fit of two nested models,
 and the Wald statistics, obtained with an appropriate contrast matrix for testing the model parameters (Johnson and Wichern, 1998).
 These are specified by the argument \code{criterion}, with respective values \code{"LRT"} and \code{"Wald"}. By default, the LRT
 statistics are computed.

 If \code{criterion} is \code{"LRT"}, the argument \code{type} determines the models to be compared by means of the LRT statistics.
 The three possible values of \code{type} are: \code{type="both"} (default) which tests the hypothesis \eqn{H_0: \gamma_1 = \delta_1=0} by comparing models 
 \eqn{M_0} and \eqn{M_2}; \code{type="nudif"} which tests the hypothesis \eqn{H_0: \delta_1 = 0} by comparing models \eqn{M_0} and \eqn{M_1}; and \code{type="udif"}
 which tests the hypothesis \eqn{H_0: \gamma_1 = 0} by comparing models \eqn{M_1} and \eqn{M_2} (assuming that \eqn{\delta_1 = 0}). In other words, \code{type="both"} tests for
 DIF (without distinction between uniform and nonuniform effects), while \code{type="udif"} and \code{type="nudif"} test for uniform and nonuniform DIF,
 respectively. 

 If \code{criterion} is \code{"Wald"}, the argument \code{type} determines the logistic model to be considered and the appropriate contrast matrix. 
 If \code{type=="both"}, the considered model is model \eqn{M_0} and the contrast matrix has two rows, (0,0,1,0) and (0,0,0,1). If \code{type=="nudif"}, 
 the considered model is also model \eqn{M_0} but the contrast matrix has only one row, (0,0,0,1). Eventually, if \code{type=="udif"}, the considered model
 is model \eqn{M_1} and the contrast matrix has one row, (0,0,1). 

 The data are passed through the \code{data} argument, with one row per subject and one column per item. Missing values are not allowed.
  
 The vector of group membership, specified with \code{member} argument, must hold only zeros and ones, a value of zero corresponding to the
 reference group and a value of one to the focal group.

 Option \code{anchor} sets the items which are considered as anchor items for computing logistic regression DIF statistics. Items other than the anchor 
 items and the tested item are discarded. \code{anchor} must hold integer values specifying the column numbers of the corresponding anchor items. It is 
 mainly designed to perform item purification.

 The output contains: the selected DIF statistics (either the LRT or the Wald statistic) computed for each item, and two matrices with the parameter estimates of both models, for each item. In
 addition, Nagelkerke's \eqn{R^2} coefficients (Nagelkerke, 1991) are computed for each model and the output returns the differences in these coefficients. 
 Such differences are used as measures of effect size by the \code{\link{difLogistic}} command; see Gomez-Benito, Dolores Hidalgo and Padilla (2009), 
 Jodoign and Gierl (2001) and Zumbo and Thomas (1997). The \code{criterion} argument is also returned.
}
 
\references{
 Gomez-Benito, J., Dolores Hidalgo, M. and Padilla, J.-L. (2009). Efficacy of effect size measures in logistic regression: an application for detecting DIF. 
 \emph{Methodology, 5}, 18-25.

 Jodoin, M. G. and Gierl, M. J. (2001). Evaluating Type I error and power rates using an effect size measure with logistic regression procedure for DIF detection.
 \emph{Applied Measurement in Education, 14}, 329-349.

 Johnson, R. A. and Wichern, D. W. (1998). \emph{Applied multivariate statistical analysis (fourth edition)}. Upper saddle river, NJ: prentice-Hall.

 Magis, D., Beland, S., Tuerlinckx, F. and De Boeck, P. (2010). A general framework and an R package for the detection
 of dichotomous differential item functioning. \emph{Behavior Research Methods, 42}, 847-862.

 Nagelkerke, N. J. D. (1991). A note on a general definition of the coefficient of determination. \emph{Biometrika, 78}, 691-692.

 Swaminathan, H. and Rogers, H. J. (1990). Detecting differential item functioning using logistic regression procedures. \emph{Journal of Educational Measurement,
 27}, 361-370.

 Zumbo, B. D. and Thomas, D. R. (1997). A measure of effect size for a model-based approach for studying DIF. Prince George, Canada: University of Northern British
 Columbia, Edgeworth Laboratory for Quantitative Behavioral Science.
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
 \code{\link{difLogistic}}, \code{\link{dichoDif}}
}

\examples{
\dontrun{

 # Loading of the verbal data
 data(verbal)

 # Testing both types of DIF simultaneously
 # With all items
 Logistik(verbal[,1:24], verbal[,26])

 # Testing both types of DIF simultaneously
 # With all items and Wald test
 Logistik(verbal[,1:24], verbal[,26], criterion="Wald")

 # Removing item 6 from the set of anchor items
 Logistik(verbal[,1:24], verbal[,26], anchor=c(1:5,7:24))

 # Testing for nonuniform DIF
 Logistik(verbal[,1:24], verbal[,26], type="nudif")

 # Testing for uniform DIF
 Logistik(verbal[,1:24], verbal[,26], type="udif")
 }
 }
