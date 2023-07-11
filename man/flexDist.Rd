\name{flexDist}
\alias{flexDist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Non-parametric pdf from limited information data
}
\description{
This is an attempt to create a distribution function if the only existing information is the quantiles or expectiles of the distribution.  
}
\usage{
flexDist(quantiles = list(values=c(-1.96,0,1.96), prob=c(0.05, .50, 0.95)), 
         expectiles = list(), lambda = 10, 
         kappa = 10, delta = 1e-07, order = 3, n.iter = 200, 
         plot = TRUE, no.inter = 100, lower = NULL, 
         upper = NULL, perc.quant = 0.3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{quantiles}{a list with components \code{values} and \code{prob}}
  \item{expectiles}{a list with components \code{values} and \code{prob}}
  \item{lambda}{smoothing parameter for the log-pdf}
  \item{kappa}{smoothing parameter for log concavity}
  \item{delta}{smoothing parameter for ridge penalty}
  \item{order}{the order of the penalty for log-pdf}
  \item{n.iter}{maximum number of iterations}
  \item{plot}{whether to plot the result}
  \item{no.inter}{How many discrete probabilities to evaluate}
  \item{lower}{the lower value of the x}
  \item{upper}{the upper value of the x}
  \item{perc.quant}{how far from the quantile should go out to define the limit of x if not set by \code{lower} or \code{upper}}
  \item{\dots}{additional arguments}
}

\value{
Returns a list with components 
\item{pdf}{the hights of the fitted pdf, the sum of it multiplied by the Dx should add up to 1  i.e. \code{sum(object$pdf*diff(object$x)[1])} }
\item{cdf}{the fitted cdf}
\item{x}{the values of x where the discretise distribution is defined}
\item{pFun}{the cdf of the fitted non-parametric distribution}
\item{qFun}{the inverse cdf function of the fitted non-parametric distribution}
\item{rFun}{a function to generate a random sample from the fitted non-parametric distribution}     
}
\references{

Eilers, P. H. C.,  Voudouris, V.,  Rigby R. A.,  Stasinopoulos D. M. (2012)  Estimation of nonparametric density from sparse summary information, under review.
 
Rigby, R. A. and Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), Appl. Statist., 54, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R. Journal of Statistical Software, Vol. 23, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  
Chapman and Hall/CRC. \doi{10.1201/b21973}  

(see also \url{https://www.gamlss.com/}).

}
\author{Mikis Stasinopoulos, Paul Eilers, Bob Rigby and Vlasios Voudouris}

 
\seealso{\code{\link[gamlss]{histSmo}}}
\examples{
# Normal
r1<-flexDist(quantiles=list(values=qNO(c(0.05, 0.25, 0.5,0.75, 0.95), mu=0, 
             sigma=1), prob=c( 0.05, 0.25, 0.5,0.75,0.95 )), 
             no.inter=200, lambda=10,  kappa=10, perc.quant=0.3)
# GAMMA
r1<-flexDist(quantiles=list(values=qGA(c(0.05,0.25, 0.5,0.75,0.95), mu=1, 
       sigma=.8), prob=c(0.05,0.25, 0.5,0.75,0.95)), 
       expectiles=list(values=1, prob=0.5),  lambda=10, 
       kappa=10, lower=0, upper=5)# 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}

