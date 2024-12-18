\name{DPO}
\alias{DPO}
\alias{pDPO}
\alias{dDPO}
\alias{qDPO}
\alias{rDPO}
\alias{get_C}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{The Double Poisson distribution}
\description{
The function \code{DPO()} defines the double Poisson distribution, a two parameters distribution, for a \code{gamlss.family} object to be used in GAMLSS fitting using the function \code{gamlss()}. The functions \code{dDPO}, \code{pDPO}, \code{qDPO} and \code{rPO} define the density, distribution function, quantile function and random generation for the double Poisson, \code{DPO()}, distribution. The function \code{get_C()} calculates numericaly the constant of proportionality needed for the pdf to sum up to 1.

}
\usage{
DPO(mu.link = "log", sigma.link = "log")
dDPO(x, mu = 1, sigma = 1, log = FALSE)
pDPO(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qDPO(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE, 
     max.value = 10000)
rDPO(n, mu = 1, sigma = 1, max.value = 10000)
get_C(x, mu, sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.link}{the link function for \code{mu} with default \code{log}}
  \item{sigma.link}{the link function for \code{sigma} with default \code{log}}
  \item{x, q}{vector of (non-negative integer) quantiles}
   \item{p}{vector of probabilities}
  \item{mu}{the \code{mu} parameter}
  \item{sigma}{the \code{sigma} parameter}
  \item{lower.tail}{logical; if \code{TRUE} (default), probabilities are P[X <= x], otherwise, P[X > x]}
  \item{log, log.p}{logical; if \code{TRUE}, probabilities p are given as log(p)}
    \item{max.value}{a constant, set to the default value of 10000 for how far the algorithm should look for q}
    \item{n}{how many random values  to  generate}
}
\details{
 The definition for the Double Poisson distribution first introduced by Efron (1986)   [ see also Rigby et al (2019) page 480] is:   
 
 \deqn{f(y|\mu,\sigma)= \left( \frac{1}{\sigma} \right)^{1/2} e^{- \mu / \sigma} \left( \frac{\mu}{y} \right)^{y/\sigma}  \left(\frac{e^{y/\sigma -y y^y}}{y!} \right) C(\mu, \sigma)}{f(y|mu,sigma)=(1/sigma)^{1/2} [(e^{-y} y^y)/y!] [(e mu)/y]^{y/sigma} C} 
for \eqn{y=0,1,2,\ldots,\infty}{y=0,1,2, ...,Inf}, \eqn{\mu>0} and \eqn{\sigma>0} where \eqn{C}{C} is the constant of proportinality which is calculated numerically using the function \code{get_C}.  

}
\value{
The function \code{DPO} returns a \code{gamlss.family} object which can be used to fit a double Poisson distribution in the \code{gamlss()} function. 
}
\references{

Efron, B., 1986. Double exponential families and their use in generalized linear Regression. Journal of the American Statistical Association 81 (395), 709-721.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  
Chapman and Hall/CRC. \doi{10.1201/b21973}  

(see also \url{https://www.gamlss.com/}).

}
\author{
Mikis Stasinopoulos, Bob Rigby and Marco Enea
}
\note{The distributons calculates the constant of proportionality numerically therefore it can be slow for large data }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{PO}}
}
\examples{
DPO()
# overdisperse DPO
x <- 0:20
plot(x, dDPO(x, mu=5, sigma=3), type="h", col="red")
# underdisperse DPO
plot(x, dDPO(x, mu=5, sigma=.3), type="h", col="red")
# generate random sample
 Y <- rDPO(100,5,.5)
plot(table(Y))
points(0:20, 100*dDPO(0:20, mu=5, sigma=.5)+0.2,  col="red")
# fit a model to the data 
# library(gamlss)
# gamlss(Y~1,family=DPO)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}% 
