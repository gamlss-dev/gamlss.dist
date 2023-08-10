\name{BNB}
\alias{BNB}
\alias{dBNB}
\alias{pBNB}
\alias{qBNB}
\alias{rBNB}
\alias{ZABNB}
\alias{dZABNB}
\alias{pZABNB}
\alias{qZABNB}
\alias{rZABNB}
\alias{ZIBNB}
\alias{dZIBNB}
\alias{pZIBNB}
\alias{qZIBNB}
\alias{rZIBNB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Beta Negative Binomial  distribution for fitting a GAMLSS}
\description{
The \code{BNB()} function defines the beta negative binomial distribution, a three parameter distribution, for a \code{gamlss.family} object to be used  in GAMLSS fitting using the function \code{gamlss()}.   

The functions \code{dBNB}, \code{pBNB}, \code{qBNB} and \code{rBNB} define the density, distribution function, quantile function and random
generation for the beta negative binomial distribution, \code{BNB()}.

The functions \code{ZABNB()} and \code{ZIBNB()} are the zero adjusted (hurdle) and zero inflated versions of the beta negative binomial distribution, respectively. That is four  parameter distributions. 

The functions \code{dZABNB}, \code{dZIBNB}, \code{pZABNB},\code{pZIBNB}, \code{qZABNB} \code{qZIBNB}  \code{rZABNB} and  \code{rZIBNB} define the probability,  cumulative, quantile  and random
generation functions for the zero adjusted and zero inflated  beta negative binomial distributions, \code{ZABNB()}, \code{ZIBNB()}, respectively.

}
\usage{
BNB(mu.link = "log", sigma.link = "log", nu.link = "log")
dBNB(x, mu = 1, sigma = 1, nu = 1, log = FALSE)
pBNB(q, mu = 1, sigma = 1, nu = 1, lower.tail = TRUE, log.p = FALSE)
qBNB(p, mu = 1, sigma = 1, nu = 1, lower.tail = TRUE, log.p = FALSE, 
     max.value = 10000)
rBNB(n, mu = 1, sigma = 1, nu = 1, max.value = 10000)

ZABNB(mu.link = "log", sigma.link = "log", nu.link = "log",
      tau.link = "logit")
dZABNB(x, mu = 1, sigma = 1, nu = 1, tau = 0.1, log = FALSE)
pZABNB(q, mu = 1, sigma = 1, nu = 1, tau = 0.1, lower.tail = TRUE, 
       log.p = FALSE)
qZABNB(p, mu = 1, sigma = 1, nu = 1, tau = 0.1, lower.tail = TRUE, 
       log.p = FALSE, max.value = 10000)
rZABNB(n, mu = 1, sigma = 1, nu = 1, tau = 0.1, max.value = 10000)

ZIBNB(mu.link = "log", sigma.link = "log", nu.link = "log", 
      tau.link = "logit")
dZIBNB(x, mu = 1, sigma = 1, nu = 1, tau = 0.1, log = FALSE)
pZIBNB(q, mu = 1, sigma = 1, nu = 1, tau = 0.1, lower.tail = TRUE, 
       log.p = FALSE)
qZIBNB(p, mu = 1, sigma = 1, nu = 1, tau = 0.1, lower.tail = TRUE, 
       log.p = FALSE, max.value = 10000)
rZIBNB(n, mu = 1, sigma = 1, nu = 1, tau = 0.1, max.value = 10000)       
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{mu.link}{The link function for \code{mu}}
  \item{sigma.link}{The link function for \code{sigma}}
  \item{nu.link}{The link function for \code{nu}}
  \item{tau.link}{The link function for \code{tau}}
  \item{x}{vector of (non-negative integer)}
  \item{mu}{vector of positive means}
  \item{sigma}{vector of positive dispersion parameter}
  \item{nu}{vector of a positive parameter}
  \item{tau}{vector of probabilities}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x]}
  \item{p}{vector of probabilities}
  \item{q}{vector of quantiles}
  \item{n}{number of random values to return}
  \item{max.value}{a constant, set to the default value of 10000 for how far the algorithm  should look for q}
}
\details{
  The probability function of the \code{BNB} is 
  
   \deqn{P(Y=y|\mu,\sigma, \nu) = \frac{\Gamma(y+\nu^{-1})  B(y+\mu \sigma^{-1} \nu,\sigma^{-1}+\nu^{-1}+1)}
{\Gamma(y+1)  \Gamma(\nu^{-1})  B(\mu \sigma^{-1} \nu,\sigma^{-1}+1)}}{f(y|\mu,\sigma, \nu)=(\Gamma(y+1/\nu)\Beta(y+(\mu\nu)/\sigma, 1/\sigma+1/\nu +1 )/(\Gamma(y+1) \Gamma(1/\nu) \Beta((\mu\nu)/\sigma, 1/\sigma+1) )}
 for \eqn{y=0,1,2,3,...}, \eqn{\mu>0}, \eqn{\sigma>0} and \eqn{\nu>0}, see pp 502-503 of Rigby \emph{et al.} (2019).    

The distribution has mean \eqn{\mu}.


The  definition of the zero adjusted beta negative  binomial distribution, \code{ZABNB} and  the the zero inflated beta negative  binomial  distribution, \code{ZIBNB}, are given in p. 517 and pp. 519 of  of Rigby \emph{et al.} (2019), respectively.
}
\value{
returns a \code{gamlss.family} object which can be used to fit a Poisson distribution in the \code{gamlss()} function.  
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{https://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.
\doi{10.1201/b21973}

(see also  \url{https://www.gamlss.com/}).
}
\author{ Bob Rigby and Mikis Stasinopoulos}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{NBI}}, \code{\link{NBII}}}

\examples{
BNB()   # gives information about the default links for the beta negative binomial
# plotting the distribution
plot(function(y) dBNB(y, mu = 10, sigma = 0.5, nu=2), from=0, to=40, n=40+1, type="h")
# creating random variables and plot them 
tN <- table(Ni <- rBNB(1000, mu=5, sigma=0.5, nu=2))
r <- barplot(tN, col='lightblue')

ZABNB()
ZIBNB()
# plotting the distribution
plot(function(y) dZABNB(y, mu = 10, sigma = 0.5, nu=2, tau=.1),  
     from=0, to=40, n=40+1, type="h")
plot(function(y) dZIBNB(y, mu = 10, sigma = 0.5, nu=2, tau=.1),  
     from=0, to=40, n=40+1, type="h")
\dontrun{
library(gamlss)
data(species)
species <- transform(species, x=log(lake))
m6 <- gamlss(fish~ pb(x), sigma.fo=~1, data=species, family=BNB)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}
