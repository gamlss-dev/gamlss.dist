\name{NBII}
\alias{NBII}
\alias{dNBII}
\alias{pNBII}
\alias{qNBII}
\alias{rNBII}


\title{Negative Binomial type II distribution for fitting a GAMLSS}
\description{
The \code{NBII()} function defines the Negative Binomial type II distribution, a two parameter distribution, for a \code{gamlss.family} object to be used 
in GAMLSS fitting using the function \code{gamlss()}.    
The functions \code{dNBII}, \code{pNBII}, \code{qNBII} and \code{rNBII} define the density, distribution function, quantile function and random
generation for the Negative Binomial type II, \code{NBII()}, distribution. 
}

\usage{
NBII(mu.link = "log", sigma.link = "log")
dNBII(x, mu = 1, sigma = 1, log = FALSE)
pNBII(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qNBII(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rNBII(n, mu = 1, sigma = 1)
}

\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "log" link as the default for the mu parameter}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter}
  \item{x}{ vector of (non-negative integer) quantiles}
  \item{mu}{vector of positive means}
  \item{sigma}{vector of positive despersion parameter}
  \item{p}{vector of probabilities}
  \item{q}{vector of quantiles}
  \item{n}{number of random values to return}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}  
}
\details{
  Definition file for Negative Binomial type II distribution. 
 \deqn{f(y|\mu,\sigma)=\frac{\Gamma(y+(\mu/\sigma)) \sigma^y }{\Gamma(\mu/\sigma)\Gamma(y+1) (1+\sigma)^{y+(\mu/\sigma)}}}{f(y|mu,sigma)=Gamma(y+(mu/sigma))*sigma^y/(Gamma(mu/sigma)*Gamma(y+1)*(1+sigma)^(y+(mu/sigma)))} 
 for \eqn{y=0,1,2,...,\infty}{y=0,1,2, ...,Inf}, \eqn{\mu>0} and \eqn{\sigma>0}. 
 This parameterization was used by Evans (1953)  and also by Johnson \emph{et al.} (1993) p 200.
}
\value{
  returns a \code{gamlss.family} object which can be used to fit a Negative Binomial type II distribution in the \code{gamlss()} function.   
}

\references{ 

Evans, D. A. (1953). Experimental evidence concerning contagious distributions in ecology. \emph{Biometrika}, \bold{40}: 186-211.

Johnson, N. L., Kotz, S. and Kemp, A. W. (1993). \emph{Univariate Discrete Distributions},
2nd edn. Wiley, New York.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}). 

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
 }
\author{Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, Bob Rigby \email{r.rigby@londonmet.ac.uk} and Calliope Akantziliotou}

\note{\eqn{\mu}{mu} is the mean and \eqn{[(1+\sigma)\mu]^{0.5}}{((1+sigma)*mu)**0.5}  is the standard deviation of the Negative Binomial type II distribution, so 
\eqn{\sigma}{sigma} is a dispersion parameter}


\seealso{  \code{\link{gamlss.family}}, \code{\link{NBI}}, \code{\link{PIG}}, 
\code{\link{SI}}}

\examples{
NBII()  # gives information about the default links for the Negative Binomial type II distribution  
# plotting the distribution
plot(function(y) dNBII(y, mu = 10, sigma = 0.5 ), from=0, to=40, n=40+1, type="h")
# creating random variables and plot them 
tN <- table(Ni <- rNBII(1000, mu=5, sigma=0.5))
r <- barplot(tN, col='lightblue')
# library(gamlss)
# data(aids)
# h<-gamlss(y~cs(x,df=7)+qrt, family=NBII, data=aids) # fits a model 
# plot(h)
# pdf.plot(family=NBII, mu=10, sigma=0.5, min=0, max=40, step=1)
}
\keyword{distribution}
\keyword{regression}% 
