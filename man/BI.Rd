\name{BI}
\alias{BI}
\alias{pBI}
\alias{dBI}
\alias{qBI}
\alias{rBI}

\title{Binomial distribution for fitting a GAMLSS}
\description{ 
The \code{BI()} function defines the binomial distribution, a one parameter family distribution, for a \code{gamlss.family} object to be used 
in GAMLSS fitting using the function \code{gamlss()}.    
The functions \code{dBI}, \code{pBI}, \code{qBI} and \code{rBI} define the density, distribution function, quantile function and random
generation for the binomial, \code{BI()}, distribution. 
}
\usage{
BI(mu.link = "logit")
dBI(x, bd = 1, mu = 0.5, log = FALSE)
pBI(q, bd = 1, mu = 0.5, lower.tail = TRUE, log.p = FALSE)
qBI(p, bd = 1, mu = 0.5, lower.tail = TRUE, log.p = FALSE)
rBI(n, bd = 1, mu = 0.5)
}

\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "logit" link as the default for the \code{mu} parameter.  Other links are "probit" and "cloglog"'(complementary log-log)}
  \item{x}{ vector of (non-negative integer) quantiles}
  \item{mu}{vector of positive probabilities}
  \item{bd}{vector of binomial denominators}
  \item{p}{vector of probabilities}
  \item{q}{vector of quantiles}
  \item{n}{number of random values to return}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}  
  }
\details{
  Definition file for binomial distribution. 
 \deqn{f(y|\mu)=\frac{\Gamma(n+1)}{\Gamma(y+1) \Gamma{(n-y+1)}} \mu^y (1-\mu)^{(n-y)}}{f(y|mu)=(Gamma(n+1)*Gamma(y+1)/Gamma(n-y+1))* mu^y *(1-mu)^(n-y)} 
 for \eqn{y=0,1,2,...,n} and \eqn{0<\mu< 1}. 
}
\value{
  returns a \code{gamlss.family} object which can be used to fit a binomial distribution in the \code{gamlss()} function.   
}
\references{ 
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{Mikis Stasinopoulos, Bob Rigby and Calliope Akantziliotou}
\note{ The response variable should be a matrix containing two columns, the first with the count of successes and the second with the count of failures. 
      The parameter \code{mu} represents a probability parameter with limits \eqn{0 < \mu < 1}{0 < mu <1}.
       \eqn{n\mu}{n*mu} is the mean of the distribution where n is the binomial denominator. }


\seealso{ \code{\link{gamlss.family}}, \code{\link{ZABI}},  \code{\link{ZIBI}}}

\examples{
 BI()# gives information about the default links for the Binomial distribution 
# data(aep)   
# library(gamlss)
# h<-gamlss(y~ward+loglos+year, family=BI, data=aep)  
# plot of the binomial distribution
curve(dBI(x, mu = .5, bd=10), from=0, to=10, n=10+1, type="h")
tN <- table(Ni <- rBI(1000, mu=.2, bd=10))
r <- barplot(tN, col='lightblue')
}
\keyword{distribution}
\keyword{regression}% 
