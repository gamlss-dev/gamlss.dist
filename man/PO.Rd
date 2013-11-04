\name{PO}
\alias{PO}
\alias{dPO}
\alias{pPO}
\alias{qPO}
\alias{rPO}


\title{Poisson distribution for fitting a GAMLSS model}
\description{
This function \code{PO} defines the Poisson distribution, an one parameter distribution, for a \code{gamlss.family} object to be used in GAMLSS fitting 
using the function \code{gamlss()}. The functions \code{dPO}, \code{pPO}, \code{qPO} and \code{rPO} define the density, distribution function, quantile function and random
generation for the Poisson, \code{PO()}, distribution. 
    
}
\usage{
PO(mu.link = "log")
dPO(x, mu = 1, log = FALSE)
pPO(q, mu = 1, lower.tail = TRUE, log.p = FALSE)
qPO(p, mu = 1, lower.tail = TRUE, log.p = FALSE)
rPO(n, mu = 1)
}

\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "log" link as the default for the mu parameter}
  \item{x}{ vector of (non-negative integer) quantiles}
  \item{mu}{vector of positive means}
  \item{p}{vector of probabilities}
  \item{q}{vector of quantiles}
  \item{n}{number of random values to return}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}
  }
\details{
  Definition file for Poisson distribution. 
 \deqn{f(y|\mu)=\frac{e^{-\mu}\mu^y}{\Gamma(y+1)}}{f(y|mu)=e^(-mu)*mu^y/\Gamma(y+1)}
 for \eqn{y=0,1,2,...} and \eqn{\mu>0}. 
}
\value{
  returns a \code{gamlss.family} object which can be used to fit a Poisson distribution in the \code{gamlss()} function.   
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{ Bob Rigby \email{r.rigby@londonmet.ac.uk}, Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, and Kalliope Akantziliotou }
\note{\eqn{\mu}{mu} is the mean of the Poisson distribution}


\seealso{ \code{\link{gamlss.family}},  \code{\link{NBI}}, \code{\link{NBII}}, 
\code{\link{SI}}, \code{\link{SICHEL}}}

\examples{
PO()# gives information about the default links for the Poisson distribution  
# fitting data using PO()

# plotting the distribution
plot(function(y) dPO(y, mu=10 ), from=0, to=20, n=20+1, type="h")
# creating random variables and plot them 
tN <- table(Ni <- rPO(1000, mu=5))
 r <- barplot(tN, col='lightblue')
# library(gamlss)
# data(aids)
# h<-gamlss(y~cs(x,df=7)+qrt, family=PO, data=aids) # fits the constant+x+qrt model 
# plot(h)
# pdf.plot(family=PO, mu=10, min=0, max=20, step=1)
}
\keyword{distribution}
\keyword{regression}% 
