\name{ZIPIG}
\alias{ZIPIG}
\alias{dZIPIG}
\alias{pZIPIG}
\alias{qZIPIG}
\alias{rZIPIG}


\title{Zero inflated Poisson inverse Gaussian distributions for fitting a GAMLSS model}

\description{
The function \code{ZIPIG} defines the zero inflated  Poisson inverse Gaussian distribution, 
a three  parameter distribution, for a 
\code{gamlss.family} object to be used in GAMLSS fitting  using the function \code{gamlss()}. 
The functions \code{dZIPIG}, \code{pZIPIG}, 
\code{qZIPIG} and \code{rZIPIG} define the 
density, distribution function, quantile function 
and random generation for the zero inflated negative binomial, \code{ZIPIG()}, distribution. 
}
\usage{
ZIPIG(mu.link = "log", sigma.link = "log", nu.link = "logit")
dZIPIG(x, mu = 1, sigma = 1, nu = 0.3, log = FALSE)
pZIPIG(q, mu = 1, sigma = 1, nu = 0.3, lower.tail = TRUE, log.p = FALSE)
qZIPIG(p, mu = 1, sigma = 1, nu = 0.3, lower.tail = TRUE, log.p = FALSE)
rZIPIG(n, mu = 1, sigma = 1, nu = 0.3)
}

\arguments{
\item{mu.link}{ Defines the \code{mu.link}, with "log" link as the default for the mu parameter}
  \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter}
  \item{nu.link}{Defines the \code{mu.link}, with "logit" link as the default for the nu parameter}
  \item{x}{ vector of (non-negative integer) quantiles}
  \item{mu}{vector of positive means}
  \item{sigma}{vector of positive despersion parameter}
  \item{nu}{vector of zero probability parameter}
  \item{p}{vector of probabilities}
  \item{q}{vector of quantiles}
  \item{n}{number of random values to return}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}  
}


\details{
For the definition of the distribution see   Rigby and Stasinopoulos (2010) below.
}

\value{The function \code{ZIPIG} return a \code{gamlss.family} object which can be used to fit a
zero inflated  Poisson inverse Gaussian in the \code{gamlss()} function}

\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).  

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.

Rigby, R. A.  and Stasinopoulos D. M. (2010) The gamlss.family distributions, (distributed with this package or see 
\url{http://www.gamlss.com/}) 
}

\author{Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, Bob Rigby \email{r.rigby@londonmet.ac.uk} }



\seealso{\code{\link{gamlss.family}}, \code{\link{PIG}}}

\examples{
ZIPIG()
# creating data and plotting them 
 dat <- rZIPIG(1000, mu=5, sigma=.5, nu=0.1)
   r <- barplot(table(dat), col='lightblue')
 }


\keyword{distribution}
\keyword{regression}% 
