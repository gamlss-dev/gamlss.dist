\name{ZANBI}
\alias{ZANBI}
\alias{dZANBI}
\alias{pZANBI}
\alias{qZANBI}
\alias{rZANBI}
\alias{ZINBI}
\alias{dZINBI}
\alias{pZINBI}
\alias{qZINBI}
\alias{rZINBI}


\title{
Zero inflated and zero adjusted negative binomial distributions for fitting a GAMLSS model
}
\description{
The function \code{ZINBI} defines the zero inflated  negative binomial distribution, a three  parameter distribution, for a 
\code{gamlss.family} object to be used in GAMLSS fitting  using the function \code{gamlss()}. The functions \code{dZINBI}, \code{pZINBI}, 
\code{qZINBI} and \code{rZINBI} define the 
density, distribution function, quantile function 
and random generation for the zero inflated negative binomial, \code{ZINBI()}, distribution. 

The function \code{ZANBI} defines the zero adjusted negative binomial distribution, a three  parameter distribution, for a 
\code{gamlss.family} object to be used in GAMLSS fitting  using the function \code{gamlss()}. The functions \code{dZANBI}, \code{pZANBI}, 
\code{qZANBI} and \code{rZANBI} define the 
density, distribution function, quantile function 
and random generation for the zero inflated negative binomial, \code{ZANBI()}, distribution.  
}
\usage{
ZINBI(mu.link = "log", sigma.link = "log", nu.link = "logit")
dZINBI(x, mu = 1, sigma = 1, nu = 0.3, log = FALSE)
pZINBI(q, mu = 1, sigma = 1, nu = 0.3, lower.tail = TRUE, log.p = FALSE)
qZINBI(p, mu = 1, sigma = 1, nu = 0.3, lower.tail = TRUE, log.p = FALSE)
rZINBI(n, mu = 1, sigma = 1, nu = 0.3)
ZANBI(mu.link = "log", sigma.link = "log", nu.link = "logit")
dZANBI(x, mu = 1, sigma = 1, nu = 0.3, log = FALSE)
pZANBI(q, mu = 1, sigma = 1, nu = 0.3, lower.tail = TRUE, log.p = FALSE)
qZANBI(p, mu = 1, sigma = 1, nu = 0.3, lower.tail = TRUE, log.p = FALSE)
rZANBI(n, mu = 1, sigma = 1, nu = 0.3)
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
The  definition for the zero inflated Negative Binomial type I distribution and 
for the zero adjusted Negative Binomial type I distribution is given in Rigby and Stasinopoulos (2010) below 
}
\value{
The functions \code{ZINBI} and \code{ZANBI} return a \code{gamlss.family} object which can be used to fit a
zero inflated or zero adjusted Negative Binomial type I distribution respectively in the \code{gamlss()} function.   
}
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
\author{
Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, Bob Rigby \email{r.rigby@londonmet.ac.uk} 
}

\seealso{
 \code{\link{gamlss.family}}, \code{\link{NBI}}, \code{\link{NBII}}}

\examples{
ZINBI() 
ZANBI()
# creating data and plotting them 
 dat <- rZINBI(1000, mu=5, sigma=.5, nu=0.1)
   r <- barplot(table(dat), col='lightblue')
dat1 <- rZANBI(1000, mu=5, sigma=.5, nu=0.1)
   r1 <- barplot(table(dat1), col='lightblue')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}% 
