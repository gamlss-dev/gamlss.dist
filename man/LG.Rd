\name{LG}
\alias{LG}
\alias{dLG}
\alias{pLG}
\alias{qLG}
\alias{rLG}
\alias{ZALG}
\alias{dZALG}
\alias{pZALG}
\alias{qZALG}
\alias{rZALG}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{Logarithmic and zero adjusted logarithmic distributions for fitting a GAMLSS model}
\description{
The function \code{LG} defines the logarithmic distribution, a one parameter distribution, for a \code{gamlss.family} object to be 
used in GAMLSS fitting  using the function \code{gamlss()}. The functions \code{dLG}, \code{pLG}, \code{qLG} and \code{rLG} define the 
density, distribution function, quantile function 
and random generation for the logarithmic  , \code{LG()}, distribution. 

The function \code{ZALG} defines the zero adjusted logarithmic  distribution, a two parameter distribution, for a \code{gamlss.family} object to be 
used in GAMLSS fitting  using the function \code{gamlss()}. The functions \code{dZALG}, \code{pZALG}, \code{qZALG} and \code{rZALG} define the 
density, distribution function, quantile function 
and random generation for the inflated logarithmic , \code{ZALG()}, distribution. 
}
\usage{
LG(mu.link = "logit")
dLG(x, mu = 0.5, log = FALSE)
pLG(q, mu = 0.5, lower.tail = TRUE, log.p = FALSE)
qLG(p, mu = 0.5, lower.tail = TRUE, log.p = FALSE, max.value = 10000)
rLG(n, mu = 0.5)
ZALG(mu.link = "logit", sigma.link = "logit")
dZALG(x, mu = 0.5, sigma = 0.1, log = FALSE)
pZALG(q, mu = 0.5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
qZALG(p, mu = 0.5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
rZALG(n, mu = 0.5, sigma = 0.1)
}

\arguments{
 \item{mu.link}{defines the \code{mu.link}, with \code{logit} link as the default for the \code{mu} parameter}
  \item{sigma.link}{defines the \code{sigma.link}, with \code{logit} link as the default for the sigma parameter which in this case 
                   is the probability at zero.}
  \item{x}{ vector of (non-negative integer)}
  \item{mu}{vector of positive means}
  \item{sigma}{vector of probabilities at zero}
  \item{p}{vector of probabilities}
  \item{q}{vector of quantiles}
  \item{n}{number of random values to return}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}  
  \item{max.value}{valued needed for the numerical calculation of the q-function}
}
\details{
For the definition of the distributions see   Rigby and Stasinopoulos (2010) below.
}

\value{
The function \code{LG} and \code{ZALG} return a \code{gamlss.family} object which can be used to fit a 
logarithmic and a zero inflated logarithmic  distributions respectively  in the \code{gamlss()} function.  
}
\references{
Johnson, Norman Lloyd; Kemp, Adrienne W; Kotz, Samuel (2005). "Chapter 7: Logarithmic and Lagrangian distributions". 
Univariate discrete distributions (3 ed.). John Wiley & Sons. ISBN 9780471272465.  

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


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gamlss.family}}, \code{\link{PO}}, \code{\link{ZAP}} 
}
\examples{
LG() 
ZAP()
# creating data and plotting them 
 dat <- rLG(1000, mu=.3)
   r <- barplot(table(dat), col='lightblue')
dat1 <- rZALG(1000, mu=.3, sigma=.1)
  r1 <- barplot(table(dat1), col='lightblue')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}

