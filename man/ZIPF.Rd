\name{ZIPF}
\alias{ZIPF}
\alias{dZIPF}
\alias{pZIPF}
\alias{qZIPF}
\alias{rZIPF}
\alias{zetaP}
\alias{ZAZIPF}
\alias{dZAZIPF}
\alias{pZAZIPF}
\alias{qZAZIPF}
\alias{rZAZIPF}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{The zipf and zero adjusted zipf distributions for fitting a GAMLSS model}
\description{
This function \code{ZIPF()} defines the zipf distribution, Johnson et. al., (2005), sections 11.2.20,  p 527-528. The zipf distribution is an one parameter distribution with long tails (a discete version of the Pareto distrbution).  The function \code{ZIPF()} creates a  \code{gamlss.family} object to be used in GAMLSS fitting. The functions \code{dZIPF}, \code{pZIPF}, \code{qZIPF} and \code{rZIPF} define the density, distribution function, quantile function and random generation for the zipf, \code{ZIPF()}, distribution. The function \code{zetaP()} defines the zeta function and it is based on the zeta function defined on the \code{VGAM} package of Thomas Yee, see Yee (2017). 

The distribution zipf is defined on \eqn{y=1,2,3, \ldots,\infty}{y=1,2,3, ...,Inf}, the zero adjusted zipf permits values on  \eqn{y=,0 1,2, \ldots,\infty}{y=0,1,2,3, ...,Inf}.  The function \code{ZAZIPF()} defines the zero adjusted zipf distribution.  The function \code{ZAZIPF()} creates a  \code{gamlss.family} object to be used in GAMLSS fitting. The functions \code{dZAZIPF}, \code{pZAZIPF}, \code{qZAZIPF} and \code{rZAZIPF} define the density, distribution function, quantile function and random generation for the zero adjusted zipf, \code{ZAZIPF()}, distribution.
}
\usage{
ZIPF(mu.link = "log")
dZIPF(x, mu = 1, log = FALSE)
pZIPF(q, mu = 1, lower.tail = TRUE, log.p = FALSE)
qZIPF(p, mu = 1, lower.tail = TRUE, log.p = FALSE, 
       max.value = 10000)
rZIPF(n, mu = 1, max.value = 10000)
zetaP(x)
ZAZIPF(mu.link = "log", sigma.link = "logit")
dZAZIPF(x, mu = 0.5, sigma = 0.1, log = FALSE)
pZAZIPF(q, mu = 0.5, sigma = 0.1, lower.tail = TRUE, 
        log.p = FALSE)
qZAZIPF(p, mu = 0.5, sigma = 0.1, lower.tail = TRUE, 
       log.p = FALSE, max.value = 10000)
rZAZIPF(n, mu = 0.5, sigma = 0.1, max.value = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.link}{the link function for the parameter \code{mu} with  default \code{log}}
  \item{x,q}{vectors of (non-negative integer) quantiles} 
  \item{p}{vector of probabilities}
  \item{mu}{vector of positive parameter}
  \item{log, log.p}{logical; if \code{TRUE}, probabilities \code{p} are given as \code{log(p)}}
  \item{lower.tail}{logical; if \code{TRUE} (default), probabilities are \code{P[X <= x]}, otherwise, \code{P[X > x]}}
  \item{n}{number of random values to return}
  \item{max.value}{a constant, set to the default value of 10000, It is used in the \code{q} function which numerically calculates how far the algorithm should look for q. Maybe for zipf data the values has to increase at a considerable computational cost.}
    \item{sigma.link}{the link function for the parameter \code{aigma} with  default \code{logit}}
      \item{sigma}{a vector of probabilities of zero}
  }
  
\details{
 The probability density for the zipf distribution, \code{ZIPF}, is: 
 \deqn{f(y|\mu)=\frac{y^{-(\mu+1)}}{\zeta(\mu+1)}}{f(y|mu)= y^{-(\mu+1)}/\zeta(\mu+1)} 
 for \eqn{y=1,2,\ldots,\infty}{y=1,2, ...,Inf}, \eqn{\mu>0} and where 
 \eqn{\zeta() = \sum_i^n i^{-b}} is the (Reimann) zeta function.
 
The distribution has mean \eqn{\zeta(\mu)/\zeta(\mu+1)} and variance \eqn{{\zeta(\mu+1)\zeta(\mu-1)-[\zeta(\mu)]^2 }/ [\zeta(\mu+1)]^2}, see  pp 479-480 of Rigby et al. (2019) 

For more details about the zero-adjusted Zipf distributions, \code{ZAZIPF}, see see pp 496-498 of Rigby \emph{et al.} (2019).    
}  
  
\value{
The function \code{ZIPF()} returns a \code{gamlss.family} object which can be used to fit a zipf distribution in the \code{gamlss()} function.
}
\references{


N. L. Johnson, A. W. Kemp, and S. Kotz. (2005) Univariate Discrete Distributions.
Wiley, New York, 3rd edition.

Thomas W. Yee (2017). VGAM: Vector Generalized Linear and Additive Models. R
  package version 1.0-3. \url{https://CRAN.R-project.org/package=VGAM}
  
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
\author{Mikis Stasinopoulos and Bob Rigby}
\note{Because the zipf distrbution  has very long tails the \code{max.value} in the \code{q} and \code{r}, may  need to increase.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{PO}}, \code{\link{LG}}, \code{\link{GEOM}}, \code{\link{YULE}}}
\examples{
# ZIPF
par(mfrow=c(2,2))
y<-seq(1,20,1)
plot(y, dZIPF(y), type="h")
q <- seq(1, 20, 1)
plot(q, pZIPF(q), type="h")
p<-seq(0.0001,0.999,0.05)
plot(p , qZIPF(p), type="s")
dat <- rZIPF(100)
hist(dat)
# ZAZIPF
y<-seq(0,20,1)
plot(y, dZAZIPF(y,  mu=.9, sigma=.1), type="h")
q <- seq(1, 20, 1)
plot(q, pZAZIPF(q,  mu=.9, sigma=.1), type="h")
p<-seq(0.0001,0.999,0.05)
plot(p, qZAZIPF(p,  mu=.9, sigma=.1), type="s")
dat <- rZAZIPF(100, mu=.9, sigma=.1)
hist(dat)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}
