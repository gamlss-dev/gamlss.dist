\name{DBURR12}
\alias{DBURR12}
\alias{dDBURR12}
\alias{pDBURR12}
\alias{qDBURR12}
\alias{rDBURR12}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The Discrete Burr type XII  distribution for fitting a GAMLSS model
}
\description{
The \code{DBURR12()} function defines the discrete Burr type XII  distribution, a three parameter discrete distribution, for a \code{gamlss.family} object to be used  in GAMLSS fitting using the function \code{gamlss()}.    
The functions \code{dDBURR12()}, \code{pDBURR12()}, \code{qDBURR12()} and \code{rDBURR12()} define the density, distribution function, quantile function and random generation for the discrete Burr type XII  \code{DBURR12()}, distribution. 
}
\usage{
DBURR12(mu.link = "log", sigma.link = "log", nu.link = "log")
dDBURR12(x, mu = 5, sigma = 2, nu = 2, log = FALSE)
pDBURR12(q, mu = 5, sigma = 2, nu = 2, lower.tail = TRUE, 
        log.p = FALSE)
qDBURR12(p, mu = 5, sigma = 2, nu = 2, lower.tail = TRUE, 
        log.p = FALSE)
rDBURR12(n, mu = 5, sigma = 2, nu = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "log" link as the  default for the \code{mu} parameter}
 \item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the \code{sigma} parameter}
 \item{nu.link}{ Defines the \code{nu.link}, with "log" link as the default for the \code{nu} parameter}
  \item{x}{
vector of (non-negative integer) quantiles
}
  \item{p}{
vector of probabilities
}
  \item{q}{
vector of quantiles
}
  \item{mu}{
vector of positive \code{mu}
}
  \item{sigma}{
vector of positive dispersion parameter \code{sigma}
}
  \item{nu}{
vector of \code{nu}
}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{ logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}  
  \item{n}{number of random values to return}
}

\details{
The probability function of the discrete Burr XII  distribution is given by
\deqn{f(y|\mu,\sigma,\nu)= (1+(y/\mu)^\sigma)^\nu - (1+((y+1)/\mu)^\sigma)^\nu}{ f(y|\mu,\sigma,\nu)= (1+(y/\mu)^\sigma)^\nu - (1+((y+1)/\mu)^\sigma)^\nu } 

for \eqn{y=0,1,2,...,\infty}{y=0,1,2,...},   \eqn{\mu>0}{mu>0} , \eqn{\sigma>0}{\sigma>0} and \eqn{\mu>0}{nu>0}  see pp 504-505 of Rigby \emph{et al.} (2019).     

Note that the above parametrization is different from Para and Jan  (2016).
}
\value{
 The function \code{DBURR12()} Returns a \code{gamlss.family} object which can be used to fit a discrete Burr XII distribution in the \code{gamlss()} function. 
}
\references{

Para, B. A. and Jan, T. R. (2016). On discrete three parameter Burr type XII and discrete Lomax distributions and their applications to model count data from medical science. \emph{Biometrics and Biostatistics International Journal}, \bold{54}, part 3, pp 507-554.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{4},  pp 1-15.

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
Rigby, R. A.,  Stasinopoulos D. M.,  Fernanda De Bastiani.}
\note{
The parameters  of the distributioins are highly correlated  so the argument of \code{gamlss} \code{method=mixed(10,100)} may have to be used.

The distribution can be under/over dispersed and also with long tails.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{gamlss.family}}, \code{\link{DPO}}}

\examples{
DBURR12()# 
#plot the pdf using plot 
plot(function(y) dDBURR12(y, mu=10, sigma=1, nu=1), from=0, to=100, n=100+1, type="h") # pdf
# plot the cdf
plot(seq(from=0,to=100),pDBURR12(seq(from=0,to=100), mu=10, sigma=1, nu=1), type="h")   # cdf
# generate random sample
tN <- table(Ni <- rDBURR12(100, mu=5, sigma=1, nu=1))
r <- barplot(tN, col='lightblue')
}
  
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}% 
