# RAR+KA+MS Friday, April 5, 2002 at 15:10
# last change Tuesday, December 14, 2004 Saturday, October 8, 2005 
BI <- function (mu.link = "logit") 
{
  mstats <- checklink("mu.link", "Binomial", substitute(mu.link),
                      c("logit", "probit", "cloglog", "cauchit", "log", "own"))# ms 8-10-05
  structure(
    list(family = c("BI", "Binomial"),
         parameters = list(mu=TRUE), 
         nopar = 1,
         type = "Discrete",
         mu.link = as.character(substitute(mu.link)),  
         mu.linkfun = mstats$linkfun, 
         mu.linkinv = mstats$linkinv, 
         mu.dr = mstats$mu.eta,
         dldm = function(y, mu, bd) (y-bd*mu)/(mu*(1-mu)),
         d2ldm2 = function(mu,bd) -(bd/(mu*(1-mu))),
         G.dev.incr  = function(y,mu,bd,...)  -2*dBI(y,bd,mu,log=TRUE),
         rqres = expression(
           rqres(pfun="pBI", type="Discrete", ymin=0, y=y, mu=mu, bd=bd)
         ), #
         mu.initial = expression({mu <- (y + 0.5)/(bd + 1)}),
         mu.valid = function(mu) all(mu > 0) && all(mu < 1),  
         y.valid = function(y)  all(y >= 0),
         mean = function(bd ,mu) bd * mu,
         variance = function(bd, mu) bd * mu * (1 - mu)
    ),
    class = c("gamlss.family","family") )
}
#------------------------------------------------------------------------------------------
dBI<-function(x, bd = 1, mu = 0.5, log = FALSE)
{ 
  if (any(mu < 0) | any(mu > 1))  stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(bd < x))  warning(paste("x  must be <=  than the binomial denominator", bd, "\n"))   
  ly <- max(length(x),length(mu),length(bd)) 
   x <- rep(x, length = ly)      
  mu <- rep(mu, length = ly)   
  bd <- rep(bd, length = ly) 
  fy <- dbinom(x, size = bd, prob = mu, log = log)
  fy[x < 0] <- 0 
  fy
}
#------------------------------------------------------------------------------------------
pBI <- function(q, bd=1, mu=0.5, lower.tail = TRUE, log.p = FALSE)
{     
  if (any(mu < 0) | any(mu > 1))  stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(bd < q))  warning(paste("q  must be <=  than the binomial denominator", bd, "\n"))   
 ly <- max(length(q),length(mu),length(bd)) 
    q <- rep(q, length = ly)      
   mu <- rep(mu, length = ly)   
   bd <- rep(bd, length = ly)  
  cdf <- pbinom(q, size = bd, prob = mu, lower.tail=lower.tail, log.p=log.p)
  cdf <- ifelse(q < 0, 0, cdf) 
  cdf[q<0] <- 0
  cdf[q>bd] <- 1
  cdf
}
#------------------------------------------------------------------------------------------
qBI <- function(p, bd = 1, mu = 0.5,  lower.tail = TRUE, log.p = FALSE)
{      
  if (any(mu < 0) | any(mu > 1))  stop(paste("mu must be between 0 and 1", "\n", ""))
     ly <- max(length(p),length(mu), length(bd)) 
      p <- rep(p, length = ly)      
     mu <- rep(mu, length = ly)   
     bd <- rep(bd, length = ly)   
   qfun <- qbinom(p, size = bd, prob = mu, lower.tail = lower.tail, log.p = log.p)
   qfun[p == 0] <- 0
   qfun[p == 1] <- bd
   qfun[p <  0] <- NaN
   qfun
}
#------------------------------------------------------------------------------------------
rBI <- function(n, bd = 1, mu = 0.5)
{ 
  if (any(mu < 0) | any(mu > 1))  stop(paste("mu must be between 0 and 1", "\n", "")) 
  if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
  n <- ceiling(n)
  p <- runif(n)
  r <- qbinom(p, size = bd, prob = mu)
  as.integer(r)
}
#-------------------------------------------------------------------------------------------
