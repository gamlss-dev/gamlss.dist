#-------------------------------------------------------------------------------
# Bob Rigby  Mikis Stsinopoulos Marco Enea and  Fernanda de Bastiani
# last change Monday, 10  August 2017
# checked 24th of June 2024
# there is a problem with the cdf and conseqntly with the q=fun
# the Double Binomial  distribution 
# needs C code to calculate the constance of summation
#------------------------------------------------------------------------------
# TO DO
# the cdf should be fixed
#
#-------------------------------------------------------------------------------
#
# dyn.load("~/Dropbox/gamlss/R-code/Distributions/double binomial marco/getBI_C2.so")
# is.loaded("getBI_C2")
# ------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# numerical derivatives for logC 
DBI <- function (mu.link = "logit", sigma.link = "log") 
{
  mstats <- checklink("mu.link", "Double Poisson", substitute(mu.link), 
                      c("logit", "probit", "cloglog", "cauchit", "log", "own"))
  dstats <- checklink("sigma.link", "Double Poisson", substitute(sigma.link), 
                      c("inverse", "log", "identity", "sqrt"))
  structure(
    list(    family = c("DBI", "Double Binomial"),
         parameters = list(mu = TRUE,sigma = TRUE), 
              nopar = 2, 
               type = "Discrete", 
            mu.link = as.character(substitute(mu.link)),
         sigma.link = as.character(substitute(sigma.link)), 
         mu.linkfun = mstats$linkfun, 
      sigma.linkfun = dstats$linkfun, 
         mu.linkinv = mstats$linkinv, 
      sigma.linkinv = dstats$linkinv,
              mu.dr = mstats$mu.eta, 
           sigma.dr = dstats$mu.eta, 
               dldm = function(y, mu, sigma, bd)
               {
                 y/(mu*sigma)-(bd-y)/((1-mu)*sigma)+as.vector(attr(numeric.deriv(GetBI_C(mu, sigma, bd), "mu"),"gradient"))
                 }, 
            d2ldm2 = function(y, mu, sigma, bd) 
                 {
               dldm <- y/(mu*sigma)-(bd-y)/((1-mu)*sigma)+as.vector(attr(numeric.deriv(GetBI_C(mu, sigma, bd), "mu"),"gradient"))
           d2ldm2 <- -dldm^2
           d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2,-1e-15)    
           d2ldm2  
         },
         dldd = function(y, mu, sigma, bd)
         { 
             logofy <- ifelse(y==0,1,log(y))
           logofn_y <- ifelse(bd==y, 1,log(bd-y))
              dldd <- (y*logofy)/sigma^2-(log(mu)*y)/sigma^2+(logofn_y*(bd-y))/sigma^2-
                      (log(1-mu)*(bd-y))/sigma^2-(bd*log(bd))/sigma^2+
                      as.vector(attr(numeric.deriv(GetBI_C(mu, sigma, bd), "sigma"),"gradient"))
         dldd
         },
         d2ldd2 = function(y, mu, sigma, bd) {
            logofy <- ifelse(y==0,1,log(y))
          logofn_y <- ifelse(bd==y,1,log(bd-y))
              dldd <- (y*logofy)/sigma^2-(log(mu)*y)/sigma^2+(logofn_y* (bd-y))/sigma^2-
                      (log(1-mu)*(bd-y))/sigma^2-(bd*log(bd))/sigma^2+
                       as.vector(attr(numeric.deriv(GetBI_C(mu, sigma, bd), "sigma"),"gradient"))
             d2ldd2 <- -dldd^2
             d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2,-1e-15)       
             d2ldd2               
         }, #change this
             d2ldmdd = function(y) rep(0,length(y)),     
         G.dev.incr  = function(y, mu, sigma, bd,...) -2*dDBI(y, mu, sigma, bd, log = TRUE), 
               rqres = expression(
           rqres(pfun="pDBI", type="Discrete", ymin=0, bd=bd, y=y, mu=mu, sigma=sigma)
                       ), 
          mu.initial =  expression({mu <- (y + 0.5)/(bd + 1)}),
       sigma.initial = expression(sigma <- rep(1.1,length(y))),
            mu.valid = function(mu) all(mu > 0) && all(mu < 1), 
         sigma.valid = function(sigma)  all(sigma > 0), 
             y.valid = function(y)  all(y >= 0)
    ),
     class = c("gamlss.family","family"))
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# C function in R for checking
# getBI1_C <- function( mu, sigma,  bd)
# { 
#   ly <- max(length(bd),length(mu),length(sigma)) 
#   bd <- rep(bd, length = ly)
#   sigma <- rep(sigma, length = ly)
#   mu <- rep(mu, length = ly) 
#   theC <- rep(0, ly) 
#   for (i in 1:ly)
#   {
#     x <- 0:bd[i]
#     logofx <- ifelse(x==0,1,log(x))
#     logofbd_x <- ifelse(bd[i]==x,1,log(bd[i]-x))
#     ss <- lchoose(bd[i],x)+x*logofx+(bd[i]-x)*logofbd_x-bd[i]*log(bd[i])+
#       bd[i]*sigma[i]*log(bd[i]) + sigma[i]*x*log(mu[i])+sigma[i]*(bd[i]-x)*log(1-mu[i])-
#       sigma[i]*x*logofx - sigma[i]*(bd[i]-x)*logofbd_x
#     expss <- exp(ss)
#     theC[i] <- log(1/sum(expss))
#   }
#   theC
# }
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# the C function calling C-code
# Note that the original parametrization of sigma was 1/sigma
# That is the C(mu, sigma, n) function was writted for 
# 1/sigma so we have to inverse sigma here
# the function returns res=-log(C)=1/log(C)
GetBI_C <- function(mu, sigma, bd)
{ 
     ly <- max(length(bd),length(mu),length(sigma)) 
     bd <- rep(bd, length = ly)
  sigma <- rep(1/sigma, length = ly)# inverse sigma 
     mu <- rep(mu, length = ly) 
   TheC <- .C("getBI_C2",as.double(mu),as.double(sigma),as.double(bd),
             as.integer(ly), theC=double(ly))$theC
  TheC          
}          
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# the d function
#-------------------------------------------------------------------------------
dDBI <- function(x, mu = .5, sigma = 1, bd=2,  log = FALSE)
{ 
  if (any(mu < 0) | any(mu > 1))  stop(paste("mu must be between 0 and 1", "\n", "")) 
 # if (any(bd < x))  warning(paste("x  must be <=  than the binomial denominator", bd, "\n")) 
  if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
  if (any(sigma < 1e-10)) warning(" values of sigma in BB less that 1e-10 are set to 1e-10" )
       ly <- max(length(x),length(bd),length(mu),length(sigma)) 
       ll <- rep(0, length = ly)
   logofx <- rep(1, length = ly)
logofbd_x <- rep(1, length = ly)  
       xx <- rep(x, length = ly)
       xx[x<0 | x>bd] <- 1
       bd <- rep(bd, length = ly)
    sigma <- rep(sigma, length = ly)
       mu <- rep(mu, length = ly) 
logofx[xx!=0] <- log(xx)[xx!=0]
#   logofx <- ifelse(xx==0,1,log(xx))
logofbd_x[bd<xx] <-  log(bd-xx) 
#logofbd_x <- ifelse(bd==xx,1,log(bd-xx))
      res <- GetBI_C(mu,sigma,bd)
       
ll[abs(sigma-1) >= 0.001] <-  lchoose(bd,xx)+xx*logofx+(bd-xx)*logofbd_x-bd*log(bd)+
                              (bd/sigma)*log(bd) + (xx/sigma)*log(mu)+((bd-xx)/sigma)*log(1-mu)-
                              (xx/sigma)*logofx - ((bd-xx)/sigma)*logofbd_x+res
ll[abs(sigma-1) < 0.001] <-dbinom(xx, size = bd, prob = mu, log = TRUE)
     #  ll <- ifelse((abs(sigma-1) < 0.001), ,
                  
  if(log==FALSE) fy <- exp(ll) else fy <- ll 
fy[x < 0] <- 0 
fy[x >bd] <- 0 
       fy     
}
#-------------------------------------------------------------------------------
#  The p function  
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
pDBI<-function(q, mu = .5, sigma = 1, bd=2, lower.tail = TRUE, log.p = FALSE)
{ 
  if (any(mu < 0) | any(mu > 1))  stop(paste("mu must be between 0 and 1", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
     ly <- max(length(q),length(mu),length(sigma)) 
      q <- rep(q, length = ly)      
  sigma <- rep(sigma, length = ly)
     mu <- rep(mu, length = ly) 
     bd <- rep(bd, length = ly)  
        fn <- function(q, mu, sigma, bd) sum(dDBI(0:q, mu=mu, sigma=sigma, bd=bd))
      Vcdf <- Vectorize(fn)
       cdf <- Vcdf(q=q, mu=mu, sigma=sigma, bd=bd)     
       cdf <- if(lower.tail==TRUE) cdf else 1-cdf
       cdf <- if(log.p==FALSE) cdf else log(cdf)   
  cdf[q<0] <- 0
 cdf[q>bd] <- 1
       cdf
}
#-------------------------------------------------------------------------------
# the q function
#-------------------------------------------------------------------------------
qDBI <- function(p, mu = .5, sigma = 1, bd=2, lower.tail = TRUE, log.p = FALSE  )
{      
  if (any(mu < 0) | any(mu > 1))  stop(paste("mu must be between 0 and 1", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
  # if (any(p < 0) | any(p > 1.0001))  stop(paste("p must be between 0 and 1", "\n", "")) 
  if (log.p==TRUE) p <- exp(p) 
  if (lower.tail==FALSE)  p <- 1-p 
         ly <- max(length(p),length(mu),length(sigma)) 
          p <- rep(p, length = ly)      
          
      sigma <- rep(sigma, length = ly)
         mu <- rep(mu, length = ly) 
         bd <- rep(bd, length = ly) 
        QQQ <- rep(0,ly)                         
     nsigma <- rep(sigma, length = ly)
       nmu <- rep(mu, length = ly)                
  for (i in seq(along=p))                                                          
  {
    cumpro <- 0                                                                         
    if (p[i]+0.000000001 >= 1) {QQQ[i] <- bd[i]}
    else  
    {  
      for (j in seq(from = 0, to = bd[i]))
      {
        cumpro <-  pDBI(j, mu = mu[i], sigma = sigma[i], bd=bd[i], log.p = FALSE) 
        # else  cumpro+dSICHEL(j, mu = nmu[i], sigma = nsigma[i], nu = nnu[i], log = FALSE)# the above is faster 
        QQQ[i] <- j 
        if  (p[i] <= cumpro ) break 
      } 
    }
  }          
       QQQ[p == 0] <- 0
       QQQ[p == 1] <- bd
       QQQ[p <  0] <- NaN
       QQQ[p >  1] <- NaN
       return(QQQ)    
} 
#-------------------------------------------------------------------------------
# the r function 
#-------------------------------------------------------------------------------
rDBI <- function(n,mu = .5, sigma = 1, bd=2)
{ 
  if (any(mu < 0) | any(mu > 1))  stop(paste("mu must be between 0 and 1", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", ""))  
  if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
  n <- ceiling(n)
  p <- runif(n)
  r <- qDBI(p, mu=mu, sigma=sigma, bd = bd )
  as.integer(r)
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------




