BE <- function (mu.link = "logit", sigma.link = "logit") 
{
    mstats <- checklink("mu.link", "Beta", substitute(mu.link), c("logit", "probit", "cloglog", "cauchit", "log", "own"))
    dstats <- checklink("sigma.link", "Beta", substitute(sigma.link),  c("logit", "probit", "cloglog", "cauchit", "log", "own"))    
    structure(
          list(family = c("BE", "Beta"),
           parameters = list(mu=TRUE,sigma=TRUE), 
                nopar = 2,  
                 type = "Continuous", 
              mu.link = as.character(substitute(mu.link)),
           sigma.link = as.character(substitute(sigma.link)), 
           mu.linkfun = mstats$linkfun, 
        sigma.linkfun = dstats$linkfun, 
           mu.linkinv = mstats$linkinv, 
        sigma.linkinv = dstats$linkinv,
                mu.dr = mstats$mu.eta, 
             sigma.dr = dstats$mu.eta, 
                 dldm = function(y,mu,sigma) 
                              { 
                        a <- mu*(1-sigma^2)/(sigma^2)
                        b <- a*(1-mu)/mu
                     dldm <- ((1-sigma^2)/(sigma^2))*( -digamma(a)
                                  +digamma(b) +log(y) - log(1-y) )
                              dldm 
                              },
               d2ldm2 = function(mu,sigma) 
                              { 
                        a <- mu*(1-sigma^2)/(sigma^2)
                        b <- a*(1-mu)/mu
                   d2ldm2 <- -(((1-sigma^2)^2)/(sigma^4))*(trigamma(a) +trigamma(b))
                           d2ldm2 
                              },
                 dldd = function(y,mu,sigma) 
                              { 
                       a <- mu*(1-sigma^2)/(sigma^2)
                       b <- a*(1-mu)/mu
                    dldd <- -(2/(sigma^3))*( mu*(-digamma(a)+digamma(a+b)+log(y))
                              +(1-mu)*(-digamma(b)+digamma(a+b)+log(1-y)) )
                           dldd 
                              }, 
               d2ldd2 = function(mu,sigma) 
                              {
                      a <- mu*(1-sigma^2)/(sigma^2)
                      b <- a*(1-mu)/mu
                 d2ldd2 <- -(4/(sigma^6))*((mu^2)*trigamma(a) +((1-mu)^2)*trigamma(b)
                                    -trigamma(a+b))
                          d2ldd2
                              }, 
              d2ldmdd = function(mu,sigma) 
                             { 
                     a <- mu*(1-sigma^2)/(sigma^2)
                     b <- a*(1-mu)/mu
               d2ldmdd <- (2*(1-sigma^2)/(sigma^5))*(mu*trigamma(a)-(1-mu)*trigamma(b))
                       d2ldmdd 
                             },
          G.dev.incr  = function(y,mu,sigma,w,...) -2*dBE(y,mu,sigma,log=TRUE),                                        
                rqres = expression(rqres(pfun="pBE", type="Continuous", y=y, mu=mu, sigma=sigma)), 
           mu.initial = expression({mu <- (y+mean(y))/2}),
        sigma.initial = expression({sigma <- rep(0.5,length(y))}) ,
             mu.valid = function(mu) all(mu > 0 & mu < 1) , 
          sigma.valid = function(sigma)  all(sigma > 0 & sigma < 1), 
              y.valid = function(y)  all(y > 0 & y < 1),
                 mean = function(mu, sigma) mu,
             variance = function(mu, sigma) sigma^2 * mu * (1 - mu)
          ),
                class = c("gamlss.family","family"))
}
#------------------------------------------------------------------------------------------
dBE<-function(x, mu = 0.5, sigma = 0.2, log = FALSE)
 { 
          if (any(mu <= 0) | any(mu >= 1) )  stop(paste("mu must be between 0 and 1", "\n", "")) 
          if (any(sigma <= 0) | any(sigma >= 1))  stop(paste("sigma must be between 0 and 1", "\n", "")) 
          n <- max(length(x), length(mu), length(sigma))
          x <- rep_len(x, n)
         mu <- rep_len(mu, n)
      sigma <- rep_len(sigma, n)
          a <- mu*(1-sigma^2)/(sigma^2)
          b <- a*(1-mu)/mu
         fy <- dbeta(x, shape1=a, shape2=b, ncp=0, log=log)
         fy[x <= 0] <- 0
         fy[x >= 1] <- 0
         fy
  }
#------------------------------------------------------------------------------------------
pBE <- function(q, mu=0.5, sigma=0.2, lower.tail = TRUE, log.p = FALSE)
  {     
         if (any(mu <= 0) | any(mu >= 1) )  stop(paste("mu must be between 0 and 1", "\n", "")) 
         if (any(sigma <= 0) | any(sigma >= 1))  stop(paste("sigma must be between 0 and 1", "\n", "")) 
          n <- max(length(q), length(mu), length(sigma))
          q <- rep_len(q, n)
         mu <- rep_len(mu, n)
      sigma <- rep_len(sigma, n)
          a <- mu*(1-sigma^2)/(sigma^2)
          b <- a*(1-mu)/mu
          cdf <- pbeta(q, shape1=a, shape2=b, ncp=0, lower.tail=lower.tail, log.p=log.p)
          cdf[q<0] <- 0
          cdf[q>=1] <- 1
          cdf
   }
#------------------------------------------------------------------------------------------
qBE <- function(p, mu=0.5, sigma=0.2,  lower.tail = TRUE, log.p = FALSE)
  {      
if (any(mu <= 0) | any(mu >= 1) )  stop(paste("mu must be between 0 and 1", "\n", "")) 
if (any(sigma <= 0) | any(sigma >= 1))  stop(paste("sigma must be between 0 and 1", "\n", ""))   
          n <- max(length(p), length(mu), length(sigma))
          p <- rep_len(p, n)
if (log.p==TRUE) p <- exp(p) else p <- p
if (lower.tail==TRUE) p <- p else p <- 1-p
         mu <- rep_len(mu, n)
      sigma <- rep_len(sigma, n)
          a <- mu*(1-sigma^2)/(sigma^2)
          b <- a*(1-mu)/mu
          q <- qbeta(p, shape1=a, shape2=b, lower.tail=lower.tail, log.p=log.p)
          q[p-0 < abs(1e-10)]  <- 0
          q[1-p < abs(1e-10)]  <- 1
          q[p <  0] <- NaN
          q[p >  1] <- NaN
          return(q)
   }
#------------------------------------------------------------------------------------------
rBE <- function(n, mu=0.5, sigma=0.2)
  { if (any(mu <= 0) | any(mu >= 1) )  stop(paste("mu must be between 0 and 1", "\n", "")) 
    if (any(sigma <= 0) | any(sigma >= 1))  stop(paste("sigma must be between 0 and 1", "\n", ""))   
    if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
          n <- ceiling(n)
          p <- runif(n)
          a <- mu*(1-sigma^2)/(sigma^2)
          b <- a*(1-mu)/mu
          r <- qbeta(p, shape1=a, shape2=b)
          r
  }
#-----------------------------------------------------------------------------------------
