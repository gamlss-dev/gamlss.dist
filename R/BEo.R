BEo <- function (mu.link = "log", sigma.link = "log") 
{
    mstats <- checklink("mu.link", "BEo", substitute(mu.link), 
        c("inverse", "log", "identity", "own"))
    dstats <- checklink("sigma.link", "BEo", substitute(sigma.link), 
        c("inverse", "log", "identity", "own"))
  
    structure(
          list(family = c("BEo", "Beta original"),
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
                     dldm <- -digamma(mu)+digamma(mu+sigma)+log(y) 
                              dldm 
                              },
               d2ldm2 = function(mu,sigma) 
                              { 
                   d2ldm2 <- -trigamma(mu)+trigamma(mu+sigma)
                           d2ldm2 
                              },
                 dldd = function(y,mu,sigma) 
                              {
                    dldd <- -digamma(sigma)+digamma(mu+sigma)+log((1-y))
                           dldd 
                              }, 
               d2ldd2 = function(mu,sigma) 
                              {
                 d2ldd2 <- -trigamma(sigma)+trigamma(mu+sigma)
                          d2ldd2
                              }, 
              d2ldmdd = function(mu,sigma) 
                             { 
               d2ldmdd <- trigamma(mu+sigma)
                        d2ldmdd 
                             },
          G.dev.incr  = function(y,mu,sigma,w,...) -2*dBEo(y,mu,sigma,log=TRUE),                                        
                rqres = expression(rqres(pfun="pBEo", type="Continuous", y=y, mu=mu, sigma=sigma)), 
           mu.initial = expression({mu <- rep(2,length(y))}),
        sigma.initial = expression({sigma <- rep(2,length(y))}) ,
             mu.valid = function(mu) all(mu > 0 & mu < 1) , 
          sigma.valid = function(sigma)  all(sigma > 0 & sigma < 1), 
              y.valid = function(y)  all(y > 0 & y < 1),
                 mean = function(mu, sigma) mu/(mu+sigma),
             variance = function(mu, sigma) (mu*sigma)/((mu+sigma)^2 *(mu+sigma+1))
          ),
                class = c("gamlss.family","family"))
}
#------------------------------------------------------------------------------------------
dBEo<-function(x, mu = 0.5, sigma = 0.2, log = FALSE)
 { 
          if (any(mu <= 0)) stop(paste("mu must be positive", "\n", ""))
          if (any(sigma <= 0)) stop(paste("sigma must be positive", "\n", "")) 
      #    if (any(x <= 0) | any(x >= 1))  stop(paste("x must be between 0 and 1", "\n", ""))  
          ly <- max(length(x),length(mu),length(sigma)) 
           x <- rep(x, length = ly)      
       sigma <- rep(sigma, length = ly)
          mu <- rep(mu, length = ly)   
          fy <- dbeta(x, shape1=mu, shape2=sigma, ncp=0, log=log)
          fy[x <= 0] <- 0
          fy[x >= 1] <- 0
          fy
  }
#------------------------------------------------------------------------------------------
pBEo <- function(q, mu=0.5, sigma=0.2, lower.tail = TRUE, log.p = FALSE)
  {     
         if (any(mu <= 0)) stop(paste("mu must be positive", "\n", ""))
          if (any(sigma <= 0)) stop(paste("sigma must be positive", "\n", "")) 
 #        if (any(q <= 0) | any(q >= 1))  stop(paste("y must be between 0 and 1", "\n", ""))  
            n <- max(length(q), length(mu), length(sigma))
            q <- rep_len(q, n)
           mu <- rep_len(mu, n)
        sigma <- rep_len(sigma, n)
          cdf <- pbeta(q, shape1=mu, shape2=sigma, ncp=0, lower.tail=lower.tail, log.p=log.p)
          cdf[q<0] <- 0
          cdf[q>=1] <- 1
          cdf
   }
#------------------------------------------------------------------------------------------
qBEo <- function(p, mu=0.5, sigma=0.2,  lower.tail = TRUE, log.p = FALSE)
  {      if (any(mu <= 0)) stop(paste("mu must be positive", "\n", ""))
          if (any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))  
          n <- max(length(p), length(mu), length(sigma))
          p <- rep_len(p, n)
         mu <- rep_len(mu, n)
      sigma <- rep_len(sigma, n) 
          q <- qbeta(p, shape1=mu, shape2=sigma, lower.tail=lower.tail, log.p=log.p)
          q[p-0 < abs(1e-10)]  <- 0
          q[1-p < abs(1e-10)]  <- 1
          q[p <  0] <- NaN
          q[p >  1] <- NaN
          return(q)
   }
#------------------------------------------------------------------------------------------
rBEo <- function(n, mu=0.5, sigma=0.2)
  { if (any(mu <= 0)) stop(paste("mu must be positive", "\n", ""))
          if (any(sigma <= 0)) stop(paste("sigma must be positive", "\n", "")) 
    if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
          n <- ceiling(n)
          p <- runif(n)
          r <- qbeta(p, shape1=mu, shape2=sigma)
          r
  }
#-----------------------------------------------------------------------------------------
