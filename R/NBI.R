# RAR MS and KA
# last change Monday, December 13 2004
# last modification Tuesday, March 28, 2006 at 11:16 DS
#------------------------------------------------------------------------------------------
NBI <- function (mu.link = "log", sigma.link = "log") 
{
    mstats <- checklink("mu.link", "Negative Binomial type I", substitute(mu.link), 
                        c("inverse", "log", "identity", "sqrt"))
    dstats <- checklink("sigma.link", "Negative Binomial type I", substitute(sigma.link), 
                        c("inverse", "log", "identity", "sqrt"))
    structure(
          list(family = c("NBI", "Negative Binomial type I"),
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
                 dldm = function(y,mu,sigma){(y-mu)/(mu*(1+mu*sigma))}, 
               d2ldm2 = function(mu,sigma) { -1/(mu*(1+mu*sigma))},
                 dldd = function(y,mu,sigma)
                                 {
                -((1/sigma)^2)* (digamma(y+(1/sigma))-digamma(1/sigma)-log(1+mu*sigma)
                        -(y-mu)*sigma/(1+mu*sigma))
                                 },
               d2ldd2 = function(y,mu,sigma) {# eval.parent(quote(-dldp*dldp))
                    dldd <- -((1/sigma)^2)* (digamma(y+(1/sigma))-digamma(1/sigma)
                             -log(1+mu*sigma)-(y-mu)*sigma/(1+mu*sigma))
                  d2ldd2 <- -dldd^2
                  d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2,-1e-15)    
                  d2ldd2               
                                   }, #change this
              d2ldmdd = function(y) rep(0,length(y)),     
          G.dev.incr  = function(y,mu,sigma,...) -2*dNBI(y, mu = mu, sigma = sigma, log = TRUE), 
               rqres = expression(
                          rqres(pfun="pNBI", type="Discrete", ymin=0, y=y, mu=mu, sigma=sigma)
                                 ), 
            mu.initial = expression(mu <-  (y+mean(y))/2),
         sigma.initial = expression(
                      sigma <- rep( max( ((var(y)-mean(y))/(mean(y)^2)),0.1),length(y))),
              mu.valid = function(mu) all(mu > 0) , 
           sigma.valid = function(sigma)  all(sigma > 0), 
               y.valid = function(y)  all(y >= 0),
                  mean = function(mu, sigma) mu,
              variance = function(mu, sigma) mu + sigma * mu^2
          ),
            class = c("gamlss.family","family"))
}
#-------------------------------------------------------------------------------------------
dNBI<-function(x, mu = 1, sigma = 1, log = FALSE)
 { 
if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
#  if (any(x < 0) )  stop(paste("x must be >=0", "\n", ""))  
            n <- max(length(x), length(mu), length(sigma))
            x <- rep_len(x, n)
           mu <- rep_len(mu, n)
        sigma <- rep_len(sigma, n)
           fy <- rep_len(0,n)        
        fy <-  dnbinom(x, size=1/sigma, mu = mu, log = log) 
       # fy[sigma<=0.0001] <-  dPO(x, mu = mu, log = log)
        fy[x < 0]  <- 0 
        fy[x == Inf]  <- 0 
        return(fy)
  }
#------------------------------------------------------------------------------------------
pNBI <- function(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
  {     
if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
    #    if (any(q < 0) )  stop(paste("y must be >=0", "\n", ""))
                n <- max(length(q), length(mu), length(sigma))
                q <- rep_len(q, n)
               mu <- rep_len(mu, n)
            sigma <- rep_len(sigma, n)
              cdf <- rep_len(0,n)
             cdf  <-  pnbinom(q, size=1/sigma, mu=mu, 
                              lower.tail=lower.tail,log.p=log.p)
#cdf[sigma<=0.0001] <-  ppois(q, lambda = mu, lower.tail = lower.tail, 
#                              log.p = log.p)
        cdf[q < 0] <- 0
        cdf[q == Inf] <- 1
        return(cdf)
   }
#------------------------------------------------------------------------------------------
qNBI <- function(p, mu = 1, sigma = 1,  lower.tail = TRUE, log.p = FALSE)
  {  
if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
              n <- max(length(p), length(mu), length(sigma))
             mu <- rep_len(mu, n)
          sigma <- rep_len(sigma, n)
              q <- rep_len(0,n)
              q <- qnbinom(p, size=1/sigma, mu=mu, 
                           lower.tail=lower.tail, log.p=log.p)
#q[sigma<=0.0001] <- qpois(p, lambda = mu, lower.tail = lower.tail, log.p = log.p)
      q[p == 0] <- 0
      q[p == 1] <- Inf
      q[p <  0] <- NaN
      q[p >  1] <- NaN
      return(q)
   }
#------------------------------------------------------------------------------------------
rNBI <- function(n, mu = 1, sigma = 1)
  { 
          if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
          if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
          if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
          n <- ceiling(n)
          p <- runif(n)
          r <- qNBI(p, mu=mu, sigma=sigma)
          return(as.integer(r))
  }
#------------------------------------------------------------------------------------------
