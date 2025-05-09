# RAR, MS, KA
# last change Dec 2004
# JL added moments, Nov 2018
################################################################################
################################################################################
################################################################################
################################################################################
NBII <-function (mu.link ="log", sigma.link="log") 
{
    mstats <- checklink("mu.link", "Negative Binomial type II", substitute(mu.link), 
                          c("inverse", "log", "identity", "sqrt"))
    dstats <- checklink("sigma.link", "Negative Binomial type II", substitute(sigma.link), 
                          c("inverse", "log", "identity", "sqrt"))
    structure(
          list(family = c("NBII", "Negative Binomial type II"),
           parameters = list(mu = TRUE, sigma = TRUE), 
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
                 dldm = function(y,mu,sigma)
                                  (1/sigma)*(digamma(y+(mu/sigma))-digamma((mu/sigma))-
                                   log(1+sigma)), 
               d2ldm2 = function(y,mu,sigma)
                               {
                          dldm <- (1/sigma)*(digamma(y+(mu/sigma))-digamma((mu/sigma))-
                                   log(1+sigma))  
                         d2ldm2 <- ((1/sigma)^2)*(trigamma(y+(mu/sigma))-trigamma((mu/sigma))) #
                         d2ldm2 <- if (any(d2ldm2 >= 0))  -dldm^2 else  d2ldm2 # MS Thursday, September 29, 2005
                         # d2ldm2 <- -dldm^2
                         d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2,-1e-15)  
                         d2ldm2   
                               }, 
                               #observed ((1/sigma)^2)*(trigamma(y+(mu/sigma))-trigamma((mu/sigma))),
                 dldd = function(y,mu,sigma) 
                                 -(mu/(sigma^2))*(digamma(y+(mu/sigma))-digamma((mu/sigma))-
                                   log(1+sigma))+(y-mu)/(sigma*(1+sigma)),
               d2ldd2 = function(y,mu,sigma) 
                               { # eval.parent(quote(-dldp*dldp))
                        dldd <- -(mu/(sigma^2))*(digamma(y+(mu/sigma))-digamma((mu/sigma))-
                                   log(1+sigma))+(y-mu)/(sigma*(1+sigma))
                      d2ldd2 <- -dldd^2
                      d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2,-1e-15)  
                      d2ldd2
                               }, 
#     d2ldd2 = function()  ((mu^2)/(sigma^4))* (trigamma(y+(mu/sigma))-trigamma((mu/sigma)))
#                                 +mu/((sigma^2)*(1+sigma)),
              d2ldmdd = function(y,mu,sigma) 
                                 {
                                 #(0,length(y))
                          dldm <- (1/sigma)*( digamma(y+(mu/sigma))-digamma((mu/sigma))-
                                   log(1+sigma))
                          dldd <- -(mu/(sigma^2))*(digamma(y+(mu/sigma))-digamma((mu/sigma))-
                                    log(1+sigma))+(y-mu)/(sigma*(1+sigma))
                       d2ldmdd <- -dldm*dldd
                       d2ldmdd
                                 },
                                   #-(mu/(sigma^3))*(trigamma(y+(mu/sigma))
                                   #-trigamma((mu/sigma)))-1/(sigma*(1+sigma)),
          G.dev.incr  = function(y,mu,sigma,...)  -2*dNBII(y, mu=mu, sigma=sigma, log=TRUE),    
               rqres = expression(
                        rqres(pfun="pNBII", type="Discrete", ymin=0, y=y, mu=mu, sigma=sigma)
                                  ), #
            mu.initial = expression( mu <- (y+mean(y))/2),
         sigma.initial = expression( 
                               sigma <- rep( max(((var(y)/mean(y))-1),0.1), length(y))),
              mu.valid = function(mu) all(mu > 0)  , 
           sigma.valid = function(sigma)  all(sigma > 0), 
               y.valid = function(y)  all(y >= 0),
                  mean = function(mu,sigma) mu,
              variance = function(mu, sigma) mu + sigma * mu
          ),
            class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dNBII<-function(x, mu=1, sigma=1, log=FALSE)
 { 
if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
#  if (any(x < 0) )  stop(paste("x must be >=0", "\n", ""))  
                n <- max(length(x), length(mu), length(sigma))
                x <- rep_len(x, n)
               mu <- rep_len(mu, n)
            sigma <- rep_len(sigma, n)
               fy <- rep_len(0,n)        
               fy <- dnbinom(x, size=mu/sigma, mu=mu, log=log) 
fy[sigma<=0.0001] <-  dpois(x, lambda = mu, log = log)
        fy[x < 0] <- 0 
     fy[x == Inf] <- 0 
  return(fy)  
  }
################################################################################
################################################################################
################################################################################
################################################################################
pNBII <- function(q, mu=1, sigma=1, lower.tail = TRUE, log.p = FALSE)
  {     
       if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
       if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
            n <- max(length(q), length(mu), length(sigma))
             q <- rep_len(q, n)
            mu <- rep_len(mu, n)
         sigma <- rep_len(sigma, n)
           cdf <- rep_len(0,n)
              cdf <- pnbinom(q, size=mu/sigma, mu=mu, lower.tail = lower.tail, log.p = log.p)
cdf[sigma>0.0001] <- ppois(q, lambda = mu, lower.tail = lower.tail, log.p = log.p)
    #   if (any(q < 0) )  stop(paste("y must be >=0", "\n", ""))
        # if (length(sigma)>1) cdf <- ifelse(sigma>0.0001, 
        #     pnbinom(q, size=mu/sigma, mu=mu, lower.tail=lower.tail, log.p=log.p),
        #             ppois(q, lambda = mu, lower.tail = lower.tail, log.p = log.p))
        # else cdf <- if (sigma<0.0001) ppois(q, lambda = mu, lower.tail = lower.tail, log.p = log.p)
        # else  pnbinom(q, size=mu/sigma, mu=mu, lower.tail=lower.tail, log.p=log.p)
           cdf[q < 0] <- 0
           cdf[q == Inf] <- 1
        return(cdf)
   }
################################################################################
################################################################################
################################################################################
################################################################################
qNBII <- function(p, mu=1, sigma=1,  lower.tail = TRUE, log.p = FALSE)
  {      
         if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
         if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", ""))
           n <- max(length(p), length(mu), length(sigma))
          mu <- rep_len(mu, n)
       sigma <- rep_len(sigma, n)
           q <- rep_len(0,n) 
           q <-  qnbinom(p, size=mu/sigma, mu=mu, lower.tail=lower.tail, log.p=log.p)
           return(q)
   }
################################################################################
################################################################################
################################################################################
################################################################################
rNBII <- function(n, mu=1, sigma=1)
  { 
if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
          n <- ceiling(n)
          p <- runif(n)
          r <- qNBII(p, mu=mu, sigma=sigma)
          return(as.integer(r))
  }
################################################################################
################################################################################
################################################################################
################################################################################
