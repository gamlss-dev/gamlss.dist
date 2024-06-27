################################################################################
################################################################################
################################################################################
################################################################################
# MS + BR last change Thursday, April 13, 2006
NBF <- function (mu.link="log", sigma.link="log", nu.link ="log")
{
  mstats <- checklink("mu.link", "NB Family", substitute(mu.link), c("inverse", "log", "identity"))
  dstats <- checklink("sigma.link", "NB Family", substitute(sigma.link), c("inverse", "log", "identity"))
  vstats <- checklink("nu.link", "NB Family", substitute(nu.link), c("1/mu^2", "log", "identity"))
  
  structure(
    list(    family = c("NBF", "NB Family"),
         parameters = list(mu=TRUE, sigma=TRUE, nu=TRUE), 
              nopar = 3, 
               type = "Discrete", 
            mu.link = as.character(substitute(mu.link)), 
         sigma.link = as.character(substitute(sigma.link)), 
            nu.link = as.character(substitute(nu.link)), 
         mu.linkfun = mstats$linkfun, 
      sigma.linkfun = dstats$linkfun, 
         nu.linkfun = vstats$linkfun, 
         mu.linkinv = mstats$linkinv, 
      sigma.linkinv = dstats$linkinv,
         nu.linkinv = vstats$linkinv, 
              mu.dr = mstats$mu.eta, 
           sigma.dr = dstats$mu.eta, 
              nu.dr = vstats$mu.eta, 
         dldm = function(y,mu,sigma, nu){ mu1  <- mu
                 sigma1 <- sigma*(mu^(nu-2))
                  dldm1 <- (y-mu1)/(mu1*(1+mu1*sigma1))
                 #ndldm <-(y-mu) /(mu *(1+mu*sigma)) 
                  dldd1 <- -((1/sigma1)^2)* (digamma(y+(1/sigma1))-digamma(1/sigma1)-log(1+mu1*sigma1)-(y-mu1)*sigma1/(1+mu1*sigma1))
                 #dldd1 <- -((1/sigma1)^2)* (digamma(y+(1/sigma1))-digamma(1/sigma1)-log(1+mu1*sigma1)-(y-mu1)*sigma1/(1+mu1*sigma1))
                   dldm <- dldm1+dldd1*sigma*(nu-2)*(mu^(nu-3))
                  dldm
         }, 
         d2ldm2 = function(y,mu,sigma, nu) {
                    mu1  <- mu
                  sigma1 <- sigma*(mu^(nu-2))
                   dldm1 <- (y-mu1)/(mu1*(1+mu1*sigma1))
                #  ndldm <- (y-mu) /(mu *(1+mu*sigma)) 
                   dldd1 <- -((1/sigma1)^2)* (digamma(y+(1/sigma1))-digamma(1/sigma1)-log(1+mu1*sigma1)-(y-mu1)*sigma1/(1+mu1*sigma1))
                  #dldd1 <- -((1/sigma1)^2)* (digamma(y+(1/sigma1))-digamma(1/sigma1)-log(1+mu1*sigma1)-(y-mu1)*sigma1/(1+mu1*sigma1))
                    dldm <- dldm1+dldd1*sigma*(nu-2)*(mu^(nu-3))
           #                         -1/(mu*(1+mu*sigma))
                  d2ldm2 <- -dldm^2
                  d2ldm2 
         },
         dldd = function(y,mu,sigma, nu)
         {          mu1  <- mu
                  sigma1 <- sigma*(mu^(nu-2))
                   dldd1 <- -((1/sigma1)^2)* (digamma(y+(1/sigma1))-digamma(1/sigma1)-log(1+mu1*sigma1)-(y-mu1)*sigma1/(1+mu1*sigma1))
                    dldd <- dldd1*(mu^(nu-2))          
         },
         d2ldd2 = function(y,mu,sigma, nu) {
                     mu1 <- mu
                  sigma1 <- sigma*(mu^(nu-2))
                   dldd1 <- -((1/sigma1)^2)* (digamma(y+(1/sigma1))-digamma(1/sigma1)-log(1+mu1*sigma1)-(y-mu1)*sigma1/(1+mu1*sigma1))
                    dldd <- dldd1*(mu^(nu-2))        
                  d2ldd2 <- -dldd^2                 
         }, #change this   
         dldv = function(y,mu,sigma, nu) {
           # r1 <- -log(mu)*(mu^(2-nu))/sigma
           # r2 <- digamma(y+(mu^(2-nu))/sigma)-digamma((mu^(2-nu))/sigma)
           # r3 <- -log(1+sigma*mu^(nu-1))-(1/(1+sigma*mu^(nu-1))) + 1
           # r4 <- (y/(1+sigma*mu^(nu-1)))*sigma*log(mu)*mu^(nu-1) +y*log(mu)
           #dldv <- r1*(r2+r3)+r4
                    nd <- numeric.deriv(dNBF(y, mu, sigma, nu, log=TRUE), "nu", delta=0.0001)
                  dldv <- as.vector(attr(nd, "gradient"))
                  dldv
         },
         d2ldv2 = function(y,mu,sigma, nu)  {
           #   r1 <- -log(mu)*(mu^(2-nu))/sigma
           #  r2 <- digamma(y+(mu^(2-nu))/sigma)-digamma((mu^(2-nu))/sigma)
           #  r3 <- -log(1+sigma*mu^(nu-1))-(1/(1+sigma*mu^(nu-1))) + 1
           #  r4 <- (y/(1+sigma*mu^(nu-1)))*sigma*log(mu)*mu^(nu-1) +y*log(mu)
           #dldv <- r1*(r2+r3)+r4
           #d2ldv2 <- -dldv^2
                   nd <- numeric.deriv(dNBF(y, mu, sigma, nu, log=TRUE), "nu", delta=0.0001)
                 dldv <- as.vector(attr(nd, "gradient"))
               d2ldv2 <- -dldv*dldv
               d2ldv2 
         },
         d2ldmdd = function(y)  rep(0,length(y)),# to be changed
         d2ldmdv = function(y)  rep(0,length(y)), #to be changed
         d2ldddv = function(y)   rep(0,length(y)),# to be changed
         G.dev.incr  = function(y,mu,sigma,nu,...) -2*dNBF(y,mu,sigma,nu,log=TRUE),                           
         rqres = expression(
           rqres(pfun="pNBF", type="Continuous", y=y, mu=mu, sigma=sigma, nu=nu) 
         ),
         mu.initial = expression(mu <- (y+mean(y))/2),
         sigma.initial = expression(
           sigma <- rep( max( ((var(y)-mean(y))/(mean(y)^2)),0.1),length(y))),
         nu.initial = expression(nu <- 2),
         mu.valid = function(mu) all(mu > 0) , 
         sigma.valid = function(sigma)  all(sigma > 0), 
         nu.valid = function(nu) all(nu > 0), # maybe it should be TRUE
         y.valid = function(y)  all(y >= 0),
            mean = function(mu, sigma, nu) mu,
        variance = function(mu, sigma, nu) mu + sigma * mu^(nu)
    ),
    class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dNBF<-function(x, mu=1, sigma=1, nu=2, log=FALSE)
{  
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
 # if (any(x < 0) )  stop(paste("x must be >=0", "\n", ""))  
      ly <- max(length(x),length(mu), length(sigma)) 
       x <- rep(x, length = ly)      
      mu <- rep(mu, length = ly) 
   sigma <- rep(sigma, length = ly) 
     mu1 <- mu
  sigma1 <- sigma*mu^(nu-2)
  if (length(sigma1)>1) 
 fy[sigma1>0.0001] <-  dnbinom(x, size=1/sigma1, mu = mu1, log = log)
fy[sigma1<=0.0001] <-  dPO(x, mu = mu1, log = log) 
  fy[x < 0] <- 0 
  fy
}
################################################################################
################################################################################
################################################################################
################################################################################
pNBF <- function(q, mu=1, sigma=1, nu=2, lower.tail = TRUE, log.p = FALSE)
{ 
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
 # if (any(q < 0) )  stop(paste("q must be >=0", "\n", ""))
      ly <- max(length(q),length(mu), length(sigma)) 
       q <- rep(q, length = ly)      
      mu <- rep(mu, length = ly) 
   sigma <- rep(sigma, length = ly)
     cdf <- rep(0,length = ly) 
     mu1 <- mu
  sigma1 <- sigma*mu^(nu-2)
 cdf[sigma1>0.0001] <- pnbinom(q, size=1/sigma1, mu=mu1, lower.tail=lower.tail,log.p=log.p)
cdf[sigma1<=0.0001] <-  ppois(q, lambda = mu1, lower.tail = lower.tail, log.p = log.p) 

  cdf[q < 0] <- 0 
  cdf
}
################################################################################
################################################################################
################################################################################
################################################################################
qNBF <- function(p, mu=1, sigma=1, nu=2, lower.tail = TRUE, log.p = FALSE)
{ 
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
  if (any(p < 0) | any(p > 1))  stop(paste("p must be between 0 and 1", "\n", ""))   
     mu1 <- mu
  sigma1 <- sigma*mu^(nu-2)
  if (length(sigma1)>1) q <- ifelse(sigma1>0.0001,  qnbinom(p, size=1/sigma1, mu=mu1, lower.tail=lower.tail, log.p=log.p), 
                                    qpois(p, lambda = mu1, lower.tail = lower.tail, log.p = log.p) )
  else q <- if (sigma1<0.0001) qpois(p, lambda = mu1, lower.tail = lower.tail, log.p = log.p)
            else qnbinom(p, size=1/sigma1, mu=mu1, lower.tail=lower.tail, log.p=log.p)
  q
}
################################################################################
################################################################################
################################################################################
################################################################################
rNBF <- function(n, mu=1, sigma=1, nu=2)
{
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
  if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
  n <- ceiling(n)
  p <- runif(n)
  r <- qNBF(p, mu=mu, sigma=sigma, nu=nu)
  as.integer(r)
}
################################################################################
################################################################################
################################################################################
################################################################################