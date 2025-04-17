# THIS IS THE  NEW FILE
#  Mikis 15-07-2025
################################################################################
################################################################################
################################################################################
################################################################################
LOGSHASH <- function (mu.link="identity", sigma.link="log", nu.link ="log", tau.link="log")
{
    mstats <- checklink(   "mu.link", "LOGSinh-Arcsinh", substitute(mu.link), 
                           c("1/mu^2", "log", "identity")) # may change this 
    dstats <- checklink("sigma.link", "LOGSinh-Arcsinh", substitute(sigma.link), 
                           c("inverse", "log", "identity"))
    vstats <- checklink(   "nu.link", "LOGSinh-Arcsinh", substitute(nu.link),    
                           c("1/nu^2", "log", "identity"))
    tstats <- checklink(  "tau.link", "LOGSinh-Arcsinh", substitute(tau.link),   
                           c("1/tau^2", "log", "identity")) 
    structure(
          list(family = c("LOGSHASH", "LOGSinh-Arcsinh"),
           parameters = list(mu=TRUE, sigma=TRUE, nu=TRUE, tau=TRUE), 
                nopar = 4, 
                 type = "Continuous",
              mu.link = as.character(substitute(mu.link)),  
           sigma.link = as.character(substitute(sigma.link)), 
              nu.link = as.character(substitute(nu.link)), 
             tau.link = as.character(substitute(tau.link)), 
           mu.linkfun = mstats$linkfun, 
        sigma.linkfun = dstats$linkfun, 
           nu.linkfun = vstats$linkfun,
          tau.linkfun = tstats$linkfun,  
           mu.linkinv = mstats$linkinv, 
        sigma.linkinv = dstats$linkinv,
           nu.linkinv = vstats$linkinv,
          tau.linkinv = tstats$linkinv, 
                mu.dr = mstats$mu.eta, 
             sigma.dr = dstats$mu.eta, 
                nu.dr = vstats$mu.eta,
               tau.dr = tstats$mu.eta, 
    dldm = function(y,mu,sigma,nu,tau) 
       {
      dldm <- SHASH()$dldm(log(y),mu,sigma, nu, tau)
      dldm
       },
   d2ldm2 = function(y,mu,sigma,nu,tau)
      {
      d2ldm2 = SHASH()$d2ldm2(log(y),mu,sigma,nu,tau)
      d2ldm2
      },     
   dldd = function(y,mu,sigma,nu,tau) 
      {  
      dldd <- SHASH()$dldd(log(y),mu,sigma, nu,tau)
      dldd                 
      } ,
   d2ldd2 = function(y,mu,sigma,nu,tau)
      {
     d2ldd2 = SHASH()$d2ldd2(log(y),mu,sigma,nu,tau)
     d2ldd2   
      },   
     dldv = function(y,mu,sigma,nu,tau) 
       { 
      dldv <- SHASH()$dldv(log(y),mu,sigma, nu,tau)
      dldv    
      },
   d2ldv2 = function(y,mu,sigma,nu,tau) 
      { 
     d2ldv2 = SHASH()$d2ldv2(log(y),mu,sigma,nu,tau)
     d2ldv2      
      },
      dldt = function(y,mu,sigma,nu,tau) 
      {
        dldt <- SHASH()$dldt(log(y),mu,sigma, nu,tau)
        dldt        
      },
   d2ldt2 = function(y,mu,sigma,nu,tau) 
      { 
     d2ldt2 = SHASH()$d2ldt2(log(y),mu,sigma,nu,tau)
     d2ldt2    
      },
d2ldmdd = function(y,mu,sigma,nu,tau)## ok
      {
  d2ldmdd = SHASH()$d2ldmdd(log(y),mu,sigma,nu,tau)
  d2ldmdd
      },
 d2ldmdv = function(y,mu,sigma,nu,tau)# OK
      { 
   d2ldmdv = SHASH()$d2ldmdv(log(y),mu,sigma,nu,tau)
   d2ldmdv   
   },
d2ldmdt = function(y,mu,sigma,nu,tau) #ok
      {
  d2ldmdt = SHASH()$d2ldmdt(log(y),mu,sigma,nu,tau)
  d2ldmdt 
      },
d2ldddv = function(y,mu,sigma,nu,tau) #ok
               {
  d2ldddv = SHASH()$d2ldddv(log(y),mu,sigma,nu,tau)
  d2ldddv
      },
d2ldddt = function(y,mu,sigma,nu,tau) #ok
      {
  d2ldddt = SHASH()$d2ldddt(log(y),mu,sigma,nu,tau)
  d2ldddt
      },
d2ldvdt = function(y,mu,sigma,nu,tau) #ok
      { 
  d2ldvdt = SHASH()$d2ldvdt(log(y),mu,sigma,nu,tau)
  d2ldvdt   
      },
 G.dev.incr  = function(y,mu,sigma,nu,tau,...) -2*dLOGSHASH(y,mu,sigma,nu,tau,log=TRUE),               
         rqres = expression(rqres(pfun="pLOGSHASH", type="Continuous", y=y, mu=mu, sigma=sigma, nu=nu, tau=tau)),
 mu.initial = expression(mu <- (log(y)+mean(log(y)))/2),   
 sigma.initial = expression(sigma<- rep(sd(log(y))/5, length(y))),
    nu.initial = expression(nu <- rep(.5, length(y))), 
   tau.initial = expression(tau <-rep(.5, length(y))),
      mu.valid = function(mu) TRUE,
   sigma.valid = function(sigma)  all(sigma > 0),
      nu.valid = function(nu) TRUE, 
     tau.valid = function(tau) all(tau > 0),
       y.valid = function(y)  TRUE
       ),
            class = c("gamlss.family","family")
)
}
################################################################################
################################################################################
################################################################################
################################################################################
dLOGSHASH <- function(x, mu = 0, sigma = 1, nu = .5, tau = .5, log = FALSE)
 {
if (any(sigma < 0))  stop(paste("sigma must be positive", "\n", "")) 
if (any(tau < 0))  stop(paste("tau must be positive", "\n", ""))  
if (any(nu < 0))  stop(paste("nu must be positive", "\n", ""))
         n <- max(length(x), length(mu), length(sigma), length(nu), length(tau))
         x <- rep_len(x, n)
        mu <- rep_len(mu, n)
     sigma <- rep_len(sigma, n)
        nu <- rep_len(nu, n)
       tau <- rep_len(tau, n)
    loglik <- dSHASH(log(x), mu=mu, sigma=sigma, nu=nu, tau=tau, log=TRUE)-log(x)
if(log==FALSE) ft  <- exp(loglik) else ft <- loglik 
    ft[x <= 0] <- 0
    ft
       
  }    
################################################################################
################################################################################
################################################################################
################################################################################
pLOGSHASH <- function(q, mu = 0, sigma = 1, nu = .5, tau = .5, lower.tail = TRUE, log.p = FALSE)
 { 
if (any(sigma < 0))  stop(paste("sigma must be positive", "\n", "")) 
if (any(tau < 0))  stop(paste("tau must be positive", "\n", "")) 
if (any(nu < 0))  stop(paste("nu must be positive", "\n", ""))  
      n <- max(length(q), length(mu), length(sigma), length(nu), length(tau))
      q <- rep_len(q, n)
     mu <- rep_len(mu, n)
  sigma <- rep_len(sigma, n)
     nu <- rep_len(nu, n)
    tau <- rep_len(tau, n)
    cdf <- pSHASH(log(q), mu=mu, sigma=sigma, nu=nu, tau=tau)
if (lower.tail==FALSE)  cdf <- 1-cdf 
if (log.p ==TRUE)  cdf <- log(cdf)      
cdf[q<=0] <- 0 
cdf[q==Inf] <- 1 
   cdf     
 }
################################################################################
################################################################################
################################################################################
################################################################################
qLOGSHASH <-  function(p, mu=0, sigma=1, nu=.5, tau=.5, lower.tail = TRUE, log.p = FALSE)
  { 
if (any(sigma < 0))  stop(paste("sigma must be positive", "\n", "")) 
if (any(tau < 0))  stop(paste("tau must be positive", "\n", "")) 
if (any(nu < 0))  stop(paste("nu must be positive", "\n", ""))  
if (log.p) p <- exp(p)
if (!lower.tail) p <- 1 - p
      n <- max(length(p), length(mu), length(sigma), length(nu), length(tau))
      p <- rep_len(p, n)
     mu <- rep_len(mu, n)
  sigma <- rep_len(sigma, n)
     nu <- rep_len(nu, n)
    tau <- rep_len(tau, n)
   q <- exp(qSHASH(p, mu=mu, sigma=sigma, nu=nu, tau=tau)) 
   q[p == 0] <- 0
   q[p == 1] <- Inf
   q[p <  0] <- NaN
   q[p >  1] <- NaN
   q
   }
################################################################################
################################################################################
################################################################################
################################################################################
rLOGSHASH <- function(n, mu=0, sigma=1, nu=.5, tau=.5)
  {
    if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
    n <- ceiling(n)
    p <- runif(n)
    r <- qLOGSHASH(p, mu=mu, sigma=sigma, nu=nu, tau=tau)
  r
  }
################################################################################
################################################################################
################################################################################
################################################################################
