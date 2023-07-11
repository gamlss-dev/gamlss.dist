#----------------------------------------------------------------------------------------
# MS + BR last change 05 12, 2018 at 15:06
GAF <- function (mu.link="log", sigma.link="log", nu.link ="identity")
{
    mstats <- checklink("mu.link", "gamma family", substitute(mu.link), c("1/mu^2", "log", "identity"))
    dstats <- checklink("sigma.link", "gamma family", substitute(sigma.link), c("inverse", "log", "identity"))
    vstats <- checklink("nu.link", "gamma family", substitute(nu.link), c("1/mu^2", "log", "identity"))
    
    structure(
          list(family = c("GAF", "gamma family"),
           parameters = list(mu=TRUE, sigma=TRUE, nu=TRUE), 
                nopar = 3, 
                 type = "Continuous",
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
                 dldm = function(y,mu,sigma,nu) {   
                                      mu1 <- mu
                                   sigma1 <- sigma*(mu^(nu/2-1))
                                    dldm1 <- (y-mu1)/((sigma1^2)*(mu1^2))
                                    dldd1 <- (2/sigma1^3)*((y/mu1)-log(y)+log(mu1)+log(sigma1^2)-1+digamma(1/(sigma1^2)))
                                     dldm <- dldm1+dldd1*sigma*(nu/2-1)*(mu^((nu/2)-2)) 
                                     dldm
                                    },
               d2ldm2 = function(y,mu,sigma,nu) {
                                      mu1 <- mu
                                   sigma1 <- sigma*(mu^(nu/2-1))
                                  d2ldm21 <- -1/((sigma1^2)*(mu1^2))
                                  d2ldd21 <- (4/sigma1^4)-(4/sigma1^6)*trigamma((1/sigma1^2))
                                   d2ldm2 <- d2ldm21+d2ldd21*(sigma^2)*(((nu/2)-1)^2)*(mu^(nu-4))
                                   d2ldm2
                                   },
                 dldd = function(y,mu,sigma,nu) {
                                     mu1 <- mu
                                  sigma1 <- sigma*(mu^(nu/2-1))
                                   dldd1 <- (2/sigma1^3)*((y/mu1)-log(y)+log(mu1)+log(sigma1^2)-1+digamma(1/(sigma1^2)))
                                    dldd <- dldd1*(mu^((nu/2)-1)) 
                                   dldd  
                                    },
               d2ldd2 = function(y,mu,sigma,nu) {
                                      mu1 <- mu
                                   sigma1 <- sigma*(mu^(nu/2-1))
                                  d2ldd21 <- (4/sigma1^4)-(4/sigma1^6)*trigamma((1/sigma1^2))
                                  d2ldd2 <- d2ldd21*(mu^(nu-2))
                                  d2ldd2
                                    },
                 dldv = function(y,mu,sigma,nu) {  
                                     mu1 <- mu
                                  sigma1 <- sigma*(mu^(nu/2-1))
                                   dldd1 <- (2/(sigma1^3))*((y/mu1)-log(y)+log(mu1)+log(sigma1^2)-1+digamma(1/(sigma1^2)))
                                    dldv <- dldd1*sigma*(mu^((nu/2)-1))*(log(mu))/2 
                                   dldv
                                    },
               d2ldv2 = function(y,mu,sigma,nu)  {
                                     mu1 <- mu
                                  sigma1 <- sigma*(mu^(nu/2-1))
                                 d2ldd21 <- (4/sigma1^4)-(4/sigma1^6)*trigamma((1/sigma1^2))
                                 d2ldv2 <- d2ldd21*(sigma^2)*(mu^(nu-2))*((log(mu))^2)*0.25
                               #    d2ldv2 <- d2ldd21*((sigma*(mu^((nu/2)-1))*(log(mu))/2)^2) 
                                    d2ldv2
                                    },
              d2ldmdd = function(y,mu,sigma,nu)  {
                                    mu1 <- mu
                                 sigma1 <- sigma*(mu^(nu/2-1))
                                d2ldd21 <- (4/sigma1^4)-(4/sigma1^6)*trigamma((1/sigma1^2))
                                d2ldmdd <- d2ldd21*(mu^(nu-3))*sigma*((nu/2)-1)
                                d2ldmdd
                                    },
              d2ldmdv = function(y,mu,sigma,nu)  {
                                    mu1 <- mu
                                 sigma1 <- sigma*(mu^(nu/2-1))
                                d2ldd21 <- (4/sigma1^4)-(4/sigma1^6)*trigamma((1/sigma1^2))
              	                d2ldmdv <- d2ldd21*(sigma^2)*(mu^(nu-3))*(log(mu))*0.5*((nu/2)-1)
              	                d2ldmdv
                                    },
              d2ldddv = function(y,mu,sigma,nu)  {
                                     mu1 <- mu
                                 sigma1 <- sigma*(mu^(nu/2-1))
                                d2ldd21 <- (4/sigma1^4)-(4/sigma1^6)*trigamma((1/sigma1^2))
              	                d2ldddv <- d2ldd21*sigma*(mu^(nu-2))*(log(mu))*0.5
              	                d2ldddv
                                    },
          G.dev.incr  = function(y,mu,sigma,nu,...) -2*dGAF(y,mu,sigma,nu,log=TRUE),                           
                rqres = expression(
                rqres(pfun="pGAF", type="Continuous", y=y, mu=mu, sigma=sigma, nu=nu) 
                                   ),
            mu.initial =  expression(  mu <- (y+mean(y))/2), 
         sigma.initial =  expression({ sigma <- rep((0.2*sd(y))/mean(y),length(y))}),#rep((0.2*sd(y))/sqrt(mean(y)),length(y)) }), 
            nu.initial =  expression(   nu <- rep(2, length(y))), # previouly 1 
              mu.valid = function(mu) all(mu > 0), 
           sigma.valid = function(sigma)  all(sigma > 0),
              nu.valid = function(nu) TRUE, 
               y.valid = function(y)  all(y>0),
                  mean = function(mu, sigma, nu) mu,
              variance = function(mu, sigma, nu) sigma^2 * mu^nu 
          ),
            class = c("gamlss.family","family"))
}
#----------------------------------------------------------------------------------------
dGAF<-function(x, mu=1, sigma=1, nu=2, log=FALSE)
 { 
  if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
    if (any(mu <= 0))  stop(paste("mu must be positive", "\n", "")) 
    #if (any(nu <= 0))  stop(paste("nu must be positive", "\n", "")) 
               mu1 <- mu
             sigma1 <- sigma*(mu^(nu/2-1))
    log.lik <- (1/sigma1^2)*log(x/(mu1*sigma1^2))-x/(mu1*sigma1^2)-log(x)-lgamma(1/sigma1^2)
     if(log==FALSE) fy  <- exp(log.lik) else fy <- log.lik
      fy <- ifelse(x <= 0, 0, fy)
      fy 
  }
#---------------------------------------------------------------------------------------- 
pGAF <- function(q, mu=1, sigma=1, nu=2, lower.tail = TRUE, log.p = FALSE)
  { if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
    if (any(mu <= 0))  stop(paste("mu must be positive", "\n", "")) 
    #if (any(nu <= 0))  stop(paste("nu must be positive", "\n", ""))  
               mu1 <- mu
             sigma1 <- sigma*(mu^(nu/2-1))
           cdf <- pgamma(q,shape=1/sigma1^2,scale=mu*sigma1^2, lower.tail = lower.tail, log.p = log.p)
    cdf
   }
#----------------------------------------------------------------------------------------
qGAF <- function(p, mu=1, sigma=1, nu=2, lower.tail = TRUE, log.p = FALSE)
  { if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", ""))
    if (any(mu <= 0))  stop(paste("mu must be positive", "\n", ""))  
    #if (any(nu <= 0))  stop(paste("nu must be positive", "\n", ""))  
    if (any(p < 0)|any(p > 1))  stop(paste("p must be between 0 and 1", "\n", ""))    
      mu1 <- mu
       sigma1 <- sigma*(mu^(nu/2-1))
    q <- qgamma(p,shape=1/sigma1^2,scale=mu*sigma1^2, lower.tail = lower.tail, log.p = log.p)
    q
   }
#----------------------------------------------------------------------------------------
rGAF <- function(n, mu=1, sigma=1, nu=2)
  { if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
    if (any(mu <= 0))  stop(paste("mu must be positive", "\n", "")) 
   # if (any(nu <= 0))  stop(paste("nu must be positive", "\n", ""))  
    if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
    n <- ceiling(n)
    p <- runif(n)
    r <- qGAF(p, mu=mu, sigma=sigma, nu=nu)
    r
  }
