# the BCCG distribution used for the LMS medod in centile estimation 
################################################################################
################################################################################
################################################################################
################################################################################
BCCG <- function (mu.link="identity", sigma.link="log", nu.link ="identity") 
{
    mstats <- checklink("mu.link", "BC Cole Green", substitute(mu.link), 
                         c("inverse", "log", "identity", "own"))
    dstats <- checklink("sigma.link", "BC Cole Green", substitute(sigma.link), #
                         c("inverse", "log", "identity", "own"))
    vstats <- checklink("nu.link", "BC Cole Green",substitute(nu.link), 
                         c("inverse", "log", "identity", "own"))  
    structure(
          list(family = c("BCCG", "Box-Cox-Cole-Green"),
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
      z <- ifelse(nu != 0,(((y/mu)^nu-1)/(nu*sigma)),log(y/mu)/sigma)
   dldm <- ((z/sigma)+nu*(z*z-1))/mu
   dldm
                                    },
               d2ldm2 = function(y,mu,sigma,nu) {
 d2ldm2 <- -(1+2*nu*nu*sigma*sigma)
 d2ldm2 <- d2ldm2/(mu*mu*sigma*sigma)
 d2ldm2                                 },
                 dldd = function(y,mu,sigma,nu) {
      z <- ifelse(nu != 0,(((y/mu)^nu-1)/(nu*sigma)),log(y/mu)/sigma)
      h <- dnorm(1/(sigma*abs(nu)))/pnorm(1/(sigma*abs(nu)))
   dldd <- (z^2-1)/sigma+ h/(sigma^2*abs(nu))
   dldd                                   },
               d2ldd2 = function(sigma) {
 d2ldd2 <- -2/(sigma^2)
 d2ldd2
                                    },
                 dldv = function(y,mu,sigma,nu) {
       z <- ifelse(nu != 0,(((y/mu)^nu-1)/(nu*sigma)),log(y/mu)/sigma)
       h <- dnorm(1/(sigma*abs(nu)))/pnorm(1/(sigma*abs(nu)))   
       l <- log(y/mu)                                       
    dldv <- (z-(l/sigma))*(z/nu) -l*(z*z-1)
    dldv <- dldv+sign(nu)*h/(sigma*nu^2)                 
    dldv
                                   },
               d2ldv2 = function(sigma) {
   d2ldv2 <- -7*sigma*sigma/4
   d2ldv2                                   },
              d2ldmdd = function(mu,sigma,nu) -2*nu/(mu*sigma),
              d2ldmdv = function(mu) 1/(2*mu),
              d2ldddv = function(sigma,nu) -sigma*nu,
          G.dev.incr  = function(y,mu,sigma,nu,...) 
                     -2*dBCCG(y,mu=mu,sigma=sigma,nu=nu,log=TRUE), 
             rqres = expression(rqres(pfun="pBCCG", type="Continuous", y=y, mu=mu, sigma=sigma, nu=nu)),
        mu.initial = expression( mu <- (y+mean(y))/2), 
     sigma.initial = expression( sigma <- rep(0.1,length(y))), 
        nu.initial = expression( nu <- rep(0.5, length(y))), 
          mu.valid = function(mu) TRUE , 
       sigma.valid = function(sigma)  all(sigma > 0),
          nu.valid = function(nu) TRUE , 
           y.valid = function(y) all(y>0)
          ),
            class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dBCCG <-dBCCGo <- function(x, mu=1, sigma=0.1, nu=1,  log = FALSE)
 {
## check whether parameters are within range  
if (any(mu <= 0))  stop(paste("mu must be positive", "\n", "")) 
if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
## length of return value
      n <- max(length(x), length(mu), length(sigma), length(nu))
      x <- rep_len(x, n)
     mu <- rep_len(mu, n)
  sigma <- rep_len(sigma, n)
     nu <- rep_len(nu, n)
## calculating the pdf
        z <- ifelse(nu != 0,((((x/mu)^nu)-1)/(nu*sigma)),log(x/mu)/sigma)
   loglik <- nu*log(x/mu)-log(sigma)-(z*z)/2 -log(x) -(log(2*pi))/2 
   loglik <- loglik-log(pnorm(1/(sigma*abs(nu))))
if(log==FALSE) ft  <- exp(loglik) else ft <- loglik 
       ft <- ifelse(x <= 0, 0, ft)
       ft
  }    
################################################################################
################################################################################
################################################################################
################################################################################ 
pBCCG <- pBCCGo <- function(q, mu=1, sigma=0.1, nu=1,  lower.tail = TRUE, 
                            log.p = FALSE)
 {
## check whether parameters are within range    
if (any(mu <= 0))  stop(paste("mu must be positive", "\n", "")) 
if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
## length of return value
         n <- max(length(q), length(mu), length(sigma), length(nu))
         q <- rep_len(q, n)
        mu <- rep_len(mu, n)
     sigma <- rep_len(sigma, n)
        nu <- rep_len(nu, n)   
         z <- rep_len(0, n)
       FYy1 <- rep_len(0, n)
       FYy2 <- rep_len(0, n)
       FYy3 <- rep_len(0, n)
          z <- ifelse(nu != 0, (((q/mu)^nu - 1)/(nu * sigma)), 
                   log(q/mu)/sigma)
      FYy1 <- pnorm(z)
      FYy2 <- ifelse(nu > 0, pnorm(-1/(sigma * abs(nu))), 0)
      FYy3 <- pnorm(1/(sigma * abs(nu)))
       FYy <- (FYy1 - FYy2)/FYy3
## calculation the cdf
#          z <- ifelse(nu != 0,(((q/mu)^nu-1)/(nu*sigma)),log(q/mu)/sigma)
# FYy[nu<=0] <- pnorm(z)/ pnorm(1/(sigma * abs(nu)))
# FYy[nu>0]  <- (pnorm(z)-pnorm(-1/(sigma*abs(nu)))) / 
#                pnorm(1/(sigma * abs(nu)))     
         #   if(length(nu)>1)  z <- ifelse(nu != 0,(((q/mu)^nu-1)/(nu*sigma)),log(q/mu)/sigma)
         # else   if (nu != 0) z <- (((q/mu)^nu-1)/(nu*sigma)) else z <- log(q/mu)/sigma
         # FYy1 <- pnorm(z)
         # if(length(nu)>1)  FYy2 <- ifelse(nu > 0, pnorm(-1/(sigma*abs(nu))),0)
         # else   if (nu>0)  FYy2 <-  pnorm(-1/(sigma*abs(nu))) else FYy2 <- 0
         # FYy3 <- pnorm(1/(sigma*abs(nu)))
         # FYy  <- (FYy1-FYy2)/FYy3
if(lower.tail==TRUE) FYy  <- FYy else  FYy <- 1-FYy 
if(log.p==FALSE)     FYy  <- FYy else  FYy<- log(FYy) 
 FYy[q<=0] <- 0 
         FYy     
 }
################################################################################
################################################################################
################################################################################
################################################################################
qBCCG <- qBCCGo <- function(p, mu=1, sigma=0.1, nu=1,  lower.tail = TRUE, log.p = FALSE )
 { 
## check whether parameters are within range
  if (any(mu < 0))  stop(paste("mu must be positive", "\n", "")) 
  if (any(sigma < 0))  stop(paste("sigma must be positive", "\n", "")) 
## extra arguments
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
## length of return value
       n <- max(length(p), length(mu), length(sigma), length(nu))
       p <- rep_len(p, n)
      mu <- rep_len(mu, n)
   sigma <- rep_len(sigma, n)
      nu <- rep_len(nu, n)
       z <- rep_len(0, n)
      ya <- rep_len(0, n)
## compute quantile
# equation page 440 8 line 
    #   z <- ifelse((nu <= 0), qnorm(p * pnorm(1/(sigma * abs(nu)))), 
    #             qnorm(1 - (1 - p) * pnorm(1/(sigma * abs(nu)))))
      z[nu <= 0] <- qnorm( p[nu <= 0] * pnorm(1/(sigma[nu <= 0] * abs(nu[nu <= 0]))))
       z[nu > 0] <- qnorm(1 - (1 - p[nu > 0]) * pnorm(1/(sigma[nu > 0] * abs(nu[nu > 0]))))                    
      # ya <- ifelse(nu != 0, mu * ((nu * sigma * z + 1)^(1/nu)), 
      #                      mu * exp(sigma * z))
      ya[nu != 0] <- mu[nu != 0] * ((nu[nu != 0] * sigma[nu != 0] * z[nu != 0] + 1)^(1/nu[nu != 0]))
      ya[nu == 0] <-  mu[nu == 0] * exp(sigma[nu == 0] * z[nu == 0])
## catch edge cases and return
  ya[p == 0] <- 0
  ya[p == 1] <- Inf
  ya[p <  0] <- NaN
  ya[p >  1] <- NaN
       ya
 }
################################################################################
################################################################################
################################################################################
################################################################################
rBCCG <- rBCCGo <- function(n, mu=1, sigma=0.1, nu=1)
  {
    if (any(mu <= 0))  stop(paste("mu must be positive", "\n", "")) 
    if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
    if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))    
    n <- ceiling(n)
    p <- runif(n)
    r <- qBCCG(p,mu=mu,sigma=sigma,nu=nu)
    r
  }
################################################################################
################################################################################
################################################################################
################################################################################
# the older function for LMS methos not used anymore 
BCCGuntr <- function (mu.link="identity", sigma.link="log", nu.link ="identity") 
{
  mstats <- checklink("mu.link", "BC Cole Green", substitute(mu.link), c("1/mu^2", "log", "identity"))
    dstats <- checklink("sigma.link", "BC Cole Green", substitute(sigma.link), c("inverse", "log", "identity"))
    vstats <- checklink("nu.link", "BC Cole Green",substitute(nu.link), c("1/nu^2", "log", "identity"))  
    
    structure(
          list(family = c("BCCGuntr", "Boc Cox Cole and Green untrucated"),
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
                                    z <- ifelse(nu != 0,1,0)*((y/mu)^nu-1)/(nu*sigma)
                                    z <- z + ifelse(nu==0,1,0)*log(y/mu)/sigma
                                    dldm <- ((z/sigma)+nu*(z*z-1))/mu
                                    dldm
                                    },
               d2ldm2 = function(mu,sigma,nu) {
                                    d2ldm2 <- -(1+2*nu*nu*sigma*sigma)
                                    d2ldm2 <- d2ldm2/(mu*mu*sigma*sigma)
                                    d2ldm2
                                    },
                 dldd = function(y,mu,sigma,nu) {
                                    z <- ifelse(nu != 0,1,0)*((y/mu)^nu-1)/(nu*sigma)
                                    z <- z + ifelse(nu==0,1,0)*log(y/mu)/sigma
                                    dldd <- (z*z-1)/sigma
                                    dldd
                                    },
               d2ldd2 = function(sigma) {
                                    d2ldd2 <- -2/(sigma^2)
                                    d2ldd2
                                    },
                 dldv = function(y,mu,sigma,nu) {
                                    z <- ifelse(nu != 0,1,0)*((y/mu)^nu-1)/(nu*sigma)
                                    l <- log(y/mu)
                                    z <- z + ifelse(nu==0,1,0)*log(y/mu)/sigma
                                    dldv <- (z-(l/sigma))*(z/nu) -l*(z*z-1)
                                    dldv
                                    },
               d2ldv2 = function(sigma) {
                                    d2ldv2 <- -7*sigma*sigma/4
                                    d2ldv2
                                    },
              d2ldmdd = function(mu,sigma,nu) -2*nu/(mu*sigma),
              d2ldmdv = function(mu) 1/(2*mu),
              d2ldddv = function(sigma,nu) -sigma*nu,
          G.dev.incr  = function(y,mu,sigma,nu,...) 
            {
                   tf <- pnorm(1/(sigma*abs(nu)))
                    if(any(tf<.99)) 
              warning(paste("The truncation factor F(1/(sigma*|nu|)) \n", 
                      "of CG is less than 0.99 for some observations",
                                                             "\n"))       
                  z <- ifelse(nu != 0,1,0)*((y/mu)^nu-1)/(nu*sigma)
                     z <- z + ifelse(nu==0,1,0)*log(y/mu)/sigma
                   lik <- nu*log(y/mu)-log(sigma)-(z*z)/2 -log(y) -(log(2*pi))/2
            G.dev.incr <- -2*lik
              G.dev.incr
              }, 
                rqres = expression(ifelse(nu != 0,((y/mu)^nu -1)/(sigma*nu),log(y/mu)/sigma )),
            mu.initial = expression( mu <- y+0.00001), 
         sigma.initial = expression( sigma <- rep(0.1,length(y))), #sigma <- rep(sqrt(var(y))/mean(y),length(y))
            nu.initial = expression( nu <- rep(0.5, length(y))), 
               mu.valid = function(mu) all(mu > 0), # MS Friday, September 5, 2003 at 19:43
           sigma.valid = function(sigma)  all(sigma > 0),
              nu.valid = function(nu) TRUE , 
               y.valid = function(y) all(y>0)
          ),
            class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
BCCGo <- function (mu.link="log", sigma.link="log", nu.link ="identity") 
{
    mstats <- checklink("mu.link", "BC Cole Green", substitute(mu.link), 
                         c("inverse", "log", "identity", "own"))
    dstats <- checklink("sigma.link", "BC Cole Green", substitute(sigma.link), #
                         c("inverse", "log", "identity", "own"))
    vstats <- checklink("nu.link", "BC Cole Green",substitute(nu.link), 
                         c("inverse", "log", "identity", "own"))  
    structure(
          list(family = c("BCCGo", "Box-Cox-Cole-Green-orig."),
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
      z <- ifelse(nu != 0,(((y/mu)^nu-1)/(nu*sigma)),log(y/mu)/sigma)
   dldm <- ((z/sigma)+nu*(z*z-1))/mu
   dldm
                                    },
               d2ldm2 = function(y,mu,sigma,nu) {
 d2ldm2 <- -(1+2*nu*nu*sigma*sigma)
 d2ldm2 <- d2ldm2/(mu*mu*sigma*sigma)
 d2ldm2
                                    },
                 dldd = function(y,mu,sigma,nu) {
      z <- ifelse(nu != 0,(((y/mu)^nu-1)/(nu*sigma)),log(y/mu)/sigma)
      h <- dnorm(1/(sigma*abs(nu)))/pnorm(1/(sigma*abs(nu)))
   dldd <- (z^2-1)/sigma+ h/(sigma^2*abs(nu))
   dldd
                                    },
               d2ldd2 = function(sigma) {
 d2ldd2 <- -2/(sigma^2)
 d2ldd2
                                    },
                 dldv = function(y,mu,sigma,nu) {
       z <- ifelse(nu != 0,(((y/mu)^nu-1)/(nu*sigma)),log(y/mu)/sigma)
       h <- dnorm(1/(sigma*abs(nu)))/pnorm(1/(sigma*abs(nu)))   
       l <- log(y/mu)                                       
    dldv <- (z-(l/sigma))*(z/nu) -l*(z*z-1)
    dldv <- dldv+sign(nu)*h/(sigma*nu^2)                 
    dldv
                                    },
               d2ldv2 = function(sigma) {
   d2ldv2 <- -7*sigma*sigma/4
   d2ldv2
                                    },
              d2ldmdd = function(mu,sigma,nu) -2*nu/(mu*sigma),
              d2ldmdv = function(mu) 1/(2*mu),
              d2ldddv = function(sigma,nu) -sigma*nu,
          G.dev.incr  = function(y,mu,sigma,nu,...) 
                     -2*dBCCGo(y,mu=mu,sigma=sigma,nu=nu,log=TRUE), 
             rqres = expression(rqres(pfun="pBCCGo", type="Continuous", y=y, mu=mu, sigma=sigma, nu=nu)),
        mu.initial = expression( mu <- (y+mean(y))/2), 
     sigma.initial = expression( sigma <- rep(0.1,length(y))), 
        nu.initial = expression( nu <- rep(0.5, length(y))), 
          mu.valid = function(mu) TRUE , 
       sigma.valid = function(sigma)  all(sigma > 0),
          nu.valid = function(nu) TRUE , 
           y.valid = function(y) all(y>0)
          ),
            class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################

