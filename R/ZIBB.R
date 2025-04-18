################################################################################
################################################################################
################################################################################
################################################################################
# NOTE sigma link function should be the log link *****************************************
ZIBB <- function (mu.link ="logit", sigma.link = "log", nu.link = "logit")
{
    mstats <- checklink("mu.link", "ZIBB", substitute(mu.link),
                        c("logit", "probit", "cloglog", "cauchit", "log", "own"))
    dstats <- checklink("sigma.link", "Beta Binomial", substitute(sigma.link), 
                        c("inverse", "log", "identity", "sqrt", "own"))   
    vstats <- checklink("nu.link", "ZIBB", substitute(nu.link),
                        c("logit", "probit", "cloglog", "cauchit", "log", "own"))
    structure(
            list(family = c("ZIBB", "Zero Inflated Beta Binomial"),
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
#========================First Derivatives========================
    dldm = function(y,mu,sigma,nu,bd) 
                             {
                        dldm0 <- (1-nu)*((nu+(1-nu)*dBB(0,mu,sigma,bd))^(-1))*dBB(0,mu,sigma,bd)*BB()$dldm(0,mu,sigma,bd)
                         dldm <- ifelse(y==0, dldm0, BB()$dldm(y,mu,sigma,bd))
                         dldm},
    dldd = function(y,mu,sigma,nu,bd) 
                             {
                        dldd0 <- (1-nu)*((nu+(1-nu)*dBB(0,mu,sigma,bd))^(-1))*dBB(0,mu,sigma,bd)*BB()$dldd(0,mu,sigma,bd)
                         dldd <- ifelse(y==0, dldd0, BB()$dldd(y,mu,sigma,bd))
                         dldd},
    dldv = function(y,mu,sigma,nu,bd) 
                             {
                        dldv0 <- ((nu+(1-nu)*dBB(0,mu,sigma,bd))^(-1))*(1-dBB(0,mu,sigma,bd))
                         dldv <- ifelse(y==0, dldv0, -1/(1-nu))
                         dldv},
#========================Second Derivatives 1========================
    d2ldm2 = function(y,mu,sigma,nu,bd) {dldm0 <- (1-nu)*((nu+(1-nu)*dBB(0,mu,sigma,bd))^(-1))*dBB(0,mu,sigma,bd)*BB()$dldm(0,mu,sigma,bd)
                         dldm <- ifelse(y==0, dldm0, BB()$dldm(y,mu,sigma,bd))
                       d2ldm2 <- -dldm*dldm    
                       d2ldm2},
    d2ldd2 = function(y,mu,sigma,nu,bd) {dldd0 <- (1-nu)*((nu+(1-nu)*dBB(0,mu,sigma,bd))^(-1))*dBB(0,mu,sigma,bd)*BB()$dldd(0,mu,sigma,bd)
                         dldd <- ifelse(y==0, dldd0, BB()$dldd(y,mu,sigma,bd))
                       d2ldd2 <- -dldd*dldd
                       d2ldd2},
    d2ldv2 = function(y,mu,sigma,nu,bd) {dldv0 <- ((nu+(1-nu)*dBB(0,mu,sigma,bd))^(-1))*(1-dBB(0,mu,sigma,bd))
                         dldv <- ifelse(y==0, dldv0, -1/(1-nu))
                       d2ldv2 <- -dldv*dldv
                       d2ldv2},
#========================Second Derivatives 2========================
    d2ldmdd = function(y,mu,sigma,nu,bd) 
                              {
                        dldm0 <- (1-nu)*((nu+(1-nu)*dBB(0,mu,sigma,bd))^(-1))*dBB(0,mu,sigma,bd)*BB()$dldm(0,mu,sigma,bd)
                         dldm <- ifelse(y==0, dldm0, BB()$dldm(y,mu,sigma,bd))  
                        dldd0 <- (1-nu)*((nu+(1-nu)*dBB(0,mu,sigma,bd))^(-1))*dBB(0,mu,sigma,bd)*BB()$dldd(0,mu,sigma,bd)
                         dldd <- ifelse(y==0, dldd0, BB()$dldd(y,mu,sigma,bd))
                      d2ldmdd <- -dldm*dldd
                       d2ldmdd},
    d2ldmdv = function(y,mu,sigma,nu,bd) 
                              {
                        dldm0 <- (1-nu)*((nu+(1-nu)*dBB(0,mu,sigma,bd))^(-1))*dBB(0,mu,sigma,bd)*BB()$dldm(0,mu,sigma,bd)
                         dldm <- ifelse(y==0, dldm0, BB()$dldm(y,mu,sigma,bd))  
                        dldv0 <- ((nu+(1-nu)*dBB(0,mu,sigma,bd))^(-1))*(1-dBB(0,mu,sigma,bd))
                         dldv <- ifelse(y==0, dldv0, -1/(1-nu))
                      d2ldmdv <- -dldm*dldv
                      d2ldmdv},
    d2ldddv = function(y,mu,sigma,nu,bd) 
                             {
                        dldd0 <- (1-nu)*((nu+(1-nu)*dBB(0,mu,sigma,bd))^(-1))*dBB(0,mu,sigma,bd)*BB()$dldd(0,mu,sigma,bd)
                         dldd <- ifelse(y==0, dldd0, BB()$dldd(y,mu,sigma,bd))
                        dldv0 <- ((nu+(1-nu)*dBB(0,mu,sigma,bd))^(-1))*(1-dBB(0,mu,sigma,bd))
                         dldv <- ifelse(y==0, dldv0, -1/(1-nu))
                      d2ldddv <- -dldd*dldv
                      d2ldddv},
    G.dev.incr = function(y,mu,sigma,nu,bd,...) -2*dZIBB(y,mu,sigma,nu,bd,log=TRUE),   # sigma and nu were in the wrong order
         rqres = expression(rqres(pfun="pZIBB", type="Discrete", ymin=0, y=y, mu=mu, sigma=sigma, nu=nu, bd=bd)),
    mu.initial = expression(mu <- rep(0.5,length(y))),
 sigma.initial = expression(sigma <- rep(0.5,length(y))),
    nu.initial = expression(nu <-rep(0.3, length(y))),
      mu.valid = function(mu) all(mu > 0 & mu < 1),
      sigma.valid = function(sigma)  all(sigma > 0), 
      nu.valid = function(nu) all(nu > 0 & nu < 1), 
       y.valid = function(y)  all(y >= 0)),
        class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dZIBB<-function(x, mu = 0.5, sigma = 0.5, nu = 0.1, bd = 1, log = FALSE)
 { 
if (any(mu <= 0) ||  any(mu >= 1) )  stop(paste("mu must be between 0 and 1", "\n", ""))           
if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
if (any(nu <= 0) |  any(nu >= 1) )  stop(paste("nu must be between 0 and 1", "\n", "")) 
             ly <- max(length(x),length(mu),length(sigma),length(nu),length(bd)) 
              x <- rep(x, length = ly)      
          sigma <- rep(sigma, length = ly)
             mu <- rep(mu, length = ly)
             nu <- rep(nu, length = ly)   
             bd <- rep(bd, length = ly) 
          logfy <- rep(0, ly)
if (any(x==0))  logfy[x==0] <- log(nu[x==0]+(1-nu[x==0])*dBB(0,mu[x==0],sigma[x==0],bd[x==0]))
if (any(x!=0))  logfy[x!=0] <- log(1-nu[x!=0]) + dBB(x[x!=0],mu[x!=0],sigma[x!=0],bd[x!=0],log=T)
if(log == FALSE) fy <- exp(logfy) else fy <- logfy
          fy[x < 0] <- 0 
          fy[x > bd] <- 0 
          fy
  }
################################################################################
################################################################################
################################################################################
################################################################################
pZIBB <- function(q, mu = 0.5, sigma = 0.5, nu = 0.1, bd = 1, lower.tail = TRUE, log.p = FALSE)
  {     
         if (any(mu <= 0) |  any(mu >= 1) )  stop(paste("mu must be between 0 and 1", "\n", ""))  
         if (any(nu <= 0) |  any(nu >= 1) )  stop(paste("nu must be between 0 and 1", "\n", ""))         
         if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
   #      if (any(q < 0) )  stop(paste("y must be 0 or greater than 0", "\n", "")) 
          ly <- max(length(q),length(mu),length(sigma),length(nu),length(bd)) 
           q <- rep(q, length = ly)      
       sigma <- rep(sigma, length = ly)
          mu <- rep(mu, length = ly)
          nu <- rep(nu, length = ly)   
          bd <- rep(bd, length = ly) 
         cdf <- rep(0,ly)
         cdf <- pBB(q = q, mu = mu, sigma = sigma, bd = bd, lower.tail = TRUE, log.p = FALSE)
         cdf <- nu + (1-nu)*cdf 
         cdf <- ifelse(cdf>1L, 1L , cdf)
         if(lower.tail == TRUE) cdf <- cdf else cdf <-1-cdf
         if(log.p==FALSE) cdf <- cdf else cdf <- log(cdf) 
         cdf[q<0] <- 0
         cdf[q>bd] <- 1
         cdf
   }
################################################################################
################################################################################
################################################################################
################################################################################
qZIBB <- function(p, mu = 0.5, sigma = 0.5, nu = 0.1, bd = 1, lower.tail = TRUE, log.p = FALSE)
  {      
if (any(mu <= 0) |  any(mu >= 1) )  stop(paste("mu must be between 0 and 1", "\n", "")) 
if (any(nu <= 0) |  any(nu >= 1) )  stop(paste("nu must be between 0 and 1", "\n", ""))        
if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
            ly <- max(length(p),length(mu),length(sigma),length(nu),length(bd)) 
             p <- rep(p, length = ly)   
             q <- rep(0, length = ly)      
         sigma <- rep(sigma, length = ly)
            mu <- rep(mu, length = ly)
            nu <- rep(nu, length = ly)   
            bd <- rep(bd, length = ly) 
          pnew <- ((p-nu)/(1-nu)) - (1e-7)
if (any( pnew<0))   pnew[ pnew<0 ] <- 0
if (any( pnew>0))   q[ pnew>0 ] <- qBB(p = pnew[ pnew>0 ], mu = mu[ pnew>0 ], 
    sigma=sigma[ pnew>0 ], bd=bd[ pnew>0 ], lower.tail = lower.tail, log.p = log.p)
     q[p == 0] <- 0
     q[p == 1] <- bd
     q[p <  0] <- NaN
     q[p >  1] <- NaN
          return(q)
   }
################################################################################
################################################################################
################################################################################
################################################################################
rZIBB <- function(n, mu = 0.5, sigma = 0.5, nu = 0.1, bd = 1)
  { 
    if (any(mu <= 0) |  any(mu >= 1) )  stop(paste("mu must be between 0 and 1", "\n", ""))
    if (any(nu <= 0) |  any(nu >= 1) )  stop(paste("mu must be between 0 and 1", "\n", ""))           
    if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
    if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
          n <- ceiling(n)
          p <- runif(n)
          r <- qZIBB(p, mu = mu, sigma = sigma, nu = nu, bd = bd)
          as.integer(r)
  }
################################################################################
################################################################################
################################################################################
################################################################################
