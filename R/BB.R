
################################################################################
################################################################################
################################################################################
################################################################################
# MS , RAR, KA amended 21_04_2010
BB <- function (mu.link = "logit", sigma.link = "log") 
{
    mstats <- checklink("mu.link", "Beta Binomial", substitute(mu.link),
                       c("logit", "probit", "cloglog", "cauchit", "log", "own"))
    dstats <- checklink("sigma.link", "Beta Binomial", substitute(sigma.link), 
                        c("inverse", "log", "identity", "sqrt", "own"))   
    structure(
          list(family = c("BB", "Beta Binomial"),
           parameters = list(mu=TRUE,sigma=TRUE), 
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
                 dldm = function(y,mu,sigma,bd) 
                                    (1/sigma)*(digamma(y+(mu/sigma))
                                    -digamma(bd+((1-mu)/sigma)-y)
                                    -digamma(mu/sigma)
                                    +digamma((1-mu)/sigma)),
               d2ldm2 = function(y,mu,sigma,bd) 
                                     (1/(sigma)^2)*(trigamma(y+(mu/sigma))
                                     +trigamma(bd+((1-mu)/sigma)-y)
                                     -trigamma(mu/sigma)
                                     -trigamma((1-mu)/sigma)),
                 dldd = function(y,mu,sigma,bd) 
                   {
                          k <- 1/sigma
                       dldd <- -(k^2)*(digamma(k)+mu*digamma(y+mu*k)+(1-mu)*
                                digamma(bd+(1-mu)*k-y)-mu*digamma(mu*k)-(1-mu)*
                                digamma((1-mu)*k)-digamma(bd+k))
                  dldd 
                    }, 
               d2ldd2 = function(y,mu,sigma,bd) 
                    {
                          k <- 1/sigma
                       dldd <- -(k^2)*(digamma(k)+mu*digamma(y+mu*k)+(1-mu)*
                                digamma(bd+(1-mu)*k-y)-mu*digamma(mu*k)-(1-mu)*
                                digamma((1-mu)*k)-digamma(bd+k))
                     d2ldd2 <- -dldd^2
                     d2ldd2 
                    }, 
              d2ldmdd = function(y) rep(0,length(y)),
           G.dev.incr = function(y,mu,sigma,bd,...) -2*dBB(y,mu,sigma,bd, log = TRUE), 
                rqres = expression(
                 rqres(pfun="pBB", type="Discrete", ymin=0, y=y, mu=mu, sigma=sigma, bd=bd)
                                   ), 
            mu.initial = expression (   mu <- (y+0.5)/(bd+1)),      
         sigma.initial = expression (sigma <- rep(1,length(y))),
              mu.valid = function(mu) all(mu > 0) && all(mu < 1), 
           sigma.valid = function(sigma)  all(sigma > 0), 
               y.valid = function(y)  all(y >= 0), 
                  mean = function(bd, mu, sigma) bd * mu,
              variance = function(bd, mu, sigma) bd * mu * (1 - mu) * (1 + sigma* (bd - 1) / (1 + sigma))
          ),
            class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dBB <- function(x, mu = 0.5, sigma = 1, bd = 10, log = FALSE)
 { 
    if (any(mu < 0) | any(mu > 1))   stop(paste("mu must be between 0 and 1 ", "\n", "")) 
    if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", ""))
    if (any(sigma < 1e-10)) warning(" values of sigma in BB less that 1e-10 are set to 1e-10" )
          ly <- max(length(x),length(mu),length(sigma),length(bd)) 
          xx <- rep(x, length = ly) 
    xx[x>bd] <- 0
       sigma <- rep(sigma, length = ly)
 sigma[sigma < 1e-10] <- 1e-10 
          mu <- rep(mu, length = ly)   
          bd <- rep(bd, length = ly) 
       logfy <-  (lgamma(bd+1)-lgamma(xx+1)-lgamma(bd-xx+1)
                  +lgamma((1/sigma))+lgamma(xx+mu*(1/sigma))
                  +lgamma(bd+((1-mu)/sigma)-xx)-lgamma(mu*(1/sigma))
                  -lgamma((1-mu)/sigma)-lgamma(bd+(1/sigma)))
logfy[sigma<0.0001]  <- dBI(xx, mu = mu, bd=bd, log = TRUE)
         fy <- if(log == FALSE) exp(logfy) else logfy
  fy[x < 0] <- 0 
fy[x > bd] <- 0 
          fy
  }
################################################################################
################################################################################
################################################################################
################################################################################
pBB <- function(q, mu = 0.5, sigma = 1, bd = 10, lower.tail = TRUE, log.p = FALSE)
  {     
    if (any(mu <= 0) | any(mu >= 1))   stop(paste("mu must be between 0 and 1 ", "\n", "")) 
    if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
        ly <- max(length(q),length(mu),length(sigma),length(bd)) 
        qq <- rep(q, length = ly)
  qq[q>bd] <- qq[q<0] <- 0
     sigma <- rep(sigma, length = ly)
        mu <- rep(mu, length = ly)   
        bd <- rep(bd, length = ly)   
        fn <- function(q, mu, sigma, bd) sum(dBB(0:q, mu=mu, sigma=sigma, bd=bd))
      Vcdf <- Vectorize(fn)
       cdf <- Vcdf(q=qq, mu=mu, sigma=sigma, bd=bd)
cdf[sigma<0.0001] <- pbinom(qq, prob = mu, size = bd, lower.tail=lower.tail, 
                                   log.p = log.p)
if (lower.tail==FALSE)  cdf <- 1-cdf
if (log.p==TRUE)        cdf <- log(cdf) 
      cdf[q<0] <- 0
      cdf[q>=bd] <- 1
      cdf
  }
################################################################################
################################################################################
################################################################################
################################################################################
qBB <- function(p, mu=0.5, sigma=1, bd=10, lower.tail = TRUE, log.p = FALSE)
  {      
if (any(mu <= 0) | any(mu >= 1))   stop(paste("mu must be between 0 and 1 ", "\n", "")) 
if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", ""))
if (log.p==TRUE) p <- exp(p) 
if (lower.tail == FALSE) p <- 1-p 
        ly <- max(length(p),length(mu),length(sigma),length(bd)) 
         p <- rep(p, length = ly) 
     sigma <- rep(sigma, length = ly)
        mu <- rep(mu, length = ly)   
        bd <- rep(bd, length = ly)   
       QQQ <- rep(0,ly)                         
    nsigma <- rep(sigma, length = ly)
       nmu <- rep(mu, length = ly) 
       nbd <- rep(bd, length = ly)  
    for (i in seq(along=p))                                                          
      { 
    cumpro <- 0                                                                      
          for (j in seq(from = 0, to = nbd[i]))
            {
       cumpro <- pBB(j, mu = nmu[i], sigma = nsigma[i] , bd = nbd[i] , log.p = FALSE)
       QQQ[i] <- j 
       if  (p[i] <= cumpro ) break 
            } 
    }    
          invcdf <- QQQ
        invcdf[sigma<0.0001] <-  qBI(p[sigma<0.0001], mu = mu[sigma<0.0001], bd=bd[sigma<0.0001], lower.tail=TRUE)
          invcdf[p == 0] <- 0
          invcdf[p == 1] <- bd
          invcdf[p <  0] <- NaN
          invcdf[p >  1] <- NaN
     return(invcdf)    
   }
################################################################################
################################################################################
################################################################################
################################################################################
rBB <- function(n, mu = 0.5, sigma = 1, bd = 10)
  { 
          if (any(mu <= 0) | any(mu >= 1))   stop(paste("mu must be between 0 and 1 ", "\n", "")) 
          if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
          if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
          n <- ceiling(n)
          p <- runif(n)
          r <- qBB(p, mu=mu, sigma=sigma, bd=bd)
          as.integer(r)
  }
################################################################################
################################################################################
################################################################################
################################################################################
