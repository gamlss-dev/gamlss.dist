# it could be something wrong with the derivative of mu
# does it do the right thing?
ZAZIPF <- function (mu.link = "log", sigma.link = "logit")
{
    mstats <- checklink("mu.link", "ZIPF", substitute(mu.link),
                           c("inverse", "log", "sqrt", "identity"))   
    dstats <- checklink("sigma.link", "ZALG", substitute(sigma.link), 
                           c("logit", "probit", "cloglog", "cauchit", "log", "own"))   
    structure(
          list(family = c("ZAZIPF", "Zero Adjusted ZIPF"),
           parameters = list(mu=TRUE, sigma=TRUE), 
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
                    {
                  dldm <- ifelse(y==0, 0, as.vector(attr(numeric.deriv(dZIPF(y[y!=0], mu, log=TRUE), "mu", delta=0.001), "gradient")))
                  dldm
                    }, 
    d2ldm2 = function(y,mu,sigma) {
                  dldm <- ifelse(y==0, 0, as.vector(attr(numeric.deriv(dZIPF(y[y!=0], mu, log=TRUE), "mu", delta=0.001), "gradient")))
                d2ldm2 <- -dldm^2
                         d2ldm2},
     dldd = function(y,mu,sigma) {dldd <- ifelse(y==0, 1/sigma, -1/(1-sigma))
                         dldd}, 
    d2ldd2 = function(y,mu,sigma) {dldd <- ifelse(y==0, 1/sigma, -1/(1-sigma))
                         d2ldd2<- -dldd^2
                         d2ldd2},
  d2ldmdd = function(y,mu,sigma) {d2ldmdd <- 0
                      d2ldmdd},
 G.dev.incr  = function(y,mu,sigma,...) -2*dZAZIPF(y,mu,sigma,log=TRUE),                      
         rqres = expression(rqres(pfun="pZAZIPF", type="Discrete", ymin=0, y=y, mu=mu, sigma=sigma)) ,
    mu.initial = expression({mu <- rep(.1,length(y))}),
   sigma.initial = expression(sigma <-rep(0.1, length(y))), 
      mu.valid = function(mu) all(mu > 0),
   sigma.valid = function(sigma)  all(sigma > 0 & sigma < 1), 
       y.valid = function(y)  all(y >= 0),
          mean = function(mu, sigma) {
                         b <- zetaP(mu) / zetaP(mu + 1)
                         return(ifelse(mu > 1, b * (1 - sigma), Inf))
                  },
      variance = function(mu, sigma) {
                         b <- zetaP(mu) / zetaP(mu + 1)
                         return( ifelse(mu > 2, (1 - sigma) * (zetaP(mu - 1) / zetaP(mu + 1)) - (1 - sigma)^2 * b^2, Inf) )
                  }
          ),
            class = c("gamlss.family","family"))
}
#------------------------------------------------------------------------------------------
dZAZIPF<-function(x, mu = 0.5, sigma = 0.1, log = FALSE)
 { 
          if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", ""))      
          if (any(sigma <= 0) | any(sigma >= 1) )  stop(paste("sigma must be between 0 and 1", "\n", "")) 
        #  if (any(x < 0) )  stop(paste("x must be 0 or greater than 0", "\n", "")) 
              ly <- max(length(x),length(mu),length(sigma)) 
               x <- rep(x, length = ly)      
           sigma <- rep(sigma, length = ly)
              mu <- rep(mu, length = ly)
           logfy <- rep(0, ly)
           logfy <- ifelse((x==0), log(sigma), log(1-sigma)+dZIPF(ifelse(x==0,1,x),mu,log = TRUE))      
          if(log == FALSE) fy <- exp(logfy) else fy <- logfy
          fy <-ifelse(x < 0, 0, fy)  
          fy
  }
#------------------------------------------------------------------------------------------
pZAZIPF <- function(q, mu = 0.5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
  {     
         if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", ""))         
         if (any(sigma <= 0) | any(sigma >= 1) )  stop(paste("sigma must be between 0 and 1", "\n", "")) 
     #    if (any(q < 0) )  stop(paste("y must be 0 or greater than 0", "\n", ""))
           ly <- max(length(q),length(mu),length(sigma)) 
            q <- rep(q, length = ly)      
        sigma <- rep(sigma, length = ly)
           mu <- rep(mu, length = ly) 
          cdf <- rep(0,ly)
         cdf1 <- ifelse((q==0),0,pZIPF(ifelse(q==0,1,q), mu, log.p = FALSE))
         cdf2 <- sigma + (1-sigma)*cdf1
          cdf <- ifelse((q==0),sigma,  cdf2)
         if(lower.tail == TRUE) cdf <- cdf else cdf <-1-cdf
         if(log.p==FALSE) cdf <- cdf else cdf <- log(cdf) 
         cdf <-ifelse(q < 0, 0, cdf)
         cdf
   }
#-----------------------------------------------------------------------------------------
qZAZIPF <- function(p, mu = 0.5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE, max.value = 10000)
  {      
         if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", ""))   
         if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0", "\n", "")) 
         if (any(p <= 0) | any(p >= 1))  stop(paste("p must be between 0 and 1", "\n", "")) 
         if (log.p == TRUE) p <- exp(p)   else p <- p
         if (lower.tail == TRUE)  p <- p  else p <- 1 - p
             ly <- max(length(p),length(mu),length(sigma)) 
              p <- rep(p, length = ly)      
          sigma <- rep(sigma, length = ly)
             mu <- rep(mu, length = ly)
          pnew  <- (p-sigma)/(1-sigma)-1e-10
          pnew <- ifelse((pnew >0) ,pnew, 0)
          q <- ifelse((pnew > 0 ), qZIPF(pnew, mu, log.p=FALSE, max.value = max.value  ), 0)   
          q
   }
#-----------------------------------------------------------------------------------------
rZAZIPF <- function(n, mu = 0.5, sigma=0.1, max.value = 10000)
  {
    if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", ""))         
    if (any(sigma <= 0) )  stop(paste("sigma must greated than 0", "\n", "")) 
    if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
          n <- ceiling(n)
          p <- runif(n)
          r <- qZAZIPF(p, mu = mu, sigma = sigma, max.value = 10000)
          as.integer(r)
  }
#-----------------------------------------------------------------------------------------
