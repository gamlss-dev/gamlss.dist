################################################################################
###GAMLSS Regression - WARING###
################################################################################
#Probability density function
dWARING<-function (x, mu=2, sigma=2, log = FALSE)
{
    if (any(mu < 0))
        stop(paste("mu must be > 0", "\n", ""))
    if (any(sigma < 0))
    	stop(paste("sigma must be > 0", "\n", ""))
  #  if (any(x < 0)) stop(paste("x must be >=0", "\n", ""))
     ly <- max(length(x),length(mu),length(sigma)) 
      x <- rep(x, length = ly)      
  sigma <- rep(sigma, length = ly)
     mu <- rep(mu, length = ly)   
      #b <- 1+(1/sigma)
      #n <- mu*(b-1)
     #fx <- lbeta(n+x, b+1)-lbeta(n,b)
     fx <- lbeta(x+(mu/sigma), (1/sigma)+2)-lbeta(mu/sigma,(1/sigma+1))
     fx <- if(log) fx else exp(fx) 
     fx[x < 0] <- 0
     fx[x == Inf] <- 0
     fx
}
################################################################################
################################################################################
################################################################################
################################################################################
#Cumulative density function
pWARING<-function (q,  mu=2, sigma=2,  lower.tail = TRUE, log.p = FALSE)
{
    if (any(mu < 0)) stop(paste("mu must be > 0", "\n", ""))
    if (any(sigma < 0)) stop(paste("sigma must be > 0", "\n", ""))
  #  if (any(q < 0)) stop(paste("q must be >=0", "\n", ""))
        ly <- max(length(q), length(mu), length(sigma))
        qq <- rep(q, length = ly)
qq[q==Inf] <- 1000
         p <- rep(0, length = ly)
        mu <- rep(mu, length = ly)
     sigma <- rep(sigma, length = ly)
     fn <- function(q, mu, sigma) sum(dWARING(0:q, mu=mu, sigma=sigma))
   Vcdf <- Vectorize(fn)
    cdf <- Vcdf(q=qq, mu=mu, sigma=sigma)
    # cdf <- 1- ((gamma((1+mu+sigma)/sigma)*gamma(1+(mu/sigma)+q))/
    #       (gamma(mu/sigma)*gamma(2+((1+mu)/sigma)+q)))
    if (lower.tail == TRUE) 
        cdf <- cdf
    else cdf = 1 - cdf
    if (log.p==TRUE) cdf <- log(cdf)
    cdf[q < 0] <- 0 
    cdf[q == Inf] <- 1   
    cdf
}
################################################################################
################################################################################
################################################################################
################################################################################
#Quantile Function
qWARING<- function (p,  mu=2, sigma=2, lower.tail = TRUE, log.p = FALSE, max.value = 10000)
{
if (any(mu < 0))
        stop(paste("mu must be > 0", "\n", ""))
if (any(sigma < 0))
    	stop(paste("sigma must be > 0", "\n", ""))
if (log.p==TRUE) p <- exp(p) 
if (lower.tail == FALSE) p <- 1-p 
    ly <- max(length(p), length(mu), length(sigma))
     p <- rep(p, length = ly)
   QQQ <- rep(0, length = ly)
    mu <- rep(mu, length = ly)
 sigma <- rep(sigma, length = ly)
for (i in seq(along = p)) {
        cumpro <- 0
        if (p[i] + 1e-09 >= 1)
            QQQ[i] <- Inf 
        else {
            for (j in seq(from = 0, to = max.value)) {
                cumpro <- pWARING(j, mu=mu[i], sigma=sigma[i])
                QQQ[i] <- j
                if (p[i] <= cumpro)
                  break
            }
        }
    }
 QQQ[p == 0] <- 0
 QQQ[p == 1] <- Inf
 QQQ[p <  0] <- NaN
 QQQ[p >  1] <- NaN
 return(QQQ)  
}
################################################################################
################################################################################
################################################################################
################################################################################
#Random Generating Function
rWARING<- function(n, mu=2, sigma=2)
{
    if (any(mu < 0))
        stop(paste("mu must be > 0", "\n", ""))
    if (any(sigma < 0))
    	stop(paste("sigma must be > 0", "\n", ""))
    if (any(n <= 0))
        stop(paste("n must be a positive integer", "\n", ""))
     n <- ceiling(n)
     p <- runif(n)
     r <- qWARING(p, mu=mu, sigma=sigma)
     as.integer(r)
}
################################################################################
################################################################################
################################################################################
################################################################################
#GAMLSS

WARING<-function (mu.link = "log", sigma.link = "log")
{
    mstats <- checklink("mu.link", "WARING", substitute(mu.link), "log")
    dstats <- checklink("sigma.link", "WARING", substitute(sigma.link), "log")
    structure(list(family = c("WARING", "Waring"),
        parameters = list(mu = TRUE, sigma = TRUE), nopar = 2,
        type = "Discrete", mu.link = as.character(substitute(mu.link)),
        sigma.link = as.character(substitute(sigma.link)), 
        mu.linkfun = mstats$linkfun,
        sigma.linkfun = dstats$linkfun, mu.linkinv = mstats$linkinv,
        sigma.linkinv = dstats$linkinv, mu.dr = mstats$mu.eta,
        sigma.dr = dstats$mu.eta,
        dldm = function(y, mu, sigma){ 
          dldm <- (1/sigma)*(digamma((mu/sigma) + y) - digamma(y+((mu+1)/sigma)+2) - 
                  digamma(mu/sigma) + digamma((mu+sigma+1)/sigma))
          dldm          
        },
        d2ldm2 = function(y, mu, sigma){
          dldm <- (1/sigma)*(digamma((mu/sigma) + y) - digamma(y+((mu+1)/sigma)+2) - 
                  digamma(mu/sigma) + digamma((mu+sigma+1)/sigma))
          d2ldm2 <- -dldm*dldm
          d2ldm2 <- ifelse(d2ldm2 < -1e-7, d2ldm2,-1e-7) 
          d2ldm2
        },
        dldd = function(y, mu, sigma){ 
          dldd <- (1/sigma^2)*(-1+(1/(sigma+1))-mu*digamma(y+(mu/sigma))+
		  (mu+1)*digamma(y+((mu+1)/sigma)+2)-(mu+1)*digamma(((mu+1)/sigma)+1)+
		  mu*digamma(mu/sigma))
          dldd                      
        },
        d2ldd2 = function(y, mu, sigma){
          dldd <- (1/sigma^2)*(-1+(1/(sigma+1))-mu*digamma(y+(mu/sigma))+
		  (mu+1)*digamma(y+((mu+1)/sigma)+2)-(mu+1)*digamma(((mu+1)/sigma)+1)+
		  mu*digamma(mu/sigma))
          d2ldd2 <- -dldd*dldd
          d2ldd2 <- ifelse(d2ldd2 < -1e-7, d2ldd2,-1e-7) 
          d2ldd2
        },
        d2ldmdd = function(y, mu, sigma){
          dldm <- (1/sigma)*(digamma((mu/sigma) + y) - digamma(y+((mu+1)/sigma)+2) - 
                  digamma(mu/sigma) + digamma((mu+sigma+1)/sigma))
          dldd <- (1/sigma^2)*(-1+(1/(sigma+1))-mu*digamma(y+(mu/sigma))+
		  (mu+1)*digamma(y+((mu+1)/sigma)+2)-(mu+1)*digamma(((mu+1)/sigma)+1)+
		  mu*digamma(mu/sigma))
         d2ldmdd <- -dldm*dldd
         d2ldmdd <- ifelse(d2ldmdd < -1e-7, d2ldmdd,-1e-7) 
         d2ldmdd
        },
        G.dev.incr = function(y, mu, sigma, ...) -2 * dWARING(y, mu, sigma, log = TRUE),
        rqres = expression(rqres(pfun = "pWARING",
            type = "Discrete", ymin = 0, y = y, mu = mu, sigma = sigma)),
        mu.initial = expression(mu <- rep(mean(y), length(y))), 
        sigma.initial = expression(sigma <- rep(1,length(y))), 
        mu.valid = function(mu) all(mu > 0),
        sigma.valid = function(sigma) all(sigma > 0), 
        y.valid = function(y) all(y >= 0),
		       mean = function(mu, sigma) mu,
		   variance = function(mu, sigma) ifelse(sigma < 1, mu * (mu + 1) * (1 + sigma) / (1 - sigma), Inf)
		  ), 
        class = c("gamlss.family", "family"))
}
################################################################################
################################################################################
################################################################################
################################################################################