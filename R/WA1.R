################################
###GAMLSS Regression - WARING###
################################

#Probability density function
dWA1<-function (x, mu=2, sigma=2, log = FALSE)
{
    if (any(mu < 0))
        stop(paste("mu must be > 0", "\n", ""))
    if (any(sigma < 0))
    	stop(paste("sigma must be > 0", "\n", ""))
    if (any(x < 0))
        stop(paste("x must be >=0", "\n", ""))
    fx <- beta(sigma+x, mu+1)/beta(sigma,mu)
    if (log==TRUE) fx <- lbeta(sigma+x, mu+1)-lbeta(sigma,mu)
    fx
}


#Cumulative density function
pWA1<-function (q,  mu=2, sigma=2,  lower.tail = TRUE, log.p = FALSE)
{
    if (any(mu < 0))
        stop(paste("mu must be > 0", "\n", ""))
    if (any(sigma < 0))
    	stop(paste("sigma must be > 0", "\n", ""))
    if (any(q < 0))
        stop(paste("q must be >=0", "\n", ""))
    ly <- max(length(q), length(mu), length(sigma))
    q <- rep(q, length = ly)
    mu<- rep(mu, length = ly)
    sigma<- rep(sigma, length = ly)
    s1<- seq(0, max(q))
    cdf<- cumsum(dWA1(s1, mu=mu, sigma=sigma))
    s2<-match(q,s1,nomatch=0)
    cdf<- cdf[s2]
    if (log.p==TRUE) cdf <- log(cdf)
    cdf
}

#Quantile Function
qWA1<-
function (p,  mu=2, sigma=2, lower.tail = TRUE, log.p = FALSE, max.value = 10000)
{
    if (any(mu < 0))
        stop(paste("mu must be > 0", "\n", ""))
    if (any(sigma < 0))
    	stop(paste("sigma must be > 0", "\n", ""))
    if (any(p < 0) | any(p > 1.0001))
        stop(paste("p must be in [0,1]", "\n", ""))
if (lower.tail) p <- p
else p <- 1 - p
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
                cumpro <- pWA1(j, mu=mu[i], sigma=sigma[i])
                QQQ[i] <- j
                if (p[i] <= cumpro)
                  break
            }
        }
    }
QQQ
}

#Random Generating Function
rWA1<- function(n, mu=2, sigma=2)
{
    if (any(mu < 0))
        stop(paste("mu must be > 0", "\n", ""))
    if (any(sigma < 0))
    	stop(paste("sigma must be > 0", "\n", ""))
    if (any(n <= 0))
        stop(paste("n must be a positive integer", "\n", ""))
     n <- ceiling(n)
     p <- runif(n)
     r <- qWA1(p, mu=mu, sigma=sigma)
     r
}

#GAMLSS

WA1<-function (mu.link = "log", sigma.link = "log")
{
    mstats <- checklink("mu.link", "WA1", substitute(mu.link), "log")
    dstats <- checklink("sigma.link", "WA1", substitute(sigma.link), "log")
    structure(list(family = c("WA1", "Waring"),
        parameters = list(mu = TRUE, sigma = TRUE), nopar = 2,
        type = "Discrete", mu.link = as.character(substitute(mu.link)),
        sigma.link = as.character(substitute(sigma.link)), 
        mu.linkfun = mstats$linkfun,
        sigma.linkfun = dstats$linkfun, mu.linkinv = mstats$linkinv,
        sigma.linkinv = dstats$linkinv, mu.dr = mstats$mu.eta,
        sigma.dr = dstats$mu.eta,
        dldm = function(y, mu, sigma){ 
          dldm <- digamma(sigma + y) - digamma(sigma + y +  mu + 1) + 
                  digamma(sigma + mu) + digamma(sigma)
          dldm          
        },
        d2ldm2 = function(y, mu, sigma){
          dldm <- digamma(sigma + y) - digamma(sigma + y +  mu + 1) + 
                  digamma(sigma + mu) + digamma(sigma)
          d2ldm2 <- -dldm*dldm
          d2ldm2
        },
        dldd = function(y, mu, sigma){ 
          dldd <- digamma(mu + 1) - digamma(sigma + y + mu + 1) +
                  digamma(sigma + mu) - digamma(mu)
          dldd                      
        },
        d2ldd2 = function(y, mu, sigma){
          dldd <- digamma(mu + 1) -digamma(sigma + y + mu + 1) +
                  digamma(sigma + mu) - digamma(mu)   
          d2ldd2 <- -dldd*dldd
          d2ldd2
        },
        d2ldmdd = function(y, mu, sigma){
          dldm <- digamma(sigma + y) - digamma(sigma + y +  mu + 1) + 
                  digamma(sigma + mu) + digamma(sigma)
          dldd <- digamma(mu + 1) -digamma(sigma + y + mu + 1) +
                  digamma(sigma + mu) - digamma(mu)
          d2ldmdd <- -dldm*dldd
          d2ldmdd
        },
        G.dev.incr = function(y, mu, sigma, ...) -2 * dWA1(y, mu, sigma, log = TRUE),
        rqres = expression(rqres(pfun = "pWA1",
            type = "Discrete", ymin = 0, y = y, mu = mu, sigma = sigma)),
        mu.initial = expression(mu <- (y + mean(y))/2), sigma.initial = expression(sigma <- rep(2,
            length(y))), mu.valid = function(mu) all(mu > 0),
        sigma.valid = function(sigma) all(sigma > 0), 
        y.valid = function(y) all(y >= 0)), 
        class = c("gamlss.family", "family"))
}

WA1<-function (mu.link = "log", sigma.link = "log")
{
    mstats <- checklink("mu.link", "WA1", substitute(mu.link),
        "log")
    dstats <- checklink("sigma.link", "WA1", substitute(sigma.link),
        "log")
    structure(list(family = c("WA1", "Waring"),
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
        dldm = function(y, mu, sigma){ 
          dldm <- (1/mu)+harmonic(mu+sigma-1)-harmonic(y+mu+sigma)
          dldm
        },
        d2ldm2 = function(y, mu, sigma){
          dldm <- (1/mu)+harmonic(mu+sigma-1)-harmonic(y+mu+sigma)
          d2ldm2 <- -dldm*dldm
          d2ldm2
        },
        dldd = function(y, mu, sigma){
          dldd <- harmonic(mu+sigma-1)-harmonic(y+mu+sigma)-
                  digamma(sigma)+digamma(y+sigma)
          dldd
        },
        d2ldd2 = function(y, mu, sigma ){
          dldd <- harmonic(mu+sigma-1)-harmonic(y+mu+sigma)-
                  digamma(sigma)+digamma(y+sigma)  
          d2ldd2 <- -dldd*dldd
        },
        d2ldmdd = function(y, mu, sigma){
          dldm <- (1/mu)+harmonic(mu+sigma-1)-harmonic(y+mu+sigma)
          dldd <- harmonic(mu+sigma-1)-harmonic(y+mu+sigma)-
                  digamma(sigma)+digamma(y+sigma)  
          d2ldmdd <- -dldm*dldd
        },
        G.dev.incr = function(y, mu, sigma, ...) -2 * dWA1(y, mu, sigma, log = TRUE),
        rqres = expression(rqres(pfun = "pWA1",
            type = "Discrete", ymin = 0, y = y, mu = mu, sigma = sigma)),
        mu.initial = expression(mu <- (y + mean(y))/2), 
        sigma.initial = expression(sigma <- rep(1, length(y))), 
        mu.valid = function(mu) all(mu > 0),
        sigma.valid = function(sigma) all(sigma > 0), 
        y.valid = function(y) all(y >= 0)), 
        class = c("gamlss.family", "family"))
}


#Need to program a harmonic function --> add this into other functions file.

harmonic<-function(z){
0.5772156649015328606065+digamma(z+1)
}