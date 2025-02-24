# ---------------------------------------------------------------------------------------
# The zero inflated beta distribution (BEZI)
# created by Raydonal Ospina 01 June of 2006
# IME-USP. University of San Paulo,
# Department of Statistics. San Paulo-Brazil
# rospina@ime.usp.br
# ---------------------------------------------------------------------------------------
BEZI = function (mu.link = "logit", sigma.link = "log", nu.link = "logit") 
{
    mstats <- checklink("mu.link", "BEZI", substitute(mu.link), 
        c("logit", "probit", "cloglog", "log", "own"))
    dstats <- checklink("sigma.link", "BEZI", substitute(sigma.link), 
        c("inverse", "log", "identity"))
    vstats <- checklink("nu.link", "BEZI", substitute(nu.link), 
        c("logit", "probit", "cloglog", "log", "own"))

    structure(list(family = c("BEZI", "Zero Inflated Beta"),
               parameters = list(mu = TRUE, sigma = TRUE, nu = TRUE),
                    nopar = 3,
                     type = "Mixed", 
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
        dldm = function(y,mu,sigma) #first derivate of log-density respect to mu 
        {           
                  p <- mu * sigma
                  q <- (1-mu)*sigma
            mustart <-digamma(p)-digamma(q) 
            ystart  <- log(y) - log(1 - y)
               dldm <- ifelse((y == 0) , 0, sigma * (ystart-mustart))
               dldm
        },

        d2ldm2 = function(y,mu,sigma) {        #second derivate of log-density respect to mu 
                  p <- mu * sigma
                  q <- (1-mu)*sigma
             d2ldm2 <- ifelse((y == 0) , 0,-sigma^2 * (trigamma(p)+trigamma(q)))
             d2ldm2
        },


        dldd = function(y,mu,sigma) {      #first derivate log-density respect to sigma 
                  p <- mu * sigma
                  q <- (1-mu)*sigma
            mustart <-digamma(p)-digamma(q) 
            ystart  <- log(y) - log(1 - y)
               dldd <- ifelse((y==0),0,mu*(ystart-mustart)+log(1-y)-digamma(q)+digamma(sigma))
               dldd
        },

        d2ldd2 = function(y,mu,sigma) {      #second derivate log-density respect to sigma 
                  p <- mu * sigma
                  q <- (1-mu)*sigma
            #mustart <-digamma(p)-digamma(q) 
             d2ldd2 <- ifelse((y==0),0,-(mu^2*trigamma(p)+(1-mu)^2*trigamma(q)-trigamma(sigma)))
             d2ldd2
        },

        dldv = function(y,nu) {       #first derivate log-density respect to nu
            dldv <- ifelse(y == 0, 1/nu, -1/(1 - nu))  
            dldv
        },


        d2ldv2 = function(nu) {         #second derivate log-density respect to nu
            d2ldv2 <- -1/(nu * (1 - nu))
            d2ldv2
        },
        

        d2ldmdd = function(y,mu,sigma) {   #partial derivate of log-density respect to mu and sigma  
            p <- mu * sigma
            q <- (1-mu)*sigma
            d2ldmdd <- ifelse((y == 0), 0, -sigma * (trigamma(p) * mu) - (trigamma(q) * (1 - mu)))
            d2ldmdd
        },

        d2ldmdv = function(y) {  #partial derivate of log-density respect to mu and alpha
            d2ldmdv <- rep(0,length(y))
            d2ldmdv
        },
        
        d2ldddv = function(y) {   #partial derivate of log-density respect to sigma and alpha
            d2ldddv <- rep(0,length(y))
            d2ldddv
        },

        
        G.dev.incr = function(y, mu, sigma, nu, ...){  # Global deviance
            -2 * dBEZI(y, mu, sigma, nu, log = TRUE)
        },

        rqres = expression({     # (Normalize quantile) residuals
            uval <- ifelse(y == 0, nu * runif(length(y), 0, 1), 
                (1 - nu) * pBEZI(y, mu, sigma, nu))
            rqres <- qnorm(uval) 
        }),
           mu.initial = expression(mu <- (y + mean(y))/2),
        sigma.initial = expression(sigma <- rep(1, length(y))),
           nu.initial = expression(nu <- rep(0.3, length(y))),
             mu.valid = function(mu) all(mu > 0 & mu < 1),
          sigma.valid = function(sigma) all(sigma > 0),
             nu.valid = function(nu) all(nu > 0 & nu < 1),
              y.valid = function(y) all(y >= 0 & y < 1)),
                class = c("gamlss.family", "family"))

}

#----------------------------------------------------------------------------------------
########## Density  function of Zero Inflated Beta ##########
dBEZI = function (x, mu = 0.5, sigma = 1, nu = 0.1, log = FALSE) 
{
    if (any(mu <= 0)|any(mu >= 1)) 
        stop(paste("mu must be beetwen 0 and 1 ", "\n", ""))
    if (any(sigma < 0))  #In this parametrization  sigma = phi
        stop(paste("sigma must be positive", "\n", ""))
    if (any(nu <= 0)|any(nu >= 1))  #In this parametrization  nu = alpha
        stop(paste("nu must be beetwen 0 and 1 ", "\n", ""))
    # if (any(x < 0)|any(x >= 1) ) 
    #     stop(paste("x must be beetwen [0, 1)", "\n", ""))
    a = mu * sigma
    b = (1 - mu) * sigma

    log.beta = dbeta(x, shape1 = a, shape2 = b, ncp = 0, log = TRUE)
        
    log.lik <- ifelse(x == 0, log(nu), log(1 - nu) + log.beta)
    if (log == FALSE) 
        fy <- exp(log.lik)
    else fy <- log.lik
    fy <- ifelse( x < 0 | x >= 1, 0, fy)
    fy
}
#----------------------------------------------------------------------------------------
########## Accumulate  function distribution of Zero Inflated Beta ##########
pBEZI = function (q, mu = 0.5, sigma = 1, nu = 0.1, lower.tail = TRUE, log.p = FALSE) 
{

    if (any(mu <= 0)|any(mu >= 1)) 
        stop(paste("mu must be beetwen 0 and 1 ", "\n", ""))
    if (any(sigma < 0))  #In this parametrization  sigma = phi
        stop(paste("sigma must be positive", "\n", ""))
    if (any(nu <= 0)|any(nu >= 1))  #In this parametrization  nu = alpha
        stop(paste("nu must be beetwen 0 and 1 ", "\n", ""))
 
    a = mu * sigma
    b = (1 - mu) * sigma

    cdf <- ifelse((q > 0 & q < 1), nu + (1-nu)*pbeta(q, shape1 = a, 
        shape2 = b, ncp = 0, lower.tail = TRUE, log.p = FALSE), 0)
    cdf <- ifelse((q == 0), nu, cdf)
    cdf <- ifelse((q >= 1), 1, cdf)

    if (lower.tail == TRUE) 
        cdf <- cdf
    else cdf = 1 - cdf
    if (log.p == FALSE) 
        cdf <- cdf
    else cdf <- log(cdf)
    cdf <- ifelse( q< 0, 0, cdf)
    cdf <- ifelse( q >= 01, 01, cdf)
    cdf
}

#----------------------------------------------------------------------------------------
########## Quantile function of Zero Inflated Beta ##########

qBEZI = function (p, mu = 0.5, sigma = 1, nu = 0.1, lower.tail = TRUE,
        log.p = FALSE)
{
    if (any(mu <= 0)|any(mu >= 1)) 
        stop(paste("mu must be beetwen 0 and 1 ", "\n", ""))
    if (any(sigma < 0))  #In this parametrization  sigma = phi
        stop(paste("sigma must be positive", "\n", ""))
    if (any(nu <= 0)|any(nu >= 1))  #In this parametrization  nu = alpha
        stop(paste("nu must be beetwen 0 and 1 ", "\n", ""))

    if (log.p == TRUE) 
        p <- exp(p)
    else p <- p
    if (lower.tail == TRUE) 
        p <- p
    else p <- 1 - p
    a = mu * sigma
    b = (1 - mu) * sigma

    suppressWarnings(q <- ifelse((nu >= p), 
        0, qbeta((p - nu)/(1-nu), 
            shape1 = a, shape2 = b, lower.tail = TRUE, log.p = FALSE)))
    q[p == 0] <- 0
    q[p == 1] <- Inf
    q[p <  0] <- NaN
    q[p >  1] <- NaN
    return(q)
}

#----------------------------------------------------------------------------------------
######## Random generation function of Zero Inflated Beta ########

rBEZI = function (n, mu = 0.5, sigma = 1, nu = 0.1) 
{
    if (any(mu <= 0) | any(mu >= 1)) 
        stop(paste("mu must be between 0 and 1", "\n", ""))
    if (any(sigma < 0))  #In this parametrization  sigma = phi
        stop(paste("sigma must be positive", "\n", ""))
    if (any(nu <= 0)|any(nu >= 1))  #In this parametrization  nu = alpha
        stop(paste("nu must be beetwen 0 and 1 ", "\n", ""))
    if (any(n <= 0)) 
        stop(paste("n must be a positive integer", "\n", ""))
    n <- ceiling(n)
    p <- runif(n)
    r <- qBEZI(p, mu = mu, sigma = sigma, nu = nu)
    r
}


#----------------------------------------------------------------------------------------
########## plot the function density of Zero Inflated Beta ##########

plotBEZI = function (mu = .5, sigma = 1, nu = 0.1, from = 0, to = 0.999, n = 101, 
    ...) 
{
    y = seq(from = 0.001, to = to, length.out = n)
    pdf <- dBEZI(y, mu = mu, sigma = sigma, nu = nu)
    pr0 <- c(dBEZI(0, mu = mu, sigma = sigma, nu = nu))
    po <- c(0)
    plot(pdf ~ y, main = "Zero Inflated Beta", ylim = c(0, max(pdf, 
        pr0)), type = "l")
    points(po, pr0, type = "h")
    points(po, pr0, type = "p", col = "blue")
}

#----------------------------------------------------------------------------------------
#calculates the expected value of the response for a Zero Inflated Beta fitted model 
meanBEZI = function (obj) 
{
    if (obj$family[1] != "BEZI") 
        stop("the object do not have a BEZI distribution")
    meanofY <- (1 - fitted(obj, "nu")) * fitted(obj, "mu")
    meanofY
}

#----------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------
#Examples

#BEZI()# gives information about the default links for the BEZI distribution
#dat<-rBEZI(100, mu=.5, sigma=5, nu=0.1)
#hist(dat)        
#mod1<-gamlss(dat~1,sigma.formula=~1, nu.formula=~1, family=BEZI) # fits a constant for mu, sigma and nu
#
#fitted(mod1)[1]
#
#summary(mod1)
#
#fitted(mod1,"mu")[1]
#fitted(mod1,"sigma")[1]
#fitted(mod1,"nu")[1]
#
#plot(function(y) dBEZI(y, mu=.5 ,sigma=5, nu=0.1), 0, .999)
#plot(function(y) pBEZI(y, mu=.5 ,sigma=5, nu=0.1), 0, 0.999)
#plot(function(y) qBEZI(y, mu=.5 ,sigma=5, nu=0.1), 0, 0.999)
#plot(function(y) qBEZI(y, mu=.5 ,sigma=5, nu=0.1, lower.tail=FALSE), 0, .999)
