# this functions takes a gamlss.family distribution defined in -Inf to Inf and 
# creates a log or logit version of it
# for gamlss.family distributions on positive line there is a problem 
# in that mu must be positive 
# 
# TO DO
# it would be nice if the parametrization for mu change log(mu) or logit(mu)
# this is more difficult sinse all mu aprearancies in the derivatives 
# have to change
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# pdf
Family.d <-function(family = "NO", type = c("log", "logit"), ...)
  {
    type <- match.arg(type)
   fname <- family
   if (mode(family) != "character" && mode(family) != "name")
   fname <- as.character(substitute(family))
 distype <- eval(call(family))$type
    dfun <- paste("d",fname,sep="")
     pdf <- eval(parse(text=dfun))   
fun <- if (type=="log")  
       function(x, log = FALSE, ...)
        {
      #  if (any(x < 0)) stop(paste("x must be positive", "\n", ""))
        dfun <- pdf(log(x),log = TRUE,...)-log(x)
        dfun <- if (log == TRUE) dfun else exp(dfun)
        dfun
       }
     else if (type=="logit")
      function(x, log = FALSE, ...)
       {
      #  if (any(x < 0) | any(x > 1)) 
      #  stop(paste("x must be between 0 and 1", "\n", "")) 
        dfun <-  pdf(log(x/(1-x)), log=TRUE, ...)-log(x)-log(1-x)
        dfun <- if (log == TRUE) dfun else exp(dfun)
        dfun
       } 
  fun
  }
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# cdf
Family.p <-function(family = "NO", type = c("log", "logit"), ...)
  {
  #	par=1
    type <- match.arg(type)
   fname <- family
   if (mode(family) != "character" && mode(family) != "name")
   fname <- as.character(substitute(family))
 distype <- eval(call(family))$type
    pfun <- paste("p",fname,sep="")
     cdf <- eval(parse(text=pfun))
fun <- if (type=="log")  
       function(q, ...)
        {
      #  if (any(q < 0)) stop(paste("q must be positive", "\n", ""))
        pfun <- cdf(log(q),...)
        pfun
       }
     else if (type=="logit")
      function(q,  ...)
       {
      #  if (any(q < 0) | any(q > 1)) 
     #   stop(paste("q must be between 0 and 1", "\n", "")) 
        pfun <- cdf(log(q/(1-q)),...)
        pfun
       } 
  fun
  }
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# inverse cdf
Family.q <-function(family = "NO", type = c("log", "logit"), ...)
  {
    type <- match.arg(type)
   fname <- family
   if (mode(family) != "character" && mode(family) != "name")
   fname <- as.character(substitute(family))
 distype <- eval(call(family))$type
    qfun <- paste("q",fname,sep="")
  invcdf <- eval(parse(text=qfun))
fun <- if (type=="log")  
       function(p,  ...)
        {
        qfun <- invcdf(p, ...)
        q <- exp(qfun)
        q
       }
     else if (type=="logit")
      function(p, ...)
       {
        qfun <- invcdf(p, ...)
        q <- 1/(1+exp(-qfun))
        q
       } 
  fun
  }
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# random generating
Family.r <-function(family = "NO", type = c("log", "logit"), ...)
{
  type <- match.arg(type)
  fname <- family
  if (mode(family) != "character" && mode(family) != "name")
    fname <- as.character(substitute(family))
  distype <- eval(call(family))$type
  rfun <- paste("r",fname,sep="")
  rfun <- eval(parse(text=rfun))
  fun <- if (type=="log")  
    function(n,  ...)
    {
      r <- rfun(n, ...)
      x <- exp(r)
      x
    }
  else if (type=="logit")
    function(n, ...)
    {
      r <- rfun(n, ...)
      x <- 1/(1+exp(-r))
      x
    } 
  fun
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# function to generate d p q r and fitting functions
gen.Family <-function(family = "NO", 
                      type = c("log", "logit"),
                    ...)
{
    type <- match.arg(type)
    fam  <- as.gamlss.family(family) #ds Monday, March 10, 2008 at 10:07
   fname <- fam$family[[1]] 
  # fname <- family
  # if (mode(family) != "character" && mode(family) != "name")
  # fname <- as.character(substitute(family))
    dfun <- paste( paste("d",type,sep=""), fname, sep="")
    pfun <- paste( paste("p",type,sep=""), fname, sep="")
    qfun <- paste( paste("q",type,sep=""), fname, sep="")
    rfun <- paste( paste("r",type,sep=""), fname, sep="")
     fun <- paste(type, fname, sep="")
  alldislist <-c(dfun,pfun,qfun,rfun,fun)
  # generate d 
  eval(dummy <- Family.d(family = fname, type = type, ...))
  eval(call("<-",as.name(dfun),dummy), envir=parent.frame(n = 1))
  # generate p
  eval(dummy <- Family.p(family = fname, type = type, ...))
  eval(call("<-",as.name(pfun),dummy), envir=parent.frame(n = 1))
  # generate q
  eval(dummy <- Family.q(family = fname, type = type, ...))
  eval(call("<-",as.name(qfun),dummy), envir=parent.frame(n = 1))
  # generate r
  eval(dummy <- Family.r(family = fname, type = type, ...))
  eval(call("<-",as.name(rfun),dummy), envir=parent.frame(n = 1))
  # generate the fitting distribution
  eval(dummy <- Family(family = family, type = type,  local=FALSE, ...))
  eval(call("<-",as.name(fun),dummy), envir=parent.frame(n = 1))
  cat("A ", type,  " family of distributions from",  fname, "has been generated \n", 
      "and saved under the names: ", "\n",paste(alldislist,sep=","),"\n")#
}
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# this function creates a function for fitting
# a log or logit gamlss.family continues distribution in gamlss() 
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
Family <- function (
  family = "NO", 
    type = c("log", "logit"),
   local = TRUE, 
    ...)
{
  #------------------------------------------
# # get the formal arguments of the gamlss. family
#   if (is.function(family))          Argum <- formals(family) 
#    else if (is.character(family))   Argum <- formals(get(family))
#    else  
#     {
#     chFamily <- deparse(substitute(family))
#           nc <- nchar(chFamily)
#          FAM <- substr(chFamily, 1, nc-2)
#        Argum <- formals(get(FAM))
#     }
# dummy name 
     TEST <- "TEST" 
     type <- match.arg(type)
     yvar <- switch(type,"log"="log(y)", "logit"="log(y/(1-y))")
    fname <- if (is.name(family)) as.character(family)
             else if (is.character(family)) family
             else if (is.call(family)) as.character(family[[1]])
             else if (is.function(family)) deparse(substitute(family))
             else if (is(family, "gamlss.family"))  family$family[1]
             else stop("the family must be a character or a gamlss.family name")
     fam1 <- eval(parse(text=fname)) # the family to output
      fam <- as.gamlss.family(family) # this is created so I can get things
    nopar <- fam$nopar
   family <- c("None", "None")  # 
   dorfun <- paste("d",fname,sep="") # say "dNO"
   porfun <- paste("p",fname,sep="") # say "pNO"
     dfun <- paste(paste("d",substr(type,start=1,stop=5),sep=""),fname, sep="") # say dlohNO
     pfun <- paste(paste("p",substr(type,start=1,stop=5),sep=""),fname, sep="") # 
  if (local)
  {
    #--trying to get gamlss sys.frame--  
   rexpr <- regexpr("gamlss",sys.calls())
    for (i in 1:length(rexpr)){ 
      position <- i 
      if (rexpr[i]==1) break}
    gamlss.environment <- sys.frame(position)      
    #--end here------------------------
  }
  else gamlss.environment <- sys.frame(0)
  #   generate d within gamlss
   eval(dummy <- Family.d(family = fname, type = type, ...))
   eval(call("<-",as.name(dfun),dummy), envir=gamlss.environment)# parent.frame(n = 1)
  # generate p within gamlss
   eval(dummy <- Family.p(family = fname, type = type, ...))
   eval(call("<-",as.name(pfun),dummy), envir=gamlss.environment)# parent.frame(n = 1)
  # rename the family 
  family[[1]] <- paste( substr(type,start=1,stop=5),fname, sep="")
  family[[2]] <- paste(type, fam$family[[2]])
   fam$family <- family 
body(fam1)[[nopar+2]][[2]]$family <- family
  # Global deviance increment  
          sGD <- gsub(dorfun, dfun, deparse(body(fam$G.dev.incr)))
body(fam$G.dev.incr) <- parse(text=sGD)
body(fam1)[[nopar+2]][[2]]$G.dev.incr <- fam$G.dev.incr
  # checking whether continuous
  if (!(fam$type=="Continuous")) stop("the gamlss.family distribution should be continuous")
  # now change the first derivatives
  switch(nopar,  
{
  # 1 parameter 
  # since only exponential exist do not use (we have not corrected for mu yet) 
  # fam$dldm
       yval <- gsub("y", yvar,  deparse(body(fam$dldm)))
       yval <- gsub('any[,1]', 'any', yval, fixed=T)
       body(fam$dldm) <- parse(text=yval)[[1]]   
       body(fam1)[[nopar+2]][[2]]$dldm  <- fam$dldm
       # fam$d2ldd2
       yval <-  gsub("y", yvar,  deparse(body(fam$d2ldm2)))
       yval <-  gsub('any[,1]', 'any', yval, fixed=T)
       body(fam$d2ldm2) <- parse(text=yval)[[1]] 
       body(fam1)[[nopar+2]][[2]]$d2ldm2  <- fam$d2ldm2
  #residuals
       sres <- gsub(porfun, pfun,   deparse(fam$rqres[[1]])) 
       sres <- gsub("expression", "",  sres)
  fam$rqres <- parse(text=sres)
  body(fam1)[[nopar+2]][[2]]$rqres <- fam$rqres  
  # initial mu fam$mu.initial
      inimu <- gsub("y", yvar,  deparse(fam$mu.initial[[1]]))
      #inimu <- gsub("expression", "",  inimu)
  fam$mu.initial <- parse(text=inimu)
  body(fam1)[[nopar+2]][[2]]$mu.initial <- fam$mu.initial
  #y.valid           
  yval <- switch(type,"log"="all(y > 0)", "logit"="all(y > 0 & y < 1)")
  body(fam$y.valid) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$y.valid  <- fam$y.valid 
}, 
{ 
  # 2 parameters 
  # fam$dldm
     yval <- gsub("y", yvar,  deparse(body(fam$dldm)))
     yval <-  gsub('any[,1]', 'any', yval, fixed=T)
    body(fam$dldm) <- parse(text=yval)[[1]] 
    body(fam1)[[nopar+2]][[2]]$dldm  <- fam$dldm
  # fam$d2ldd2
     yval <- gsub("y", yvar,  deparse(body(fam$d2ldm2)))
     yval <-  gsub('any[,1]', 'any', yval, fixed=T)
     body(fam$d2ldm2) <- parse(text=yval)[[1]]
     body(fam1)[[nopar+2]][[2]]$d2ldm2  <- fam$d2ldm2
  # fam$dldd
     yval <- gsub("y", yvar,  deparse(body(fam$dldd)))
     yval <-  gsub('any[,1]', 'any', yval, fixed=T)
     body(fam$dldd) <- parse(text=yval)[[1]]
     body(fam1)[[nopar+2]][[2]]$dldd  <- fam$dldd
  # fam$d2ldd2
     yval <- gsub("y", yvar,  deparse(body(fam$d2ldd2)))
     yval <-  gsub('any[,1]', 'any', yval, fixed=T)
     body(fam$d2ldd2) <- parse(text=yval)[[1]]
     body(fam1)[[nopar+2]][[2]]$d2ldd2  <- fam$d2ldd2
  # fam$d2ldmdd
     yval <- gsub("y", yvar,  deparse(body(fam$d2ldmdd)))
     yval <-  gsub('any[,1]', 'any', yval, fixed=T)
     body(fam$d2ldmdd) <- parse(text=yval)[[1]]
     body(fam1)[[nopar+2]][[2]]$d2ldmdd  <- fam$d2ldmdd
  #residuals
     sres <- gsub(porfun, pfun,  deparse(fam$rqres[[1]])) # mikis pick up only first
     #sres <- gsub("expression", "",  sres)
     fam$rqres <- parse(text=sres)
     body(fam1)[[nopar+2]][[2]]$rqres <- fam$rqres  
  # initial mu fam$mu.initial
    inimu <- gsub("y", yvar,  deparse(fam$mu.initial[[1]]))
    #inimu <- gsub("expression", "",  inimu)
    fam$mu.initial <- parse(text=inimu)
    body(fam1)[[nopar+2]][[2]]$mu.initial <- fam$mu.initial  
  # initial sigma fam$sigma.initial
 inisigma <- gsub("y", yvar,  deparse(fam$sigma.initial[[1]]))
 #inisigma <- gsub("expression", "",  inisigma)
  fam$sigma.initial <- parse(text=inisigma) 
  body(fam1)[[nopar+2]][[2]]$sigma.initial <- fam$sigma.initial 
  #y.valid           
     yval <- switch(type,"log"="all(y > 0)", "logit"="all(y > 0 & y < 1)")
     body(fam$y.valid) <- parse(text=yval)[[1]]
     body(fam1)[[nopar+2]][[2]]$y.valid <- fam$y.valid 
 }, 
{
  # 3 parameters   
  # fam$dldm
   yval <- gsub("y", yvar,  deparse(body(fam$dldm)))
   yval <-  gsub('any[,1]', 'any', yval, fixed=T)
   body(fam$dldm) <- parse(text=yval)[[1]]  
   body(fam1)[[nopar+2]][[2]]$dldm  <- fam$dldm
  # fam$d2ldm2
   yval <- gsub("y", yvar,  deparse(body(fam$d2ldm2)))
   yval <-  gsub('any[,1]', 'any', yval, fixed=T)
   yval <-  gsub('anlog(y)', 'any', yval, fixed=T)
   body(fam$d2ldm2) <- parse(text=yval)[[1]]
   body(fam1)[[nopar+2]][[2]]$d2ldm2  <- fam$d2ldm2
  # fam$dldd
   yval <- gsub("y", yvar,  deparse(body(fam$dldd)))
   yval <-  gsub('any[,1]', 'any', yval, fixed=T)
   body(fam$dldd) <- parse(text=yval)[[1]]
   body(fam1)[[nopar+2]][[2]]$dldd  <- fam$dldd
  # fam$d2ldd2
   yval <- gsub("y", yvar,  deparse(body(fam$d2ldd2)))
   yval <-  gsub('any[,1]', 'any', yval, fixed=T)
   body(fam$d2ldd2) <- parse(text=yval)[[1]]
   body(fam1)[[nopar+2]][[2]]$d2ldd2  <- fam$d2ldd2
  # fam$d2ldmdd
  yval <- gsub("y", yvar,  deparse(body(fam$d2ldmdd)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$d2ldmdd) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$d2ldmdd  <- fam$d2ldmdd
  # fam$dldv
  yval <- gsub("y", yvar,  deparse(body(fam$dldv)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$dldv) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$dldv  <- fam$dldv
  # fam$d2ldv2
  yval <- gsub("y", yvar,  deparse(body(fam$d2ldv2)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$d2ldv2) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$d2ldv2  <- fam$d2ldv2
  # fam$d2ldmdv
  yval <- gsub("y", yvar,  deparse(body(fam$d2ldmdv)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$d2ldmdv) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$d2ldmdv  <- fam$d2ldmdv
  # fam$d2ldddv
  yval <- gsub("y", yvar,  deparse(body(fam$d2ldddv)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$d2ldddv) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$d2ldddv  <- fam$d2ldddv
  #residuals
  sres <- gsub(porfun, pfun,   deparse(fam$rqres[[1]])) 
  sres <- gsub("expression", "",  sres)
  fam$rqres <- parse(text=sres)
  body(fam1)[[nopar+2]][[2]]$rqres <- fam$rqres 
  # initial mu fam$mu.initial
  inimu <- gsub("y", yvar,  deparse(fam$mu.initial[[1]]))
  #inimu <- gsub("expression", "",  inimu)
  fam$mu.initial <- parse(text=inimu)
  body(fam1)[[nopar+2]][[2]]$mu.initial <- fam$mu.initial 
  # initial sigma fam$sigma.initial
  inisigma <- gsub("y", yvar,  deparse(fam$sigma.initial[[1]]))
  #inisigma <- gsub("expression", "",  inisigma)
  fam$sigma.initial <- parse(text=inisigma) 
  body(fam1)[[nopar+2]][[2]]$sigma.initial <- fam$sigma.initial
  # initial nu fam$nu.initial
  ininu <- gsub("y", yvar,  deparse(fam$nu.initial[[1]]))
  #ininu <- gsub("expression", "",  ininu)
  fam$nu.initial <- parse(text=ininu)
  body(fam1)[[nopar+2]][[2]]$nu.initial <- fam$nu.initial 
  #y.valid           
  yval <- switch(type,"log"="all(y > 0)", "logit"="all(y > 0 & y < 1)")
  body(fam$y.valid) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$y.valid <- fam$y.valid 
},
{
  # 4 parameters
  # fam$dldm
  yval <- gsub("y", yvar,  deparse(body(fam$dldm)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$dldm) <- parse(text=yval)[[1]] 
  body(fam1)[[nopar+2]][[2]]$dldm  <- fam$dldm
  # fam$d2ldm2
  yval <- gsub("y", yvar,  deparse(body(fam$d2ldm2)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$d2ldm2) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$d2ldm2  <- fam$d2ldm2
  # fam$dldd
  yval <- gsub("y", yvar,  deparse(body(fam$dldd)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$dldd) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$dldd  <- fam$dldd
  # fam$d2ldd2
  yval <- gsub("y", yvar,  deparse(body(fam$d2ldd2)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$d2ldd2) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$d2ldd2  <- fam$d2ldd2
  # fam$d2ldmdd
  yval <- gsub("y", yvar,  deparse(body(fam$d2ldmdd)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$d2ldmdd) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$d2ldmdd  <- fam$d2ldmdd
  # fam$dldv
  yval <- gsub("y", yvar,  deparse(body(fam$dldv)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$dldv) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$dldv  <- fam$dldv
  # fam$d2ldv2
  yval <- gsub("y", yvar,  deparse(body(fam$d2ldv2)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$d2ldv2) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$d2ldv2  <- fam$d2ldv2
  # fam$d2ldmdv
  yval <- gsub("y", yvar,  deparse(body(fam$d2ldmdv)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$d2ldmdv) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$d2ldmdv  <- fam$d2ldmdv
  # fam$d2ldddv
  yval <- gsub("y", yvar,  deparse(body(fam$d2ldddv)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$d2ldddv) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$d2ldddv  <- fam$d2ldddv
  # fam$dldt
  yval <- gsub("y", yvar,  deparse(body(fam$dldt)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$dldt) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$dldt  <- fam$dldt
  # fam$d2ldt2
  yval <- gsub("y", yvar,  deparse(body(fam$d2ldt2)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$d2ldt2) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$d2ldt2 <- fam$d2ldt2
  # fam$d2ldmdt
  yval <- gsub("y", yvar,  deparse(body(fam$d2ldmdt)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$d2ldmdt) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$d2ldmdt  <- fam$d2ldmdt
  # fam$d2ldddt
  yval <- gsub("y", yvar,  deparse(body(fam$d2ldddt)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$d2ldddt) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$d2ldddt  <- fam$d2ldddt
  # fam$d2ldvdt
  yval <- gsub("y", yvar,  deparse(body(fam$d2ldvdt)))
  yval <-  gsub('any[,1]', 'any', yval, fixed=T)
  body(fam$d2ldvdt) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$d2ldvdt  <- fam$d2ldvdt
  #residuals
  sres <- gsub(porfun, pfun,   deparse(fam$rqres[[1]])) 
  sres <- gsub("expression", "",  sres)
  fam$rqres <- parse(text=sres)
  body(fam1)[[nopar+2]][[2]]$rqres   <- fam$rqres 
  # initial mu fam$mu.initial
  inimu <- gsub("y", yvar,  deparse(fam$mu.initial[[1]]))
  #inimu <- gsub("expression", "",  inimu)
  fam$mu.initial <- parse(text=inimu)
  body(fam1)[[nopar+2]][[2]]$mu.initial  <- fam$mu.initial
  # initial sigma fam$sigma.initial
  inisigma <- gsub("y", yvar,  deparse(fam$sigma.initial[[1]]))
  #inisigma <- gsub("expression", "",  inisigma)
  fam$sigma.initial <- parse(text=inisigma) 
  body(fam1)[[nopar+2]][[2]]$sigma.initial  <- fam$sigma.initial
  # initial nu fam$nu.initial
  ininu <- gsub("y", yvar,  deparse(fam$nu.initial[[1]]))
  #ininu <- gsub("expression", "",  ininu)
  fam$nu.initial <- parse(text=ininu)
  body(fam1)[[nopar+2]][[2]]$nu.initial <- fam$nu.initial
  # initial tau fam$tau.initial
  initau <- gsub("y", yvar,  deparse(fam$tau.initial[[1]]))
  #initau <- gsub("expression", "",  initau)
  fam$tau.initial <- parse(text=initau)
  body(fam1)[[nopar+2]][[2]]$tau.initial  <- fam$tau.initial
  #y.valid           
  yval <- switch(type,"log"="all(y > 0)", "logit"="all(y > 0 & y < 1)")
  body(fam$y.valid) <- parse(text=yval)[[1]]
  body(fam1)[[nopar+2]][[2]]$y.valid  <- fam$y.valid
})
#            fun <- function() {}
#   formals(fun) <- Argum
#      body(fun) <- fam
fam1
}