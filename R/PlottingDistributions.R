################################################################################
################################################################################
################################################################################
################################################################################
# function to plot distribution in the book
#
#require(gamlss)
#rm(list=ls())
################################################################################
################################################################################
################################################################################
################################################################################
# GENERAL FUNCTIONS 
# 1 mtexti() TO CREATE TEXT AROUND A PLOT
#----------------------------------------------------
# FUNCTIONS FOR DISCRETE family DISTRIBUTIONS
# 2   disc1_3()  one parameter family count with three plots
# 3   disc1_22() one parameter family count with 2X2 plots
# 4   disc2_32() two parameter count with 3x2 plot
# 5   disc2_23() two parameter count with 2x3 plot
# 6   disc2_33() two parameter count with 3x3 plot
# 7   disc3_32() three parameters count with 3x2 plot
# 8   disc3_33() three parameters count with 3x3 plot
#-----------------------------------------------------
# FUNCTIONS FOR CONTINUOUS family DISTIBUTIONS
# 9    contR2  
# 10   contR3
# 11   contR4
# 12   contRplus2
# 13   contRplus3
# 14   contRplus4
################################################################################
################################################################################
################################################################################
################################################################################
# putting text around a graph
################################################################################
################################################################################
################################################################################
################################################################################
# FUNCTION 1
################################################################################
################################################################################
################################################################################
################################################################################
mtexti <- function(text, side, off = 0.25,
                   srt = if(side == 2) 90  else
                     if(side == 4) 270 else 0, 
                    ypos=NULL, xpos=NULL, ...) {
  # dimensions of plotting region in user units
  usr <- par("usr")
  # dimensions of plotting region in inches
  pin <- par("pin")
  # user units per inch
  upi <- c(usr[2]-usr[1],
           usr[4]-usr[3]) / pin
  # default x and y positions
  xpos <- if(is.null(xpos)) (usr[1] + usr[2])/2 else xpos
  ypos <- if(is.null(ypos)) (usr[3] + usr[4])/2 else ypos 
  if(1 == side)
    ypos <- usr[3] - upi[2] * off
  if(2 == side)
    xpos <- usr[1] - upi[1] * off
  if(3 == side)
    ypos <- usr[4] + upi[2] * off
  if(4 == side)
    xpos <- usr[2] + upi[1] * off
  text(x=xpos, y=ypos, text, xpd=NA, srt=srt, ...)
}
################################################################################
################################################################################
################################################################################
################################################################################
# I am not sure about this
#<<echo=FALSE,include=FALSE>>=
# dgen<-function(familyr=c("norm","gamma"),x){
#   familyr <- match.arg(familyr)
#   familyr <- paste0("d", familyr, "(", as.character(x),")")
#   cat("Evaluating", familyr, "...\n")
#   return(eval(parse(text=familyr)))
# }
################################################################################
################################################################################
################################################################################
################################################################################
# Gillian's one parameter discrete
### Discrete count, 1 parameter 3 figures 3x1
# disc1_3("PO)
#################################################################################
################################################################################
################################################################################
################################################################################
# FUNCTION 2
################################################################################
################################################################################
################################################################################
################################################################################
#plot one parameter with three plots
disc1_3 <- function(family=PO, mu=c(1,2,5), miny=0, maxy=10, cex.y.axis=1.2, cex.all=1.5)
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
           else if (is.character(family)) family
           else if (is.call(family)) as.character(family[[1]])
           else if (is.function(family)) deparse(substitute(family))
           else if (is(family, "gamlss.family"))  family$family[1]
          else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
    type <- fam$type
   if (type!="Discrete") stop("This plot is design for discrete distributions")
   if (nopar>=2) stop("This plot is design for one parameter discrete distributions")
  #---------------------------------------------------------
     op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,1))
      y <- miny:maxy
  #1
  fy <- eval(parse(text=paste0("d",fname,"(y,mu[1])")))
  plot(y,fy,type="h",ylab="f(y)",lwd=1.5,xaxt="n",ylim=c(0,1.2*max(fy)),
       cex.axis=cex.y.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[1]),"  ")),bty="n",
         cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  mtexti(fname, 3 , off=.5, cex=1.5 )
  #2
  fy <- eval(parse(text=paste0("d",fname,"(y,mu[2])")))
  plot(y,fy,type="h",ylab="f(y)",lwd=1.5,xaxt="n",yaxt="n",ylim=c(0,1.2*max(fy)),
       cex.axis=cex.y.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[2]),"  ")),bty="n",
         cex=cex.all)
  axis(4,cex.axis=cex.y.axis)
  mtexti("f(y)", 2, 0.6,cex=cex.all)  
  #3
  fy <- eval(parse(text=paste0("d",fname,"(y,mu[3])")))
  plot(y,fy,type="h",ylab="f(y)",lwd=1.5,ylim=c(0,1.2*max(fy)),
       cex.axis=cex.y.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[3]),"  ")),bty="n",
         cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}
################################################################################
################################################################################
################################################################################
################################################################################
# FUNCTION 3
################################################################################
################################################################################
################################################################################
################################################################################
# Gillian's one parameter discrete
### Discrete count, 1 parameter   2x2 figures
# disc1_22()
###################################################################
# four plots
disc1_22 <- function(family=PO, mu=c(1,2,5,10), miny=0, maxy=20, cex.y.axis=1.2, cex.all=1.5)
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
  fam1 <- eval(parse(text=fname)) # the family to output
  fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
  nopar <- fam$nopar # or fam1$nopar
  type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (nopar>=2) stop("This plot is design for one parameter discrete distributions")
  #---------------------------------------------------------
  op<-par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(2,2))
      y <- miny:maxy
    fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[1])")))
    fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[2])")))
    fy3 <- eval(parse(text=paste0("d",fname,"(y,mu[3])")))
    fy4 <- eval(parse(text=paste0("d",fname,"(y,mu[4])")))
  mm <- max(fy1, fy2, fy3, fy4)
  #1
  #fy <- eval(parse(text=paste0("d",family,"(y,mu[1])")))
  plot(y,fy1,type="h",ylab="f(y)",lwd=1.5, xaxt="n", ylim=c(0,1.1*mm),
       cex.axis=cex.y.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[1]),"  ")),bty="n",
         cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all) 
  mtexti(fname, 3 , off=.5, cex=1.5, xpos=21 )
  #2
  fy <- eval(parse(text=paste0("d",fname,"(y,mu[2])")))
  plot(y,fy2,type="h",ylab="f(y)",lwd=1.5,xaxt="n",yaxt="n", ylim=c(0,1.1*mm),
       cex.axis=cex.y.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[2]),"  ")),bty="n",
         cex=cex.all)
  # axis(4,cex.axis=cex.y.axis)
  #mtexti("f(y)", 2, 0.6,cex=cex.all)  
  #3
  fy <- eval(parse(text=paste0("d",fname,"(y,mu[3])")))
  plot(y,fy3,type="h",ylab="f(y)",lwd=1.5, ylim=c(0,1.1*mm),yaxt="n",
       cex.axis=cex.y.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[3]),"  ")),bty="n",
         cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  #4
  fy <- eval(parse(text=paste0("d",fname,"(y,mu[4])")))
  plot(y,fy4,type="h",ylab="f(y)",lwd=1.5,yaxt="n",  ylim=c(0,1.1*mm),
       cex.axis=cex.y.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[4]),"  ")),bty="n",
         cex=cex.all)
  axis(4,cex.axis=cex.y.axis)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}
###################################################################
###################################################################
### Discrete count, 2 parameter 3 mu x 2 sigma 
# disc2_32()
################################################################################
################################################################################
################################################################################
################################################################################
#-----------------------------------------------------------------------
# FUNCTION 4
################################################################################
################################################################################
################################################################################
################################################################################
#### Discrete count, 2 parameters
#------------------------------------------------------------------
# a 3 mu  by 2 sigma  table 
disc2_32 <- function(family=NBI, mu= c(0.5, 1,5), sigma = c(.1, 2), miny=0, maxy=10, cex.y.axis=1.5, cex.all=1.5 )
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
    type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (!(nopar==2)) stop("This plot is design for two parameter discrete distributions")
  #---------------------------------------------------------
      op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,2))
  ### mu: 3 values
  ### sigma: 2 values
  y <- miny:maxy
  #if(family=="BB") bd <- ",bd=maxy" else bd=""
  ### Row 1
  i=1
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2]",")")))
  maxy <- 1.2*max(fy1,fy2)
  #1,1
  j=1
  plot(y,fy1,type="h",xaxt="n",ylim=c(0,maxy),cex=cex.all,
       cex.axis=cex.y.axis)
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  mtexti(fname, 3 , off=.5, cex=1.5, xpos=10 )
  #1,2
  i=1
  j=2
  plot(y,fy2,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=.6,srt=90)
  ### Row 2
  i=2
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2]",")")))
  maxy <- 1.2*max(fy1,fy2)
  #2,1
  j=1
  plot(y,fy1,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  #2,2
  j=2
  plot(y,fy2,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(4,cex.axis=cex.all)
  ### Row 3
  i=3
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2]",")")))
  maxy <- 1.2*max(fy1,fy2)
  #3,1
  j=1
  plot(y,fy1,type="h",xaxt="n",ylim=c(0,maxy),cex.axis=cex.y.axis)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  axis(1,cex.axis=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all) 
  #3,2
  j=2
  plot(y,fy2,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(1,cex.axis=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}

################################################################################
################################################################################
################################################################################
################################################################################
# FUNCTION 5
################################################################################
################################################################################
################################################################################
################################################################################
### Discrete count, 2 parameter reverse 3 sigma x 2 mu 
# disc2_32R()
###################################################################
# a 3 mu  by 2 sigma  table 
disc2_32R <- function(family=NBI, mu= c(1,2), sigma=c(0.1,1,2), miny=0, 
                   maxy=10, cex.y.axis=1.5, cex.all=1.5 )
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
    type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (!(nopar==2)) stop("This plot is design for two parameter discrete distributions")
  #---------------------------------------------------------
     op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,2))
  ### mu: 3 values
  ### sigma: 2 values
  y <- miny:maxy
 # if(family=="BB") bd <- ",bd=maxy" else bd=""
  ### Row 1
  i=1
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[i]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[2],sigma[i]",")")))
  maxy <- 1.2*max(fy1,fy2)
  #1,1
  j=1
  plot(y,fy1,type="h",xaxt="n",ylim=c(0,maxy),cex=cex.all,
       cex.axis=cex.y.axis)
  mtexti(bquote(paste(mu," = ",.(mu[j]))),3,cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  mtexti(fname, 3 , off=.5, cex=1.5, xpos=10 )
  #1,2
  i=1
  j=2
  plot(y,fy2,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(mu," = ",.(mu[j]))),3,cex=cex.all)
  mtexti(bquote(paste(sigma," = ",.(sigma[i]))),4,cex=cex.all,off=.6,srt=90)
  ### Row 2
  i=2
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[i]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[2],sigma[i]",")")))
  maxy <- 1.2*max(fy1,fy2)
  #2,1
  j=1
  plot(y,fy1,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  #2,2
  j=2
  plot(y,fy2,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(sigma," = ",.(sigma[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(4,cex.axis=cex.all)
  ### Row 3
  i=3
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[1],sigma[i]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[2],sigma[i]",")")))
  maxy <- 1.2*max(fy1,fy2)
  #3,1
  j=1
  plot(y,fy1,type="h",xaxt="n",ylim=c(0,maxy),cex.axis=cex.y.axis)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  axis(1,cex.axis=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all) 
  #3,2
  j=2
  plot(y,fy2,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(sigma," = ",.(sigma[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(1,cex.axis=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}
###################################################################
### Discrete, 2 parameter 3 mu x 3 sigma 
# disc2_33()
###################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# FUNCTION 6
################################################################################
################################################################################
################################################################################
################################################################################
#### Discrete count, 2 parameters
#------------------------------------------------------------------
# a 3 mu  by 3 sigma  table 
disc2_33 <- function(family=NBI, mu= c(0.1,1, 2), sigma = c(0.5, 1, 2), miny=0, maxy=10, cex.y.axis=1.5, cex.all=1.5 )
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
    type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (!(nopar==2)) stop("This plot is design for two parameter discrete distributions")
  #--------------------------------------------------------- 
       op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,3))
  ### mu: 3 values
  ### sigma: 2 values
  y <- miny:maxy
  #if(family=="BB") bd <- ",bd=maxy" else bd=""
  #--------------------------------------------
  ### Row 1
  i=1
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2]",")")))
  fy3 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3]",")")))
  maxy <- 1.2*max(fy1,fy2, fy3)
  #----------
  #1,1
  j=1
  plot(y,fy1,type="h",xaxt="n",ylim=c(0,maxy),cex=cex.all,
       cex.axis=cex.y.axis)
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  #----------
  #1,2
  i=1
  j=2
  plot(y,fy2,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti(fname, 3 ,off=.5, cex=1.5 )
  #mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=.6,srt=90)
  #-----------
  #1,3
  i=1
  j=3
  plot(y,fy3,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=.6,srt=90)
  #------------------------------------- 
  ### Row 2
  i=2
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2]",")")))
  fy3 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3]",")")))
  maxy <- 1.2*max(fy1,fy2,fy3)
  #-----------
  #2,1
  j=1
  plot(y,fy1,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  #-----------
  #2,2
  j=2
  plot(y,fy2,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  #mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  #axis(4,cex.axis=cex.all)
  #-----------
  #2,3
  j=3
  plot(y,fy3,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(4,cex.axis=cex.all)
  #----------------------------------------
  ### Row 3
  i=3
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1]",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2]",")")))
  fy3 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3]",")")))
  maxy <- 1.2*max(fy1,fy2, fy3)
  #3,1
  j=1
  plot(y,fy1,type="h",xaxt="n",ylim=c(0,maxy),cex.axis=cex.y.axis)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  axis(1,cex.axis=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all) 
  #3,2
  j=2
  plot(y,fy2,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy))
  #mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(1,cex.axis=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  
  #3,3
  j=3
  plot(y,fy3,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(1,cex.axis=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}
################################################################################
################################################################################
################################################################################
################################################################################
# FUNCTION 7
################################################################################
################################################################################
################################################################################
################################################################################
### Discrete, 3 parameter 3 mu x 2 sigma  plus nu superimposed
# disc3_32()
###################################################################
disc3_32 <- function(family=SICHEL, mu=c(1,5,10), sigma = c(0.5,1), nu=c(-0.5,0.5),
                  miny=0, maxy=10, cex.y.axis=1.5, cex.all=1.5,
                  cols=c("darkgray", "black"), spacing = 0.2 )
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
  fam1 <- eval(parse(text=fname)) # the family to output
  fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
  nopar <- fam$nopar # or fam1$nopar
  type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (!(nopar==3)) stop("This plot is design for three parameter discrete distributions")
  #--------------------------------------------------------- 
       op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,2))
        y <- miny:maxy
  cex.axis.discrete.3 <- cex.y.axis
  cex.axis.discrete.2 <- cex.all
  ### Row 1
  i=1
  fy11 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1]",")")))
  fy12 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2]",")")))
  fy21 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1]",")")))
  fy22 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2]",")")))
  maxy <- 1.2*max(fy11,fy12,fy21,fy22)
  #1,1
  j=1
  plot(y,fy11,type="h",xaxt="n",ylim=c(0,maxy),cex=cex.axis.discrete.3,
       cex.axis=cex.axis.discrete.2, col=cols[1])
  lines(y+spacing,fy12,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.axis.discrete.3)
  mtexti("f(y)", 2, 0.6,cex=cex.axis.discrete.3)
  mtexti(fname, 3, off=.5, cex=1.5, xpos=10)
  #1,2
  i=1
  j=2
  plot(y,fy21,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy22,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.axis.discrete.3)
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.axis.discrete.3,off=.6,srt=90)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))))
  legend("topright",legend=as.expression(ex.leg),
         lty=1:2,col=cols[1:2],lwd=2,cex=1.2)

  ### Row 2
  i=2
  fy11 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1]",")")))
  fy12 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2]",")")))
  fy21 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1]",")")))
  fy22 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2]",")")))
  maxy <- 1.2*max(fy11,fy12,fy21,fy22)


  #2,1
  j=1
  plot(y,fy11,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy12,type="h",lty=2,col=cols[2])
  mtexti("f(y)", 2, 0.6,cex=cex.axis.discrete.3)


  #2,2
  j=2
  plot(y,fy21,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy22,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.axis.discrete.3,off=0.6,srt=90)
  axis(4,cex.axis=cex.axis.discrete.3)


  ### Row 3
  i=3
  fy11 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1]",")")))
  fy12 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2]",")")))
  fy21 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1]",")")))
  fy22 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2]",")")))
  maxy <- 1.2*max(fy11,fy12,fy21,fy22)

  #3,1
  j=1
  plot(y,fy11,type="h",xaxt="n",ylim=c(0,maxy),cex.axis=cex.axis.discrete.2,col=cols[1])
  lines(y+spacing,fy12,type="h",lty=2,col=cols[2])
  mtexti("f(y)", 2, 0.6,cex=cex.axis.discrete.3)
  axis(1,cex.axis=cex.axis.discrete.3)
  mtexti("y", 1, 0.6,cex=cex.axis.discrete.3)

  #3,2
  j=2
  plot(y,fy21,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy22,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.axis.discrete.3,off=0.6,srt=90)
  axis(1,cex.axis=cex.axis.discrete.3)
  mtexti("y", 1, 0.6,cex=cex.axis.discrete.3)
  par(op)
}
################################################################################
################################################################################
################################################################################
################################################################################
# FUNCTION 8
################################################################################
################################################################################
################################################################################
################################################################################
### Discrete, 3 parameter 3 mu x 3 sigma  plus nu superimposed
# disc3_33 ()
###################################################################
disc3_33 <- function(family=SICHEL, mu= c(1,5,10), sigma = c(0.5,1,2), nu=c(-0.5,0.5,1),
                     miny=0, maxy=10, cex.y.axis=1.5, cex.all=1.5,
                     cols=c("darkgray", "black"), spacing = 0.3 )
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
     type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (!(nopar==3)) stop("This plot is design for three parameter discrete distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,3))
  y <- miny:maxy
  cex.axis.discrete.3 <- cex.y.axis
  cex.axis.discrete.2 <- cex.all
  ### Row 1
  i=1
  fy111 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1]",")")))
  fy112 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2]",")")))
  fy121 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1]",")")))
  fy122 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2]",")")))
  fy131 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[1]",")")))
  fy132 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[2]",")")))
  maxy <- 1.2*max(fy111,fy112,fy121,fy122,fy131, fy132)
  
  #1,1 nu 1 2
  j=1
  plot(y,fy111,type="h",xaxt="n",ylim=c(0,maxy),cex=cex.axis.discrete.3,
       cex.axis=cex.axis.discrete.2, col=cols[1])
  lines(y+spacing,fy112,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.axis.discrete.3)
  mtexti("f(y)", 2, 0.6,cex=cex.axis.discrete.3)
  #1,2 nu 1 2
  i=1
  j=2
  plot(y,fy121,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy122,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.axis.discrete.3)
  mtexti(fname, 3 ,off=.5, cex=1.5 )
  #1,3 nu 1 2
  i=1
  j=3
  plot(y,fy131,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy132,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.axis.discrete.3)
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.axis.discrete.3,off=.6,srt=90)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))))
  legend("topright",legend=as.expression(ex.leg),
         lty=1:2,col=cols[1:2],lwd=2,cex=1.2)
  ### Row 2
  i=2
  fy211 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1]",")")))
  fy212 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2]",")")))
  fy221 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1]",")")))
  fy222 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2]",")")))
  fy231 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[1]",")")))
  fy232 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[2]",")")))
  maxy <- 1.2*max(fy211,fy212,fy221,fy222,fy221,fy232)
  #2,1 nu 1,2 
  j=1
  plot(y,fy211,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy212,type="h",lty=2,col=cols[2])
  mtexti("f(y)", 2, 0.6,cex=cex.axis.discrete.3)
  #2,2 nu 1,2 
  j=2
  plot(y,fy221,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy222,type="h",lty=2,col=cols[2])
  #2,2 nu 1,2 
  j=3
  plot(y,fy231,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy232,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.axis.discrete.3,off=0.6,srt=90)
  axis(4,cex.axis=cex.axis.discrete.3)
  ### Row 3
  i=3
  fy311 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1]",")")))
  fy312 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2]",")")))
  fy321 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1]",")")))
  fy322 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2]",")")))
  fy331 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[1]",")")))
  fy332 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[2]",")")))
  maxy <- 1.2*max(fy311,fy312,fy321,fy322,fy331, fy332)
  
  #3,1  nu 1,2 
  j=1
  plot(y,fy311,type="h",xaxt="n",ylim=c(0,maxy),cex.axis=cex.axis.discrete.2,col=cols[1])
  lines(y+spacing,fy312,type="h",lty=2,col=cols[2])
  mtexti("f(y)", 2, 0.6,cex=cex.axis.discrete.3)
  axis(1,cex.axis=cex.axis.discrete.3)
  mtexti("y", 1, 0.6,cex=cex.axis.discrete.3)
  
  #3,2 nu 1,2 
  j=2
  plot(y,fy321,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy322,type="h",lty=2,col=cols[2])
  # mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.axis.discrete.3,off=0.6,srt=90)
  # axis(1,cex.axis=cex.axis.discrete.3)
  # mtexti("y", 1, 0.6,cex=cex.axis.discrete.3)
  
  #3,3 nu 1,2 
  j=2
  plot(y,fy331,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy332,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.axis.discrete.3,off=0.6,srt=90)
  axis(1,cex.axis=cex.axis.discrete.3)
  mtexti("y", 1, 0.6,cex=cex.axis.discrete.3)
  par(op)
}

################################################################################
################################################################################
################################################################################
################################################################################
# FUNCTION 9
################################################################################
################################################################################
################################################################################
################################################################################
# Continuous 2 parameters -Inf +Inf  
#------------------------------------------------------------------------
contR2 <- function(family="NO", mu=c(0,-1,1), sigma=c(1,0.5,2), cols=c(1,2,3), 
                   ltype = c(1,2,3), maxy=7, no.points=201, y.axis.lim=1.1 )
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
         else if (is.character(family)) family
         else if (is.call(family)) as.character(family[[1]])
         else if (is.function(family)) deparse(substitute(family))
         else if (is(family, "gamlss.family"))  family$family[1]
         else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
    type <- fam$type
  if (type!="Continuous") stop("This plot is design for continuous distributions")
  if (!(nopar==2)) stop("This plot is design for two parameter discrete distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(1,2))
  #mu=vector of length 3
  #sigma= vector of length 3
  
  y<-seq(-maxy,maxy,length=no.points)
  
  pdf11 <- paste0("d",fname,"(y,mu[1],1)")
   fy11 <- eval(parse(text=pdf11))
  pdf12 <- paste0("d",fname,"(y,mu[2],1)")
   fy12 <- eval(parse(text=pdf12))
  pdf13 <- paste0("d",fname,"(y,mu[3],1)")
   fy13 <- eval(parse(text=pdf13))
  
  pdf21 <- paste0("d",fname,"(y,0,sigma[1])")
  fy21 <- eval(parse(text=pdf21))
  pdf22 <- paste0("d",fname,"(y,0,sigma[2])")
  fy22 <- eval(parse(text=pdf22))
  pdf23 <- paste0("d",fname,"(y,0,sigma[3])")
  fy23 <- eval(parse(text=pdf23))
  maxfy=y.axis.lim*max(fy11,fy12,fy13,fy21,fy22,fy23)
  ylabel <- "f(y)"
  #1
  plot(fy11~y, type="l", col=cols[1],
       ylab="", xlab="y",lty=ltype[1], lwd=2, ylim =c(0,maxfy))
  lines(fy12~y, col=cols[2], lty=ltype[2], lwd=2)
  lines(fy13~y, col=cols[3], lty=ltype[3], lwd=2)
  ex.leg <- c(bquote(paste(mu," = ",.(mu[1]))),bquote(paste(mu," = ",.(mu[2]))),bquote(paste(mu," = ",.(mu[3]))))
  legend("topleft",legend=as.expression(ex.leg),
         lty=ltype, col=cols[1:3], lwd=2, cex=1)
  
  mtexti( ylabel, 2, 0.6,cex=1.2)
  mtexti("y", 1, 0.6,cex=1.2)
  mtexti(bquote(paste(sigma," = 1")),3, cex=1)

  mtexti(fname, 3 , off=.5, cex=1.2, xpos=8 )
  #2
  plot(fy21~y, type="l", col=cols[1],
       yaxt="n", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy))
  lines(fy22~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy23~y, col=cols[3],lty=ltype[3],lwd=2)
  ex.leg <- c(bquote(paste(sigma," = ",.(sigma[1]))),bquote(paste(sigma," = ",.(sigma[2]))),
              bquote(paste(sigma," = ",.(sigma[3]))))
  legend("topleft",legend=as.expression(ex.leg),
         lty=ltype, col=cols[1:3], lwd=2, cex=1)
  mtexti("y", 1, 0.6,cex=1.2)
  mtexti(bquote(paste(mu," = 0")),3,cex=1)
  par(op)
}

#contR2("NO",c(1,2,3),c(1,2,3),6)
################################################################################
################################################################################
################################################################################
################################################################################
contR3 <- function(family="PE", mu=0, sigma=1, nu=c(1,2,3), 
                   cols=c(1,2,3), maxy=7, no.points=201, 
                   ltype = c(1,2,3), y.axis.lim=1.1)
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
          else if (is.character(family)) family
          else if (is.call(family)) as.character(family[[1]])
          else if (is.function(family)) deparse(substitute(family))
          else if (is(family, "gamlss.family"))  family$family[1]
          else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
    type <- fam$type
  if (type!="Continuous") stop("This plot is design for continuous distributions")
  if (!(nopar==3)) stop("This plot is design for three parameter continuous distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(1,1))
  #mu=vector of length 3
  #sigma= vector of length 3
      y <-seq(-maxy,maxy,length=no.points)
  pdf11 <- paste0("d",fname,"(y, mu=mu,sigma=sigma, nu=nu[1])")
   fy11 <- eval(parse(text=pdf11))
  pdf12 <- paste0("d",fname,"(y, mu=mu,sigma=sigma, nu=nu[2])")
   fy12 <- eval(parse(text=pdf12))
  pdf13 <- paste0("d",fname,"(y, mu=mu,sigma=sigma, nu=nu[3])")
   fy13 <- eval(parse(text=pdf13))
  maxfy <- y.axis.lim*max(fy11,fy12,fy13)
 ylabel <- "f(y)"
  #1
  plot(fy11~y, type="l", col=cols[1],
       ylab="", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy))
  lines(fy12~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy13~y, col=cols[3],lty=ltype[3],lwd=2)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))),bquote(paste(nu," = ",.(nu[3]))))
  legend("topleft",legend=as.expression(ex.leg),
         lty=ltype, col=cols[1:3], lwd=2, cex=1)
  mtexti( ylabel, 2, 0.6,cex=1.2)
  mtexti("y", 1, 0.6,cex=1.2)
  mtexti(fname, 3 , off=.7, cex=1.2)
  # mtexti(paste0("mu = ",mu),3, cex=1)
  mtexti(bquote(paste(mu," = ",.(mu)," , ",sigma," = ",.(sigma))),3,cex=1)
  # bquote(paste0(bquote(paste0("mu = ",mu))), ", sigma =",sigma))
  par(op)
}
################################################################################
################################################################################
################################################################################
################################################################################
contR4 <- function(family="SEP3", mu=0, sigma=1,  nu=c(0.5,1,2), 
                   tau=c(1,2,5),  cols=c(1,2,3), maxy=7, 
                   no.points=201, ltype = c(1,2,3), y.axis.lim=1.1)
{
#--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
         else if (is.character(family)) family
         else if (is.call(family)) as.character(family[[1]])
         else if (is.function(family)) deparse(substitute(family))
         else if (is(family, "gamlss.family"))  family$family[1]
         else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
    type <- fam$type
  if (type!="Continuous") stop("This plot is design for continuous distributions")
  if (!(nopar==4)) stop("This plot is design for four parameter continuous distributions")
#--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(1,3))
  #mu=vector of length 3
  #sigma= vector of length 3
  
  y<-seq(-maxy,maxy,length=no.points)
  pdf11 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[1], tau=tau[1])")
   fy11 <- eval(parse(text=pdf11))
  pdf12 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[2], tau=tau[1])")
   fy12 <- eval(parse(text=pdf12))
  pdf13 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[3], tau=tau[1])")
   fy13 <- eval(parse(text=pdf13))
  
  pdf21 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[1], tau=tau[2])")
   fy21 <- eval(parse(text=pdf21))
  pdf22 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[2], tau=tau[2])")
   fy22 <- eval(parse(text=pdf22))
  pdf23 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[3], tau=tau[2])")
   fy23 <- eval(parse(text=pdf23))
  
  pdf31 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[1], tau=tau[3])")
   fy31 <- eval(parse(text=pdf31))
  pdf32 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[2], tau=tau[3])")
   fy32 <- eval(parse(text=pdf32))
  pdf33 <- paste0("d",fname,"(y, mu=mu, sigma=sigma, nu=nu[3], tau=tau[3])")
   fy33 <- eval(parse(text=pdf33))
  
  
  maxfy= y.axis.lim*max(fy11,fy12,fy13,fy21,fy22,fy23,fy31,fy32,fy33)
  ylabel <-"f(x)"
  #1
  plot(fy11~y, type="l", col=cols[1],
       ylab="", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy))
  lines(fy12~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy13~y, col=cols[3],lty=ltype[3],lwd=2)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))),bquote(paste(nu," = ",.(nu[3]))))
  legend("topleft",legend=as.expression(ex.leg),
         lty=ltype, col=cols[1:3], lwd=2, cex=1)
  
  mtexti( ylabel, 2, 0.6,cex=1.2)
  mtexti("y", 1, 0.6,cex=1.2)
  mtexti(bquote(paste(tau," = ",.(tau[1]))),3, cex=1)
  #2
  plot(fy21~y, type="l", col=cols[1],
       yaxt="n", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy))
  lines(fy22~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy23~y, col=cols[3],lty=ltype[3],lwd=2)
  # ex.leg <- c(bquote(paste(sigma," = ",.(sigma[1]))),bquote(paste(sigma," = ",.(sigma[2]))),
  #             bquote(paste(sigma," = ",.(sigma[3]))))
  # legend("topleft",legend=as.expression(ex.leg),
  #       lty=1:3, col=cols[1:3], lwd=2, cex=1)
  mtexti("y", 1, 0.6,cex=1.2)
  mtexti(bquote(paste(tau," = ",.(tau[2]))),3, cex=1)
  mtexti(fname, 3 , off=.7, cex=1.5)
  #3
  plot(fy31~y, type="l", col=cols[1],
       yaxt="n", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy))
  lines(fy32~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy33~y, col=cols[3],lty=ltype[3],lwd=2)
  # ex.leg <- c(bquote(paste(sigma," = ",.(sigma[1]))),bquote(paste(sigma," = ",.(sigma[2]))),
  #             bquote(paste(sigma," = ",.(sigma[3]))))
  # legend("topleft",legend=as.expression(ex.leg),
  #        lty=1:3, col=cols[1:3], lwd=2, cex=1)
  mtexti("y", 1, 0.6,cex=1.2)
  mtexti(bquote(paste(tau," = ",.(tau[3]))),3, cex=1)
  par(op)
}
################################################################################
################################################################################
################################################################################
################################################################################
contRplus2 <- function(family=GA, mu=1, sigma=c(.1,.6,1), 
                   cols=c(1,2,3), maxy=4, no.points=201, 
                   y.axis.lim=1.1, ltype = c(1,2,3))
{
  #--------------------------------------------------------
   fname <- if (is.name(family)) as.character(family)
         else if (is.character(family)) family
         else if (is.call(family)) as.character(family[[1]])
         else if (is.function(family)) deparse(substitute(family))
         else if (is(family, "gamlss.family"))  family$family[1]
         else stop("the family must be a character or a gamlss.family name")
     fam1 <- eval(parse(text=fname)) # the family to output
      fam <- as.gamlss.family(family) # this is created so I can get things
   dorfun <- paste("d",fname,sep="") # say dNO
    nopar <- fam$nopar # or fam1$nopar
     type <- fam$type
  if (type!="Continuous") stop("This plot is design for continuous distributions")
  if (!(nopar==2)) stop("This plot is design for two parameter continuous distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(1,1))
  #mu=vector of length 3
  #sigma= vector of length 3
  y <-seq(0.001,maxy,length=no.points)
   pdf11 <- paste0("d",fname,"(y, mu=mu,sigma=sigma[1])")
    fy11 <- eval(parse(text=pdf11))
   pdf12 <- paste0("d",fname,"(y,mu=mu,sigma=sigma[2])")
  fy12 <- eval(parse(text=pdf12))
  pdf13 <- paste0("d",fname,"(y, mu=mu,sigma=sigma[3])")
  fy13 <- eval(parse(text=pdf13))
  maxfy <- y.axis.lim*max(fy11,fy12,fy13)
  ylabel <- "f(y)"
  #1
  plot(fy11~y, type="l", col=cols[1],
       ylab="", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy))
  lines(fy12~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy13~y, col=cols[3],lty=ltype[3],lwd=2)
  ex.leg <- c(bquote(paste(sigma," = ",.(sigma[1]))),bquote(paste(sigma," = ",.(sigma[2]))),bquote(paste(sigma," = ",.(sigma[3]))))
  legend("topright",legend=as.expression(ex.leg),
         lty=ltype, col=cols[1:3], lwd=2, cex=1)
  mtexti( ylabel, 2, 0.6,cex=1.2)
  mtexti("y", 1, 0.6,cex=1.2)
  mtexti(fname, 3 , off=.7, cex=1.2)
  # mtexti(paste0("mu = ",mu),3, cex=1)
  mtexti(bquote(paste(mu," = ",.(mu))),3,cex=1)
  par(op)
}
################################################################################
################################################################################
################################################################################
################################################################################
#***********************************************************************
################################################################################
################################################################################
################################################################################
################################################################################
contRplus3 <- function(family="BCCG", mu=1, sigma=c(0.15, 0.2, 0.5),  nu=c(-2,0,4), 
                   cols=c(1,2,3), maxy=4, ltype = c(1,2,3),
                   no.points=201, y.axis.lim=1.1)
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
  fam1 <- eval(parse(text=fname)) # the family to output
  fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
  nopar <- fam$nopar # or fam1$nopar
  type <- fam$type
  if (type!="Continuous") stop("This plot is design for continuous distributions")
  if (!(nopar==3)) stop("This plot is design for three parameter continuous distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(1,3))
  #mu=vector of length 3
  #sigma= vector of length 3
  y <- seq(0.001,maxy,length=no.points)
  pdf11 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[1], nu=nu[1])")
   fy11 <- eval(parse(text=pdf11))
  pdf12 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[1], nu=nu[2])")
   fy12 <- eval(parse(text=pdf12))
  pdf13 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[1], nu=nu[3])")
   fy13 <- eval(parse(text=pdf13))
  
  pdf21 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[2], nu=nu[1])")
  fy21 <- eval(parse(text=pdf21))
  pdf22 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[2], nu=nu[2])")
  fy22 <- eval(parse(text=pdf22))
  pdf23 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[2], nu=nu[3])")
  fy23 <- eval(parse(text=pdf23))
  
  pdf31 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[3], nu=nu[1])")
  fy31 <- eval(parse(text=pdf31))
  pdf32 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[3], nu=nu[2])")
  fy32 <- eval(parse(text=pdf32))
  pdf33 <- paste0("d",fname,"(y, mu=mu, sigma=sigma[3], nu=nu[3])")
  fy33 <- eval(parse(text=pdf33))
  
  
  maxfy= y.axis.lim*max(fy11,fy12,fy13,fy21,fy22,fy23,fy31,fy32,fy33)
  ylabel <-"f(x)"
  #1
  plot(fy11~y, type="l", col=cols[1],
       ylab="", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy))
  lines(fy12~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy13~y, col=cols[3],lty=ltype[3],lwd=2)
  # ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))),bquote(paste(nu," = ",.(nu[3]))))
  # legend("topleft",legend=as.expression(ex.leg),
  #        lty=ltype, col=cols[1:3], lwd=2, cex=1)
  
  mtexti( ylabel, 2, 0.6,cex=1.2)
  mtexti("y", 1, 0.6,cex=1.2)
  mtexti(bquote(paste(sigma," = ",.(sigma[1]))),3, cex=1)
  #2
  plot(fy21~y, type="l", col=cols[1],
       yaxt="n", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy))
  lines(fy22~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy23~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti("y", 1, 0.6,cex=1.2)
  mtexti(bquote(paste(sigma," = ",.(sigma[2]))),3, cex=1)
  mtexti(fname, 3 , off=.7, cex=1.5)
  #3
  plot(fy31~y, type="l", col=cols[1],
       yaxt="n", xlab="y",lty=ltype[1],lwd=2,ylim=c(0,maxfy))
  lines(fy32~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy33~y, col=cols[3],lty=ltype[3],lwd=2)
  # ex.leg <- c(bquote(paste(sigma," = ",.(sigma[1]))),bquote(paste(sigma," = ",.(sigma[2]))),
  #             bquote(paste(sigma," = ",.(sigma[3]))))
  # legend("topleft",legend=as.expression(ex.leg),
  #        lty=1:3, col=cols[1:3], lwd=2, cex=1)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))),bquote(paste(nu," = ",.(nu[3]))))
  legend("topright",legend=as.expression(ex.leg),
         lty=ltype, col=cols[1:3], lwd=2, cex=1)
  mtexti("y", 1, 0.6,cex=1.2)
  mtexti(bquote(paste(sigma," = ",.(sigma[3]))),3, cex=1)
  par(op)
}
################################################################################
################################################################################
################################################################################
################################################################################
contRplus4 <- function(family=BCT, mu=1, sigma = c(0.15,0.2,0.5), nu=c(-4,0,2), 
                       tau=c(100, 5, 1),
                       cols=c(1,2,3), maxy=4, ltype = c(1,2,3),
                       no.points=201, y.axis.lim=1.1)
{
  #--------------------------------------------------------
   fname <- if (is.name(family)) as.character(family)
       else if (is.character(family)) family
       else if (is.call(family)) as.character(family[[1]])
       else if (is.function(family)) deparse(substitute(family))
       else if (is(family, "gamlss.family"))  family$family[1]
       else stop("the family must be a character or a gamlss.family name")
    fam1 <- eval(parse(text=fname)) # the family to output
     fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
   nopar <- fam$nopar # or fam1$nopar
    type <- fam$type
    if (type!="Continuous") stop("This plot is design for continuous distributions")
    if (!(nopar==4)) stop("This plot is design for four parameter continuous distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,3))
    y <- seq(0.001,maxy,length=no.points)
  ### Row 1
  i=1 # i tau j sigma h nu
  fy111 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[1]",",tau[1])")))
  fy112 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[2]",",tau[1])")))
  fy113 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[3]",",tau[1])")))
  
  fy121 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[1]",",tau[1])")))
  fy122 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[2]",",tau[1])")))
  fy123 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[3]",",tau[1])")))
  
  fy131 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[1]",",tau[1])")))
  fy132 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[2]",",tau[1])")))
  fy133 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[3]",",tau[1])")))
  
  maxyrow1 <- y.axis.lim*max(fy111 ,fy112,fy113, fy121,fy122,fy123, fy131, fy132,fy133)
  #i = 2
  fy211 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[1]",",tau[2])")))
  fy212 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[2]",",tau[2])")))
  fy213 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[3]",",tau[2])")))
  
  fy221 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[1]",",tau[2])")))
  fy222 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[2]",",tau[2])")))
  fy223 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[3]",",tau[2])")))
  
  fy231 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[1]",",tau[2])")))
  fy232 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[2]",",tau[2])")))
  fy233 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[3]",",tau[2])")))
  
  maxyrow2 <- y.axis.lim*max(fy211 ,fy212,fy213, fy221,fy222,fy223, fy231, fy232,fy233)
  # i = 3
  fy311 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[1]",",tau[3])")))
  fy312 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[2]",",tau[3])")))
  fy313 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[1],nu[3]",",tau[3])")))
  
  fy321 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[1]",",tau[3])")))
  fy322 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[2]",",tau[3])")))
  fy323 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[2],nu[3]",",tau[3])")))
  
  fy331 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[1]",",tau[3])")))
  fy332 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[2]",",tau[3])")))
  fy333 <- eval(parse(text=paste0("d",fname,"(y,mu,sigma[3],nu[3]",",tau[3])")))
  maxyrow3 <- y.axis.lim*max(fy311 ,fy312,fy313, fy321,fy322,fy323, fy331, fy332,fy333)  
  
  ylabel <-"f(x)"

  # 1 1 
  plot(fy111~y, type="l", xaxt="n",  col=cols[1], ylab="", xlab="", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow1))
  lines(fy112~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy113~y, col=cols[3],lty=ltype[3],lwd=2)
  
  mtexti( ylabel, 2, 0.6,cex=1.2)
  mtexti(bquote(paste(sigma," = ",.(sigma[1]))),3, cex=1)
  # 1 2 
  plot(fy121~y, type="l", xaxt="n", yaxt="n", col=cols[1], ylab="", xlab="y", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow1))
  lines(fy122~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy123~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti(bquote(paste(sigma," = ",.(sigma[2]))),3, cex=1)
  mtexti(fname, 3 , off=.7, cex=1.5)
  # 1 3
  plot(fy131~y, type="l",  xaxt="n", yaxt="n", col=cols[1], ylab="", xlab="y", 
       lty=ltype[1], lwd=2, ylim=c(0,maxyrow1))
  lines(fy132~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy133~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti(bquote(paste(sigma," = ",.(sigma[3]))),3, cex=1)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))),bquote(paste(nu," = ",.(nu[3]))))
  legend("topright",legend=as.expression(ex.leg),
         lty=ltype, col=cols[1:3], lwd=2, cex=1)
  mtexti(bquote(paste(tau," = ",.(tau[1]))),4, cex=1)
  # 2 1 
  plot(fy211~y, type="l", xaxt="n", col=cols[1], ylab="", xlab="", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow2))
  lines(fy212~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy213~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti( ylabel, 2, 0.6,cex=1.2)
  # 2 2 
  plot(fy221~y, type="l", xaxt="n", yaxt="n",col=cols[1], ylab="", xlab="y", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow2))
  lines(fy222~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy223~y, col=cols[3],lty=ltype[3],lwd=2)
  # 2 3
  plot(fy231~y, type="l",  xaxt="n",yaxt="n",col=cols[1], ylab="", xlab="y", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow2))
  lines(fy232~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy233~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti(bquote(paste(tau," = ",.(tau[2]))),4, cex=1)
  # 3 1
  plot(fy311~y, type="l",  col=cols[1], ylab="", xlab="", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow3))
  lines(fy312~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy313~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti( ylabel, 2, 0.6,cex=1.2)
  mtexti("y", 1, 0.6, cex=1.2)
  # 2 2 
  plot(fy321~y, type="l",  yaxt="n",col=cols[1], ylab="", xlab="y", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow3))
  lines(fy322~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy323~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti("y", 1, 0.6, cex=1.2)
  # 2 3
  plot(fy331~y, type="l",yaxt="n",col=cols[1], ylab="", xlab="y", lty=ltype[1],
       lwd=2, ylim=c(0,maxyrow3))
  lines(fy232~y, col=cols[2],lty=ltype[2],lwd=2)
  lines(fy233~y, col=cols[3],lty=ltype[3],lwd=2)
  mtexti("y", 1, 0.6, cex=1.2)
  mtexti(bquote(paste(tau," = ",.(tau[3]))),4, cex=1)
  
    
  
#mtexti(bquote(paste(sigma," = ",.(sigma[1]))),3,cex=cex.axis.discrete.3)
  
  par(op)
}

################################################################################
################################################################################
################################################################################
################################################################################

binom1_3 <- function(family=BI, mu=c(0.1,.5,.7), bd=NULL, miny=0, maxy=20, cex.y.axis=1.2, cex.all=1.5)
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
  fam1 <- eval(parse(text=fname)) # the family to output
  fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
  nopar <- fam$nopar # or fam1$nopar
  type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (nopar>=2) stop("This plot is design for one parameter discrete distributions")
  #---------------------------------------------------------
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,1))
  y <-0:maxy
  bd <- if (is.null(bd))  maxy else bd
  #1
  fy <- eval(parse(text=paste0("d",fname,"(y,mu[1], bd=bd)")))
  plot(y,fy,type="h",ylab="f(y)",lwd=1.5,xaxt="n",ylim=c(0,1.2*max(fy)),
       cex.axis=cex.y.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[1]),"  ")),bty="n",
         cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  mtexti(fname, 3 , off=.5, cex=1.5 )
  #2
  fy <-eval(parse(text=paste0("d",fname,"(y,mu[2], bd=bd)")))
  plot(y,fy,type="h",ylab="f(y)",lwd=1.5,xaxt="n",yaxt="n",ylim=c(0,1.2*max(fy)),
       cex.axis=cex.y.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[2]),"  ")),bty="n",
         cex=cex.all)
  axis(4,cex.axis=cex.y.axis)
  mtexti("f(y)", 2, 0.6,cex=cex.all)  
  #3
  fy <- eval(parse(text=paste0("d",fname,"(y,mu[3], bd=bd)")))
  plot(y,fy,type="h",ylab="f(y)",lwd=1.5,ylim=c(0,1.2*max(fy)),
       cex.axis=cex.y.axis, cex=cex.all)
  legend("topright",legend=bquote(paste(mu," = ",.(mu[3]),"  ")),bty="n",
         cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}
#----------------------------------------------------------------------
#----------------------------------------------------------------------
binom2_33 <- function(family=BB, mu= c(0.1,.5, .8), sigma = c(0.5, 1, 2), bd=NULL, miny=0, maxy=10, cex.y.axis=1.5, cex.all=1.5 )
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
  fam1 <- eval(parse(text=fname)) # the family to output
  fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
  nopar <- fam$nopar # or fam1$nopar
  type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (!(nopar==2)) stop("This plot is design for two parameter discrete distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,3))
  ### mu: 3 values
  ### sigma: 2 values
  y <-0:maxy
  bd <- if (is.null(bd))  maxy else bd
  #--------------------------------------------
  ### Row 1
  i=1
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1], bd=bd",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2], bd=bd",")")))
  fy3 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3], bd=bd",")")))
  maxy <- 1.2*max(fy1,fy2, fy3)
  #----------
  #1,1
  j=1
  plot(y,fy1,type="h",xaxt="n",ylim=c(0,maxy),cex=cex.all,
       cex.axis=cex.y.axis)
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  #----------
  #1,2
  i=1
  j=2
  plot(y,fy2,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti(fname, 3 ,off=.5, cex=1.5 )
  #mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=.6,srt=90)
  #-----------
  #1,3
  i=1
  j=3
  plot(y,fy3,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.all)
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=.6,srt=90)
  #------------------------------------- 
  ### Row 2
  i=2
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1], bd=bd",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2], bd=bd",")")))
  fy3 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3], bd=bd",")")))
  maxy <- 1.2*max(fy1,fy2,fy3)
  #-----------
  #2,1
  j=1
  plot(y,fy1,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  #-----------
  #2,2
  j=2
  plot(y,fy2,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  #mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  #axis(4,cex.axis=cex.all)
  #-----------
  #2,3
  j=3
  plot(y,fy3,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(4,cex.axis=cex.all)
  #----------------------------------------
  ### Row 3
  i=3
  fy1 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1], bd=bd",")")))
  fy2 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2], bd=bd",")")))
  fy3 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3], bd=bd",")")))
  maxy <- 1.2*max(fy1,fy2, fy3)
  #3,1
  j=1
  plot(y,fy1,type="h",xaxt="n",ylim=c(0,maxy),cex.axis=cex.y.axis)
  mtexti("f(y)", 2, 0.6,cex=cex.all)
  axis(1,cex.axis=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all) 
  #3,2
  j=2
  plot(y,fy2,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy))
  #mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(1,cex.axis=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  
  #3,3
  j=3
  plot(y,fy3,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy))
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.all,off=0.6,srt=90)
  axis(1,cex.axis=cex.all)
  mtexti("y", 1, 0.6,cex=cex.all)
  par(op)
}
################################################################################
################################################################################
################################################################################
################################################################################
binom3_33 <- function(family=ZIBB, mu= c(.1,.5,.8), sigma = c(0.5,1,2), 
                     nu=c(0.01,0.3), bd = NULL,
                     miny=0, maxy=10, cex.y.axis=1.5, cex.all=1.5,
                     cols=c("darkgray", "black"), spacing = 0.3 )
{
  #--------------------------------------------------------
  fname <- if (is.name(family)) as.character(family)
  else if (is.character(family)) family
  else if (is.call(family)) as.character(family[[1]])
  else if (is.function(family)) deparse(substitute(family))
  else if (is(family, "gamlss.family"))  family$family[1]
  else stop("the family must be a character or a gamlss.family name")
  fam1 <- eval(parse(text=fname)) # the family to output
  fam <- as.gamlss.family(family) # this is created so I can get things
  dorfun <- paste("d",fname,sep="") # say dNO
  nopar <- fam$nopar # or fam1$nopar
  type <- fam$type
  if (type!="Discrete") stop("This plot is design for discrete distributions")
  if (!(nopar==3)) stop("This plot is design for three parameter discrete distributions")
  #--------------------------------------------------------- 
  op <- par(omi=rep(1.0, 4), mar=c(0,0,0,0), mfrow=c(3,3))
  y <-0:maxy
  bd <- if (is.null(bd))  maxy else bd
  cex.axis.discrete.3 <- cex.y.axis
  cex.axis.discrete.2 <- cex.all
  ### Row 1
  i=1
  fy111 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1], bd=bd",")")))
  fy112 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2], bd=bd",")")))
  fy121 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1], bd=bd",")")))
  fy122 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2], bd=bd",")")))
  fy131 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[1], bd=bd",")")))
  fy132 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[2], bd=bd",")")))
  maxy <- 1.2*max(fy111,fy112,fy121,fy122,fy131, fy132)
  
  #1,1 nu 1 2
  j=1
  plot(y,fy111,type="h",xaxt="n",ylim=c(0,maxy),cex=cex.axis.discrete.3,
       cex.axis=cex.axis.discrete.2, col=cols[1])
  lines(y+spacing,fy112,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.axis.discrete.3)
  mtexti("f(y)", 2, 0.6,cex=cex.axis.discrete.3)
  #1,2 nu 1 2
  i=1
  j=2
  plot(y,fy121,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy122,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.axis.discrete.3)
  mtexti(fname, 3 ,off=.5, cex=1.5 )
  #1,3 nu 1 2
  i=1
  j=3
  plot(y,fy131,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy132,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(sigma," = ",.(sigma[j]))),3,cex=cex.axis.discrete.3)
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.axis.discrete.3,off=.6,srt=90)
  ex.leg <- c(bquote(paste(nu," = ",.(nu[1]))),bquote(paste(nu," = ",.(nu[2]))))
  legend("topright",legend=as.expression(ex.leg),
         lty=1:2,col=cols[1:2],lwd=2,cex=1.2)
  ### Row 2
  i=2
  fy211 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1], bd=bd",")")))
  fy212 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2], bd=bd",")")))
  fy221 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1], bd=bd",")")))
  fy222 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2], bd=bd",")")))
  fy231 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[1], bd=bd",")")))
  fy232 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[2], bd=bd",")")))
  maxy <- 1.2*max(fy211,fy212,fy221,fy222,fy221,fy232)
  #2,1 nu 1,2 
  j=1
  plot(y,fy211,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy212,type="h",lty=2,col=cols[2])
  mtexti("f(y)", 2, 0.6,cex=cex.axis.discrete.3)
  #2,2 nu 1,2 
  j=2
  plot(y,fy221,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy222,type="h",lty=2,col=cols[2])
  #2,2 nu 1,2 
  j=3
  plot(y,fy231,type="h",xaxt="n", yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy232,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.axis.discrete.3,off=0.6,srt=90)
  axis(4,cex.axis=cex.axis.discrete.3)
  ### Row 3
  i=3
  fy311 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[1], bd=bd",")")))
  fy312 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[1],nu[2], bd=bd",")")))
  fy321 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[1], bd=bd",")")))
  fy322 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[2],nu[2], bd=bd",")")))
  fy331 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[1], bd=bd",")")))
  fy332 <- eval(parse(text=paste0("d",fname,"(y,mu[i],sigma[3],nu[2], bd=bd",")")))

  maxy <- 1.2*max(fy311,fy312,fy321,fy322,fy331, fy332)
  
  #3,1  nu 1,2 
  j=1
  plot(y,fy311,type="h",xaxt="n",ylim=c(0,maxy),cex.axis=cex.axis.discrete.2,col=cols[1])
  lines(y+spacing,fy312,type="h",lty=2,col=cols[2])
  mtexti("f(y)", 2, 0.6,cex=cex.axis.discrete.3)
  axis(1,cex.axis=cex.axis.discrete.3)
  mtexti("y", 1, 0.6,cex=cex.axis.discrete.3)
  
  #3,2 nu 1,2 
  j=2
  plot(y,fy321,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy322,type="h",lty=2,col=cols[2])
  # mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.axis.discrete.3,off=0.6,srt=90)
  # axis(1,cex.axis=cex.axis.discrete.3)
  # mtexti("y", 1, 0.6,cex=cex.axis.discrete.3)
  
  #3,3 nu 1,2 
  j=2
  plot(y,fy331,type="h",xaxt="n",yaxt="n",ylim=c(0,maxy),col=cols[1])
  lines(y+spacing,fy332,type="h",lty=2,col=cols[2])
  mtexti(bquote(paste(mu," = ",.(mu[i]))),4,cex=cex.axis.discrete.3,off=0.6,srt=90)
  axis(1,cex.axis=cex.axis.discrete.3)
  mtexti("y", 1, 0.6,cex=cex.axis.discrete.3)
  par(op)
}

################################################################################
################################################################################
################################################################################
################################################################################




