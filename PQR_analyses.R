nel.one.test <- function(spc.object)
{
  data.points <- spc.object$data
  LCL <- spc.object$LCL
  UCL <- spc.object$UCL
  mr.data.points <- spc.object$mr
  mr.UCL <- spc.object$mr.UCL
  index.above.ucl <- seq(along = data.points)[data.points > UCL]
  index.below.lcl <- seq(along = data.points)[data.points < LCL]
  index.above.mrucl <- seq(along = mr.data.points)[mr.data.points > mr.UCL]
  
  RVAL = list(ind.nel=c(index.above.ucl,index.below.lcl),mr.nel=index.above.mrucl)
  
  return(RVAL)
}

norm.prob <- function(data,data.name=NULL,norm.test=TRUE)
{
  #----First set the names for the data and the lot IDs-------------------------#
  if(is.null(data.name)) data.name<-deparse(substitute(data))	
  
  # If we want the Shapiro-Wilk Test then build the legend for it
  if (norm.test==TRUE) {
    if((shapiro.test(data)$p.value<0.0001))
      p.val<-"p<0.0001"
    else	
      p.val<-paste("p=",round(shapiro.test(data)$p.value,4),sep="")
    
    w.val<-paste("w=",round(shapiro.test(data)$statistic,4),sep="")
    sw.legend<-c(shapiro.test(data)$method,w.val,p.val)
  }
  
  # Next plot the normal probability and SW data in the lower left
  qqnorm(data,main=paste("Normal Probability Plot of ",data.name,sep=""))
  qqline(data)
  
  if (norm.test==TRUE) legend(x="topleft",sw.legend,cex=1.0)
}

MR<-function(data,k=2,d2=1.128379)
{
  data<-as.vector(data)
  n<-length(data)
  d<-0
  for(j in k:n)
    d<-d+abs(diff(data[c(j:(j-1))]))
  sd<-(d/(n-1))/d2
  
  mr<-(1:(n-1))
  for (j in k:n)
    mr[j-1]<-abs(diff(data[c(j:(j-1))]))
  RVAL=list(sd=sd,mr=mr)
  return(RVAL)
}

i.mr.data<-function(data,data.name=NULL,lot.num=NULL,units=NULL,
                    LCL=NULL,UCL=NULL,cL=NULL,mr.UCL=NULL,st.dev=NULL,plot=TRUE){
  
  call<-match.call()
  
  #----First set the names for the data and the lot IDs-------------------------#
  if(is.null(data.name)) data.name<-deparse(substitute(data))  
  if(is.null(lot.num)) lot.num<-c(1:length(data))
  
  #----Are the control limits already set? If not, then calculate them----------#
  if(is.null(LCL)||is.null(cL)||is.null(UCL)||is.null(mr.UCL))
    mov.range<-MR(data)
  
  #----If the standard deviation isn't provided use the MR value
  if(is.null(st.dev)||!is.numeric(st.dev))
    st.dev<-mov.range$sd
  cL<-mean(data)
  LCL<-cL-3*st.dev
  UCL<-cL+3*st.dev
  mr.UCL<-3.266534*mean(mov.range$mr)
  
  object<-list(call=call,data.name=data.name,data=data,units=units,lot.num=lot.num)
  object$type<-"SPC.data"
  object$mr<-mov.range$mr
  object$LCL<-LCL
  object$UCL<-UCL
  object$cL<-cL
  object$sd<-st.dev
  object$mr.UCL<-mr.UCL
  object$mr.cL<-mean(mov.range$mr)
  
  if(plot==TRUE) {
    windows()
    old.par<-par(no.readonly=TRUE)
    par(mfrow=c(2,1))
    plot.i.chart(object)
    plot.mr.chart(object)
  }
  
  return(object)
}

cusum.data<-function(data,data.name=NULL,lot.num=NULL,target=NULL,
                     se.shift=1,boundary=5,plot=TRUE){
  
  # Perform the usual functions for data names
  if (is.null(data.name)) data.name <- deparse(substitute(data))
  if (is.null(lot.num)) lot.num <- c(1:length(data))
  
  # If the target isn't given set it to the mean of the data set
  if (is.null(target)) target <- mean(data)
  
  # Generate the raw differences (data - mean) Ã· sd
  n <- length(data)
  st.dev <- MR(data)$sd
  z_t <- (data-target)/st.dev
  
  # Calculate pos.cusum
  z_f <- z_t - se.shift/2
  pos.cusum <- rep(NA,n)
  pos.cusum[1] <- max(0,z_f[1])
  for (i in 2:n) pos.cusum[i] <- max(0,pos.cusum[i-1]+z_f[i])
  
  # Calculate the neg.cusum
  z_f<-z_t + se.shift/2
  neg.cusum <- rep(NA,n)
  neg.cusum[1] <- max(0,-z_f[1])
  for (i in 2:n) neg.cusum[i] <- max(0,neg.cusum[i-1]-z_f[i])
  neg.cusum <- -neg.cusum
  
  object <- list(pos=pos.cusum,neg=neg.cusum,target=target,
                 boundary=boundary,data.name=data.name,lot.num=lot.num)
  
  if (plot==TRUE) {
    plot.cusum(object)
  }
  
  return(object)
  
}

cap.analysis<-function(data,LSL=NULL,USL=NULL,target=NULL,
                       sigma=NULL,k.sigma=3)
{
  # First, do we have everything we need
  if (is.null(LSL) && is.null(USL))
    stop("At least one specification limit is required for capability analysis")
  
  # Okay, if we have both limits and the target isn't given, then the center is the target
  if (is.null(target))
    if (is.null(LSL) || is.null(USL))
      target = NULL
  else
    target = mean(LSL,USL)
  
  # Now, get the basic information to feed the calculations
  n = length(data)
  data.ave = mean(data)
  if (is.null(sigma)) sigma=sd(data)
  
  # Calculate the values
  if (is.null(LSL) || is.null(USL))
    Xp.p <- NULL
  else
    Xp.p <- (USL-LSL)/(2*k.sigma*sigma)
  
  if (is.null(LSL))
    Xp.l <- NULL
  else
    Xp.l <- (data.ave - LSL)/(k.sigma*sigma)
  
  if (is.null(USL))
    Xp.u <- NULL
  else
    Xp.u <- (USL - data.ave)/(k.sigma*sigma)
  
  Xp.k <- min(Xp.l,Xp.u)
  
  # What percent of batches have failed
  obs.LSL <- sum(data<LSL)/n*100
  obs.USL <- sum(data>USL)/n*100
  
  # What percent of batches are expected to fail in the future
  if (is.null(LSL))
    exp.LSL <- NULL
  else
    exp.LSL <- pnorm((LSL-data.ave)/sigma)*100
  
  if (is.null(USL))
    exp.USL <- NULL
  else
    exp.USL <- (1-(pnorm((USL-data.ave)/sigma)))*100
  
  # Put all of the values into an object
  RVAL <- list(Pp = Xp.p, Pp.l = Xp.l, Pp.u = Xp.u, Pp.k = Xp.k,
               obs.LSL = obs.LSL, obs.USL = obs.USL, exp.LSL = exp.LSL, exp.USL = exp.USL)
  
  # Return the object
  return(RVAL)
  
}

plot.i.chart<-function(spc,new.lots=NULL,nel.1.test=FALSE){
  # Transfer list objects into local holders
  data.name <- spc$data.name
  units <- spc$units
  lot.num <- spc$lot.num
  data <- spc$data
  LCL <- spc$LCL
  UCL <- spc$UCL
  cL <- spc$cL
  
  draw.hline <- FALSE
  
  if (!is.null(new.lots)) {
    if (length(data)>29){
      start.point<-length(data)-new.lots+1
      data <- data[start.point:length(data)]
      lot.num <- lot.num[start.point:length(lot.num)]
    } else {
      draw.hline <- TRUE
      lot.line <- length(data)-new.lots+0.5
    }
    
  }
  
  # Do we need to find any Nel 1 violations? If so, get the index of values
  if (nel.1.test==TRUE) nel.one.violations <- nel.one.test(spc)$ind.nel
  if (!is.null(new.lots) & length(spc$data)>29) {
    nel.one.violations <- nel.one.violations - start.point + 1
    nel.one.violations <- nel.one.violations[nel.one.violations > 0]
  }
  
  # First, calculate the axis limits
  yaxis<-range(data,LCL,UCL)
  yaxis<-yaxis+diff(yaxis)*c(-0.1,0.1)
  xlabelsize=20/length(lot.num)
  if (xlabelsize<0.6) xlabelsize<-0.6
  if (xlabelsize>1.0) xlabelsize<-1.0
  
  # Now, plot the individuals data
  # If this function is called from release.analysis, don't set the margins
  func.name <- as.character(sys.call(-1))[1]
  if (!(func.name=="release.analysis")||is.na(func.name)) par(mar=c(4.1,4.1,4.1,4.1))
  
  plot(data,main=paste("Individuals Chart of",data.name,collapse="\n"),
       sub="Lot ID",ylim=yaxis,type="o",xlab="",ylab=units,axes=FALSE)
  if (nel.1.test==TRUE) 
    points(nel.one.violations,data[nel.one.violations],pch=19,col="red")
  
  box()
  # Add lines for LCL, cL, UCL and the associated text in the margin
  abline(h=c(LCL,UCL),col="red",lty=2)
  abline(h=cL,col="dark gray",lty=2)
  if (draw.hline == TRUE) {
    abline(v=lot.line,col="dark gray",lty=2)
    text(lot.line,par()$usr[4],label="Current Lots",adj=c(0,1),cex=0.8)
  }
  mtext(paste("UCL=",round(UCL,2),sep=""),side=4,at=UCL,line=0.25,cex=0.7,las=2)
  mtext(paste("cL=",round(cL,2),sep=""),side=4,at=cL,line=0.25,cex=0.7,las=2)
  mtext(paste("LCL=",round(LCL,2),sep=""),side=4,at=LCL,line=0.25,cex=0.7,las=2)
  axis(2)
  axis(1,at=c(1:length(lot.num)),label=lot.num,cex.axis=xlabelsize,las=2)
  
}

plot.mr.chart<-function(spc, new.lots=NULL, nel.1.test=FALSE){
  
  # Transfer list objects into local holders
  data.name <- spc$data.name
  units <- spc$units
  lot.num <- spc$lot.num
  mr.data <- spc$mr
  mr.UCL <- spc$mr.UCL
  mr.cL <- spc$mr.cL
  
  draw.hline <- FALSE
  
  if (!is.null(new.lots)) {
    if (length(mr.data)>28){
      start.point<-length(mr.data)-new.lots+1
      mr.data <- mr.data[start.point:length(mr.data)]
      lot.num <- lot.num[start.point:length(lot.num)]
    } else {
      draw.hline <- TRUE
      lot.line <- length(mr.data)-new.lots+0.5
    }  
  }
  
  # Do we need to find any Nel 1 violations? If so, get the index of values
  if (nel.1.test==TRUE) nel.one.violations <- nel.one.test(spc)$mr.nel
  if (!is.null(new.lots) & length(spc$mr)>28) {
    nel.one.violations <- nel.one.violations - start.point + 1
    nel.one.violations <- nel.one.violations[nel.one.violations > 0]
  }
  
  # First, calculate the axis limits
  mr.yaxis<-range(mr.data,mr.UCL)
  mr.yaxis<-mr.yaxis+diff(mr.yaxis)*c(-0.1,0.1)
  xlabelsize=20/length(lot.num)
  if (xlabelsize<0.6) xlabelsize<-0.6
  if (xlabelsize>1.0) xlabelsize<-1.0
  
  # Plot the moving range chart
  # If this function is called from release.analysis, don't set the margins
  func.name <- as.character(sys.call(-1))[1]
  if (!(func.name=="release.analysis")||is.na(func.name)) par(mar=c(4.1,4.1,4.1,4.1))
  
  plot(mr.data,main=paste("Moving Range of",data.name,collapse="\n"),
       sub="Lot ID",type="o",xlab="",ylab=paste("Moving range of ",units,sep=""),
       ylim=mr.yaxis,axes=FALSE)
  if (nel.1.test==TRUE) 
    points(nel.one.violations,mr.data[nel.one.violations],pch=19,col="red")
  box()
  abline(h=mr.cL,col="dark gray",lty=2)
  abline(h=mr.UCL,col="red",lty=2)
  if (draw.hline == TRUE) {
    abline(v=lot.line,col="dark gray",lty=2)
    text(lot.line,par()$usr[4],label="Current Lots",adj=c(0,1),cex=0.8)
  }
  mtext(paste("UCL=",round(mr.UCL,2),sep=""),side=4,at=mr.UCL,line=0.25,cex=0.7,las=2)
  mtext(paste("Mean=",round(mr.cL,2),sep=""),side=4,at=mr.cL,line=0.25,cex=0.7,las=2)
  axis(2)
  axis(1,at=c(1:(length(lot.num)-1)),label=lot.num[2:length(lot.num)],
       cex.axis=xlabelsize,las=2)
  
}

plot.cusum<-function(cusum.object){
  
  pos <- cusum.object$pos
  neg <- cusum.object$neg
  ubound <- as.numeric(cusum.object$boundary)
  lbound <- -ubound
  data.name <- cusum.object$data.name
  lot.num <- cusum.object$lot.num
  
  # First, calculate the axis limits
  yaxis<-range(pos,neg,ubound,lbound)
  yaxis<-yaxis+diff(yaxis)*c(-0.1,0.1)
  if (length(lot.num)>25) {
    interval <- round.up((length(lot.num)/25))
    ind <- seq(1, length(lot.num), by = interval)
  }
  else
    ind <- c(1:length(lot.num))
  num.lots <- c(1:length(lot.num))
  
  # Now, plot the  data
  par(mar=c(4.1,4.1,4.1,2.1))
  plot(pos,main=paste("CUSUM Chart of",data.name,collapse="\n"),
       sub="Lot ID",ylim=yaxis,type="o",xlab="",ylab="Cumulative Sum",
       pch="+",axes=FALSE)
  lines(neg,type="o",pch="-")
  box()
  # Add lines for Upper and Lower Decision Bounds, target and the associated text in the margin
  abline(h=c(ubound,lbound),col="red",lty=2)
  abline(h=0,col="dark gray",lty=2)
  mtext(paste("UDB=",ubound,sep=""),side=4,at=ubound,line=0.25,cex=0.7,las=2)
  #   mtext(paste("cL=",round(cL,2),sep=""),side=4,at=cL,line=0.25,cex=0.7,las=2)
  mtext(paste("LDB=",lbound,sep=""),side=4,at=lbound,line=0.25,cex=0.7,las=2)
  axis(2)
  axis(1,at=num.lots[ind],label=format(lot.num[ind]),cex.axis=1.0,las=2)
}

run.chart <- function(data,LSL=NULL,USL=NULL, 
                      data.name=NULL,lot.num=NULL,units=NULL){
  #---First set the names for the data and the lot IDs-------------------------#
  if(is.null(data.name)) data.name<-deparse(substitute(data))  
  if(is.null(lot.num)) lot.num<-c(1:length(data))
  
  #----Calculate axis values for the run chart----------------------------------#
  yaxis<-range(data,USL,LSL)
  yaxis<-yaxis+diff(yaxis)*c(-0.1,0.1)
  if (length(lot.num)>25) {
    interval <- round.up((length(lot.num)/25))
    ind <- seq(1, length(lot.num), by = interval)
  }
  else
    ind <- c(1:length(lot.num))
  num.lots <- c(1:length(lot.num))
  
  # Plot the run chart
  par(mar=c(4.1,4.1,4.1,2.1))
  plot(data,main=paste("Run Chart of",data.name,collapse="\n"),
       sub="Lot ID",ylim=yaxis,type="o",xlab="",ylab=units,axes=FALSE)
  box()
  abline(h=c(LSL,USL),col="red",lty=2)
  if(!is.null(USL))
    mtext(paste("USL=",round(USL,2),sep=""),side=4,at=USL,line=0.25,cex=0.7,las=2)
  if(!is.null(LSL))
    mtext(paste("LSL=",round(LSL,2),sep=""),side=4,at=LSL,line=0.25,cex=0.7,las=2)
  axis(2)
  axis(1,at=num.lots[ind],label=format(lot.num[ind]),cex.axis=1.0,las=2)
  
}

round.up <- function(x) {
  RVAL <- trunc(round(x,2)) + 1
  return(RVAL)
}
