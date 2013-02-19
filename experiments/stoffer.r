# This is taken from http://www.stat.pitt.edu/stoffer/tsa2/Examples.htm

lag.plot1=function(series,max.lag=1,corr=TRUE,smooth=FALSE){ 
   name1=paste(deparse(substitute(series)),"(t-",sep="")
   name2=paste(deparse(substitute(series)),"(t)",sep="")
   data1=as.ts(series)
   max.lag=as.integer(max.lag)
   prow=ceiling(sqrt(max.lag))
   pcol=ceiling(max.lag/prow)
   a=acf(series,max.lag,plot=FALSE)$acf[-1]
   old.par <- par(no.readonly = TRUE)
   par(mfrow=c(prow,pcol), mar=c(2.5, 4, 2.5, 1), cex.main=1.1, font.main=1)
  for(h in 1:max.lag){                       
   plot(lag(series,-h), data1, xy.labels=FALSE, main=paste(name1,h,")",sep=""), ylab=name2, xlab="") 
    if (smooth==TRUE) 
    lines(lowess(ts.intersect(lag(series,-h),series)[,1],
                 ts.intersect(lag(series,-h),series)[,2]), col="red")
    if (corr==TRUE)
    legend("topright", legend=round(a[h], digits=2), text.col ="blue", bg="white", x.intersp=0)
   on.exit(par(old.par))
   }
}

lag.plot2=function(series1,series2,max.lag=0,corr=TRUE,smooth=FALSE){ 
   name1=paste(deparse(substitute(series1)),"(t-",sep="")
   name2=paste(deparse(substitute(series2)),"(t)",sep="")
   data1=as.ts(series1)
   data2=as.ts(series2)
   max.lag=as.integer(max.lag)
   m1=max.lag+1
   prow=ceiling(sqrt(m1))
   pcol=ceiling(m1/prow)
   a=ccf(series1,series2,max.lag,plot=FALSE)$acf
   old.par <- par(no.readonly = TRUE)
   par(mfrow=c(prow,pcol), mar=c(2.5, 4, 2.5, 1), cex.main=1.1, font.main=1)
   for(h in 0:max.lag){                   
   plot(lag(series1,-h), series2, xy.labels=FALSE, main=paste(name1,h,")",sep=""), ylab=name2, xlab="") 
    if (smooth==TRUE) 
    lines(lowess(ts.intersect(lag(series1,-h),series2)[,1],
                 ts.intersect(lag(series1,-h),series2)[,2]), col="red")
    if (corr==TRUE)
    legend("topright", legend=round(a[m1-h], digits=2), text.col ="blue", bg="white", x.intersp=0)             
   on.exit(par(old.par))
   }
}

acf2=function(series,max.lag=NULL){
  num=length(series)
  if (is.null(max.lag)) max.lag=ceiling(10+sqrt(num))
  if (max.lag > (num-1)) stop("Number of lags exceeds number of observations")
  ACF=acf(series, max.lag, plot=FALSE)$acf[-1]
  PACF=pacf(series, max.lag, plot=FALSE)$acf
  LAG=1:max.lag/frequency(series)
  minA=min(ACF)
  minP=min(PACF)
  U=2/sqrt(num)
  L=-U
  minu=min(minA,minP,L)-.01
  old.par <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar = c(3,3,2,0.8),
    oma = c(1,1.2,1,1), mgp = c(1.5,0.6,0))
  plot(LAG, ACF, type="h",ylim=c(minu,1), 
    main=paste("Series: ",deparse(substitute(series))))
    abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
  plot(LAG, PACF, type="h",ylim=c(minu,1))
    abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
  on.exit(par(old.par))  
  ACF<-round(ACF,2); PACF<-round(PACF,2)    
  return(cbind(ACF, PACF)) 
  }

sarima = function(xdata,p,d,q,P=0,D=0,Q=0,S=-1,details=TRUE,tol=sqrt(.Machine$double.eps),no.constant=FALSE)
{ 
  n = length(xdata)
  constant = 1:n   
  xmean = rep(1,n) 
  trc = ifelse(details==TRUE, 1, 0)
  if (d==0 & D==0) {
    fitit = arima(xdata, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=S),
              xreg=xmean,include.mean=FALSE, optim.control=list(trace=trc,REPORT=1,reltol=tol))
} else if (xor(d==1, D==1) & no.constant==FALSE) {
    fitit = arima(xdata, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=S),
              xreg=constant,optim.control=list(trace=trc,REPORT=1,reltol=tol))
} else fitit = arima(xdata, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=S), 
              optim.control=list(trace=trc,REPORT=1,reltol=tol))
#
#  replace tsdiag with a better version
    old.par <- par(no.readonly = TRUE)
    layout(matrix(c(1,2,4, 1,3,4), nc=2))
    rs <- fitit$residuals
    stdres <- rs/sqrt(fitit$sigma2)
    num <- sum(!is.na(rs))
     plot.ts(stdres,  main = "Standardized Residuals", ylab = "")
    alag <- 10+sqrt(num)
    ACF = acf(rs, alag, plot=FALSE, na.action = na.pass)$acf[-1] 
    LAG = 1:alag/frequency(xdata)
    L=2/sqrt(num)
     plot(LAG, ACF, type="h", ylim=c(min(ACF)-.1,min(1,max(ACF+.4))), main = "ACF of Residuals")
     abline(h=c(0,-L,L), lty=c(1,2,2), col=c(1,4,4))  
    qqnorm(stdres, main="Normal Q-Q Plot of Std Residuals"); qqline(stdres, col=4) 
    nlag <- ifelse(S<4, 20, 3*S)
    ppq <- p+q+P+Q
    pval <- numeric(nlag)
    for (i in (ppq+1):nlag) {u <- Box.test(rs, i, type = "Ljung-Box")$statistic
                             pval[i] <- pchisq(u, i-ppq, lower=FALSE)}            
     plot( (ppq+1):nlag, pval[(ppq+1):nlag], xlab = "lag", ylab = "p value", ylim = c(0, 
        1), main = "p values for Ljung-Box statistic")
     abline(h = 0.05, lty = 2, col = "blue")  
    on.exit(par(old.par))    
#  end new tsdiag
#
  k = length(fitit$coef)
  BIC = log(fitit$sigma2)+(k*log(n)/n)
  AICc = log(fitit$sigma2)+((n+k)/(n-k-2))
  AIC = log(fitit$sigma2)+((n+2*k)/n)
  innov<<-fitit$resid
  list(fit=fitit, AIC=AIC, AICc=AICc, BIC=BIC)
}

sarima.for=function(xdata,n.ahead,p,d,q,P=0,D=0,Q=0,S=-1,tol=sqrt(.Machine$double.eps),no.constant=FALSE){ 
  xname=deparse(substitute(xdata))
  xdata=as.ts(xdata) 
  n=length(xdata)
  constant=1:n
  xmean=rep(1,n)
  if (d==0 & D==0) {
    fitit=arima(xdata, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=S),
            xreg=xmean,include.mean=FALSE, optim.control=list(reltol=tol));
    nureg=matrix(1,n.ahead,1)        
} else if (xor(d==1, D==1) & no.constant==FALSE) {
    fitit=arima(xdata, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=S),
            xreg=constant,optim.control=list(reltol=tol));
    nureg=(n+1):(n+n.ahead)       
} else { fitit=arima(xdata, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=S), 
            optim.control=list(reltol=tol));
          nureg=NULL   
}     
#--
 fore=predict(fitit, n.ahead, newxreg=nureg)  
#-- graph:
  U = fore$pred + 2*fore$se
  L = fore$pred - 2*fore$se
   a=max(1,n-100)
  minx=min(xdata[a:n],L)
  maxx=max(xdata[a:n],U)
   t1=xy.coords(xdata, y = NULL)$x 
   if(length(t1)<101) strt=t1[1] else strt=t1[length(t1)-100]
   t2=xy.coords(fore$pred, y = NULL)$x 
   endd=t2[length(t2)]
   xllim=c(strt,endd)
  ts.plot(xdata,fore$pred,col=1:2, xlim=xllim, ylim=c(minx,maxx), ylab=xname) 
  lines(fore$pred, col="red", type="p")
  lines(U, col="blue", lty="dashed")
  lines(L, col="blue", lty="dashed") 
#
  return(fore)
}


spec.arma <- function(ar=0,ma=0,var.noise=1,n.freq=500, ...)
{ 
    # check causality
     ar.poly <- c(1, -ar)
     z.ar <- polyroot(ar.poly)
     if(any(abs(z.ar) <= 1)) cat("WARNING: Model Not Causal", "\n")  
    # check invertibility
     ma.poly <- c(1, ma)
     z.ma <- polyroot(ma.poly)
     if(any(abs(z.ma) <= 1)) cat("WARNING: Model Not Invertible", "\n")
     if(any(abs(z.ma) <= 1) || any(abs(z.ar) <= 1) ) stop("Try Again")
    #
    ar.order <- length(ar)
    ma.order <- length(ma) 
    # check (near) parameter redundancy [i.e. are any roots (approximately) equal]  
       for (i in 1:ar.order) {
       if ( (ar == 0 & ar.order == 1) || (ma == 0 & ma.order ==1) ) break
       if(any(abs(z.ar[i]-z.ma[1:ma.order]) < 1e-03)) {cat("WARNING: Parameter Redundancy", "\n"); break}
       }
    #
    freq <- seq.int(0, 0.5, length.out = n.freq)
            cs.ar <- outer(freq, 1:ar.order, function(x, y) cos(2 * 
                pi * x * y)) %*% ar
            sn.ar <- outer(freq, 1:ar.order, function(x, y) sin(2 * 
                pi * x * y)) %*% ar
            cs.ma <- outer(freq, 1:ma.order, function(x, y) cos(2 * 
                pi * x * y)) %*% -ma
            sn.ma <- outer(freq, 1:ma.order, function(x, y) sin(2 * 
                pi * x * y)) %*% -ma                      
    spec <- var.noise*((1 - cs.ma)^2 + sn.ma^2)/((1 - cs.ar)^2 + sn.ar^2)
    spg.out <- list(freq=freq, spec=spec)
    class(spg.out) <- "spec"
    plot(spg.out, ci=0, main="", ...)
    return(invisible(spg.out))
}




cat("  itall has been installed", "\n")