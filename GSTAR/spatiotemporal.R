#' Generalized Spatio Temporal Autoregressive #' Estimation using Ordinary Least Square
#' @param Data, W, p
#' @return Estimate Parameters GSTAR
#' @export
GSTAR<-function(Data,W,p){
  Series<-ts(Data)
  
  k<-ncol(Data) #Number Location
  n<-nroq(Data) #Number Time
  zt<-(stack(as.data.frame(t(Data)))[,1]) #Zt with Lag

  #function create lag data
  MD<-function(zt){
    M<-matrix(0,(n*k),k)
    z=0
    for( i in 1:(n)){
      for(j in 1:k){
        z<-z+1 M[z,j]<-zt[z]
      } 
    }
    M
  }
###################################
  M1<-MD(zt)
  MA<-matrix(0,(n*k-k*p),k*p)
  MAW<-matrix(0,(n*k-k*p),k*p)
  W1<-kronecker(diag(n), W)
  ztw<-W1%*%zt
  M2<-MD(ztw)
  zt<-as.matrix(zt)
  ZT<-zt[-(1:(k*p)),]
  for (i in 1:p){
    MA[(1:(n*k-k*p)),(k*(i-1)+1):(k*((i-1)+1))]<-M1[(k*(p- i)+1):(n*k-(k*i)),]
    MAW[(1:(n*k-k*p)),(k*(i-1)+1):(k*((i-1)+1))]<-M2[(k*(p- i)+1):(n*k-(k*i)),]
  }
  XT<-MA
  WXT<-MAW
  GSTAR<-data.frame(ZT,XT,WXT)
  GSTARfit<-lm(ZT~.-1,data=GSTAR) 
  fit<-summary(GSTARfit)
  MSE<-(fit$sigma)^2 
  MAE<-sum(abs(GSTARfit$residuals))/(n*k-k*p)
  MAPE<-sum(abs(GSTARfit$residuals)/ZT)/(n*k-k*p) 
  MADP<-sum(abs(GSTARfit$residuals))/sum(ZT) 
  AIC<-AIC(GSTARfit)
  R2<-fit$r.squared
  R2a<-fit$adj.r.squared
  Coef<-GSTARfit$coefficients
  Error<-data.frame(MSE=MSE, MAE=MAE, MAPE=MAPE, MADP=MADP, AIC=AIC, Rsquare=R2, AdjRSquare=R2a) Coefficient<-data.frame(Coef)
  Zt<-as.data.frame(matrix(ZT,n-p,k,byrow=T)) Zhat1<-as.data.frame(matrix(GSTARfit$fitted.values,(n- p),k,byrow = T))
  Residual<-as.data.frame(matrix(GSTARfit$residuals,(n- p),k,byrow = T))
  Result<-data.frame(Zt,Zhat1)
  Result<-ts(Result)
  windows()
  plot(Series,plot.type="single", lty=1:3, col = 4:2,xlab="Time",main="Data Series Plot")
  windows()
  plot(Result,plot.type="multiple", lty=1:3, col = 4:2,main="Predictive vs Observed") 
  windows()
  plot(Result,plot.type="single", lty=1:3, col = 4:2,main="Predictive vs Observed") 
  Residual<-ts(Residual)
  windows()
  plot(Residual,plot.type="single", lty=1:3, col = 4:2,main="Residual")
  Summary<-list(GOF=Error,Fit=fit)
  return(Summary)
}


#' Select the Best p #' @param p
#' @return p
#' @export
#' 
Model<-function(p){ 
  Error<-matrix(0,p,7) for (i in 1:p){
    Error[i,]<-as.matrix(GSTAR(Data,W,i)[[1]])
  }
  colnames(Error)<-c("MSE", "MAE", "MAPE", "MADP", "AIC", "Rsquare", "AdjRSquare")
  Result<-list(Error=Error)
  return(Result)
}