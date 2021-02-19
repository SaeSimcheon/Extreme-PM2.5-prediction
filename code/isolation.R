##### TSL #####
cv_ThreeStage_lasso <-function(y, x,tau, nfolds=10, cv_type='f2',threshold=76)
  {
  y=as.vector(y[!is.na(y)])
  x=x[!is.na(y),]
  folds <- createFolds(y, k = nfolds, list = FALSE)
  cv_error<-matrix(nrow=nfolds, ncol=50) # default ; 10 fold CV
  seq.lambda=seq(0.01,5, length.out=50)
  for(grid.lamm in 1:50){
    for(k in 1:nfolds){
      train_ind=which(folds!=k)
      test_ind=which(folds==k)
      
      fit=LASSO.fit(y[train_ind],x[train_ind,],  tau, lambda=seq.lambda[grid.lamm], intercept=T,coef.cutoff=10^(-7))
      yhat= cbind(1, x[test_ind,]) %*%fit
      
      if(cv_type=='rmse'){
        cv_error[k,grid.lamm]= sqrt(mean(  (yhat-y[test_ind])^2 , na.rm=T ) )
      }
      if(cv_type=='f2'){
        threshold=76
        tb1=table( factor(ifelse(yhat <threshold, 0, 1), levels=c(0,1))  ,factor( ifelse(y[test_ind]<threshold, 0, 1) , levels=c(0,1))   )
        m_tb1=confusionMatrix(tb1)
        beta=2
        F.Measure = (1+beta^2)*( m_tb1$byClass["Neg Pred Value"] * m_tb1$byClass["Specificity"]) / (beta^2*m_tb1$byClass["Neg Pred Value"] + m_tb1$byClass["Specificity"])
        cv_error[k,grid.lamm]= F.Measure
      }
      
    }
  }
  opt_tau=which.max(apply(cv_error, 2, mean, na.rm=T))
  if(length(opt_tau)==0){
    print('zero cases. Use RMSE criteria instead')
    for(grid.lamm in 1:50){
      for(k in 1:nfolds){
        train_ind=which(folds!=k)
        test_ind=which(folds==k)
        
        fit=LASSO.fit(y[train_ind],x[train_ind,],  tau, lambda=seq.lambda[grid.lamm], intercept=T,coef.cutoff=10^(-7))
        yhat= cbind(1, x[test_ind,]) %*%fit
        cv_error[k,grid.lamm]= sqrt(mean(  (yhat-y[test_ind])^2 , na.rm=T ) )
      }
    }
    opt_tau=which.min(apply(cv_error, 2, mean, na.rm=T))
  }
  lambda.opt=seq.lambda[opt_tau]
  print(lambda.opt)
  return(list(lambda.opt=lambda.opt))
} 
######################################################################
###################  PowT.1tau.func_lasso_cv function  ###############
######################################################################
# This function estimates optimal lambda which is used to transform PM2.5(response variable) in 'ThreeStage_lasso_cv' function.
# More details in section 3. STEP I.
PowT.1tau.func_lasso_cv<-function (y, x, tau, lams = seq(-2, 2, 0.1), a) 
{
  n <- length(y)
  compare.x <- diag(n)
  for (i in 1:n) {
    for (j in 1:n) {
      compare.x[i, j] <- prod(x[i, ] < x[j, ])
    }
  }
  Vn = bhat <- NULL
  for (lam in lams) {
    if (lam == 0) {
      Lam.y <- log(y + a)
    }
    else {
      Lam.y <- ((y + a)^lam - 1)/lam
    }
    idx.keep <- which(!is.na(Lam.y))
    Lam.y <- Lam.y[idx.keep]
    x2 <- x[idx.keep, ]
    cv.fit=cv.rq.pen(x2, (as.vector(Lam.y)),tau=tau,intercept=T)
    fit=LASSO.fit((as.vector(Lam.y)),x2,  tau, lambda=cv.fit$lambda.min, intercept=T,coef.cutoff=10^(-7))
    
    res <- (as.vector(Lam.y))- (cbind(1,x2)%*%fit)[,1]
    res <- round(res, 10)
    
    bhat <- rbind(bhat, fit)
    score <- tau - 1 * (res <= 0)
    Rn <- compare.x[idx.keep, idx.keep] * score
    Rn <- apply(Rn, 2, sum)/n
    Vn <- c(Vn, mean(Rn^2))
  }
  Vn=round(Vn,4)
  idx <- which(Vn==min(Vn))[length(which(Vn==min(Vn)) )]
  lam <- lams[idx]
  coef <- bhat[idx, ]
  return(list(lam = lam, coef = bhat))
}
PowT.1tau.func_lasso_cv
######################################################################
###################  ThreeStage_lasso_cv function ####################
######################################################################

# This function estimates the extreme values of PM2.5.
ThreeStage_lasso_cv<-function (y, x, xstar, tau.e, grid.lam =seq(-0.5, 1.5, 0.1), grid.k, 
                               tau.lam, a = 0, tol = 1e-04,cv_type="rmse") 
{
  x = as.matrix(x)
  n = length(y)
  p = ncol(x)
  nx = length(xstar)/p
  max.tau = (n - as.integer(n^(0.1)))/(n + 1)
  
  # If length(grid.lam) > 1, then estimate lambda for power transformation using 'PowT.1tau.func_lasso_cv' function.
  # Else, use fixed value which is put into by user.
  
  if (length(grid.lam) > 1) {
    tmp = PowT.1tau.func_lasso_cv(y, x, tau = tau.lam, lams = grid.lam,  a)
    lam = tmp$lam
  }
  else if (length(grid.lam) == 1)
    lam = grid.lam
  if (lam == 0) {
    Lam.y = log(y + a)
  }
  else {
    Lam.y = ((y + a)^lam - 1)/lam
  }
  
  if (length(grid.k) == 1) 
    k = grid.k
  else if (length(grid.k) > 1) 
    k = select.k.func_lasso_cv(y = y, x = x, Lam.y = Lam.y, lam = lam, a = a, max.tau = max.tau, grid.k = grid.k, n = n)
  tau = 1 - k/n
  taus = seq(tau, max.tau, length = k)
  
  ########### 'Verbosity' for parallel compututation  ########### 
  nreps<-length(taus)
  pb<-txtProgressBar(1,nreps,style=3)
  progress<-function(n){
    setTxtProgressBar(pb,n)
  }
  opts<-list(progress=progress)
  
  numCores <- detectCores() -1
  myCluster <- makeCluster(numCores)
  
  registerDoSNOW(myCluster)
  
  ########### Parallel computation for fitting intermediate quantiles model with power transformation ########### 
  
  rq1=  foreach(kk = 1:length(taus),.options.snow=opts,.export = "cv_ThreeStage_lasso",.combine = cbind,.packages =c("caret","rqPen","xts"))%dopar%{
    
    if(cv_type=='rmse'){
      cv.fit=cv.rq.pen(x,as.vector(Lam.y),tau=taus[kk], intercept=T)
      
      out=LASSO.fit((as.vector(Lam.y)),x,  tau=taus[kk], lambda=cv.fit$lambda.min, intercept=T,coef.cutoff=10^(-7))
    }
    if(cv_type=='f2'){
      cv.fit=cv_ThreeStage_lasso((as.vector(Lam.y)),x, tau=taus[kk])
      
      out=LASSO.fit((as.vector(Lam.y)),x,  tau=taus[kk], lambda=cv.fit$lambda.opt, intercept=T,coef.cutoff=10^(-7))
    }
    
    
    return(out)}
  
  stopCluster(myCluster)
  
  # Averaging coefficients of intermediate quantiles 
  coeff=apply(rq1, 1, mean)
  
  ########### Predicting intermediate quantiles for test data ########### 
  Lam.Q = (cbind(1, xstar[,]) %*% rq1)
  
  ########### Estiamting the extreme value index for test data - Hill Estimator ###########
  # section3. STEP IV.
  tt = est.gamma.func(taus = taus, Lam.Q, lam, a, tol)
  gamma.x = tt$gamma.x
  Q = tt$Q
  
  ########### Estiamting the extreme quantiles for test data - Wessiman Estimator ###########
  # section3. STEP IV.
  cgamma = mean(gamma.x, na.rm = T)
  Q3Stage = t(outer(((1 - tau)/(1 - tau.e)), (gamma.x), "^")) * Q[1:nx, 1]
  Q3StageP = outer(Q[1:nx, 1], (((1 - tau)/(1 - tau.e))^cgamma), "*") # pooled estimates
  out = list(coef=coeff,lam = lam, k = k, Q3Stage = Q3Stage, Q3StageP = Q3StageP, gamma.x = gamma.x, cgamma = cgamma)
  return(out)
}








