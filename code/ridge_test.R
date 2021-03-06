source("./gitgit/code/Modeling.R",encoding = "utf-8")
source("./gitgit/code/DataPreprocessing.R",encoding = "utf-8")



data=outlist[["강서구"]]

tau = 0.9
cv.fit=cv.hqreg(y = as.vector(data$trainy),X = as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),
                tau=tau,alpha = 0,type.measure = "mse",method = "quantile",lambda=seq(0.001,2,0.001))

cv.fit=cv.hqreg(y = as.vector(data$trainy),X = as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),
                tau=tau,alpha = 0,type.measure = "mse",method = "quantile",lambda = c(0,0.001,0.01,0.1,1,10,100))
cv.fit$type.measure
cv.fit$cve
cv.fit$lambda.min
cv.fit$lambda
cv.fit$fit


fit=hqreg(y = (as.vector(data$trainy)),X = as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),method = "quantile",tau=tau, lambda=cv.fit$lambda.min,alpha = 0)
fit$beta
res <- (as.vector(data$trainy))- (cbind(1,as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]]))%*%fit$beta)[,1]
res <- round(res, 10)

as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]])



