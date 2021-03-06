source("./gitgit/code/Modeling.R",encoding = "utf-8")
source("./gitgit/code/DataPreprocessing.R",encoding = "utf-8")



data=outlist[["강남구"]]

tau = 0.9
cv.fit=cv.hqreg(y = as.vector(data$trainy),X = as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),tau=tau,alpha = 0)
fit=hqreg(y = (as.vector(data$trainy)),X = as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),method = "quantile",tau=tau, lambda=cv.fit$lambda.min,alpha = 0)
fit$beta
res <- (as.vector(data$trainy))- (cbind(1,as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]]))%*%fit$beta)[,1]
res <- round(res, 10)


as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]])