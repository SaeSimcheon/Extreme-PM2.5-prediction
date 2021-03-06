##### mhsmm test ######
install.packages("mhsmm")
library(mhsmm)

J <- 3
init <- c(0,0,1)
P <- matrix(c(0,.1,.4,.5,0,.6,.5,.9,0),nrow=J)
B <- list(mu=c(10,15,20),sigma=c(2,1,1.5))
d <- list(lambda=c(10,30,60),shift=c(10,100,30),type='poisson')

model <- hsmmspec(init,P,parms.emission=B,sojourn=d,dens.emission=dnorm.hsmm)
train <- simulate(model,rand.emis=rnorm.hsmm,nsim=100,seed=123456)
plot(train,xlim=c(0,400))



J<-2
initial <- rep(1/J,J)
P <- matrix(c(.3,.5,.7,.5),nrow=J)
b <- list(mu=list(c(-3,0),c(1,2)),sigma=list(diag(2),matrix(c(4,2,2,3), ncol=2)))
model <- hmmspec(init=initial, trans=P, parms.emission=b,dens.emission=dmvnorm.hsmm)
model


N <- dim(test1$trainx)[1]

train <- simulate(model, nsim=300, seed=1234, rand.emis=rmvnorm.hsmm)

list(x = cbind(as.vector(test1$trainx[,1]),as.vector(test1$trainx[,2])), N = N)

train = list()
train$x = cbind(as.vector(test1$trainx[,1]),as.vector(test1$trainx[,2]))
train$N = N
class(train) <- "hsmm.data"



J<-2
initial <- rep(1/J,J)
P <- matrix(c(.5,.5,.5,.5),nrow=J)
b <- list(mu=list(c(0,0),c(0,0)),sigma=list(diag(2),diag(2), ncol=2))
model <- hmmspec(init=initial, trans=P, parms.emission=b,dens.emission=dmvnorm.hsmm)
model

fit = hmmfit(train,model,mstep=mstep.mvnorm)


predict(fit,test1$testx[1:10,1:2])



?auto.arima


arimafit=auto.arima(test1$trainy,xreg = as.matrix(test1$trainx[,c(1:4,6:10)]))


fore=forecast(arimafit,h=2199,xreg = test1$testx[1:2199,c(1:4,6:10)])

validation_tool(as.vector(test1$testy),fore$mean)


ARIMAX_out = list()
for (j in region_name){
  print(j)
  one_region=auto.arima(as.vector(outlist[[j]]$trainy),xreg =fourier(as.vector(outlist[[j]]$trainy),K = 24) ,seasonal = F,labmda = 0, )
  fore=forecast(arimafit,h=2199,xreg = outlist[[j]]$testx[1:2199,c(1:4,6:10)])
  print(sum(fore$mean>76))
  ARIMAX_out[[j]] =fore
}


cafe04 <- window(auscafe, start=2004)

fourier(as.ts(as.vector(outlist[[j]]$trainy)),K = 1)

fourier
ts
oneone=ARIMAX_out$중랑구
sum(oneone$mean>76)







##### quantile regression #####


library(quantreg)

testset=data.frame(data$testx)
names(testset) = names(data$trainx)

rqfit95=rq(data$trainy~.,data = data.frame(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),tau = 0.95)
rqfit98=rq(data$trainy~.,data = data.frame(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),tau = 0.98)
rqfit99=rq(data$trainy~.,data = data.frame(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),tau = 0.99)
rqfit995=rq(data$trainy~.,data = data.frame(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),tau = 0.995)


pred95=predict(rqfit95,testset)
pred98=predict(rqfit98,testset)
pred99=predict(rqfit99,testset)
pred995=predict(rqfit995,testset)


validation_tool(as.vector(data$testy),pred95)
validation_tool(as.vector(data$testy),pred98)
validation_tool(as.vector(data$testy),pred99)
validation_tool(data$testy,pred995)

plot(data$testy)


data=outlist$강남구
data$trainy