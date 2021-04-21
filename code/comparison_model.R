#quantile regression
library(dplyr)
GA=read.csv("data/0421GA.csv")
GN=read.csv("data/0421GN.csv")
EP=read.csv("data/0421EP.csv")



tail(GA %>% mutate(label=lead(PM25)))



split_lag = function(data){
  n=dim(data)[1]
  half = n*0.5
  train=data[1:half,]
  test=data[(half+1):n,]
  
  
  m_train=train %>% mutate(label=lead(PM25,1))
  m_train = na.omit(m_train)
  m_train=m_train[180:dim(m_train)[1],]
  m_train=subset(m_train , select = -(PM25))
  
  m_test=test %>% mutate(label=lead(PM25))
  m_test = na.omit(m_test)
  m_test=m_test[180:dim(m_test)[1],]
  m_test=subset(m_test , select = -(PM25))
  out=list("train"=m_train,"test"=m_test)
  return(out)
}
quantile_classifier = function(prepro_train,prepro_test,tau){
  rqfit=rq(label~.,data = prepro_train,tau = tau)
  
  testX = subset(prepro_test, select = -(label))
  
  pred=predict(rqfit,newdata = testX)
  
  pred= pred >76
  val=prepro_test$label > 76
  print(table(pred,val))
  print("total length")
  print(length(pred))
}


preprocessed_GA=split_lag(GA)
preprocessed_GN=split_lag(GN)
preprocessed_EP=split_lag(EP)

quantile_classifier(preprocessed_GA[["train"]],preprocessed_GA[["test"]],tau=.99)
quantile_classifier(preprocessed_GN[["train"]],preprocessed_GN[["test"]],tau=.99)
quantile_classifier(preprocessed_EP[["train"]],preprocessed_EP[["test"]],tau=.99)


#two stage

library(EXRQ)


Twostage_classifier = function(prepro_train,prepro_test,tau){
  trainX = subset(prepro_train, select = -(label))
  testX = subset(prepro_test, select = -(label))
  
  tsfit=TwoStage(y = preprocessed_GA[["train"]]$label,x = as.matrix(trainX),xstar =as.matrix(testX),tau.e = tau,k=50)
  
  
  pred= tsfit$Q2Stage >76
  val=prepro_test$label > 76
  print(table(pred,val))
  print("total length")
  print(length(pred))
}


Twostage_classifier(preprocessed_GA[["train"]],preprocessed_GA[["test"]],tau=.99)
Twostage_classifier(preprocessed_GN[["train"]],preprocessed_GN[["test"]],tau=.99)
Twostage_classifier(preprocessed_EP[["train"]],preprocessed_EP[["test"]],tau=.99)
