##### 0115 #####

source("./code/research_model.R",encoding = "utf-8")
source("./code/Submission_data_preprocess_codecheck.R",encoding = "utf-8")
#install.packages("jtools")
library(jtools)
data=readRDS("./data/0729data.rds")

corrplot::corrplot(cor(test1$trainx[,c(1:4,6:10)]))
mc=rq(test1$trainy~.,test1$trainx[,c(1:4,6:10)],tau=0.95)
jtools::summ(mc,vifs=T)

test1=Data_setup(data =data,location = "강남구")

dim(test2$testy)
head(test2$testy)
tail(test2$testy)
###### Research for 0108 ######
### Research 1 :: The relationship between TwoStage and ThreeStage in EXRQ 
# Q. Does ThreeStage include TwoStage ? 
names(test1$trainx)[1:10]

# twostage_r1=TwoStage(y = test1$trainy,x = as.matrix(test1$trainx),xstar = test1$testx,0.995,k=200)
# threestage_r1=ThreeStage(y = test1$trainy,x = as.matrix(test1$trainx),xstar = test1$testx,tau.e = 0.995, grid.lam=1, grid.k=200, tau.lam=0.995)

twostage_r1=TwoStage(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),xstar = test1$testx[,c(1:4,6:10)],0.985,k=200)
threestage_r1=ThreeStage(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),xstar = test1$testx[,c(1:4,6:10)],tau.e = 0.985, 
                         grid.lam=1, grid.k=200, tau.lam=0.995,a=1)

mean(twostage_r1$Q2StageP - threestage_r1$Q3StageP,na.rm=T)^2 # approximate

png("./data/sec3_1.png",width = 500, height = 300)
par(mfrow=c(1,2))
plot(twostage_r1$Q2StageP,type= "l",col="red")
points(threestage_r1$Q3StageP,type="l")
plot(threestage_r1$Q3Stage,type="l",col= "red")
dev.off()

validation_tool(true_y =test1$testy,pred_y =twostage_r1$Q2StageP)
validation_tool(true_y =test1$testy,pred_y =threestage_r1$Q3StageP)



### Research 2 :: Testing for the Constancy of Extreme Value Index (cht 3.4)
# Q. Do not reject H_0 : pooling is appropriate.
png("./data/sec4_3.png",width = 500, height = 300)
par(mfrow=c(1,2))
plot(threestage_r1$Q3StageP,type="l")
plot(threestage_r1$Q3Stage,type="l")
dev.off()

out = testC.EVI(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[1:10]]),
                a = 1,tol = 1e-4, grid.lam=1, grid.k=200)
out$pval.iid
out$pval.nid

out1 = testC.EVI(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[1:10]]),
                 a = 1,tol = 1e-4, grid.lam=1, grid.k=50)
out1$pval.iid
out1$pval.nid


out2 = testC.EVI(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[1:10]]),
                 a = 1,tol = 1e-4, grid.lam=1, grid.k=80)
out2$pval.iid
out2$pval.nid

### Research 3 :: Check variations of output with parameters
## ?EXRQ::TwoStage()

# k	: the number of upper order statistics used in Hill estimator
# tol	: the tolerance level used for checking quantile crossing
# tau.e	: the extreme quantile level of interest


## ?EXRQ::ThreeStage()

# grid.k	: the grid for the number of upper order statistics involved in Hill estimator; 
#           used for searching for the data-adaptive k. 
#           If the lenfth of grid.k is 1, then k is fixed at grid.k and no selection is performed.

# grid.lam	: the set of lambda (transformation parameter) values for grid search

# tau.e	: the extreme quantile level of interest


####################  ABOUT parameter K 

# test 1 : 'k' paremeter when fixed k

# K == 200 case and tau.e = 0.985
threestage_r2=ThreeStage(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),
                         xstar = test1$testx[,c(1:4,6:10)],
                         tau.e = 0.985, 
                         grid.lam=1, grid.k=200, tau.lam=0.995,a=1)
a = 1
lam = 1
tau.e= 0.985
k = 200
tol = 1e-04
max.tau = (n - as.integer(n^(0.1)))/(n + 1)
tau = 1 - k/n
taus = seq(tau, max.tau, length = k)


xstar= test1$testx[,c(1:4,6:10)]
Lam.y = ((test1$trainy + a)^lam - 1)/lam
x= as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]])

rq1 = rq(Lam.y ~ x, taus)

Lam.Q = cbind(1, xstar) %*% rq1$coef

# est.gamma.func (lam > 0)

if (lam > 0) {
  tt = Lam.Q * lam + 1
  tt2 = 1 * (abs(tt[, 1]) < tol)
  tt[which(tt2 == 1), ] = NA
  tt3 = rowMeans(tt < tol)
  tt[which(tt3 > 0.3)] = NA
  tt[which(tt < tol)] = NA
  Q = tt^(1/lam) - a
}

Crossing_quantile_test(Q[20,]) # rearrangement
gamma=Scaled_quantiles(vec = Q[20,],taus = taus) # scaling

weight=t(outer(((1 - tau)/(1 - tau.e)), (gamma), "^"))
Q3Stage = as.vector(t(outer(((1 - tau)/(1 - tau.e)), (gamma), "^"))) * Q[50, 1]

png("./data/sec4_7.png",width = 500, height = 300)
par(mfrow=c(1,2))
plot(threestage_r2$Q3StageP,type="l")
plot(threestage_r2$Q3Stage,type="l")
dev.off()



# K == 50 case and tau.e = 0.95
threestage_r3=ThreeStage(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),
                         xstar = test1$testx[,c(1:4,6:10)],
                         tau.e = 0.95, 
                         grid.lam=1, grid.k=50, tau.lam=0.995,a=1)
a = 1
lam = 1
tau.e= 0.95
k = 50
tol = 1e-04
max.tau = (n - as.integer(n^(0.1)))/(n + 1)
tau = 1 - k/n
taus = seq(tau, max.tau, length = k)

xstar= test1$testx[,c(1:4,6:10)]
Lam.y = ((test1$trainy + a)^lam - 1)/lam
x= as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]])

rq1 = rq(Lam.y ~ x, taus)
Lam.Q = cbind(1, xstar) %*% rq1$coef

# est.gamma.func (lam > 0)

if (lam > 0) {
  tt = Lam.Q * lam + 1
  tt2 = 1 * (abs(tt[, 1]) < tol)
  tt[which(tt2 == 1), ] = NA
  tt3 = rowMeans(tt < tol)
  tt[which(tt3 > 0.3)] = NA
  tt[which(tt < tol)] = NA
  Q = tt^(1/lam) - a
}

Crossing_quantile_test(Q[20,])

for(i in 1:length(test1$trainy)){ifelse(is.na(Crossing_quantile_test(Q[i,])),print("NA"),".")}
gamma=Scaled_quantiles(vec = Q[50,],taus = taus)

#sum(is.na(threestage_r3$Q3Stage))

weight=t(outer(((1 - tau)/(1 - tau.e)), (gamma), "^"))
Q3Stage = as.vector(t(outer(((1 - tau)/(1 - tau.e)), (gamma), "^"))) * Q[50, 1]

out = testC.EVI(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),
                a = 1,tol = 1e-4, grid.lam=1, grid.k=50)
out$pval.iid
out$pval.nid

png("./data/sec4_5.png",width = 500, height = 300)
par(mfrow=c(1,2))
plot(threestage_r3$Q3StageP,type="l")
plot(threestage_r3$Q3Stage,type="l")
dev.off()

validation_tool(true_y =test1$testy,pred_y =threestage_r3$Q3StageP)

# test2 : Estimating K

threestage_r4=ThreeStage(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),
                         xstar = test1$testx[,c(1:4,6:10)],
                         tau.e = 0.985, 
                         grid.lam=1, grid.k=seq(10,300,by = 10), tau.lam=0.995,a=1)
threestage_r4$k

out = testC.EVI(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),
                a = 1,tol = 1e-4, grid.lam=1, grid.k=80)

out$pval.nid

plot(threestage_r4$Q3StageP,type= "l")
out=validation_tool(true_y =test1$testy,pred_y =threestage_r4$Q3StageP)

library(xtable)

xtable(out$table)


#########  Transformed vs Not transformed 


# test1 : TwoStage ; tau.e == 0.995 and K == 80 and lam = 1.
#       vs ThreeStage ; tau.e == 0.995 estimating lambda and K via internal function such as PowT.1tau.func


threestage_r5=ThreeStage(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),
                         xstar = test1$testx[,c(1:4,6:10)],
                         tau.e = 0.995, 
                         grid.lam=1, grid.k=80, tau.lam=0.995,a=1)


threestage_r6=ThreeStage(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),
                         xstar = test1$testx[,c(1:4,6:10)],
                         tau.e = 0.995,
                         grid.lam=seq(-5,5,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=0.95)

threestage_r6$k
threestage_r6$lam


out = testC.EVI(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),
                tol = 1e-4, grid.lam=threestage_r6$lam, grid.k=threestage_r6$k)

out$pval.iid
out$pval.nid

val_out1=validation_tool(true_y =test1$testy,pred_y =threestage_r5$Q3StageP)
val_out2=validation_tool(true_y =test1$testy,pred_y =threestage_r6$Q3StageP)

xtable(val_out1$table)
xtable(val_out2$table)

png("./data/sec5_1.png",width = 500, height = 300)
par(mfrow=c(1,2))
par(mfrow=c(1,2))
plot(as.vector(test1$testy),type = "l")
points(threestage_r5$Q3StageP,type= "l",col="red")
plot(as.vector(test1$testy),type = "l")
points(threestage_r6$Q3StageP,type= "l",col="blue")
dev.off()

# They look similar.


# test2 : TwoStage ; tau.e == multiple tau.e and K == 80 and lam = 1.
#       vs ThreeStage ; tau.e == multiple tau.e estimating lambda and K via internal function such as PowT.1tau.func
tau.e_Vec= c(0.95,0.98,0.99,0.995)

threestage_r7=ThreeStage(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),
                         xstar = test1$testx[,c(1:4,6:10)],
                         tau.e = tau.e_Vec, 
                         grid.lam=1, grid.k=80, tau.lam=0.995,a=1)


threestage_r8=ThreeStage(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),
                         xstar = test1$testx[,c(1:4,6:10)],
                         tau.e = tau.e_Vec,
                         grid.lam=seq(-5,5,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=0.95)



val1=validation_tool(true_y =test1$testy,pred_y =threestage_r7$Q3StageP[,1])
val2=validation_tool(true_y =test1$testy,pred_y =threestage_r7$Q3StageP[,2])
val3=validation_tool(true_y =test1$testy,pred_y =threestage_r7$Q3StageP[,3])
val4=validation_tool(true_y =test1$testy,pred_y =threestage_r7$Q3StageP[,4])


val5=validation_tool(true_y =test1$testy,pred_y =threestage_r8$Q3StageP[,1])
val6=validation_tool(true_y =test1$testy,pred_y =threestage_r8$Q3StageP[,2])
val7=validation_tool(true_y =test1$testy,pred_y =threestage_r8$Q3StageP[,3])
val8=validation_tool(true_y =test1$testy,pred_y =threestage_r8$Q3StageP[,4])



xtable(val1$table)
xtable(val2$table)
xtable(val3$table)
xtable(val4$table)

xtable(val5$table)
xtable(val6$table)
xtable(val7$table)
xtable(val8$table)


##################### Tau.e 


# When setting tau.e, one can apply multiple values to model as authors did in reference.
# Moreover, tau.e does not affect the other parameters. Thus, I don't care about it.




### Research 4 :: Silly forecasting model


validation_tool(true_y =as.vector(test1$testy),pred_y =as.vector(c(test1$testy[1],test1$testy[-length(test1$testy)])))
plot(as.vector(test1$testy),type = "l")
plot(as.vector(c(test1$testy[1],test1$testy[-length(test1$testy)])),type = "l")
head(c(test1$testy[1],test1$testy[-length(test1$testy)]))



### Research 5 :: multicollinearity to quantile regression.

For_vif=rq(formula = test1$trainy~.,data = data.frame(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),tau = 0.95)
summ_out=summ(For_vif,vifs = T)
xtable(summ_out)

jtools::summ()


### Research 6 :: Measure the performance of three stage with lasso_cv.

threestage_r9=ThreeStage_lasso_cv(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),
                                  xstar = test1$testx[,c(1:4,6:10)],
                                  tau.e = tau.e_Vec,
                                  grid.lam=seq(-5,5,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=0.95)



threestage_r9$lam
threestage_r9$k

threestage_r9$Q3StageP

out = testC.EVI(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[1:10]]),
                tol = 1e-4, grid.lam=threestage_r9$lam, grid.k=threestage_r9$k)
out$pval.iid
out$pval.nid

valval1=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r9$Q3StageP[,1])
valval2=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r9$Q3StageP[,2])
valval3=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r9$Q3StageP[,3])
valval4=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r9$Q3StageP[,4])

xtable(valval1$table)
xtable(valval2$table)
xtable(valval3$table)
xtable(valval4$table)



####################
###### Research fo 0115 #####
### New data sets
test2=Data_setup(data =data,location = "동작구")
test3=Data_setup(data =data,location = "양천구")

#################### GN
threestage_r8=ThreeStage(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),
                         xstar = test1$testx[,c(1:4,6:10)],
                         tau.e = tau.e_Vec,
                         grid.lam=seq(-5,5,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=0.95)

TS_GN_95=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r8$Q3StageP[,1])
TS_GN_98=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r8$Q3StageP[,2])
TS_GN_99=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r8$Q3StageP[,3])
TS_GN_995=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r8$Q3StageP[,4])


valval1=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r9$Q3StageP[,1])
threestage_r9=ThreeStage_lasso_cv(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),
                                  xstar = test1$testx[,c(1:4,6:10)],
                                  tau.e = tau.e_Vec,
                                  grid.lam=seq(-5,5,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=0.95)

TS_LCV_GN_95=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r9$Q3StageP[,1])
TS_LCV_GN_98=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r9$Q3StageP[,2])
TS_LCV_GN_99=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r9$Q3StageP[,3])
TS_LCV_GN_995=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r9$Q3StageP[,4])


save(threestage_r8,TS_GN_95,TS_GN_98,TS_GN_99,TS_GN_995,file = "./data/TS_GN.R")
save(threestage_r9,TS_LCV_GN_95,TS_LCV_GN_98,TS_LCV_GN_99,TS_LCV_GN_995,file = "./data/TS_LCV_GN.R")

#################### DJ

#threestage_r10=ThreeStage(y = test2$trainy,x = as.matrix(test2$trainx[,names(test2$trainx)[c(1:4,6:10)]]),
#                          xstar = test2$testx[,c(1:4,6:10)],
#                          tau.e = tau.e_Vec,
#                          grid.lam=seq(-5,5,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=0.95)
# In the above parameter setting, Many missing values arose.


#threestage_r10=ThreeStage(y = test2$trainy,x = as.matrix(test2$trainx[,names(test2$trainx)[c(1:4,6:10)]]),
#                         xstar = test2$testx[,c(1:4,6:10)],
#                         tau.e = tau.e_Vec,
#                         grid.lam=seq(-2,2,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=0.95)
# There was no error ,but many mssings were in output.
# When estimating lam, the result is considerably affected by tau.lam. In ref, the authors estimated the lam by using multiple tau.lam.



threestage_r10=ThreeStage(y = test2$trainy,x = as.matrix(test2$trainx[,names(test2$trainx)[c(1:4,6:10)]]),
                          xstar = test2$testx[,c(1:4,6:10)],
                          tau.e = tau.e_Vec,
                          grid.lam=seq(-2,2,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=0.95)
threestage_r10$lam
threestage_r10$k

out = testC.EVI(y = test2$trainy,x = as.matrix(test2$trainx[,names(test2$trainx)[c(1:4,6:10)]]),
                tol = 1e-4, grid.lam=1.8, grid.k=50)
out$pval.nid

TS_DJ_95=validation_tool(true_y =as.vector(test2$testy),pred_y =threestage_r10$Q3StageP[,1])
TS_DJ_98=validation_tool(true_y =as.vector(test2$testy),pred_y =threestage_r10$Q3StageP[,2])
TS_DJ_99=validation_tool(true_y =as.vector(test2$testy),pred_y =threestage_r10$Q3StageP[,3])
TS_DJ_995=validation_tool(true_y =as.vector(test2$testy),pred_y =threestage_r10$Q3StageP[,4])

#TS_DJ_95=validation_tool(true_y =as.vector(test2$testy),pred_y =threestage_r10$Q3Stage[,1])
#TS_DJ_98=validation_tool(true_y =as.vector(test2$testy),pred_y =threestage_r10$Q3Stage[,2])
#TS_DJ_99=validation_tool(true_y =as.vector(test2$testy),pred_y =threestage_r10$Q3Stage[,3])
#TS_DJ_995=validation_tool(true_y =as.vector(test2$testy),pred_y =threestage_r10$Q3Stage[,4])


#save(threestage_r8,TS_GN_95,TS_GN_98,TS_GN_99,TS_GN_995,file = "./data/TS_GN95_-2to2.R")
#save(threestage_r8,TS_GN_95,TS_GN_98,TS_GN_99,TS_GN_995,file = "./data/TS_GN95_0to2.R")
#save(threestage_r8,TS_GN_95,TS_GN_98,TS_GN_99,TS_GN_995,file = "./data/TS_GN95_1.R")
#save(threestage_r8,TS_GN_95,TS_GN_98,TS_GN_99,TS_GN_995,file = "./data/TS_GN90_-2to2.R")




threestage_r11=ThreeStage_lasso_cv(y = test2$trainy,x = as.matrix(test2$trainx[,names(test2$trainx)[c(1:4,6:10)]]),
                                   xstar = test2$testx[,c(1:4,6:10)],
                                   tau.e = tau.e_Vec,
                                   grid.lam=seq(-2,2,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=0.95)
threestage_r11$lam
threestage_r11$k

out = testC.EVI(y = test2$trainy,x = as.matrix(test2$trainx[,names(test2$trainx)[c(1:4,6:10)]]),
                tol = 1e-4, grid.lam=threestage_r11$lam, grid.k=threestage_r11$k)
out$pval.nid

TS_LCV_DJ_95=validation_tool(true_y =as.vector(test2$testy),pred_y =threestage_r11$Q3StageP[,1])
TS_LCV_DJ_98=validation_tool(true_y =as.vector(test2$testy),pred_y =threestage_r11$Q3StageP[,2])
TS_LCV_DJ_99=validation_tool(true_y =as.vector(test2$testy),pred_y =threestage_r11$Q3StageP[,3])
TS_LCV_DJ_995=validation_tool(true_y =as.vector(test2$testy),pred_y =threestage_r11$Q3StageP[,4])

save(threestage_r10,TS_DJ_95,TS_DJ_98,TS_DJ_99,TS_DJ_995,file = "./data/TS_DJ.R")
save(threestage_r11,TS_LCV_DJ_95,TS_LCV_DJ_98,TS_LCV_DJ_99,TS_LCV_DJ_995,file = "./data/TS_LCV_DJ.R")


#################### YC
threestage_r12=ThreeStage(y = test3$trainy,x = as.matrix(test3$trainx[,names(test3$trainx)[c(1:4,6:10)]]),
                          xstar = test3$testx[,c(1:4,6:10)],
                          tau.e = tau.e_Vec,
                          grid.lam=seq(-5,5,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=0.95)


TS_DJ_95=validation_tool(true_y =as.vector(test3$testy),pred_y =threestage_r12$Q3StageP[,1])
TS_DJ_98=validation_tool(true_y =as.vector(test3$testy),pred_y =threestage_r12$Q3StageP[,2])
TS_DJ_99=validation_tool(true_y =as.vector(test3$testy),pred_y =threestage_r12$Q3StageP[,3])
TS_DJ_995=validation_tool(true_y =as.vector(test3$testy),pred_y =threestage_r12$Q3StageP[,4])

valval1=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r9$Q3StageP[,1])
threestage_r9=ThreeStage_lasso_cv(y = test1$trainy,x = as.matrix(test1$trainx[,names(test1$trainx)[c(1:4,6:10)]]),
                                  xstar = test1$testx[,c(1:4,6:10)],
                                  tau.e = tau.e_Vec,
                                  grid.lam=seq(-5,5,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=0.95)

TS_LCV_GN_95=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r9$Q3StageP[,1])
TS_LCV_GN_98=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r9$Q3StageP[,2])
TS_LCV_GN_99=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r9$Q3StageP[,3])
TS_LCV_GN_995=validation_tool(true_y =as.vector(test1$testy),pred_y =threestage_r9$Q3StageP[,4])


save(threestage_r8,TS_GN_95,TS_GN_98,TS_GN_99,TS_GN_995,file = "./data/TS_GN.R")
save(threestage_r9,TS_LCV_GN_95,TS_LCV_GN_98,TS_LCV_GN_99,TS_LCV_GN_995,file = "./data/TS_LCV_GN.R")

##### 0115~upto0202 #####
region_name=names(data$Meteorological_data$precip)[names(data$Meteorological_data$precip)!="일시"]
outlist =list()

# How can I handle list object well ?

for (j in region_name){
  one_region=Data_setup(data =data,location = j)
  outlist[[j]] =one_region
}

save(outlist,file = "./data/0119All_regions.R")
load("./data/0119All_regions.R")

outlist$강남구
location="강남구"
##### ThreeStage model applied in All 25 regions #####

tau.e_Vec =c(0.950, 0.980, 0.990, 0.995)
tau.lam_Vec = c(0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99)

baciThreeStage_parallel=function(data,tau.es=tau.e_Vec,tau.lams=tau.lam_Vec){
  
  nreps<-length(tau.lams)
  pb<-txtProgressBar(1,nreps,style=3)
  progress<-function(n){
    setTxtProgressBar(pb,n)
  }
  opts<-list(progress=progress)
  
  numCores <- detectCores() -1
  myCluster <- makeCluster(numCores)
  
  registerDoSNOW(myCluster)
  
  several_model_out=  foreach(kk = tau.lams,
                              .options.snow=opts,
                              .export = "cv_ThreeStage_lasso",
                              .packages =c("caret","rqPen","xts","EXRQ"))%dopar%{
                                model=ThreeStage(y = data$trainy,x = as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),
                                                 xstar = data$testx[,c(1:4,6:10)],
                                                 tau.e = tau.es,
                                                 grid.lam=seq(-2,2,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=kk)
                                
                                
                                return(model)}
  stopCluster(myCluster)
  
  return(several_model_out)
}




bacigThreeStage_out_parallel = list()


for (j in region_name){
  one_region=baciThreeStage_parallel(outlist[[j]])
  bacigThreeStage_out_parallel[[j]] =one_region
}
save(bacigThreeStage_out_parallel,file = "./data/0120bacigThreeStage_out_parallel.R")






ThreeStage_CVLASSO_parallel=function(data,tau.es=tau.e_Vec,tau.lams= tau.lam_Vec){
  one_list = list()
  for (tau.lam_fixed in tau.lams){
    one_model=ThreeStage_lasso_cv(y = data$trainy,x = as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),
                                  xstar = data$testx[,c(1:4,6:10)],
                                  tau.e = tau.es,
                                  grid.lam=seq(-5,5,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=tau.lam_fixed)
    
    one_list[[paste0("taulam",as.character(tau.lam_fixed))]]=one_model
  }
  return(one_list)
}

ThreeStage_CVLASSO_out_parallel = list()


for (j in region_name){
  print(j)
  one_region=ThreeStage_CVLASSO_parallel(outlist[[j]])
  ThreeStage_CVLASSO_out_parallel[[j]] =one_region
}



save(ThreeStage_CVLASSO_out_parallel,file = "./data/0120ThreeStage_CVLASSO_out_parallel.R")


gd_cvl=ThreeStage_CVLASSO_out_parallel$강남구


cvl_checking_plot = function(gd_cvl_data=gd_cvl){
  
  cvl_data$taulam0.9$Q3StageP
  cvl_data$taulam0.91$Q3StageP
  cvl_data$taulam0.92$Q3StageP
  cvl_data$taulam0.93$Q3StageP
  
  cvl_data$taulam0.94$Q3StageP
  cvl_data$taulam0.95$Q3StageP
  cvl_data$taulam0.96$Q3StageP
  cvl_data$taulam0.97$Q3StageP
  
  cvl_data$taulam0.98$Q3StageP
  cvl_data$taulam0.99$Q3StageP
  
  
}

####### GN ; cvl plot #######
par(mfrow=c(4,2))
plot(as.vector(outlist[[region_name[1]]]$testy),type="l",col="yellow")
points(gd_cvl$taulam0.9$Q3StageP[,1],type = "l",col="black")
points(gd_cvl$taulam0.9$Q3StageP[,2],type = "l",col="blue")
points(gd_cvl$taulam0.9$Q3StageP[,3],type = "l",col="green")
points(gd_cvl$taulam0.9$Q3StageP[,4],type = "l",col="purple")

plot(as.vector(outlist[[region_name[1]]]$testy),type="l",col="yellow")
points(gd_cvl$taulam0.91$Q3StageP[,1],type = "l",col="black")
points(gd_cvl$taulam0.91$Q3StageP[,2],type = "l",col="blue")
points(gd_cvl$taulam0.91$Q3StageP[,3],type = "l",col="green")
points(gd_cvl$taulam0.91$Q3StageP[,4],type = "l",col="purple")

plot(as.vector(outlist[[region_name[1]]]$testy),type="l",col="yellow")
points(gd_cvl$taulam0.92$Q3StageP[,1],type = "l",col="black")
points(gd_cvl$taulam0.92$Q3StageP[,2],type = "l",col="blue")
points(gd_cvl$taulam0.92$Q3StageP[,3],type = "l",col="green")
points(gd_cvl$taulam0.92$Q3StageP[,4],type = "l",col="purple")


plot(as.vector(outlist[[region_name[1]]]$testy),type="l",col="yellow")
points(gd_cvl$taulam0.93$Q3StageP[,1],type = "l",col="black")
points(gd_cvl$taulam0.93$Q3StageP[,2],type = "l",col="blue")
points(gd_cvl$taulam0.93$Q3StageP[,3],type = "l",col="green")
points(gd_cvl$taulam0.93$Q3StageP[,4],type = "l",col="purple")


plot(as.vector(outlist[[region_name[1]]]$testy),type="l",col="yellow")
points(gd_cvl$taulam0.95$Q3Stage[,1],type = "l",col="black")
points(gd_cvl$taulam0.95$Q3Stage[,2],type = "l",col="blue")
points(gd_cvl$taulam0.95$Q3Stage[,3],type = "l",col="green")
points(gd_cvl$taulam0.95$Q3Stage[,4],type = "l",col="purple")


plot(as.vector(outlist[[region_name[1]]]$testy),type="l",col="yellow")
points(gd_cvl$taulam0.96$Q3Stage[,1],type = "l",col="black")
points(gd_cvl$taulam0.96$Q3Stage[,2],type = "l",col="blue")
points(gd_cvl$taulam0.96$Q3Stage[,3],type = "l",col="green")
points(gd_cvl$taulam0.96$Q3Stage[,4],type = "l",col="purple")

plot(as.vector(outlist[[region_name[1]]]$testy),type="l",col="yellow")
points(gd_cvl$taulam0.97$Q3Stage[,1],type = "l",col="black")
points(gd_cvl$taulam0.97$Q3Stage[,2],type = "l",col="blue")
points(gd_cvl$taulam0.97$Q3Stage[,3],type = "l",col="green")
points(gd_cvl$taulam0.97$Q3Stage[,4],type = "l",col="purple")

plot(as.vector(outlist[[region_name[1]]]$testy),type="l",col="yellow")
points(gd_cvl$taulam0.98$Q3Stage[,1],type = "l",col="black")
points(gd_cvl$taulam0.98$Q3Stage[,2],type = "l",col="blue")
points(gd_cvl$taulam0.98$Q3Stage[,3],type = "l",col="green")
points(gd_cvl$taulam0.98$Q3Stage[,4],type = "l",col="purple")

plot(as.vector(outlist[[region_name[1]]]$testy),type="l",col="yellow")
points(gd_cvl$taulam0.99$Q3Stage[,1],type = "l",col="black")
points(gd_cvl$taulam0.99$Q3Stage[,2],type = "l",col="blue")
points(gd_cvl$taulam0.99$Q3Stage[,3],type = "l",col="green")
points(gd_cvl$taulam0.99$Q3Stage[,4],type = "l",col="purple")


####### residual plot #####

par(mfrow=c(4,2))
plot(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.9$Q3StageP[,1],type="l",col="yellow")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.9$Q3StageP[,2],type="l",col="black")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.9$Q3StageP[,3],type="l",col="blue")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.9$Q3StageP[,4],type="l",col="green")

plot(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.91$Q3StageP[,1],type="l",col="yellow")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.91$Q3StageP[,2],type="l",col="black")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.91$Q3StageP[,3],type="l",col="blue")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.91$Q3StageP[,4],type="l",col="green")

plot(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.92$Q3StageP[,1],type="l",col="yellow")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.92$Q3StageP[,2],type="l",col="black")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.92$Q3StageP[,3],type="l",col="blue")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.92$Q3StageP[,4],type="l",col="green")

plot(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.93$Q3StageP[,1],type="l",col="yellow")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.93$Q3StageP[,2],type="l",col="black")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.93$Q3StageP[,3],type="l",col="blue")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.93$Q3StageP[,4],type="l",col="green")

plot(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.94$Q3StageP[,1],type="l",col="yellow")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.94$Q3StageP[,2],type="l",col="black")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.95$Q3StageP[,3],type="l",col="blue")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.95$Q3StageP[,4],type="l",col="green")

plot(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.96$Q3StageP[,1],type="l",col="yellow")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.96$Q3StageP[,2],type="l",col="black")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.96$Q3StageP[,3],type="l",col="blue")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.96$Q3StageP[,4],type="l",col="green")

plot(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.97$Q3StageP[,1],type="l",col="yellow")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.97$Q3StageP[,2],type="l",col="black")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.97$Q3StageP[,3],type="l",col="blue")
points(as.vector(outlist[[region_name[1]]]$testy)- gd_cvl$taulam0.97$Q3StageP[,4],type="l",col="green")



plot(as.vector(outlist[[region_name[1]]]$testy),type = "l")


##### Table2 code #####




discretized_num = NULL
for (j in region_name){
  tts=outlist[[j]]$testy
  PM_01=ifelse(tts<76, 0, 1) # 36 vs 76 #range(PM [which(PM_01==1) ])
  discretized_num=rbind(discretized_num,table(PM_01))
}
discretized_num=data.frame(discretized_num)

row.names(discretized_num)=region_name


names(discretized_num) = c("lessthan76","geq76")

discretized_num=discretized_num/2199

library(xtable)
discretized_num

xtable(discretized_num)



##### Figure1 code #####
forplot_GN = c(outlist[["강남구"]]$trainy,outlist[["강남구"]]$testy)
forplot_GN["2015"]
par(mfrow=c(6,1))

TSData<-data.frame(data$Airpollution_data$PM25[,"강남구"],row.names=data$Airpollution_data$PM25[,1])
rm_index=c( 1: (which(rownames(TSData)=='2015-01-01 00:00:00' ))  ) 
TSData2=data.frame( TSData[- rm_index , ] , row.names=rownames(TSData)[-rm_index])

TSData2[which(is.na(TSData2)==T), ] =approx(TSData2, xout=which(is.na(TSData2)==T))$y
TSData=TSData2

PM25<-as.xts(TSData)



png("./data/GN_spring_plot1.png",width = 800, height = 300)
par(mfrow=c(1,3))
plot(index(forplot_GN["2015-3/2015-5"]),as.vector(forplot_GN["2015-3/2015-5"]),type = "l",main="Spring 2015 of Gangnam",ylab = "PM2.5",xlab = "Month")
plot(index(forplot_GN["2016-3/2016-5"]),as.vector(forplot_GN["2016-3/2016-5"]),type = "l",main="Spring 2016 of Gangnam",ylab = "PM2.5",xlab = "Month")
plot(index(forplot_GN["2017-3/2017-5"]),as.vector(forplot_GN["2017-3/2017-5"]),type = "l",main="Spring 2017 of Gangnam",ylab = "PM2.5",xlab = "Month")
dev.off()

png("./data/GN_spring_plot2.png",width = 1000, height =300)
par(mfrow=c(1,3))
plot(index(forplot_GN["2018-3/2018-5"]),as.vector(forplot_GN["2018-3/2018-5"]),type = "l",main="Spring 2018 of Gangnam",ylab = "PM2.5",xlab = "Month")
plot(index(forplot_GN["2019-3/2019-5"]),as.vector(forplot_GN["2019-3/2019-5"]),type = "l",main="Spring 2019 of Gangnam",ylab = "PM2.5",xlab = "Month")
plot(index(forplot_GN["2020-3/2020-5"]),as.vector(forplot_GN["2020-3/2020-5"]),type = "l",main="Spring 2020 of Gangnam",ylab = "PM2.5",xlab = "Month")
dev.off()


png("./data/GN_spring_plot3.png",width = 500, height = 1000)
par(mfrow=c(6,1))


plot(as.vector(PM25["2015-3/2015-5"]),type = "l",main="Spring 2015 of Gangnam",ylab = "PM2.5",xlab = "2015-03-01 00:00:00 to 2015-05-31 23:00:00 ")
plot(as.vector(PM25["2016-3/2016-5"]),type = "l",main="Spring 2016 of Gangnam",ylab = "PM2.5",xlab = "2016-03-01 00:00:00 to 2016-05-31 23:00:00 ")
plot(as.vector(PM25["2017-3/2017-5"]),type = "l",main="Spring 2017 of Gangnam",ylab = "PM2.5",xlab = "2017-03-01 00:00:00 to 2017-05-31 23:00:00 ")
plot(as.vector(PM25["2018-3/2018-5"]),type = "l",main="Spring 2018 of Gangnam",ylab = "PM2.5",xlab = "2018-03-01 00:00:00 to 2018-05-31 23:00:00 ")
plot(as.vector(PM25["2019-3/2019-5"]),type = "l",main="Spring 2019 of Gangnam",ylab = "PM2.5",xlab = "2019-03-01 00:00:00 to 2019-05-31 23:00:00 ")
plot(as.vector(PM25["2020-3/2020-5"]),type = "l",main="Spring 2020 of Gangnam",ylab = "PM2.5",xlab = "2020-03-01 00:00:00 to 2020-05-29 19:00:00 ")
dev.off()







plot(as.vector(PM25["2015-3/2015-5"]),type = "l",main="Spring 2015 of Gangnam",ylab = "PM2.5",xlab = "2015-03-01 00:00:00 to 2015-05-31 23:00:00 ")
points(as.vector(PM25["2016-3/2016-5"]),col= "red",type = "l",main="Spring 2015 of Gangnam",ylab = "PM2.5",xlab = "2015-03-01 00:00:00 to 2015-05-31 23:00:00 ")





library(ggplot2)
PM25["2015-3/2015-5"] = NA
PM25["2015-3/2015-5",] = "y2015"




g <- ggplot(data.frame(Time, Value, Group)) + 
  geom_line (aes(x=Time, y=Value)) +
  facet_grid(~ Group, scales = "free_x")


##### Figure2 code : correlation #####

forcol_data_GN=cbind(outlist[["강남구"]]$trainx[,c(1:4,6:10)],outlist[["강남구"]]$trainy)
par(mfrow=c(1,1))
corrplot::corrplot(cor(forcol_data_GN))

cor(forcol_data_GN)

png("./data/GN_corrplot.png",width = 500, height = 500)
GGally::ggcorr(forcol_data_GN, nbreaks=8, palette='RdGy', label=TRUE, label_size=5, label_color='white')
dev.off()


##### 0115~upto0202 #####
region_name=names(data$Meteorological_data$precip)[names(data$Meteorological_data$precip)!="일시"]


##### Train /Test split for each region ##### 
outlist =list()
for (j in region_name){
  one_region=Data_setup(data =data,location = j)
  outlist[[j]] =one_region
}

save(outlist,file = "./data/0119All_regions.Rdata")
load("./data/0119All_regions.R")


outlist$강남구
location="강남구"


tau.e_Vec =c(0.950, 0.980, 0.990, 0.995)
tau.lam_Vec = c(0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99)

##### ThreeStage Tuning with tau.lam ##### 

baciThreeStage_parallel=function(data,tau.es=tau.e_Vec,tau.lams=tau.lam_Vec){
  
  nreps<-length(tau.lams)
  pb<-txtProgressBar(1,nreps,style=3)
  progress<-function(n){
    setTxtProgressBar(pb,n)
  }
  opts<-list(progress=progress)
  
  numCores <- detectCores() -1
  myCluster <- makeCluster(numCores)
  
  registerDoSNOW(myCluster)
  
  several_model_out=  foreach(kk = tau.lams,
                              .options.snow=opts,
                              .export = "cv_ThreeStage_lasso",
                              .packages =c("caret","rqPen","xts","EXRQ"))%dopar%{
                                model=ThreeStage(y = data$trainy,x = as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),
                                                 xstar = data$testx[,c(1:4,6:10)],
                                                 tau.e = tau.es,
                                                 grid.lam=seq(-2,2,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=kk)
                                
                                
                                return(model)}
  stopCluster(myCluster)
  
  return(several_model_out)
}


bacigThreeStage_out_parallel = list()


for (j in region_name){
  one_region=baciThreeStage_parallel(outlist[[j]])
  bacigThreeStage_out_parallel[[j]] =one_region
}
save(bacigThreeStage_out_parallel,file = "./data/0209bacigThreeStage_out_parallel.R")












##### ThreeStage_LassoCV Tuning with tau.lam ##### 

ThreeStage_CVLASSO_parallel=function(data,tau.es=tau.e_Vec,tau.lams= tau.lam_Vec){
  one_list = list()
  for (tau.lam_fixed in tau.lams){
    one_model=ThreeStage_lasso_cv(y = data$trainy,x = as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),
                                  xstar = data$testx[,c(1:4,6:10)],
                                  tau.e = tau.es,
                                  grid.lam=seq(-5,5,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=tau.lam_fixed)
    
    one_list[[paste0("taulam",as.character(tau.lam_fixed))]]=one_model
  }
  return(one_list)
}


ThreeStage_CVLASSO_out_parallel = list()
for (j in region_name){
  print(j)
  one_region=ThreeStage_CVLASSO_parallel(outlist[[j]])
  ThreeStage_CVLASSO_out_parallel[[j]] =one_region
}

save(ThreeStage_CVLASSO_out_parallel,file = "./data/0120ThreeStage_CVLASSO_out_parallel.R")
