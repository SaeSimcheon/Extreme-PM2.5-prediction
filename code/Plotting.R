source("./code/research_model.R",encoding = "utf-8")
source("./code/Submission_data_preprocess_codecheck.R",encoding = "utf-8")
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
tau.lam_Vec = c(0.95,0.96,0.97,0.98,0.99)


one_list = list()
for (tau.lam_fixed in tau.lams){
  one_model=ThreeStage_lasso_cv(y = data$trainy,x = as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),
                                xstar = data$testx[,c(1:4,6:10)],
                                tau.e = tau.es,
                                grid.lam=seq(-5,5,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=tau.lam_fixed)
  
  one_list[[paste0("taulam",as.character(tau.lam_fixed))]]=one_model
}
return(one_list)


baciThreeStage_parallel=function(data,tau.es=tau.e_Vec,tau.lams=tau.lam_Vec){
  

  one_list = list()
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


bacigThreeStage_out_parallel[[1]]

JG=bacigThreeStage_out_parallel$중구
GBG=bacigThreeStage_out_parallel$강북구
SDG=bacigThreeStage_out_parallel$서대문구

length(bacigThreeStage_out_parallel)

sum(is.na(JG[[1]]$Q3StageP[,1]))
points(JG[[1]]$Q3StageP[,2])
points(JG[[1]]$Q3StageP[,3])
points(JG[[1]]$Q3StageP[,4])

oneplot = function(one){
  outmat=matrix(NA,10,4)
  for( i in 1:10){
    {#print(tau.lam_Vec[i])
    vec=c(sum(is.na(one[[i]]$Q3StageP[,1])),
      sum(is.na(one[[i]]$Q3StageP[,2])),
      sum(is.na(one[[i]]$Q3StageP[,3])),
      sum(is.na(one[[i]]$Q3StageP[,4])))
    
    outmat[i,]=vec
    
    
    }
  }
  #print(outmat)
  return(outmat)
}

oneplot(JG)

empty = list()
for (i in 1:25){
  print(region_name[i])
  empty[[region_name[i]]] = oneplot(bacigThreeStage_out_parallel[[i]])
}

xtableList(empty)
unlist(empty)
?unlist
empty
xtable(empty[[1]])

unlist(empty)

df <- data.frame(matrix(unlist(empty), nrow =250, byrow=F))


for (i in 11:25){
  print(xtable(empty[[i]]))
}

regi

oneplot(JG)
oneplot(GBG)
oneplot(SDG)




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
names(PM25)=c("PM25")
#version1

png("./data/GN_spring_plot1.png",width = 800, height = 300)
par(mfrow=c(1,3))
plot(index(forplot_GN["2015-3/2015-5"]),as.vector(forplot_GN["2015-3/2015-5"]),type = "l",main="Spring 2015 of Gangnam",ylab = "PM2.5",xlab = "Month",cex.lab=1.5,cex.axis=1.5)
plot(index(forplot_GN["2016-3/2016-5"]),as.vector(forplot_GN["2016-3/2016-5"]),type = "l",main="Spring 2016 of Gangnam",ylab = "PM2.5",xlab = "Month",cex.lab=1.5,cex.axis=1.5)
plot(index(forplot_GN["2017-3/2017-5"]),as.vector(forplot_GN["2017-3/2017-5"]),type = "l",main="Spring 2017 of Gangnam",ylab = "PM2.5",xlab = "Month",cex.lab=1.5,cex.axis=1.5)
dev.off()

png("./data/GN_spring_plot2.png",width = 800, height =300)
par(mfrow=c(1,3))
plot(index(forplot_GN["2018-3/2018-5"]),as.vector(forplot_GN["2018-3/2018-5"]),type = "l",main="Spring 2018 of Gangnam",ylab = "PM2.5",xlab = "Month",cex.lab=1.5,cex.axis=1.5)
plot(index(forplot_GN["2019-3/2019-5"]),as.vector(forplot_GN["2019-3/2019-5"]),type = "l",main="Spring 2019 of Gangnam",ylab = "PM2.5",xlab = "Month",cex.lab=1.5,cex.axis=1.5)
plot(index(forplot_GN["2020-3/2020-5"]),as.vector(forplot_GN["2020-3/2020-5"]),type = "l",main="Spring 2020 of Gangnam",ylab = "PM2.5",xlab = "Month",cex.lab=1.5,cex.axis=1.5)
dev.off()

#version2


png("./data/GN_spring_plot3.png",width = 750, height = 500)
par(mfrow=c(3,1))
plot(index(forplot_GN["2015-3/2015-5"]),as.vector(forplot_GN["2015-3/2015-5"]),type = "l",main="Spring 2015 of Gangnam",ylab = "PM2.5",xlab = "Month",cex.lab=1.5,cex.axis=1.5)
plot(index(forplot_GN["2016-3/2016-5"]),as.vector(forplot_GN["2016-3/2016-5"]),type = "l",main="Spring 2016 of Gangnam",ylab = "PM2.5",xlab = "Month",cex.lab=1.5,cex.axis=1.5)
plot(index(forplot_GN["2017-3/2017-5"]),as.vector(forplot_GN["2017-3/2017-5"]),type = "l",main="Spring 2017 of Gangnam",ylab = "PM2.5",xlab = "Month",cex.lab=1.5,cex.axis=1.5)
dev.off()

png("./data/GN_spring_plot4.png",width = 750, height = 500)
par(mfrow=c(3,1))

plot(index(forplot_GN["2018-3/2018-5"]),as.vector(forplot_GN["2018-3/2018-5"]),type = "l",main="Spring 2018 of Gangnam",ylab = "PM2.5",xlab = "Month",cex.lab=1.5,cex.axis=1.5)
plot(index(forplot_GN["2019-3/2019-5"]),as.vector(forplot_GN["2019-3/2019-5"]),type = "l",main="Spring 2019 of Gangnam",ylab = "PM2.5",xlab = "Month",cex.lab=1.5,cex.axis=1.5)
plot(index(forplot_GN["2020-3/2020-5"]),as.vector(forplot_GN["2020-3/2020-5"]),type = "l",main="Spring 2020 of Gangnam",ylab = "PM2.5",xlab = "Month",cex.lab=1.5,cex.axis=1.5)
dev.off()

#version3

png("./data/GN_spring_plot1.png",width = 800, height = 300)
index(forplot_GN["2015-3/2015-5"])

plot(c(as.POSIXct("2015-03-01 01:00:00 KST"),index(forplot_GN["2015-3/2015-5"])),c(as.vector(forplot_GN["2015-3/2015-5"])[1],as.vector(forplot_GN["2015-3/2015-5"])),type = "l",lty = 1,main="Springs of Gangnam",ylab = "PM2.5",xlab = "Month",col = "grey")
points(c(as.POSIXct("2015-03-01 01:00:00 KST"),index(forplot_GN["2015-3/2015-5"])),as.vector(forplot_GN["2016-3/2016-5"]),type = "l",col = "grey",lty = 2)
points(c(as.POSIXct("2015-03-01 01:00:00 KST"),index(forplot_GN["2015-3/2015-5"])),as.vector(forplot_GN["2017-3/2017-5"]),type = "l",col = "grey",lty = 3)
points(c(as.POSIXct("2015-03-01 01:00:00 KST"),index(forplot_GN["2015-3/2015-5"])),as.vector(forplot_GN["2018-3/2018-5"]),type = "l",col = "grey",lty = 4)
points(c(as.POSIXct("2015-03-01 01:00:00 KST"),index(forplot_GN["2015-3/2015-5"])),as.vector(forplot_GN["2019-3/2019-5"]),type = "l",col = "grey",lty = 5)
points(c(as.POSIXct("2015-03-01 01:00:00 KST"),index(forplot_GN["2015-3/2015-5"])),as.vector(forplot_GN["2020-3/2020-5"]),type = "l",col = "grey",lty = 6)


#version4

png("./data/GN_spring_plot3.png",width = 500, height = 1000)

par(mfrow=c(1,1))

test_GN=data.frame(as.vector(forplot_GN))
test_GN$date  = index(forplot_GN)

PM25$Seasons = NA

PM25["2015-3/2015-5","Seasons"] = "spring"
PM25["2016-3/2016-5","Seasons"] = "spring"
PM25["2017-3/2017-5","Seasons"] = "spring"
PM25["2018-3/2018-5","Seasons"] = "spring"
PM25["2019-3/2019-5","Seasons"] = "spring"
PM25["2020-3/2020-5","Seasons"] = "spring"

PM25[is.na(PM25$Season) ,"Seasons"] = "Non-spring"
PM25[,"PM25"]=as.numeric(PM25$PM25)
PM25=data.frame(PM25)
PM25$PM25 = as.numeric(PM25$PM25)
as.xts(PM25)

plot(PM25$PM25,type = "l",ylab = "PM2.5")
points(PM25$PM25["2015-3/2015-5"],type ="l",col = "red")
axis(labels =format(index(PM25),"%Y-%m"))

attributes(PM25)$tsp

PM25=as.data.frame(PM25)

PM25$Seasons


cbind(PM25,"Spring")

PM25$Seasons =NA
library(ggplot2)
PM25["2015-3/2015-5"] = NA
PM25["2015-3/2015-5",] = "y2015"




row.names(PM25)

ggplot() + 
  geom_line(aes(x=row.names(PM25),y=PM25$PM25,group=PM25$Seasons))


##### Figure2 code : correlation #####

forcol_data_GN=cbind(outlist[["강남구"]]$trainx[,c(1:4,6:10)],outlist[["강남구"]]$trainy)
par(mfrow=c(1,1))
corrplot::corrplot(cor(forcol_data_GN))

cor(forcol_data_GN)

png("./data/GN_corrplot.png",width = 500, height = 500)
GGally::ggcorr(forcol_data_GN, nbreaks=8, palette='RdGy', label=TRUE, label_size=5, label_color='white')
dev.off()





##### testing #####

n= 500
k=50
seq(1-k/n,1-n^(-0.9),length.out = k+1)
seq(1-k/(n+1),1-n^(-0.9),length.out = k)

max.tau = (n - as.integer(n^(0.1)))/(n + 1)

taus = seq(1-k/n,max.tau, length = k)




##### SARIMAX #####
install.packages("astsa")
library(astsa)


m=sarima(test1$trainy,p = 3,d =2 ,q = 3,P = 2,Q =1,D =1 ,S = 4, xreg = test1$trainx[,c(1:4,6:10)])



newx_test1=data.frame(test1$testx[,c(1:4,6:10)])

names(newx_test1) = names(test1$trainx[,c(1:4,6:10)])

f=forecast(m, h=60,level=c(0.95),xreg = newx_test1)



grid <- expand.grid(p=c(1,2,3,4),d=c(1,2), q=c(1,2,3,4),P=c(1,2,3,4),D=c(1,2),Q=c(1,2,3,4),S=4)

ModFit <- function(griding){
  m=sarima(test1$trainy,p = griding$p,d =griding$d ,q = griding$q,P = griding$P,Q =griding$Q,D =griding$D ,S = griding$S, xreg = test1$trainx[,c(1:4,6:10)])
  return(m)
} 

sarimaxlist = list()

for (i in dim(grid)){
  sarimaxlist[[i]]=ModFit(griding = grid[i,])
}

sarimaxlist[[1]]




ccf(x=as.numeric(test1$trainx$precip),y=as.numeric(test1$trainy))
ccf(x=as.numeric(test1$trainx$temp),y=as.numeric(test1$trainy))
ccf(x=as.numeric(test1$trainx$hum),y=as.numeric(test1$trainy))
ccf(x=as.numeric(test1$trainx$win),y=as.numeric(test1$trainy))
ccf(x=as.numeric(test1$trainx$CO),y=as.numeric(test1$trainy))

ccf(x=as.numeric(test1$trainx$NO2),y=as.numeric(test1$trainy))
ccf(x=as.numeric(test1$trainx$O3),y=as.numeric(test1$trainy))
ccf(x=as.numeric(test1$trainx$PM10),y=as.numeric(test1$trainy))
ccf(x=as.numeric(test1$trainx$SO2),y=as.numeric(test1$trainy))








##### performance graph #####

perf_extract = function(perf_obj){
  vec=c(perf_obj$senc,perf_obj$spec,perf_obj$FN,perf_obj$FP)
  return(vec)
}

dataa=rbind(
perf_extract(TS_GN_95),
perf_extract(TS_GN_98),
perf_extract(TS_GN_99),
perf_extract(TS_GN_995)
)

dataa=data.frame(dataa)
names(dataa) = c("sen","spec","FN","FP")
dataa$tau.e = c(0.95,0.98,0.99,0.995)


max_ratio = max(c(dataa$spec,dataa$sen))/max(c(dataa$FP,dataa$FN))

perf_plot =function(data){
  g <- ggplot(data, aes(x =  tau.e))
  g <- g + geom_point(aes(y = sen), colour = "black",shape=8,size=3)+geom_line(aes(y = sen))
  g <- g + ylab("Sen and Spec")
  g <- g + geom_point(aes(y = spec),colour = "black",shape=9,size=3)+geom_line(aes(y = spec))
  g <- g + geom_point(aes(y = FN), colour = "black",shape=10,size=3)+geom_line(aes(y = FN))
  g <- g + geom_point(aes(y = FP),colour = "black",shape=15,size=3)+geom_line(aes(y = FP))
  
  # adding secondary axis
  
  
  g <- g + scale_y_continuous(sec.axis = sec_axis(~., name="FN and FP"))
  g <- g + theme_bw()
  return(g)
}

datab=rbind(
perf_extract(TS_LCV_GN_95),
perf_extract(TS_LCV_GN_98),
perf_extract(TS_LCV_GN_99),
perf_extract(TS_LCV_GN_995)
)
datac=rbind(
  perf_extract(TS_DJ_95),
  perf_extract(TS_DJ_98),
  perf_extract(TS_DJ_99),
  perf_extract(TS_DJ_995)
)
datad=rbind(
  perf_extract(TS_LCV_DJ_95),
  perf_extract(TS_LCV_DJ_98),
  perf_extract(TS_LCV_DJ_99),
  perf_extract(TS_LCV_DJ_995)
)


datab=data.frame(datab)
names(datab) = c("sen","spec","FN","FP")
datab$tau.e = c(0.95,0.98,0.99,0.995)

datac=data.frame(datac)
names(datac) = c("sen","spec","FN","FP")
datac$tau.e = c(0.95,0.98,0.99,0.995)

datad=data.frame(datad)
names(datad) = c("sen","spec","FN","FP")
datad$tau.e = c(0.95,0.98,0.99,0.995)




png("./data/TS_GN.png",width = 500, height = 500)
perf_plot(dataa)
dev.off()

png("./data/TSL_GN.png",width = 500, height = 500)
perf_plot(datab)
dev.off()

#datab[1,] = c(0.8032787,0.9700000,0.19672131,   0.03000000,0.95)
#datab[2,] = c(0.8688525,0.9647619,0.13114754,    0.03523810,0.98)
#datab[3,] = c(0.9180328,0.9585714,0.08196721,    0.04142857,0.99)
#datab[4,] = c(0.9672131,0.9533333,0.03278689,    0.04666667,0.995)

png("./data/TS_DJ.png",width = 500, height = 500)
perf_plot(datac)
dev.off()

png("./data/TSL_DJ.png",width = 500, height = 500)
perf_plot(datad)
dev.off()




perf_plot1 =function(data1,data2,data3,data4){
  g <- ggplot(data1, aes(x =  tau.e))
  g <- g + geom_point(aes(y = sen), colour = "black",shape=8,size=3)+geom_line(aes(y = sen))
  g <- g + ylab("Sen and Spec")
  g <- g + geom_point(aes(y = spec),colour = "black",shape=9,size=3)+geom_line(aes(y = spec))
  g <- g + geom_point(aes(y = FN), colour = "black",shape=10,size=3)+geom_line(aes(y = FN))
  g <- g + geom_point(aes(y = FP),colour = "black",shape=15,size=3)+geom_line(aes(y = FP))
  
  g <- g + geom_point(data=data2,aes(y = sen), colour = "black",shape=8,size=3)+geom_line(data=data2,aes(y = sen))
  g <- g + geom_point(data=data2,aes(y = spec),colour = "black",shape=9,size=3)+geom_line(data=data2,aes(y = spec))
  g <- g + geom_point(data=data2,aes(y = FN), colour = "black",shape=10,size=3)+geom_line(data=data2,aes(y = FN))
  g <- g + geom_point(data=data2,aes(y = FP),colour = "black",shape=15,size=3)+geom_line(data=data2,aes(y = FP))
  
  g <- g + geom_point(data=data3,aes(y = sen), colour = "black",shape=8,size=3)+geom_line(data=data3,aes(y = sen))
  g <- g + geom_point(data=data3,aes(y = spec),colour = "black",shape=9,size=3)+geom_line(data=data3,aes(y = spec))
  g <- g + geom_point(data=data3,aes(y = FN), colour = "black",shape=10,size=3)+geom_line(data=data3,aes(y = FN))
  g <- g + geom_point(data=data3,aes(y = FP),colour = "black",shape=15,size=3)+geom_line(data=data3,aes(y = FP))
  
  
  g <- g + geom_point(data=data4,aes(y = sen), colour = "black",shape=8,size=3)+geom_line(data=data4,aes(y = sen))
  g <- g + geom_point(data=data4,aes(y = spec),colour = "black",shape=9,size=3)+geom_line(data=data4,aes(y = spec))
  g <- g + geom_point(data=data4,aes(y = FN), colour = "black",shape=10,size=3)+geom_line(data=data4,aes(y = FN))
  g <- g + geom_point(data=data4,aes(y = FP),colour = "black",shape=15,size=3)+geom_line(data=data4,aes(y = FP))
  
  g <- g + scale_y_continuous(sec.axis = sec_axis(~., name="FN and FP"))
  g <- g + theme_bw()
  return(g)
}

png("./data/Two_region_perf.png",width = 500, height = 500)
perf_plot1(dataa,datab,datac,datad)
dev.off()


dataa$model = "TS_GN"
datab$model = "TSL_GN"
datac$model = "TS_DJ"
datad$model = "TSL_DJ"



GN_perf=rbind(dataa,datab)
DJ_perf=rbind(datac,datad)




g1=GN_perf %>% ggplot(aes(x=as.factor(tau.e),y = sen,fill =model))+
  xlab("Tau.e")+
  geom_bar(stat='identity',position = 'dodge')+
  coord_flip() + 
  scale_fill_grey()+
  theme_bw()
  
g2=GN_perf %>% ggplot(aes(x=as.factor(tau.e),y = spec,fill =model))+
  xlab("Tau.e")+
  geom_bar(stat='identity',position = 'dodge')+
  coord_flip() + 
  scale_fill_grey()+
  theme_bw()


g3=GN_perf %>% ggplot(aes(x=as.factor(tau.e),y = FN,fill =model))+
  xlab("Tau.e")+
  geom_bar(stat='identity',position = 'dodge')+
  coord_flip() + 
  scale_fill_grey()+
  theme_bw()


g4=GN_perf %>% ggplot(aes(x=as.factor(tau.e),y = FP,fill =model))+
  xlab("Tau.e")+
  geom_bar(stat='identity',position = 'dodge')+
  coord_flip() + 
  scale_fill_grey()+
  theme_bw()


png("./data/GN_region_perf_bar.png",width = 800, height = 500)
grid.arrange(g1,g2,g3,g4, nrow=2, ncol=2)
dev.off()



g1=DJ_perf %>% ggplot(aes(x=as.factor(tau.e),y = sen,fill =model))+
  xlab("Tau.e")+
  geom_bar(stat='identity',position = 'dodge')+
  coord_flip() + 
  scale_fill_grey()+
  theme_bw()

g2=DJ_perf %>% ggplot(aes(x=as.factor(tau.e),y = spec,fill =model))+
  xlab("Tau.e")+
  geom_bar(stat='identity',position = 'dodge')+
  coord_flip() + 
  scale_fill_grey()+
  theme_bw()


g3=DJ_perf %>% ggplot(aes(x=as.factor(tau.e),y = FN,fill =model))+
  xlab("Tau.e")+
  geom_bar(stat='identity',position = 'dodge')+
  coord_flip() + 
  scale_fill_grey()+
  theme_bw()


g4=DJ_perf %>% ggplot(aes(x=as.factor(tau.e),y = FP,fill =model))+
  xlab("Tau.e")+
  geom_bar(stat='identity',position = 'dodge')+
  coord_flip() + 
  scale_fill_grey()+
  theme_bw()

png("./data/DJ_region_perf_bar.png",width = 800, height = 500)
grid.arrange(g1,g2,g3,g4, nrow=2, ncol=2)
dev.off()

dataa$region = "GN"
datab$region = "GN"
datac$region = "DJ"
datad$region = "DJ"


dataa$model = "TS"
datab$model = "TSL"
datac$model = "TS"
datad$model = "TSL"



sen_perf = rbind(dataa[,c("sen","tau.e","model","region")],datab[,c("sen","tau.e","model","region")],
                 datac[,c("sen","tau.e","model","region")],datad[,c("sen","tau.e","model","region")])

spec_perf = rbind(dataa[,c("spec","tau.e","model","region")],datab[,c("spec","tau.e","model","region")],
                 datac[,c("spec","tau.e","model","region")],datad[,c("spec","tau.e","model","region")])
FN_perf = rbind(dataa[,c("FN","tau.e","model","region")],datab[,c("FN","tau.e","model","region")],
                  datac[,c("FN","tau.e","model","region")],datad[,c("FN","tau.e","model","region")])

FP_perf = rbind(dataa[,c("FP","tau.e","model","region")],datab[,c("FP","tau.e","model","region")],
                datac[,c("FP","tau.e","model","region")],datad[,c("FP","tau.e","model","region")])




g1=sen_perf[sen_perf$tau.e==0.95,] %>% ggplot(aes(x=as.factor(region),y = sen,fill =model))+
  xlab("region")+
  geom_bar(stat='identity',position = 'dodge')+
  coord_flip() + 
  scale_fill_grey()+
  theme_bw()+ ggtitle('Tau.e=0.95')

g2=sen_perf[sen_perf$tau.e==0.98,] %>% ggplot(aes(x=as.factor(region),y = sen,fill =model))+
  xlab("region")+
  geom_bar(stat='identity',position = 'dodge')+
  coord_flip() + 
  scale_fill_grey()+
  theme_bw()+ ggtitle('Tau.e=0.98')


g3=sen_perf[sen_perf$tau.e==0.99,] %>% ggplot(aes(x=as.factor(region),y = sen,fill =model))+
  xlab("region")+
  geom_bar(stat='identity',position = 'dodge')+
  coord_flip() + 
  scale_fill_grey()+
  theme_bw()+ ggtitle('Tau.e=0.99')


g4=sen_perf[sen_perf$tau.e==0.995,] %>% ggplot(aes(x=as.factor(region),y = sen,fill =model))+
  xlab("region")+
  geom_bar(stat='identity',position = 'dodge')+
  coord_flip() + 
  scale_fill_grey()+
  theme_bw()+ ggtitle('Tau.e=0.995')

png("./data/All_in_plot_perf.png",width = 800, height = 500)
grid.arrange(g1,g2,g3,g4, nrow=2, ncol=2)
dev.off()