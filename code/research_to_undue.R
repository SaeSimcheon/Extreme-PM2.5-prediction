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