##### Setup #####

source("./EXRQ_research/code/Modeling.R",encoding = "utf-8")
source("./EXRQ_research/code/DataPreprocessing.R",encoding = "utf-8")

data=readRDS("./data/0729data.rds")
region_name=names(data$Meteorological_data$precip)[names(data$Meteorological_data$precip)!="일시"]
outlist =list()

for (j in region_name){
  one_region=Data_setup(data =data,location = j)
  outlist[[j]] =one_region
}
save(outlist,file = "./data/0119All_regions.R")
load("./data/0119All_regions.R")

##### Figure 1 and Table 1 were written manually #####
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
library(xts)
PM10 <- readRDS("./data/KoreaPM10.RDS")
plot(PM10$shape)
points(PM10$place$경도, PM10$place$위도, pch=16, col="red")
match(colnames(PM10$data), PM10$uniqueplace$측정소코드)
plot(PM10$data[,1])

install.packages("ggmap")
install.packages("ggplot2")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")



library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)



P <- read.csv("./data/sample.csv", header = TRUE) #시각화할 데이터셋
map <- shapefile("./data/TL_SCCO_SIG.shp") #지리 정보 데이터셋

map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))


new_map <- fortify(map, region ='SIG_CD')
View(new_map)


install.packages("rtools40")
library(Rtools)
update.packages("Rtools")



new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]

P_merge <- merge(seoul_map, P, by='id')

#writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
#Sys.which("make")

data_full$Airpollution_data$spot

library(dplyr)
# air_spot
air_spot=left_join(data_full$Airpollution_data$spot,PM10$place,by="측정소명")

# meteor_spot
meteor_spot=na.omit(datafull$Meteorological_data$spot[,c("지점","지점명","지점주소","위도","경도")])
meteor_spot=meteor_spot[!(meteor_spot$지점명 %in% c("현충원","한강","남현")),]
row.names(meteor_spot) = NULL

# plot 
png("./data/measure_spot.png",width = 500, height = 300)
ggplot() + geom_polygon(data = P_merge, aes(x=long, y=lat, group=group), fill = 'white', color='black')+
  geom_point(data=air_spot, aes(x=경도, y=위도,shape="Airpoll")) +
  geom_point(data=meteor_spot, aes(x=경도, y=위도,shape="Meteor")) +
  theme_bw() +
  scale_shape_manual(values=c(1, 8))
dev.off()
##### Figure2 code #####
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


##### Figure3 code : correlation #####

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







