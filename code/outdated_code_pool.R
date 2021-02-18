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


png("./data/GN_spring_plot1.png",width = 1000, height = 300)
par(mfrow=c(1,3))
plot(index(forplot_GN["2015-3/2015-5"]),as.vector(forplot_GN["2015-3/2015-5"]),type = "l",main="Spring 2015 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
plot(index(forplot_GN["2016-3/2016-5"]),as.vector(forplot_GN["2016-3/2016-5"]),type = "l",main="Spring 2016 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
plot(index(forplot_GN["2017-3/2017-5"]),as.vector(forplot_GN["2017-3/2017-5"]),type = "l",main="Spring 2017 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
dev.off()

png("./data/GN_spring_plot2.png",width = 1000, height =300)
par(mfrow=c(1,3))
plot(index(forplot_GN["2018-3/2018-5"]),as.vector(forplot_GN["2018-3/2018-5"]),type = "l",main="Spring 2018 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
plot(index(forplot_GN["2019-3/2019-5"]),as.vector(forplot_GN["2019-3/2019-5"]),type = "l",main="Spring 2019 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
plot(index(forplot_GN["2020-3/2020-5"]),as.vector(forplot_GN["2020-3/2020-5"]),type = "l",main="Spring 2020 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
dev.off()


png("./data/GN_spring_plot3.png",width = 300, height = 1000)
par(mfrow=c(3,1))
plot(index(forplot_GN["2015-3/2015-5"]),as.vector(forplot_GN["2015-3/2015-5"]),type = "l",main="Spring 2015 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
plot(index(forplot_GN["2016-3/2016-5"]),as.vector(forplot_GN["2016-3/2016-5"]),type = "l",main="Spring 2016 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
plot(index(forplot_GN["2017-3/2017-5"]),as.vector(forplot_GN["2017-3/2017-5"]),type = "l",main="Spring 2017 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
dev.off()

png("./data/GN_spring_plot4.png",width = 300, height =1000)
par(mfrow=c(3,1))
plot(index(forplot_GN["2018-3/2018-5"]),as.vector(forplot_GN["2018-3/2018-5"]),type = "l",main="Spring 2018 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
plot(index(forplot_GN["2019-3/2019-5"]),as.vector(forplot_GN["2019-3/2019-5"]),type = "l",main="Spring 2019 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
plot(index(forplot_GN["2020-3/2020-5"]),as.vector(forplot_GN["2020-3/2020-5"]),type = "l",main="Spring 2020 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
dev.off()


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


##### ThreeStage Tuning with tau.lam ##### 
tau.e_Vec =c(0.950, 0.980, 0.990, 0.995)
tau.lam_Vec = c(0.95)

baciThreeStage_parallel=function(data,tau.es=tau.e_Vec,tau.lams=tau.lam_Vec){
  one_list = list()
  for (tau.lam_fixed in tau.lams){
    one_model=ThreeStage(y = data$trainy,x = as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),
                                                 xstar = data$testx[,c(1:4,6:10)],
                                                 tau.e = tau.es,
                                                 grid.lam=seq(-2,2,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=tau.lam_fixed)
    one_list[[paste0("taulam",as.character(tau.lam_fixed))]]=one_model
    }
return(one_list)
}


bacigThreeStage_out_parallel = list()

for (j in region_name){
  one_region=baciThreeStage_parallel(outlist[[j]])
  bacigThreeStage_out_parallel[[j]] =one_region
}

warnings()

save(bacigThreeStage_out_parallel,file = "./data/0217bacigThreeStage_out_parallel.R")
##### ThreeStage_LassoCV Tuning with tau.lam ##### 

ThreeStage_CVLASSO_parallel=function(data,tau.es=tau.e_Vec,tau.lams= tau.lam_Vec){
  one_list = list()
  for (tau.lam_fixed in tau.lams){
    one_model=ThreeStage_lasso_cv(y = data$trainy,x = as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),
                                  xstar = data$testx[,c(1:4,6:10)],
                                  tau.e = tau.es,
                                  grid.lam=seq(-2,2,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=tau.lam_fixed)
    
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

save(ThreeStage_CVLASSO_out_parallel,file = "./data/0217ThreeStage_CVLASSO_out_parallel.R")



ThreeStage_CVLASSO_out_parallel



##### Performance of TS and TSL #####
library(xtable)

latex_perf_ftn = function(true,data1,data2){
  taus3 = c("0.95","0.98","0.99","0.995")
  TS_perf=data1
  TS_perf=TS_perf$taulam0.95
  One_region_data = true$testy
  
  
  
  ##### TS #####
  TS_perf_95=validation_tool(as.vector(One_region_data),TS_perf$Q3StageP[,1])
  TS_perf_98=validation_tool(as.vector(One_region_data),TS_perf$Q3StageP[,2])
  TS_perf_99=validation_tool(as.vector(One_region_data),TS_perf$Q3StageP[,3])
  TS_perf_995=validation_tool(as.vector(One_region_data),TS_perf$Q3StageP[,4])
  
  
  
  All_TS_one_region=data.frame(rbind(c(TS_perf_95$senc,TS_perf_95$spec,TS_perf_95$FN,TS_perf_95$FP),
                 c(TS_perf_98$senc,TS_perf_98$spec,TS_perf_98$FN,TS_perf_98$FP),
                 c(TS_perf_99$senc,TS_perf_99$spec,TS_perf_99$FN,TS_perf_99$FP),
                 c(TS_perf_995$senc,TS_perf_995$spec,TS_perf_995$FN,TS_perf_995$FP)))
  All_TS_one_region=round(All_TS_one_region,3)
  
  All_TS_one_region=cbind(taus3,All_TS_one_region)
  
  
  ##### TSL #####
  
  TSL_perf=data2
  TSL_perf=TSL_perf$taulam0.95
  
  TSL_perf_95=validation_tool(as.vector(One_region_data),TSL_perf$Q3StageP[,1])
  TSL_perf_98=validation_tool(as.vector(One_region_data),TSL_perf$Q3StageP[,2])
  TSL_perf_99=validation_tool(as.vector(One_region_data),TSL_perf$Q3StageP[,3])
  TSL_perf_995=validation_tool(as.vector(One_region_data),TSL_perf$Q3StageP[,4])
  
  
  
  All_TSL_one_region=data.frame(rbind(
                                  c(TSL_perf_95$senc,TSL_perf_95$spec,TSL_perf_95$FN,TSL_perf_95$FP),
                                 c(TSL_perf_98$senc,TSL_perf_98$spec,TSL_perf_98$FN,TSL_perf_98$FP),
                                 c(TSL_perf_99$senc,TSL_perf_99$spec,TSL_perf_99$FN,TSL_perf_99$FP),
                                 c(TSL_perf_995$senc,TSL_perf_995$spec,TSL_perf_995$FN,TSL_perf_995$FP)))
  All_TSL_one_region=round(All_TSL_one_region,3)
  
  All_TSL_one_region=cbind(taus3,All_TSL_one_region)
  
  
  ##### Binding and xtable #####
  texbind = rbind(All_TS_one_region,All_TSL_one_region,All_TSL_one_region)
  
  
  texbind=cbind(emp1,emp1,texbind)
  
  
  addtorow <- list()
  addtorow$pos <- list(0, 4,4,8,8)
  
  addtorow$command <- c("\\multirow{4}{*}{Three Stage Model} \n",
                        "\\cline{2-7} \n",
                        "& \\multirow{4}{*}{Three Stage Model with LASSO} \n",
                        "\\cline{2-7} \n",
                        "& \\multirow{4}{*}{Other_model} \n")
  
  print(xtable(texbind), add.to.row = addtorow, include.colnames = FALSE,include.rownames=FALSE)
}





for (i in c(1)){
  latex_perf_ftn(outlist[[region_name[i]]],
               bacigThreeStage_out_parallel[[region_name[i]]],
               ThreeStage_CVLASSO_out_parallel[[region_name[i]]])
}
