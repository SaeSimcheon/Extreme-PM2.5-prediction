##### Setup #####

source("./EXRQ_research/code/Modeling.R",encoding = "utf-8")
source("./EXRQ_research/code/DataPreprocessing.R",encoding = "utf-8")

data=readRDS("./data/0729data.rds")
region_name=names(data$Meteorological_data$precip)[names(data$Meteorological_data$precip)!="일시"]
# outlist =list()
for (j in region_name){
  one_region=Data_setup(data =data,location = j)
  outlist[[j]] =one_region
}
#save(outlist,file = "./data/0119All_regions.R")
load("./data/0119All_regions.Rdata")

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
points(index(forplot_GN["2015-3/2015-5"]),as.vector(forplot_GN["2015-3/2015-5"]))
abline(h=76,lty="dashed")
plot(index(forplot_GN["2016-3/2016-5"]),as.vector(forplot_GN["2016-3/2016-5"]),type = "l",main="Spring 2016 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
points(index(forplot_GN["2016-3/2016-5"]),as.vector(forplot_GN["2016-3/2016-5"]))
abline(h=76,lty="dashed")
plot(index(forplot_GN["2017-3/2017-5"]),as.vector(forplot_GN["2017-3/2017-5"]),type = "l",main="Spring 2017 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
points(index(forplot_GN["2017-3/2017-5"]),as.vector(forplot_GN["2017-3/2017-5"]))
abline(h=76,lty="dashed")
dev.off()

png("./data/GN_spring_plot2.png",width = 1000, height =300)
par(mfrow=c(1,3))
plot(index(forplot_GN["2018-3/2018-5"]),as.vector(forplot_GN["2018-3/2018-5"]),type = "l",main="Spring 2018 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
points(index(forplot_GN["2018-3/2018-5"]),as.vector(forplot_GN["2018-3/2018-5"]),main="Spring 2018 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
plot(index(forplot_GN["2019-3/2019-5"]),as.vector(forplot_GN["2019-3/2019-5"]),type = "l",main="Spring 2019 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
points(index(forplot_GN["2019-3/2019-5"]),as.vector(forplot_GN["2019-3/2019-5"]),main="Spring 2019 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
plot(index(forplot_GN["2020-3/2020-5"]),as.vector(forplot_GN["2020-3/2020-5"]),type = "l",main="Spring 2020 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
points(index(forplot_GN["2020-3/2020-5"]),as.vector(forplot_GN["2020-3/2020-5"]),main="Spring 2020 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
dev.off()


png("./data/GN_spring_plot3.png",width = 1000, height = 700)
par(mfrow=c(3,1))
plot(index(forplot_GN["2015-3/2015-5"]),as.vector(forplot_GN["2015-3/2015-5"]),type = "l",main="Spring 2015 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
points(index(forplot_GN["2015-3/2015-5"]),as.vector(forplot_GN["2015-3/2015-5"]),main="Spring 2015 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
plot(index(forplot_GN["2016-3/2016-5"]),as.vector(forplot_GN["2016-3/2016-5"]),type = "l",main="Spring 2016 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
points(index(forplot_GN["2016-3/2016-5"]),as.vector(forplot_GN["2016-3/2016-5"]),main="Spring 2016 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
plot(index(forplot_GN["2017-3/2017-5"]),as.vector(forplot_GN["2017-3/2017-5"]),type = "l",main="Spring 2017 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
points(index(forplot_GN["2017-3/2017-5"]),as.vector(forplot_GN["2017-3/2017-5"]),main="Spring 2017 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
dev.off()

png("./data/GN_spring_plot4.png",width = 1000, height =700)
par(mfrow=c(3,1))
plot(index(forplot_GN["2018-3/2018-5"]),as.vector(forplot_GN["2018-3/2018-5"]),type = "l",main="Spring 2018 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
points(index(forplot_GN["2018-3/2018-5"]),as.vector(forplot_GN["2018-3/2018-5"]),main="Spring 2018 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
plot(index(forplot_GN["2019-3/2019-5"]),as.vector(forplot_GN["2019-3/2019-5"]),type = "l",main="Spring 2019 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
points(index(forplot_GN["2019-3/2019-5"]),as.vector(forplot_GN["2019-3/2019-5"]),main="Spring 2019 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
abline(h=76,lty="dashed")
plot(index(forplot_GN["2020-3/2020-5"]),as.vector(forplot_GN["2020-3/2020-5"]),type = "l",main="Spring 2020 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
points(index(forplot_GN["2020-3/2020-5"]),as.vector(forplot_GN["2020-3/2020-5"]),main="Spring 2020 of Gangnam",ylab = "PM2.5",xlab = "Month",ylim=c(0, 150))
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




PowT.1tau.func1
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
##### ThreeStage_ridgeCV Tuning with tau.lam ##### 
tau.e_Vec =c(0.950, 0.980, 0.990, 0.995)
tau.lam_Vec = c(0.95)

ThreeStage_CVRIDGE_parallel=function(data,tau.es=tau.e_Vec,tau.lams= tau.lam_Vec){
  one_list = list()
  for (tau.lam_fixed in tau.lams){
    one_model=ThreeStage_ridge_cv(y = data$trainy,x = as.matrix(data$trainx[,names(data$trainx)[c(1:4,6:10)]]),
                                  xstar = data$testx[,c(1:4,6:10)],
                                  tau.e = tau.es,
                                  grid.lam=seq(-2,2,by=0.1),grid.k=seq(10,300,by = 10), tau.lam=tau.lam_fixed,cv_type = "f2")
    
    one_list[[paste0("taulam",as.character(tau.lam_fixed))]]=one_model
  }
  return(one_list)
}

ThreeStage_CVRIDGE_out_parallel = list()
for (j in region_name[c(1)]){
  print(j)
  one_region=ThreeStage_CVRIDGE_parallel(outlist[[j]])
  ThreeStage_CVRIDGE_out_parallel[[j]] =one_region
}

#save(ThreeStage_CVRIDGE_out_parallel,file = "./data/0306ThreeStage_CVRIDGE_out_parallel.R")
save(ThreeStage_CVRIDGE_out_parallel,file = "./data/0306some_region_ThreeStage_CVRIDGE_out_parallel.R")


test1=ThreeStage_CVRIDGE_out_parallel$강남구
test2=ThreeStage_CVRIDGE_out_parallel$광진구

validation_tool(as.vector(outlist$강남구$testy),test1$taulam0.95$Q3StageP[,2])

plot(as.vector(outlist$강남구$testy))
plot(test1$taulam0.95$Q3StageP[,3])

plot(test2$taulam0.95$Q3StageP[,2])

#ThreeStage_CVLASSO_out_parallel

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
                        "& \\multirow{4}{*}{Other model} \n")
  
  print(xtable(texbind,digits = c(NA,NA,NA,NA,3,3,3,3)), add.to.row = addtorow, include.colnames = FALSE,include.rownames=FALSE)
}


for (i in c(1,11,21)){
  print(region_name[i])
  latex_perf_ftn(outlist[[region_name[i]]],
               bacigThreeStage_out_parallel[[region_name[i]]],
               ThreeStage_CVLASSO_out_parallel[[region_name[i]]])
}









##### Performance measurement plot ; resional comparaison#####
figure5 = function(true,data1,data2,region){
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
  
  All_TS_one_region$model = "TS"
  
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
  All_TSL_one_region$model = "TSL"
  
  ##### other model #####
  All_TSL_one_region1 = All_TSL_one_region
  All_TSL_one_region1$model = "other model"
  
  ##### Binding and xtable #####
  texbind = rbind(All_TS_one_region,All_TSL_one_region,All_TSL_one_region1)
  texbind$region = region
  names(texbind) = c("tau.e","sen","spe","FN","FP","model","region")
  return(texbind)
}

ett=data.frame()

en_region_name = c("Gangnam",
                   "Gangdong",
                   "Gangbuk",
                   "Gangseo","Gwanak","Gwangjin","Guro",
                   "Geumcheon","Nowon","Dobong","Dongdaemun","Dongjak","Mapo",
                   "Seodaemun","Seocho","Seongdong  ","Seongbuk","Songpa","Yangcheon",
                   "Yeongdeungpo","Yongsan","Eunpyeong","Jongno","Jung","Jungnang")


for (i in 1:25){
  testout=figure5(outlist[[region_name[i]]],
                          bacigThreeStage_out_parallel[[region_name[i]]],
                          ThreeStage_CVLASSO_out_parallel[[region_name[i]]],
                  en_region_name[i])
  ett = rbind(ett,testout)
  }


figure_sen_ftn=
  function(data){
  g1=data[data$tau.e==0.95,] %>% ggplot(aes(x=as.factor(region),y = sen,fill =model))+
    xlab("region")+
    geom_bar(stat='identity',position = 'dodge')+
    coord_flip() + 
    scale_fill_grey()+
    theme_bw()+ ggtitle('Tau.e=0.95')
  
  g2=data[data$tau.e==0.98,] %>% ggplot(aes(x=as.factor(region),y = sen,fill =model))+
    xlab("region")+
    geom_bar(stat='identity',position = 'dodge')+
    coord_flip() + 
    scale_fill_grey()+
    theme_bw()+ ggtitle('Tau.e=0.98')
  
  
  g3=data[data$tau.e==0.99,] %>% ggplot(aes(x=as.factor(region),y = sen,fill =model))+
    xlab("region")+
    geom_bar(stat='identity',position = 'dodge')+
    coord_flip() + 
    scale_fill_grey()+
    theme_bw()+ ggtitle('Tau.e=0.99')
  
  
  g4=data[data$tau.e==0.995,] %>% ggplot(aes(x=as.factor(region),y = sen,fill =model))+
    xlab("region")+
    geom_bar(stat='identity',position = 'dodge')+
    coord_flip() + 
    scale_fill_grey()+
    theme_bw()+ ggtitle('Tau.e=0.995')
  grid.arrange(g1,g2,g3,g4, nrow=2, ncol=2)
  }
figure_spe_ftn=
  function(data){
    g1=data[data$tau.e==0.95,] %>% ggplot(aes(x=as.factor(region),y = spe,fill =model))+
      xlab("region")+
      geom_bar(stat='identity',position = 'dodge')+
      coord_flip() + 
      scale_fill_grey()+
      theme_bw()+ ggtitle('Tau.e=0.95')
    
    g2=data[data$tau.e==0.98,] %>% ggplot(aes(x=as.factor(region),y = spe,fill =model))+
      xlab("region")+
      geom_bar(stat='identity',position = 'dodge')+
      coord_flip() + 
      scale_fill_grey()+
      theme_bw()+ ggtitle('Tau.e=0.98')
    
    
    g3=data[data$tau.e==0.99,] %>% ggplot(aes(x=as.factor(region),y = spe,fill =model))+
      xlab("region")+
      geom_bar(stat='identity',position = 'dodge')+
      coord_flip() + 
      scale_fill_grey()+
      theme_bw()+ ggtitle('Tau.e=0.99')
    
    
    g4=data[data$tau.e==0.995,] %>% ggplot(aes(x=as.factor(region),y = spe,fill =model))+
      xlab("region")+
      geom_bar(stat='identity',position = 'dodge')+
      coord_flip() + 
      scale_fill_grey()+
      theme_bw()+ ggtitle('Tau.e=0.995')
    grid.arrange(g1,g2,g3,g4, nrow=2, ncol=2)
  }
figure_FN_ftn=
  function(data){
    g1=data[data$tau.e==0.95,] %>% ggplot(aes(x=as.factor(region),y = FN,fill =model))+
      xlab("region")+
      geom_bar(stat='identity',position = 'dodge')+
      coord_flip() + 
      scale_fill_grey()+
      theme_bw()+ ggtitle('Tau.e=0.95')
    
    g2=data[data$tau.e==0.98,] %>% ggplot(aes(x=as.factor(region),y = FN,fill =model))+
      xlab("region")+
      geom_bar(stat='identity',position = 'dodge')+
      coord_flip() + 
      scale_fill_grey()+
      theme_bw()+ ggtitle('Tau.e=0.98')
    
    
    g3=data[data$tau.e==0.99,] %>% ggplot(aes(x=as.factor(region),y = FN,fill =model))+
      xlab("region")+
      geom_bar(stat='identity',position = 'dodge')+
      coord_flip() + 
      scale_fill_grey()+
      theme_bw()+ ggtitle('Tau.e=0.99')
    
    
    g4=data[data$tau.e==0.995,] %>% ggplot(aes(x=as.factor(region),y = FN,fill =model))+
      xlab("region")+
      geom_bar(stat='identity',position = 'dodge')+
      coord_flip() + 
      scale_fill_grey()+
      theme_bw()+ ggtitle('Tau.e=0.995')
    grid.arrange(g1,g2,g3,g4, nrow=2, ncol=2)
  }
figure_FP_ftn=
  function(data){
    g1=data[data$tau.e==0.95,] %>% ggplot(aes(x=as.factor(region),y = FP,fill =model))+
      xlab("region")+
      geom_bar(stat='identity',position = 'dodge')+
      coord_flip() + 
      scale_fill_grey()+
      theme_bw()+ ggtitle('Tau.e=0.95')
    
    g2=data[data$tau.e==0.98,] %>% ggplot(aes(x=as.factor(region),y = FP,fill =model))+
      xlab("region")+
      geom_bar(stat='identity',position = 'dodge')+
      coord_flip() + 
      scale_fill_grey()+
      theme_bw()+ ggtitle('Tau.e=0.98')
    
    
    g3=data[data$tau.e==0.99,] %>% ggplot(aes(x=as.factor(region),y = FP,fill =model))+
      xlab("region")+
      geom_bar(stat='identity',position = 'dodge')+
      coord_flip() + 
      scale_fill_grey()+
      theme_bw()+ ggtitle('Tau.e=0.99')
    
    
    g4=data[data$tau.e==0.995,] %>% ggplot(aes(x=as.factor(region),y = FP,fill =model))+
      xlab("region")+
      geom_bar(stat='identity',position = 'dodge')+
      coord_flip() + 
      scale_fill_grey()+
      theme_bw()+ ggtitle('Tau.e=0.995')
    grid.arrange(g1,g2,g3,g4, nrow=2, ncol=2)
  }


figure_sen_ftn(ett)
figure_spe_ftn(ett)
figure_FN_ftn(ett)
figure_FP_ftn(ett)

png("./data/All_in_plot_perf_sen.png",width = 1000, height = 750)
figure_sen_ftn(ett)
dev.off()


png("./data/All_in_plot_perf_spe.png",width = 1000, height = 750)
figure_spe_ftn(ett)
dev.off()


png("./data/All_in_plot_perf_FN.png",width = 1000, height = 750)
figure_FN_ftn(ett)
dev.off()


png("./data/All_in_plot_perf_FP.png",width = 1000, height = 750)
figure_FP_ftn(ett)
dev.off()


##### Performance measurement plot ; mean comparaison#####
library(dplyr)
library(gridExtra)
summarise_df=ett %>% group_by(model,tau.e) %>% summarise_all(funs(mean(., na.rm = TRUE)))

summarise_df=subset(summarise_df,select = -c(region))

summarise_df



summarise_plot=
  function(data){
    g1=data %>% ggplot(aes(x=as.factor(tau.e),y = sen,group =model))+
      xlab("region")+
      geom_line(aes(linetype  = model))+
      geom_point()+
      #coord_flip() + 
      scale_fill_grey()+
      theme_bw()+ ggtitle('Sensitivity')
    
    g2=data %>% ggplot(aes(x=as.factor(tau.e),y = spe,group =model))+
      xlab("region")+
      geom_line(aes(linetype  = model))+
      geom_point()+
      #coord_flip() + 
      scale_fill_grey()+
      theme_bw()+ ggtitle('Specificity')
    
    
    g3=data %>% ggplot(aes(x=as.factor(tau.e),y = FP,group =model))+
      xlab("region")+
      geom_line(aes(linetype  = model))+
      geom_point()+
      #coord_flip() + 
      scale_fill_grey()+
      theme_bw()+ ggtitle('FN')
    
    
    g4=data %>% ggplot(aes(x=as.factor(tau.e),y = FN,group =model))+
      xlab("region")+
      geom_line(aes(linetype  = model))+
      geom_point()+
      #coord_flip() + 
      scale_fill_grey()+
      theme_bw()+ ggtitle('FP')
    grid.arrange(g1,g2,g3,g4, nrow=2, ncol=2)
  }

png("./data/Mean_in_plot_perf.png",width = 1000, height = 750)
summarise_plot(summarise_df)
dev.off()



##### Missing counting #####
##### The regions which have many missing values in TSL result. #####

TS_missing_df=data.frame()


count_missing = function(data){
  missing_vec=t(apply(is.na(data$Q3StageP),2,sum))
  return(missing_vec)
}

for (i in 1:25){
  datadata=bacigThreeStage_out_parallel[[region_name[i]]]$taulam0.95
  TS_missing_df = rbind(TS_missing_df,count_missing(datadata))
}

TS_missing_df$region = en_region_name


xtable(TS_missing_df)


TSL_missing_df=data.frame()


count_missing = function(data){
  missing_vec=t(apply(is.na(data$Q3StageP),2,sum))
  return(missing_vec)
}

for (i in 1:25){
  datadata=ThreeStage_CVLASSO_out_parallel[[region_name[i]]]$taulam0.95
  TSL_missing_df = rbind(TSL_missing_df,count_missing(datadata))
}

TSL_missing_df$region = en_region_name
TSL_missing_df


xtable(TSL_missing_df)


##### Checking missing values in professor's TS model #####
load("./basic_model.Rdata")
basic_three_output
basic_df = data.frame()
for (i in 1:25){
  outone = data.frame()
  print(region_name[i])
  one_region=basic_three_output[[i]]
  totalm=c(sum(is.na(one_region[[1]]$Q3StageP)),sum(is.na(one_region[[2]]$Q3StageP)),
    sum(is.na(one_region[[3]]$Q3StageP)),sum(is.na(one_region[[4]]$Q3StageP)))
  badm=c(sum((as.vector(is.na(one_region[[1]]$Q3StageP)))&(outlist[[region_name[i]]]$testy >76))/sum(outlist[[region_name[i]]]$testy >76),
         sum((as.vector(is.na(one_region[[2]]$Q3StageP)))&(outlist[[region_name[i]]]$testy >76))/sum(outlist[[region_name[i]]]$testy >76),
         sum((as.vector(is.na(one_region[[3]]$Q3StageP)))&(outlist[[region_name[i]]]$testy >76))/sum(outlist[[region_name[i]]]$testy >76),
         sum((as.vector(is.na(one_region[[4]]$Q3StageP)))&(outlist[[region_name[i]]]$testy >76))/sum(outlist[[region_name[i]]]$testy >76))
  outone=rbind(outone,totalm,badm)
  names(outone) = c("0.95","0.98","0.99","0.995")
  outone$check = c("Total_num_missings","Ratio_missing_in_target")
  outone$region = c(region_name[i])
  basic_df = rbind(basic_df,outone)
  print(outone)
  #print(validation_tool(true_y = as.vector(outlist[[region_name[i]]]$testy),pred_y = one_region[[1]]$Q3StageP))
  #print(validation_tool(true_y = as.vector(outlist[[region_name[i]]]$testy),pred_y = one_region[[2]]$Q3StageP))
  #print(validation_tool(true_y = as.vector(outlist[[region_name[i]]]$testy),pred_y = one_region[[3]]$Q3StageP))
  #print(validation_tool(true_y = as.vector(outlist[[region_name[i]]]$testy),pred_y = one_region[[4]]$Q3StageP))
}

basic_df


load("./data/0217ThreeStage_CVLASSO_out_parallel.R")



sum(is.na(one_region$taulam0.95$Q3StageP[,1])&as.vector(outlist[[region_name[i]]]$testy >76))/sum(outlist[[region_name[i]]]$testy >76)



apply(is.na(one_region$taulam0.95$Q3StageP),2,sum)


out_TSL = data.frame()
for (i in region_name){
  outone = data.frame()
  print(i)
  one_region=ThreeStage_CVLASSO_out_parallel[[i]]
  
  totalm = apply(is.na(one_region$taulam0.95$Q3StageP),2,sum)
  badm = c(sum(is.na(one_region$taulam0.95$Q3StageP[,1])&as.vector(outlist[[region_name[i]]]$testy >76))/sum(outlist[[i]]$testy >76),
           sum(is.na(one_region$taulam0.95$Q3StageP[,2])&as.vector(outlist[[region_name[i]]]$testy >76))/sum(outlist[[i]]$testy >76),
           sum(is.na(one_region$taulam0.95$Q3StageP[,3])&as.vector(outlist[[region_name[i]]]$testy >76))/sum(outlist[[i]]$testy >76),
           sum(is.na(one_region$taulam0.95$Q3StageP[,4])&as.vector(outlist[[region_name[i]]]$testy >76))/sum(outlist[[i]]$testy >76))
  outone=rbind(outone,totalm,badm)
  names(outone) = c("0.95","0.98","0.99","0.995")
  outone$check = c("Total_num_missings","Ratio_missing_in_target")
  outone$region = i
  out_TSL = rbind(out_TSL,outone)
  print(outone)
  print(validation_tool(true_y = as.vector(outlist[[i]]$testy),pred_y = one_region$taulam0.95$Q3StageP[,1]))
  print(validation_tool(true_y = as.vector(outlist[[i]]$testy),pred_y = one_region$taulam0.95$Q3StageP[,2]))
  print(validation_tool(true_y = as.vector(outlist[[i]]$testy),pred_y = one_region$taulam0.95$Q3StageP[,3]))
  print(validation_tool(true_y = as.vector(outlist[[i]]$testy),pred_y = one_region$taulam0.95$Q3StageP[,4]))
}


xtable::xtable(basic_df)
xtable::xtable(out_TSL)

##### Checking TSL vs TS (0226) #####

for (i in c(1,11,21)){
  print(region_name[i])
  latex_perf_ftn(outlist[[region_name[i]]],
                 bacigThreeStage_out_parallel[[region_name[i]]],
                 ThreeStage_CVLASSO_out_parallel[[region_name[i]]])
}

gd=basic_three_output[[11]]

gd
validation_tool(true_y = as.vector(outlist[[region_name[11]]]$testy),pred_y = as.vector(gd[[4]]$Q3StageP))


region_name[11]

basic_df_perf = data.frame()
for (i in 1:25){
  one_perf = data.frame()
  outone = data.frame()
  print(region_name[i])
  one_region=basic_three_output[[i]]
  #totalm=c(sum(is.na(one_region[[1]]$Q3StageP)),sum(is.na(one_region[[2]]$Q3StageP)),
  #         sum(is.na(one_region[[3]]$Q3StageP)),sum(is.na(one_region[[4]]$Q3StageP)))
  #badm=c(sum((as.vector(is.na(one_region[[1]]$Q3StageP)))&(outlist[[region_name[i]]]$testy >76))/sum(outlist[[region_name[i]]]$testy >76),
  #       sum((as.vector(is.na(one_region[[2]]$Q3StageP)))&(outlist[[region_name[i]]]$testy >76))/sum(outlist[[region_name[i]]]$testy >76),
  #       sum((as.vector(is.na(one_region[[3]]$Q3StageP)))&(outlist[[region_name[i]]]$testy >76))/sum(outlist[[region_name[i]]]$testy >76),
  #       sum((as.vector(is.na(one_region[[4]]$Q3StageP)))&(outlist[[region_name[i]]]$testy >76))/sum(outlist[[region_name[i]]]$testy >76))
  #outone=rbind(outone,totalm,badm)
  #names(outone) = c("0.95","0.98","0.99","0.995")
  #outone$check = c("Total_num_missings","Ratio_missing_in_target")
  #outone$region = c(region_name[i])
  #basic_df = rbind(basic_df,outone)
  #print(outone)
  perf95=validation_tool(true_y = as.vector(outlist[[region_name[i]]]$testy),pred_y = one_region[[1]]$Q3StageP)
  perf95_vec=c(perf95$senc,perf95$spec,perf95$FN,perf95$FP)
  
  perf98=validation_tool(true_y = as.vector(outlist[[region_name[i]]]$testy),pred_y = one_region[[2]]$Q3StageP)
  perf98_vec=c(perf98$senc,perf98$spec,perf98$FN,perf98$FP)
  
  perf99=validation_tool(true_y = as.vector(outlist[[region_name[i]]]$testy),pred_y = one_region[[3]]$Q3StageP)
  perf99_vec=c(perf99$senc,perf99$spec,perf99$FN,perf99$FP)
  
  perf995=validation_tool(true_y = as.vector(outlist[[region_name[i]]]$testy),pred_y = one_region[[4]]$Q3StageP)
  perf995_vec=c(perf995$senc,perf995$spec,perf995$FN,perf995$FP)
  
  one_perf=rbind(perf95_vec,perf98_vec,perf99_vec,perf995_vec)
  one_perf=data.frame(one_perf)
  row.names(one_perf) = NULL
  one_perf$tau.e = c(0.95,0.98,0.99,0.995)
  one_perf$region = region_name[i]
  print(one_perf)
  basic_df_perf = rbind(basic_df_perf,one_perf)
  #print(basic_df_perf)
}
basic_df_perf=data.frame(basic_df_perf)

names(basic_df_perf) =c("senc","spec","FN","FP","tau.e","region")

load("./data/0217ThreeStage_CVLASSO_out_parallel.R")



sum(is.na(one_region$taulam0.95$Q3StageP[,1])&as.vector(outlist[[region_name[i]]]$testy >76))/sum(outlist[[region_name[i]]]$testy >76)



apply(is.na(one_region$taulam0.95$Q3StageP),2,sum)


out_TSL = data.frame()

for (i in region_name){
  outone = data.frame()
  print(i)
  one_region=ThreeStage_CVLASSO_out_parallel[[i]]
  perf95=validation_tool(true_y = as.vector(outlist[[i]]$testy),pred_y = one_region$taulam0.95$Q3StageP[,1])
  perf95_vec=c(perf95$senc,perf95$spec,perf95$FN,perf95$FP)
  
  perf98=validation_tool(true_y = as.vector(outlist[[i]]$testy),pred_y = one_region$taulam0.95$Q3StageP[,2])
  perf98_vec=c(perf98$senc,perf98$spec,perf98$FN,perf98$FP)
  
  perf99=validation_tool(true_y = as.vector(outlist[[i]]$testy),pred_y = one_region$taulam0.95$Q3StageP[,3])
  perf99_vec=c(perf99$senc,perf99$spec,perf99$FN,perf99$FP)
  
  perf995=validation_tool(true_y = as.vector(outlist[[i]]$testy),pred_y = one_region$taulam0.95$Q3StageP[,4])
  perf995_vec=c(perf995$senc,perf995$spec,perf995$FN,perf995$FP)
  
  one_perf=rbind(perf95_vec,perf98_vec,perf99_vec,perf995_vec)
  one_perf=data.frame(one_perf)
  row.names(one_perf) = NULL
  one_perf$tau.e = c(0.95,0.98,0.99,0.995)
  one_perf$region = i
  print(one_perf)
  out_TSL = rbind(out_TSL,one_perf)
}


names(out_TSL) =c("senc","spec","FN","FP","tau.e","region")

sum(basic_df_perf$senc <= out_TSL$senc,na.rm = T)

xtable::xtable(basic_df)
xtable::xtable(out_TSL)





