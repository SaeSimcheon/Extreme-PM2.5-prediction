source("./code/research_model.R",encoding = "utf-8")
source("./code/Submission_data_preprocess_codecheck.R",encoding = "utf-8")
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
