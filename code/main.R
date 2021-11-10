setwd(getwd())
source("./functions.R",encoding = "utf-8")
source("./dataPreprocessing.R",encoding = "utf-8")

data=readRDS("./0729data.rds")
region_name=names(data$Meteorological_data$precip)[names(data$Meteorological_data$precip)!="일시"]



######################################################################
###########################  Date setup  #############################
######################################################################

# Please refer to Section 2 and 4 #
outlist =list()
for (j in region_name[1]){
  one_region=Data_setup(data =data,location = j)
  outlist[[j]] =one_region
}
#save(outlist,file = "./0119All_regions.R")
load("./0119All_regions.Rdata")



######################################################################
##############  Three-Stage model with LASSO Ensemble  ###############
######################################################################

# Please refer to Section 3 and 4 #
tau.e_Vec =c(seq(0.95,0.999,0.005),0.999)
tau.lam_Vec = c(0.95)

ThreeStage_CVLASSO_parallel=function(data,tau.es=tau.e_Vec,tau.lams= tau.lam_Vec){
  one_list = list()
  Full_features = names(data$trainx)
  cat(paste("All the features : "))
  cat(paste(Full_features[c(1:length(Full_features))]),'\n')
  cat(paste("The features selected : "))
  cat(paste(Full_features[c(1:4,6:10)]),'\n')
  
  
  Features =Full_features[c(1:4,6:10)]
  for (tau.lam_fixed in tau.lams){
    one_model=ThreeStage_lasso_cv(y = data$trainy,x = as.matrix(data$trainx[,Features]),
                                  xstar = data$testx[,c(1:4,6:10)],
                                  tau.e = tau.es,
                                  grid.lam=seq(-1.5,1.5,by=0.1),grid.k=seq(10,110,by = 20), tau.lam=tau.lam_fixed,tol = 1e-1)
    
    one_list[[paste0("taulam",as.character(tau.lam_fixed))]]=one_model
  }
  return(one_list)
}



ThreeStage_CVLASSO_out_parallel = list()

for (j in region_name){
  start_time <- Sys.time()
  cat(j,'\n')
  one_region=ThreeStage_CVLASSO_parallel(outlist[[j]])
  ThreeStage_CVLASSO_out_parallel[[j]] =one_region
  end_time <- Sys.time()
  print(end_time - start_time)
}

#save(ThreeStage_CVLASSO_out_parallel,file = "./0624ThreeStage_CVLASSO_out_hard_voting.RData")
load("./0624ThreeStage_CVLASSO_out_hard_voting.RData")

######################################################################
############################  Results  ###############################
######################################################################
TSL_out = list()
for (j in region_name){
  org=outlist[[j]]
  true_y = org$testy
  TSL_perf=ThreeStage_CVLASSO_out_parallel[[j]]
  TSL_perf=TSL_perf$taulam0.95
  
  
  for (i in 1:dim(TSL_perf$Q3StageP)[2]){
    TSL_perf$Q3StageP[,i] = na.approx(TSL_perf$Q3StageP[,i]) 
  }
  
  collection = list()
  for (k in 1:dim(TSL_perf$Q3StageP)[2] ){
    indiv = validation_tool(as.vector(true_y)[7:dim(TSL_perf$Q3StageP)[1]],TSL_perf$Q3StageP[7:dim(TSL_perf$Q3StageP)[1],k])
  
    collection[[paste0("tau",as.character(tau.e_Vec[k]))]] = indiv
  }
  
  
  TSL_perf_en=validation_tool(as.vector(true_y)[7:dim(TSL_perf$Q3StageP)[1]],apply(TSL_perf$Q3StageP[7:dim(TSL_perf$Q3StageP)[1],],FUN = mean,MARGIN = 1))
  
  
  en_vec=round(c(TSL_perf_en$PPV,TSL_perf_en$NPV,TSL_perf_en$senc,TSL_perf_en$spec,TSL_perf_en$FN,TSL_perf_en$FP,TSL_perf_en$F2score),3)
  #vote_vec=c(TSL_perf_vote$senc,TSL_perf_vote$spec,TSL_perf_vote$FN,TSL_perf_vote$FP,TSL_perf_vote$F2score)
  
  
  All_TSL_one_region = data.frame()
  for (o in 1:dim(TSL_perf$Q3StageP)[2]){
    indiv=collection[[paste0("tau",as.character(tau.e_Vec[o]))]]
    All_TSL_one_region=rbind(All_TSL_one_region,round(c(indiv$PPV,indiv$NPV,indiv$senc,indiv$spec,indiv$FN,indiv$FP,indiv$F2score),3))
  }
  
  All_TSL_one_region = rbind(All_TSL_one_region,en_vec
   
  )
  
  
  
  All_TSL_one_region= cbind(All_TSL_one_region,c(format(tau.e_Vec,3),"mean"
   
  ))
  names(All_TSL_one_region) = c("PPV","NPV","sen","spe","FN","FP","F2","tau.e")
  TSL_out[[j]] = All_TSL_one_region
}

TSL_out
