packages <- c("caret", "EXRQ", "rts", "forecast", "doSNOW", "tcltk", "foreach","doParallel")
if(length(setdiff(packages, rownames(installed.packages())))!=0){
  install.packages(setdiff(packages, rownames(installed.packages())))
}else{
  print("All installed")
}

library(caret)
library(EXRQ)
library(rts)
library(forecast)
library(doSNOW)
library(tcltk)
library(foreach)
library(doParallel)

######################################################################
###################  four_hour_avg function  #########################
######################################################################

# This function averages the 4-hour values at each variable in 'Data_setup' function.

four_hour_avg<-function(x){
  
  y<-t<-vector()
  for(i in 1: floor(length(x)/4)  ){
    y[i]=mean( x[ c( ((i-1)*4+1 ) : (i*4) )  ] , na.rm=T )
    t[i]=as.character( index(x[ c( ((i-1)*4+1 ) : (i*4) )  ][1] ) )
  }
  TSData<-data.frame(y,row.names=t)
  TSData<-as.xts(TSData)
  return(TSData)
}

######################################################################
###################  Data_setup function  ############################
######################################################################

Data_setup=function(data_full,location){
  
  
  print(location)
  print(paste("Is",location,"in data?"))
  print( location %in% colnames(data_full$Airpollution_data$PM25))
  
  TSData<-data.frame(data_full$Airpollution_data$PM25[,location],row.names=data_full$Airpollution_data$PM25$측정일시)
  
  ########### Extracting points (after 2015-01-01) and Interpolating missings in PM2.5 ########## 
  
  rm_index=c( 1: (which(rownames(TSData)=='2015-01-01 00:00:00' ))) 
  TSData2=data.frame( TSData[- rm_index , ] , row.names=rownames(TSData)[-rm_index])
  
  TSData2[which(is.na(TSData2)==T), ] =approx(TSData2, xout=which(is.na(TSData2)==T))$y
  TSData=TSData2
  
  
  ###########  Checking the extreme values of PM2.5 ###########  
  
  PM25<-as.xts(TSData)
  PM25=four_hour_avg(PM25) 
  PM_01=ifelse(PM25<76, 0, 1) 
  table(PM_01)
  
  #############################################################  
  ###########  Interpolating missing values and Averaging the 4-hour values at each variable ########### 
  
  data1=cbind(PM25, PM_01)
  
  for(varr in c("Meteorological_data","Airpollution_data")){
    if(varr=="Meteorological_data"){var_index=c("precip","temp","hum","win","windir")}
    if(varr=="Airpollution_data"){var_index=c( "CO","NO2","O3","PM10","SO2")}
    
    for( i in var_index){
      TSData<-data.frame(data_full[[varr]][[i]][,location],row.names=data_full$Airpollution_data$PM25$측정일시)
      TSData2=data.frame( TSData[- rm_index , ] , row.names=rownames(TSData)[-rm_index])
      missing_t=which(is.na(TSData2)==T)
      
      for(tt in missing_t){
        TSData2[tt,] =  mean( as.matrix(data_full[[varr]][[i]][tt,-1])[1,], na.rm=TRUE)
      }
      na_index2=which(is.na(TSData2)==T)
      
      TSData=TSData2
      temp<-as.xts((TSData))
      temp=four_hour_avg(temp) 
      data1=cbind(data1, temp)
    }
  }
  
  colnames(data1)= c("PM25","PM_01","precip","temp","hum","win","windir","CO","NO2","O3","PM10","SO2" )
  
  
  # If there are more than 50 missings in a variable, remove the variable.
  
  rm_col<-vector()
  for(k in 1:ncol(data1)){
    if( length(which(is.na(data1[,k]==T) ))  >50   ){ 
      rm_col=c(rm_col, k)
    }
  }
  
  if(length(rm_col)>0){
    data1=data1[,-rm_col]}
  
  # Remove points in which at least one variable has NA in spite of the preceding procedure.
  
  for(k in 1:ncol(data1)){
    if( length(which(is.na(data1[,k]==T) ))  >0){ 
      data1=data1[-which(is.na(data1[,k]==T) ) , ]
    }
  }
  
  full_data=data1
  time_index= index(full_data)
  
  ########### Extracting spring season from every year ###########
  
  temp<-list()
  for(k in 1:ncol(full_data)){
    temp[[k]]=
      c(
        window(xts(full_data[,k], order.by = time_index), start=strptime("2015-03-01 00:00:00", '%Y-%m-%d %H:%M:%S') ,end=strptime("2015-06-01 00:00:00", '%Y-%m-%d %H:%M:%S')),
        window(xts(full_data[,k], order.by = time_index), start=strptime("2016-03-01 00:00:00", '%Y-%m-%d %H:%M:%S') ,end=strptime("2016-06-01 00:00:00", '%Y-%m-%d %H:%M:%S')),
        window(xts(full_data[,k], order.by = time_index), start=strptime("2017-03-01 00:00:00", '%Y-%m-%d %H:%M:%S') ,end=strptime("2017-06-01 00:00:00", '%Y-%m-%d %H:%M:%S')),
        window(xts(full_data[,k], order.by = time_index), start=strptime("2018-03-01 00:00:00", '%Y-%m-%d %H:%M:%S') ,end=strptime("2018-06-01 00:00:00", '%Y-%m-%d %H:%M:%S')),
        window(xts(full_data[,k], order.by = time_index), start=strptime("2019-03-01 00:00:00", '%Y-%m-%d %H:%M:%S') ,end=strptime("2019-06-01 00:00:00", '%Y-%m-%d %H:%M:%S')),
        window(xts(full_data[,k], order.by = time_index), start=strptime("2020-03-01 00:00:00", '%Y-%m-%d %H:%M:%S') ,end=strptime("2020-05-30 00:00:00", '%Y-%m-%d %H:%M:%S'))
        
        
      )
  }
  #############################################################  
  
  ########### Train / Test split ###########
  # To evaluate performance of the model, the data is splited into two parts, train and test.
  # Samples(index <= 1100); Train 
  # The others; Test
  
  fulldata_spring<-NA
  
  for(k in 1:ncol(full_data)){
    fulldata_spring=cbind(fulldata_spring, temp[[k]])
  }
  fulldata_spring=fulldata_spring[,-1]
  time_index_spring=index(temp[[1]])
  

  test_start_day=1100 # Start point of test set
  part_end=nrow(fulldata_spring) # End point of test set
  
  print(table(fulldata_spring[ test_start_day:part_end,]$PM_01))
  pred_fullday=c(test_start_day:part_end) # indices from start point to end point
  
  #############################################################  
  ########### 'Verbosity' for parallel compututation  ########### 
  
  nreps<-length(pred_fullday)
  pb<-txtProgressBar(1,nreps,style=3)
  
  progress<-function(n){
    setTxtProgressBar(pb,n)
  } 
  
  opts<-list(progress=progress)
  
  # By doing this, we can check the progression of parallel computation.
  #############################################################  
  
  numCores <- detectCores() -1
  myCluster <- makeCluster(numCores)

  registerDoSNOW(myCluster)
  
  ########### Parallel computation for test data  ########### 
  # 1-lagged variables are also used to estimate the extreme quantiles of PM2.5 (Section 2. II)
  # For real time application, future observations at each variable are gradually estimated by 'forecast' function.
  # T+1 time is estimated using true data until T time. T+2 time is analogous to T+1 time.
  
  
  x_hat = foreach(mm= 1:length(pred_fullday),.options.snow=opts,.combine = rbind,.packages = c('xts','forecast'))%dopar%{
    pred_date=pred_fullday[mm]
    ModelData_nomissing=fulldata_spring [1:pred_date,]
    
    h1=1 # The number of lagging
    
    x_hat_temp=vector()
    
    for(l in 3:ncol(ModelData_nomissing)){

      x_hat_temp=cbind( x_hat_temp, forecast(ets( ModelData_nomissing[,l]), h=h1)$mean[1])   
    }
    for(k in c(1:ncol(ModelData_nomissing))[-2]){
      # Combine lagged variables with non-lagged variables
      x_hat_temp=cbind( x_hat_temp, forecast(ets(lag(ModelData_nomissing,1)[,k]), h=h1)$mean[1])   
    }
    return(x_hat_temp)
  } 

  stopCluster(myCluster)
  
  ########### Train data ########### 
  ModelData_nomissing=fulldata_spring [1:pred_fullday[1],]
  # Combine lagged variables with non-lagged variables 
  x=cbind(ModelData_nomissing[,-c(1:2)], lag(ModelData_nomissing,1)[,-2])
  
  x=x[-1,] # Omitting first sample, as missings in the sample.
  #############################################################  
  
  
  outdata = list()
  outdata[["trainy"]] = ModelData_nomissing$PM25[-1] # 
  outdata[["trainx"]] = x
  outdata[["testx"]] = x_hat[-1,]
  outdata[["testy"]] = fulldata_spring$PM25[(test_start_day+1):part_end]
  
  return(outdata)
}




######################################################################
###################  Not used  #######################################
######################################################################

Data_setup_for_Deep=function(data_full,location){
  
  
  print(location)
  print(paste("Is",location,"in data?"))
  print( location %in% colnames(data_full$Airpollution_data$PM25))
  
  TSData<-data.frame(data_full$Airpollution_data$PM25[,location],row.names=data_full$Airpollution_data$PM25$측정일시)
  
  ########### Extracting points (after 2015-01-01) and Interpolating missings in PM2.5 ########## 
  
  rm_index=c( 1: (which(rownames(TSData)=='2015-01-01 00:00:00' ))) 
  TSData2=data.frame( TSData[- rm_index , ] , row.names=rownames(TSData)[-rm_index])
  
  TSData2[which(is.na(TSData2)==T), ] =approx(TSData2, xout=which(is.na(TSData2)==T))$y
  TSData=TSData2
  
  
  ###########  Checking the extreme values of PM2.5 ###########  
  
  PM25<-as.xts(TSData)
  PM25=four_hour_avg(PM25) 
  PM_01=ifelse(PM25<76, 0, 1) 
  table(PM_01)
  
  #############################################################  
  ###########  Interpolating missing values and Averaging the 4-hour values at each variable ########### 
  
  data1=cbind(PM25, PM_01)
  
  for(varr in c("Meteorological_data","Airpollution_data")){
    if(varr=="Meteorological_data"){var_index=c("precip","temp","hum","win","windir")}
    if(varr=="Airpollution_data"){var_index=c( "CO","NO2","O3","PM10","SO2")}
    
    for( i in var_index){
      TSData<-data.frame(data_full[[varr]][[i]][,location],row.names=data_full$Airpollution_data$PM25$측정일시)
      TSData2=data.frame( TSData[- rm_index , ] , row.names=rownames(TSData)[-rm_index])
      missing_t=which(is.na(TSData2)==T)
      
      for(tt in missing_t){
        TSData2[tt,] =  mean( as.matrix(data_full[[varr]][[i]][tt,-1])[1,], na.rm=TRUE)
      }
      na_index2=which(is.na(TSData2)==T)
      
      TSData=TSData2
      temp<-as.xts((TSData))
      temp=four_hour_avg(temp) 
      data1=cbind(data1, temp)
    }
  }
  
  colnames(data1)= c("PM25","PM_01","precip","temp","hum","win","windir","CO","NO2","O3","PM10","SO2" )
  
  
  # If there are more than 50 missings in a variable, remove the variable.
  
  rm_col<-vector()
  for(k in 1:ncol(data1)){
    if( length(which(is.na(data1[,k]==T) ))  >50   ){ 
      rm_col=c(rm_col, k)
    }
  }
  
  if(length(rm_col)>0){
    data1=data1[,-rm_col]}
  
  # Remove points in which at least one variable has NA in spite of the preceding procedure.
  
  for(k in 1:ncol(data1)){
    if( length(which(is.na(data1[,k]==T) ))  >0){ 
      data1=data1[-which(is.na(data1[,k]==T) ) , ]
    }
  }
  
  full_data=data1
  time_index= index(full_data)
  
  ########### Extracting spring season from every year ###########
  
  temp<-list()
  for(k in 1:ncol(full_data)){
    temp[[k]]=
      c(
        window(xts(full_data[,k], order.by = time_index), start=strptime("2015-01-01 00:00:00", '%Y-%m-%d %H:%M:%S') ,end=strptime("2015-06-01 00:00:00", '%Y-%m-%d %H:%M:%S')),
        window(xts(full_data[,k], order.by = time_index), start=strptime("2016-01-01 00:00:00", '%Y-%m-%d %H:%M:%S') ,end=strptime("2016-06-01 00:00:00", '%Y-%m-%d %H:%M:%S')),
        window(xts(full_data[,k], order.by = time_index), start=strptime("2017-01-01 00:00:00", '%Y-%m-%d %H:%M:%S') ,end=strptime("2017-06-01 00:00:00", '%Y-%m-%d %H:%M:%S')),
        window(xts(full_data[,k], order.by = time_index), start=strptime("2018-01-01 00:00:00", '%Y-%m-%d %H:%M:%S') ,end=strptime("2018-06-01 00:00:00", '%Y-%m-%d %H:%M:%S')),
        window(xts(full_data[,k], order.by = time_index), start=strptime("2019-01-01 00:00:00", '%Y-%m-%d %H:%M:%S') ,end=strptime("2019-06-01 00:00:00", '%Y-%m-%d %H:%M:%S')),
        window(xts(full_data[,k], order.by = time_index), start=strptime("2020-01-01 00:00:00", '%Y-%m-%d %H:%M:%S') ,end=strptime("2020-05-30 00:00:00", '%Y-%m-%d %H:%M:%S'))
        
        
      )
  }
  
  return(temp)
}