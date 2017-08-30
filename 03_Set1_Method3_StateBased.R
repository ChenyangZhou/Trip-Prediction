library(e1071)
library(np)
library(proxy)
library(dbscan)
source("PauseCalc.R")
source("Set1_Circle_DistFun.R")
load("00_Set1_Data_AfterProcessing.RData")

# pr_DB$delete_entry('Set1_circle_DistFun')
pr_DB$set_entry(FUN = Set1_Circle_DistFun, names = c("Set1_Circle_DistFun"))

#DataSize von jedem Auto
DataSize <- rep(0,nrow(Info))
for (Veh_Nr in 1:nrow(Info)){
  DataSize[Veh_Nr] <- nrow(Drive[which(Drive$id == Info$id[Veh_Nr]),])
}

EvaList <- data.frame(FIRST = 0)

# For loop to apply the method to every vehicle
for (Veh_Nr in 1:nrow(Info)){
  # for (Veh_Nr in 207:nrow(Info)){  # for Debug
  Act_Veh <- Drive[which(Drive$id == Info$id[Veh_Nr]),]
  if(nrow(Act_Veh) < 20) next
  #CLot(Act_Veh$distance_to_company) # for Debug
  
  # Daten Preprocess
  #Pause calculation
  Last_Arr <- c(0,Act_Veh$Clc_Arr)
  This_Dep <- c(Act_Veh$Clc_Dep,0)
  Pause <- This_Dep-Last_Arr # Pause before every drive
  Pause[Pause < 0] <- Pause [Pause <0] + 1440 # correction for the pause which cover two days
  # pause before every drive
  Pause_Before <- Pause[-length(Pause)] # delete the last one
  Pause_Before[1] <- mean(Pause_Before[2:length(Pause_Before)]) # pause before the first drive is recircled with average value
  # pause after every drive
  Pause_After <- Pause[-1]
  Pause_After[nrow(Pause_After)] <- mean(Pause_After[1:length(Pause_After)]) # pause after the last drive is recircled with average value
  Act_Veh <- data.frame(Act_Veh, Pause_Before, Pause_After)
  rm(Pause, Pause_After, Pause_Before, This_Dep, Last_Arr)
  
  # If the average spd is smaller than 60 kmh AND duration is smaller than 5 AND change of Distance_to_Comp is smaller than 1 km, then delete this profil
  Pre_Samp <- which((Act_Veh$AveSpd>60) & (Act_Veh$Duration<5))
  Pre_SampDist <- Act_Veh$distance_to_company[Pre_Samp] - Act_Veh$distance_to_company[Pre_Samp -1]
  if(length(which(abs(Pre_SampDist) < 1))>0) 
  {
    Act_Veh <- Act_Veh[-Pre_Samp[which(abs(Pre_SampDist) < 1)],] 
    PauseCalc()
  }
  # delete the data of Pause_After 0
  if(length(Act_Veh$distance_to_company[which(Act_Veh$Pause_After==0)])>0) 
  {
    Act_Veh <- Act_Veh[-which(Act_Veh$Pause_After == 0),]
    PauseCalc()
  }
  rm(Pre_Samp, Pre_SampDist)
  
  nEva <- min(100, round(0.2 * nrow(Act_Veh)))
  Act_Veh.Train <- Act_Veh[1:(nrow(Act_Veh) - nEva-1),]
  Act_Veh.TrainwithAct  <- Act_Veh[1:(nrow(Act_Veh) - nEva),] # with actual circle, the calculation of the following departure and arrive etc. will be easier
  Act_Veh.Eva <- Act_Veh[(nrow(Act_Veh) - nEva + 1):(nrow(Act_Veh)),]
  
  # circle according to cluster
  Drive_Nr <- seq(1,nrow(Act_Veh.Train))
  CL_Dist <- dist(data.frame(Drive_Nr), method = "Set1_circle_DistFun")
  Cluster_Tree = hclust(CL_Dist, method = "complete")
  
  #CLot(Cluster_Tree)
  #CLot(Act_Veh.Train$distance_to_company, col = groups)
  #*********************#
  # nCL = 14
  # groups <- cutree(Cluster_Tree, k=nCL)
  # 
  # i<-1
  # nCL <- rep(0,length(seq(0.1,1.0,0.01)))
  # for(height in seq(0.1,2,0.01)){
  #   groups <- cutree(Cluster_Tree, h=height)
  #   nCL[i] <- length(levels(as.factor(groups)))
  #   i<- i+1
  # }
  # CLot(seq(0.1,2,0.01),nCL)
  
  groups <- cutree(Cluster_Tree, h=0.5) # cut the tree at height 0.5 (km)
  nCL <- length(levels(as.factor(groups))) # number of the circles
  Upper_Lim <- min(20, round(nrow(Act_Veh.Train)/5))
  if(nCL>Upper_Lim & nCL>10)
  {  # only when orignal circles are more than 10, the upper limit have effect
    groups <- cutree(Cluster_Tree, k=Upper_Lim) # if met, cut the tree according to the number of clusters
    nCL <- Upper_Lim
  }
  
  # res <- optics(CL_Dist, 0.5, minPts = 3, 0.5,search = "dist", bucketSize = 10, sCLitRule = "suggest", approx = 0)
  # res_Cut <- optics_cut(res, 0.5)
  
  groups <- as.factor(groups)
  # calculate the average of distance_to_company in each cluster
  Ave_Dist_of_Cluster <- rep(0, nCL)
  for(i in 1:nCL) {Ave_Dist_of_Cluster[i] <- mean(Act_Veh.Train$distance_to_company[which(groups == i)])}
  
  # combine the groups with training dataset
  Act_Veh.Train <- data.frame(Act_Veh.Train,groups)
  
  #Nächste 
  Follow <- groups[-1] 
  Follow <- as.factor(c(Follow, 1)) # This 1 is only used to make vectors in same size and will be updated
  Follow_Dist <- Act_Veh.TrainwithAct$distance
  Follow_Dist <- Follow_Dist[-1]
  Follow_Clc_Dep <- Act_Veh.TrainwithAct$Clc_Dep
  Follow_Clc_Dep <- Follow_Clc_Dep[-1]
  Follow_Clc_Arr <- Act_Veh.TrainwithAct$Clc_Arr
  Follow_Clc_Arr <- Follow_Clc_Arr[-1]
  Follow_Duration <- Act_Veh.TrainwithAct$Duration
  Follow_Duration <- Follow_Duration[-1]
  
  # CL_Before, find the circle where each drive event starts
  CL_Before <- as.factor(c(1,groups))
  CL_Before <- CL_Before[-length(CL_Before)]
  
  rm(Act_Veh.TrainwithAct)
  
  Act_Veh.Train <- data.frame(Act_Veh.Train,CL_Before,Follow, Follow_Dist,Follow_Clc_Dep,Follow_Clc_Arr,Follow_Duration)
  #Act_Veh.Train <- Act_Veh.Train[-1,] #die erste auslöschen
  #Act_Veh <-Act_Veh[-1,] #die erste auslöschen
  
  rm(Follow,Follow_Dist,Follow_Clc_Dep,Follow_Clc_Arr,Follow_Duration)
  
  # vector to save the evaluation results of each test data
  Eva.Follow <- rep(FALSE, nrow(Act_Veh.Eva))
  Eva.Followtrad <- rep(FALSE, nrow(Act_Veh.Eva))
  Eva.Follow_DistAbsErr <- rep(0.0,nrow(Act_Veh.Eva))
  Eva.Follow_DistRelatErr <- rep(0.0,nrow(Act_Veh.Eva))
  Eva.Follow_Clc_DepErr <- rep(0.0,nrow(Act_Veh.Eva))
  Eva.Follow_Clc_ArrErr <- rep(0.0,nrow(Act_Veh.Eva))
  Eva.Follow_DurationErr <- rep(0.0,nrow(Act_Veh.Eva))
  Eva.IsNewFollow <- rep(FALSE, nrow(Act_Veh.Eva))
  Error_Typ <-  rep(0, nrow(Act_Veh.Eva))
  # type 0: method doesn't work
  # type 1: new destination
  # type 2: new transition, but destination is old
  # type 3: new transition, new destination
  
  # only in first run, save the original train data, cluster, for Debug
  # Act_Veh.TrainOrg <- Act_Veh.Train
  # Ave_Dist_of_ClusterOrg <- Ave_Dist_of_Cluster
  # Act_Veh.Train <- Act_Veh.TrainOrg # for Debug
  # Ave_Dist_of_Cluster <- Ave_Dist_of_ClusterOrg
  
  # Estimation for every test data  
  for(i in 1:nrow(Act_Veh.Eva)){
    # for(i in 1:2){ # for Debug
    # add new Trainingsdata after every test
    if(i>1){
      NewTrain <- data.frame(Act_Veh[nrow(Act_Veh.Train)+1,],groups = which.min(abs(Ave_Dist_of_Cluster - Act_Drive$distance_to_company)),CL_Before = Act_Veh.Train[nrow(Act_Veh.Train),]$groups, Follow = which.min(abs(Ave_Dist_of_Cluster - Act_Veh.Eva[i-1,]$distance_to_company)),Follow_Dist = Act_Veh.Eva[i-1,]$distance, Follow_Clc_Dep = Act_Veh.Eva[i-1,]$Clc_Dep, Follow_Clc_Arr = Act_Veh.Eva[i-1,]$Clc_Arr, Follow_Duration = Act_Veh.Eva[i-1,]$Duration)
      Act_Veh.Train <- rbind(Act_Veh.Train,NewTrain)
    }
    
    #Actual drive (just ended)
    Act_Drive <- Act_Veh[nrow(Act_Veh.Train) + 1,]
    Act_Drive <- data.frame(Act_Drive, CL_Before =  Act_Veh.Train[nrow(Act_Veh.Train),]$groups)
    
    #check, if here is a new circle
    if(min(abs(Ave_Dist_of_Cluster - Act_Drive$distance_to_company)) < 0.5)
    {
      Act_DriveCL <- which.min(abs(Ave_Dist_of_Cluster - Act_Drive$distance_to_company))
      Act_Circle <- Act_Veh.Train[which(Act_Veh.Train$groups == Act_DriveCL),]
    }else{ #min(abs(Ave_Dist_of_Cluster - Act_Drive$distance_to_company)) > 0.5,---> a new circle
      Error_Typ[i] <- 1
      # save this new circle in Ave_Dist_of_Cluster
      Ave_Dist_of_Cluster <- append(Ave_Dist_of_Cluster,Act_Drive$distance_to_company)
      # neue Ort
      Act_DriveCL <- which.min(abs(Ave_Dist_of_Cluster - Act_Drive$distance_to_company))
      # update the Follow in the new Trainingsdata
      Act_Veh.Train$groups <- factor(Act_Veh.Train$groups, levels = c(levels(Act_Veh.Train$groups), as.character(Act_DriveCL)))
      Act_Veh.Train$CL_Before <- factor(Act_Veh.Train$CL_Before, levels = c(levels(Act_Veh.Train$CL_Before), as.character(Act_DriveCL)))
      Act_Veh.Train$Follow <- factor(Act_Veh.Train$Follow, levels = c(levels(Act_Veh.Train$Follow), as.character(Act_DriveCL)))
      Act_Veh.Train$Follow[nrow(Act_Veh.Train)] <- Act_DriveCL
      # Prediction with all Trainingsdata
      Act_Circle <- Act_Veh.Train
      # Update the levels of Factor CL_Before
      Act_Drive$CL_Before <- factor(Act_Drive$CL_Before,levels = c(levels(Act_Drive$CL_Before), as.character(Act_DriveCL)))
    }
    
    #--------------------------------------------------------------------#
    #*************Prüfen, ob es eine neue Transition ist*****************#
    #--------------------------------------------------------------------#
    
    
    #determine the Follow ~ Clc_Arr, (Weekdays...)
    if(nrow(Act_Circle) > 1)
    {
      # CLot(as.numeric(Act_Circle$Follow),Act_Circle$Clc_Arr)
      # abline(h = Act_Drive$Clc_Arr)
      # CLot(Act_Veh.Train$Follow,Act_Veh.Train$Clc_Arr)
      
      # predict the following circle using SVM
      if(length(levels(as.factor(as.numeric(Act_Circle$Follow)))) > 1)
      {
        svm.model <- svm(Follow ~ Clc_Arr + WD_Dep + CL_Before, data = Act_Circle, cost = 100, gamma = 1, na.action = na.exclude)
        svm.pred <- predict(svm.model, Act_Drive, na.action = na.exclude)
        Follow <- svm.pred
      }else{
        Follow <- Act_Circle$Follow[1] # if only one following circle in the training data, then choose it
      }
      
      # select the data with this transition (Start and end circles are the same)
      TrainData_Follow <-  Act_Circle[which(Act_Circle$Follow == Follow),]
      
      # Determine the distance, which is dependent on the following place, place is determined by circle and arrival clocktime
      Follow_Dist <- mean(TrainData_Follow$Follow_Dist)  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      #Pause(also Clc_Dep of next drive): nonparametrische Regression---  Pause_After ~ Clc_Arr, WD_Dep
      if(nrow(TrainData_Follow) > 2)
      {
        bw.Pause_After <- npregbw(formula = Pause_After ~ WD_Dep + Clc_Arr, regtype = "ll", bwmethod = "cv.aic", data = TrainData_Follow)
        model.Pause_After <- npreg(bws = bw.Pause_After)
        Pause_After.Pr <- predict(model.Pause_After, data = TrainData_Follow, newdata = Act_Drive)
        Follow_Clc_Dep <- Act_Drive$Clc_Arr + Pause_After.Pr
        
        # or linear
        #         fit <- lm( Pause_After ~  WD_Dep + Clc_Arr, data= TrainData_Follow)
        #         summary(fit)
        #         new <- data.frame(Clc_Arr = Act_Drive$Clc_Arr, WD_Dep = Act_Drive$WD_Dep)
        #         Pause_After.Pr <- predict(fit,new)
        #         Follow_Clc_Dep <- Act_Drive$Clc_Arr + Pause_After.Pr
        
        
        # Follow_Duration bzw. Abfahrtzeit: nonparametrische Regression---  Follow_Duration ~ Follow, WD_Dep
        bw.Follow_Duration <- npregbw(formula = Follow_Duration ~ WD_Dep + Follow_Clc_Dep, regtype = "ll", bwmethod = "cv.aic", data = TrainData_Follow)
        model.Follow_Duration <- npreg(bws = bw.Follow_Duration)
        Follow_Duration <- predict(model.Follow_Duration, data = TrainData_Follow, newdata = Act_Drive)
        Follow_Clc_Arr <- Follow_Clc_Dep + Follow_Duration
        # Fehler bei der CL_Beforesage des Durations kann nicht sich verstärken, da PauseCL_Beforesage ist von der Clc_Arr abhängig
      }else{ #diese Transition wurde nur einmal gefahren
        Follow_Clc_Dep <- mean(TrainData_Follow$Follow_Clc_Dep)
        Follow_Duration <- mean(TrainData_Follow$Follow_Duration)
        Follow_Clc_Arr <- Follow_Clc_Dep + Follow_Duration
      }
      #Act_Drive <- data.frame(Act_Drive,Act_DriveCL,Follow,Follow_Dist,Follow_Clc_Dep)
    }else{ #This circle were only once visited, density esti, nonpara.regression are impossible. so just copy the last such drive
      Follow <- Act_Circle$Follow
      Follow_Dist <- Act_Circle$Follow_Dist
      if(Act_Drive$Clc_Dep < Act_Circle$Follow_Clc_Dep)
      {
        Follow_Clc_Dep <- Act_Circle$Follow_Clc_Dep
        Follow_Clc_Arr <- Act_Circle$Follow_Clc_Arr
        Follow_Duration <- Act_Circle$Follow_Duration
      }else{
        Follow_Clc_Dep <- Act_Drive$Clc_Dep + Act_Circle$Pause_After
        Follow_Clc_Arr <- Follow_Clc_Dep + Act_Circle$Follow_Duration  
        Follow_Duration <- Act_Circle$Follow_Duration
      }
    }
    
    #whether the follow is a new circle
    Eva.IsNewFollow <- min(abs(Ave_Dist_of_Cluster - Act_Veh.Eva[i,]$distance_to_company))>0.5
    if(Eva.IsNewFollow)
    {    
      Eva.Follow[i] <- FALSE
      Error_Typ[i] <- 3
    }else{
      Eva.Follow[i] <- which.min(abs(Ave_Dist_of_Cluster - Act_Veh.Eva[i,]$distance_to_company)) == Follow
    }
    
    if((sum(which.min(abs(Ave_Dist_of_Cluster - Act_Veh.Eva[i,]$distance_to_company)) == as.numeric(Act_Circle$Follow)) == 0) & Error_Typ[i] != 3) Error_Typ[i] <- 2 
    #  Eva.Followtrad[i] <- which.min(abs(Ave_Dist_of_Cluster - Act_Veh.Eva[i,]$distance_to_company)) == Follow
    Eva.Follow_DistRelatErr[i] <- (Act_Veh.Eva[i,]$distance - Follow_Dist)/Act_Veh.Eva[i,]$distance
    Eva.Follow_DistAbsErr[i] <- abs(Act_Veh.Eva[i,]$distance - Follow_Dist)
    Eva.Follow_Clc_DepErr[i] <- Act_Veh.Eva[i,]$Clc_Dep - Follow_Clc_Dep
    if(Eva.Follow_Clc_DepErr[i] < -720)  Eva.Follow_Clc_DepErr[i] <-  Eva.Follow_Clc_DepErr[i]+1440
    Eva.Follow_Clc_ArrErr[i] <- Act_Veh.Eva[i,]$Clc_Arr - Follow_Clc_Arr
    Eva.Follow_DurationErr[i] <- Act_Veh.Eva[i,]$Duration - Follow_Duration
  }
  EvaSum <- data.frame(NACE = Info$nace_division[Veh_Nr],Daten = nrow(Act_Veh), nEva, nCL, Eva.Follow,Eva.Follow_DistAbsErr, Eva.Follow_DistRelatErr,Eva.Follow_DurationErr,Eva.Follow_Clc_DepErr,Eva.Follow_Clc_ArrErr,Error_Typ)
  EvaList <- c(EvaList,list(EvaSum))
}

# Extract the evaluation results from EvaList
NACE <- rep(0,length(EvaList)-1)
Daten <- rep(0,length(EvaList)-1)
nEva <- rep(0,length(EvaList)-1)
ncircle <- rep(0,length(EvaList)-1)
FollowT <- rep(0,length(EvaList)-1)
Dist_MAE <- rep(0,length(EvaList)-1)
Dist_MAPE <- rep(0,length(EvaList)-1)
Dura_MAE <- rep(0,length(EvaList)-1)
ClcDep_MAE <- rep(0,length(EvaList)-1)
ClcArr_MAE <- rep(0,length(EvaList)-1)
FollowTWithOutAlgErr <- rep(0,length(EvaList)-1)

for(i in 1:(length(EvaList)-1)){
  NACE[i] <- EvaList[[i+1]][[1]][1]
  Daten[i] <- EvaList[[i+1]][[2]][1]
  nEva[i] <- EvaList[[i+1]][[3]][1]
  ncircle[i] <- EvaList[[i+1]][[4]][1]
  FollowT[i] <- length(which(EvaList[[i+1]][[5]] == TRUE))/nEva[i]
  FollowTWithOutAlgErr[i] <- 1 - length(which((EvaList[[i+1]][[5]] == FALSE) & (EvaList[[i+1]][[10]] == 0)))/nEva[i]
  Dist_MAE[i] <- mean(abs(EvaList[[i+1]][[6]])[-which(abs(EvaList[[i+1]][[6]]) > quantile(abs(EvaList[[i+1]][[6]]),prob = 0.75))])
  Dist_MAPE[i] <- mean(abs(EvaList[[i+1]][[7]])[-which(abs(EvaList[[i+1]][[7]]) > quantile(abs(EvaList[[i+1]][[7]]),prob = 0.75))])
  Dura_MAE[i] <- mean(abs(EvaList[[i+1]][[8]])[-which(abs(EvaList[[i+1]][[8]]) > quantile(abs(EvaList[[i+1]][[8]]),prob = 0.75))])
  ClcDep_MAE[i] <- mean(abs(EvaList[[i+1]][[9]])[-which(abs(EvaList[[i+1]][[9]]) > quantile(abs(EvaList[[i+1]][[9]]),prob = 0.75))])
}

plot(Dist_MAE[-which(Dist_MAE > quantile(Dist_MAE,probs = 0.75, na.rm = TRUE))]) # remove the outlier in the result
boxplot(Dist_MAE[-which(Dist_MAE > quantile(Dist_MAE,probs = 0.75, na.rm = TRUE))]) # remove the outlier in the result

plot(Dist_MAPE[-which(Dist_MAPE > quantile(Dist_MAPE,probs = 0.75, na.rm = TRUE))]) # remove the outlier in the result
boxplot(Dist_MAPE[-which(Dist_MAPE > quantile(Dist_MAPE,probs = 0.75, na.rm = TRUE))]) # remove the outlier in the result
mean(Dist_MAPE[-which(Dist_MAPE > quantile(Dist_MAPE,probs = 0.75, na.rm = TRUE))],na.rm = TRUE)
min(Dist_MAPE[-which(Dist_MAPE > quantile(Dist_MAPE,probs = 0.75, na.rm = TRUE))],na.rm = TRUE)

plot(Dura_MAE[-which(Dura_MAE > quantile(Dura_MAE,probs = 0.75, na.rm = TRUE))])
boxplot(Dura_MAE[-which(Dura_MAE > quantile(Dura_MAE,probs = 0.75, na.rm = TRUE))])

plot(ClcDep_MAE[-which(ClcDep_MAE > quantile(ClcDep_MAE,probs = 0.75, na.rm = TRUE))]) # remove the outlier in the result
boxplot(ClcDep_MAE[-which(ClcDep_MAE > quantile(ClcDep_MAE,probs = 0.75, na.rm = TRUE))]) # remove the outlier in the result

plot(ClcArr_MAE[-which(ClcArr_MAE > quantile(ClcArr_MAE,probs = 0.75, na.rm = TRUE))])
boxplot(ClcArr_MAE[-which(ClcArr_MAE > quantile(ClcArr_MAE,probs = 0.75, na.rm = TRUE))])

save.image(file = "03_Set1_Method3_Statebased.RData")
rm(list = ls())
