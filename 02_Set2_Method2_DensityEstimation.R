# Based on Density Estimation to predict the driving distance and departure timeboxb
load("02_Set2_Method2_DsEsti_OrigData.RData")

Drive <- DriveOrg
Number_Veh <- as.numeric(levels(as.factor(Drive$id)))

DataSize <- rep(0,length(Number_Veh))
for (Veh_Nr in 1:length(Number_Veh))
{
  DataSize[Veh_Nr] <- nrow(Drive[which(Drive$id == Drive$id[Veh_Nr]),])
}
#
EvaList <- data.frame(FIRST = 0)

#Loop for every vehicle
for(Veh_Nr in Number_Veh)
{
  # preprocess, calculate the Min, Weekdays
  Act_Veh <- Drive[which(Drive$id == Veh_Nr),]    
  Day <- floor(Act_Veh$time_day)
  Min <- (Act_Veh$time_day - Day) * 24 * 60
  WD_Dep <- Day%%7
  Act_Veh <- data.frame(Act_Veh,WD_Dep,Min)
  rm(WD_Dep,Min,Day)
  
  # Divide in trainging and test data
  nEva <- min(100, round(0.2 * nrow(Act_Veh)))
  Act_Veh.Train <- Act_Veh[1:(nrow(Act_Veh) - nEva),]
  Act_Veh.Eva <- Act_Veh[(nrow(Act_Veh) - nEva + 1):(nrow(Act_Veh)),]
  Act_Veh.Train <- data.frame(Act_Veh.Train,Dist_Pause = rep(0.0,nrow(Act_Veh.Train)))
  Dist_Lev.Train <- as.numeric(levels(as.factor(Act_Veh.Train$Distance)))
  
  # Distance Pause calculation, distance pause means the pause between two driving profiles with the same distance
  for(St in Dist_Lev.Train)
  {
    Act_Veh_St <- Act_Veh.Train[which(Act_Veh.Train$Distance == St),]
    if(nrow(Act_Veh_St)> 1) # if such distance were driven more than once, then the pause can be calculated
    {
      Dist_Pause_St <- diff(Act_Veh_St$time_day) * 24 * 60
      Dist_Pause_St <- append(Dist_Pause_St,mean(Dist_Pause_St))
      Act_Veh.Train$Dist_Pause[which(Act_Veh.Train$Distance == St)] <- Dist_Pause_St
    }else{ # if such distance only appeared once, then the pause is replaced with the average value of last kind of distance
      Act_Veh.Train$Dist_Pause[which(Act_Veh.Train$Distance == St)] <- LastAve          
    }
    LastAve <- mean(Dist_Pause_St)
  }
  
  # vector to save the evaluation results of each test data
  Eva.Act_Dist <- rep(0.0,nrow(Act_Veh.Eva))
  Eva.Est_Dist_Exp <- rep(0.0,nrow(Act_Veh.Eva))
  Eva.Err_Dist_Exp_Abs <- rep(0.0,nrow(Act_Veh.Eva))
  Eva.Err_Dist_Exp_Rel <- rep(0.0,nrow(Act_Veh.Eva))
  
  Eva.Est_Dist_Max <- rep(0.0,nrow(Act_Veh.Eva))
  Eva.Err_Dist_Max_Abs <- rep(0.0,nrow(Act_Veh.Eva))
  Eva.Err_Dist_Max_Rel <- rep(0.0,nrow(Act_Veh.Eva))
  
  Eva.Fit_Exp <- 0.0
  Eva.Fit_Max <- 0.0
  
  PDist <- rep(0.0, length(Dist_Lev.Train))
  PWD_Dep <- rep(0.0, length(Dist_Lev.Train))
  PMin <- rep(0.0, length(Dist_Lev.Train))
  
  for(EvaNr in seq(1,nEva)) #Loop every test data of this actual vehicle
  {
    for(St in seq(1,length(Dist_Lev.Train))) #Loop every kind of distance
    {
      if(length(Act_Veh.Train$Dist_Pause[which(Act_Veh.Train$Distance == St)]) < 2)
      {
        PMin[which(Dist_Lev.Train == St)] <- 0
        PWD_Dep[which(Dist_Lev.Train == St)] <- 0
        PDist[which(Dist_Lev.Train == St)] <- 0
        next
      }else{
        # Pause Density
        #dfPause <- approxfun(density(Act_Veh.Train$Dist_Pause[which(Act_Veh.Train$Distance == St)],OptBW[which(Dist_Lev.Train == St)]))
        dfPause <- approxfun(density(Act_Veh.Train$Dist_Pause[which(Act_Veh.Train$Distance == St)]))
        
        # Pause to the last such drive (with the same distance)
        LastFahrtNr <- max(which(Act_Veh.Train$Distance == St))
        TDiff <- (Act_Veh.Eva[EvaNr,]$time_day - Act_Veh.Train$time_day[LastFahrtNr])*24*60
        
        # Density/Probability, how possible is it to drive such a distance now
        PDist[which(Dist_Lev.Train == St)] <- dfPause(TDiff) #??BAYES??????????????
        PWD_Dep[which(Dist_Lev.Train == St)] <- length(which((Act_Veh.Train$WD_Dep == Act_Veh.Eva[EvaNr,]$WD_Dep) & (Act_Veh.Train$Distance == St) ))/length(which(Act_Veh.Train$Distance == St))
        
        # possible departure time, in Min
        #dfMin <- approxfun(density(Act_Veh.Train$Min[which(Act_Veh.Train$Distance == St)],OptBW[length(Dist_Lev.Train)+which(Dist_Lev.Train == St)]))
        dfMin <- approxfun(density(Act_Veh.Train$Min[which(Act_Veh.Train$Distance == St)]))
        PMin[which(Dist_Lev.Train == St)] <- dfMin(Act_Veh.Eva[EvaNr,]$Min)
        
        # following sentence is still needed, because some Distance are very little in the TrainData,
        if(is.na(PDist[which(Dist_Lev.Train == St)] )) { PDist[which(Dist_Lev.Train == St)]  <- 0}
        if(is.na(PMin[which(Dist_Lev.Train == St)] )) { PMin[which(Dist_Lev.Train == St)]  <- 0}
      }
    }
    # Normalisation of PDist
    if(sum(PDist) != 0) PDist <- PDist/sum(PDist)
    # Normalisation of PWD_Dep
    if(sum(PWD_Dep) != 0) PWD_Dep <- PWD_Dep/sum(PWD_Dep)
    # Normalisation of PMin
    if(sum(PMin) != 0) PMin <- PMin/sum(PMin)
    
    # Probability
    PDist <- PDist * PWD_Dep* PMin 
    if(sum(PDist) != 0) PDist <- PDist/sum(PDist)
    
    # save the actual distance
    Eva.Act_Dist[EvaNr] <- Act_Veh.Eva[EvaNr,]$Distance
    
    # Expectation of the distance and residual
    Eva.Est_Dist_Exp[EvaNr] <- sum(PDist*Dist_Lev.Train)
    Eva.Err_Dist_Exp_Abs[EvaNr] <- Act_Veh.Eva[EvaNr,]$Distance - Eva.Est_Dist_Exp[EvaNr]
    Eva.Err_Dist_Exp_Rel[EvaNr] <- Eva.Err_Dist_Exp_Abs[EvaNr]/Act_Veh.Eva[EvaNr,]$Distance
    
    # Distance with the max. Probability and Residual
    Eva.Est_Dist_Max[EvaNr]  <- Dist_Lev.Train[which.max(PDist)]
    Eva.Err_Dist_Max_Abs[EvaNr] <- Act_Veh.Eva[EvaNr,]$Distance - Eva.Est_Dist_Max[EvaNr]
    Eva.Err_Dist_Max_Rel[EvaNr] <- Eva.Err_Dist_Max_Abs[EvaNr]/Act_Veh.Eva[EvaNr,]$Distance
  }
  # valuable predictions are those with relative error smaller than 0.2
  Eva.Fit_Exp <- length(which(abs(Eva.Err_Dist_Exp_Rel)<0.2)) / length(Eva.Est_Dist_Exp)
  Eva.Fit_Max <- length(which(abs(Eva.Err_Dist_Max_Rel)<0.2)) / length(Eva.Err_Dist_Max_Abs)
  # summarize the evaluation results and save in the list EvaList
  EvaAuto <- data.frame(Eva.Fit_Exp,Eva.Fit_Max, nEva, nTrain = nrow(Act_Veh.Train),Eva.Act_Dist,Eva.Est_Dist_Exp,Eva.Err_Dist_Exp_Abs,Eva.Err_Dist_Exp_Rel,Eva.Est_Dist_Max,Eva.Err_Dist_Max_Abs,Eva.Err_Dist_Max_Rel)
  EvaList <- c(EvaList,list(EvaAuto))  
}

# Extract the evaluation results from EvaList
nEva <- rep(0,length(EvaList)-1) # how many evaluation data there are in every vehicle
nTrain <- rep(0,length(EvaList)-1) # how many training data there are in every vehicle
Err_Dist_Exp_MAE <- rep(0,length(EvaList)-1) # Mean absolute Error of the distance estimation using the expectation value
Err_Dist_Exp_MAPE <- rep(0,length(EvaList)-1) # Mean absolute percentage Errorof the distance estimation using the expectation value
Err_Dist_Max_MAE <- rep(0,length(EvaList)-1) #Mean absolute Error of the distance estimation , using the distance with max. probability
Err_Dist_Max_MAPE <- rep(0,length(EvaList)-1)# Mean absolute percentage Error, using the distance with max. probability

for(i in 1:(length(EvaList)-1)){
  nEva[i] <- EvaList[[i+1]][[3]][1]
  nTrain[i] <- EvaList[[i+1]][[4]][1]
  Err_Dist_Exp_MAE[i] <- mean(abs(EvaList[[i+1]][[7]])[-which(abs(EvaList[[i+1]][[7]]) > quantile(abs(EvaList[[i+1]][[7]]),prob = 0.75))])
  Err_Dist_Exp_MAPE[i] <- mean(abs(EvaList[[i+1]][[8]])[-which(abs(EvaList[[i+1]][[8]]) > quantile(abs(EvaList[[i+1]][[8]]),prob = 0.75))])
  Err_Dist_Max_MAE[i] <- mean(abs(EvaList[[i+1]][[10]])[-which(abs(EvaList[[i+1]][[10]]) > quantile(abs(EvaList[[i+1]][[10]]),prob = 0.75))])
  Err_Dist_Max_MAPE[i] <- mean(abs(EvaList[[i+1]][[11]])[-which(abs(EvaList[[i+1]][[11]]) > quantile(abs(EvaList[[i+1]][[11]]),prob = 0.75))])
}

plot(Err_Dist_Exp_MAE[-which(Err_Dist_Exp_MAE > quantile(Err_Dist_Exp_MAE,probs = 0.75, na.rm = TRUE))]) # remove the outlier in the result
boxplot(Err_Dist_Exp_MAE[-which(Err_Dist_Exp_MAE > quantile(Err_Dist_Exp_MAE,probs = 0.75, na.rm = TRUE))]) # remove the outlier in the result
mean(Err_Dist_Exp_MAE[-which(Err_Dist_Exp_MAE > quantile(Err_Dist_Exp_MAE,probs = 0.75, na.rm = TRUE))],na.rm = TRUE)
min(Err_Dist_Exp_MAE[-which(Err_Dist_Exp_MAE > quantile(Err_Dist_Exp_MAE,probs = 0.75, na.rm = TRUE))],na.rm = TRUE)

plot(Err_Dist_Exp_MAPE[-which(Err_Dist_Exp_MAPE > quantile(Err_Dist_Exp_MAPE,probs = 0.75, na.rm = TRUE))])
boxplot(Err_Dist_Exp_MAPE[-which(Err_Dist_Exp_MAPE > quantile(Err_Dist_Exp_MAPE,probs = 0.75, na.rm = TRUE))])

plot(Err_Dist_Max_MAE[-which(Err_Dist_Max_MAE > quantile(Err_Dist_Max_MAE,probs = 0.75, na.rm = TRUE))]) # remove the outlier in the result
boxplot(Err_Dist_Max_MAE[-which(Err_Dist_Max_MAE > quantile(Err_Dist_Max_MAE,probs = 0.75, na.rm = TRUE))]) # remove the outlier in the result

plot(Err_Dist_Max_MAPE[-which(Err_Dist_Max_MAPE > quantile(Err_Dist_Max_MAPE,probs = 0.75, na.rm = TRUE))])
boxplot(Err_Dist_Max_MAPE[-which(Err_Dist_Max_MAPE > quantile(Err_Dist_Max_MAPE,probs = 0.75, na.rm = TRUE))])

save.image(file = "02_Set2_Method2_Density.RData")
