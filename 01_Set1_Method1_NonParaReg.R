# Nonparameterical Regression was applied to the driving profiles of every vehicles
# For more details about nonparametrical regression, please refer to the document
library(np)
load("00_Set1_Data_AfterProcessing.RData")

EvaList <- data.frame(FIRST = 0)
# For-Loop to apply the regression
for (Veh_Nr in 1:nrow(Info)){
  Act_Veh <- Drive[which(Drive$id == Info$id[Veh_Nr]),] #get the driving profiles of this actuel vehicle
  # calculate the pause before and after every drive
  Last_Arr <- c(0,Act_Veh$Clc_Arr)
  This_Dep <- c(Act_Veh$Clc_Dep,0)
  Pause <- This_Dep-Last_Arr # Pause before every drive
  Pause[Pause < 0] <- Pause [Pause <0] + 1440 # correction for the pause which cover two days
    # pause before every drive
  Pause_Before <- Pause[-length(Pause)] # delete the last one
  Pause_Before[1] <- mean(Pause_Before[2:length(Pause_Before)]) # pause before the first drive is replaced with average value
    # pause after every drive
  Pause_After <- Pause[-1]
  Pause_After[nrow(Pause_After)] <- mean(Pause_After[1:length(Pause_After)]) # pause after the last drive is replaced with average value
  # Departure time in 0, 0.5, 1, 1.5, 2.0,... Unit: Hour
  Dep_HourHalf <- as.numeric(Act_Veh$depthour)
  Dep_HourHalf[which(Act_Veh$deptminute > 30)] <- Dep_HourHalf[which(Act_Veh$deptminute > 30)] + 0.5
  Dep_HourHalf <- as.factor(Dep_HourHalf)
  
  Act_Veh <- data.frame(Act_Veh, Pause_Before, Pause_After, Dep_HourHalf)
  Act_Veh$depthour <- as.factor(Act_Veh$depthour)
  rm(Pause, Pause_After, Pause_Before, This_Dep, Last_Arr, Dep_HourHalf)
  
  # Act_Veh <- Act_Veh[-which(Act_Veh$AveSpd>60 & Act_Veh$Duration<=3),] # speed filter according to the situation
  
  # If datasize is smaller than 10, next
  if (nrow(Act_Veh) <= 10) next
  
  # nonparametric regression
  
  nEva <- min(100, round(0.2 * nrow(Act_Veh))) # 20% as test data
  Act_Veh.train <- Act_Veh[1:(nrow(Act_Veh) - nEva),]
  Act_Veh.eval <- Act_Veh[(nrow(Act_Veh) - nEva + 1):(nrow(Act_Veh)),]
  
  # Distance prediction
  bw.Dist <- npregbw(formula = distance ~ Clc_Dep + distance_to_company + Pause_Before + Dep_HourHalf + WD_Dep , regtype = "ll", bwmethod = "cv.aic", data = Act_Veh.train)
  model.Dist <- npreg(bws = bw.Dist)
  Dist.Pr <- predict(model.Dist, data = Act_Veh.train, newdata = Act_Veh.eval)
    # Error calculation
  Err_Abs_Dist <- Act_Veh.eval$distance - Dist.Pr
  Err_Rel_Dist <- (Act_Veh.eval$distance - Dist.Pr)/Act_Veh.eval$distance
  
  # Duration prediction
  bw.Duration <- npregbw(formula = Duration ~ Clc_Dep + distance_to_company + Pause_Before + Dep_HourHalf + WD_Dep , regtype = "ll", bwmethod = "cv.aic", data = Act_Veh.train)
  model.Duration <- npreg(bws = bw.Duration)
  Duration.Pr <- predict(model.Duration, data = Act_Veh.train, newdata = Act_Veh.eval)
    # Error calculation
  Err_Abs_Duration <- Act_Veh.eval$Duration - Duration.Pr
  Err_Rel_Duration <- (Act_Veh.eval$Duration - Duration.Pr)/Act_Veh.eval$Duration
  
  # Pause prediction
  bw.Pause <- npregbw(formula = Pause_After ~ Clc_Dep + distance_to_company + Pause_Before + Dep_HourHalf + WD_Dep , regtype = "ll", bwmethod = "cv.aic", data = Act_Veh.train)
  model.Pause <- npreg(bws = bw.Pause)
  Pause.Pr <- predict(model.Pause, data = Act_Veh.train, newdata = Act_Veh.eval)
    # Error calculation
  Err_Abs_Pause <- Act_Veh.eval$Pause_After - Pause.Pr
  Err_Rel_Pause <- (Act_Veh.eval$Pause_After - Pause.Pr)/Act_Veh.eval$Pause_After
  
  # Error summary
  Eva_Act_Veh <- data.frame(NACE = Info$nace_division[Veh_Nr],Data = nrow(Act_Veh), nEva, Err_Abs_Dist, Err_Rel_Dist, Err_Abs_Duration, Err_Abs_Pause)
  EvaList <- c(EvaList,list(Eva_Act_Veh)) # save in the EvaList
  
}

NACE <- rep(0,length(EvaList)-1) # NACE Division
nData<- rep(0,length(EvaList)-1) # how many data there are in every vehicle
nEva <- rep(0,length(EvaList)-1) # how many evaluation data there are in every vehicle
Err_Dist_MAE <- rep(0,length(EvaList)-1) #Mean absolute Error of the distance estimation
Err_Dist_MAPE <- rep(0,length(EvaList)-1)# Mean absolute percentage Error of distance estimation
Err_Dura_MAE <- rep(0,length(EvaList)-1) #Mean absolute Error of the duration estimation, unit: min^2
Err_Paus_MAE <- rep(0,length(EvaList)-1) #Mean absolute Error of the pause estimation, unit: min^2

for(i in 1:(length(EvaList)-1)){
  NACE[i] <- EvaList[[i+1]][[1]][1]
  nData[i] <- EvaList[[i+1]][[2]][1]
  nEva[i] <- EvaList[[i+1]][[3]][1]
  Err_Dist_MAE[i] <- mean(abs(EvaList[[i+1]][[4]])[-which(abs(EvaList[[i+1]][[4]]) > quantile(abs(EvaList[[i+1]][[4]]),prob = 0.75))])
  Err_Dist_MAPE[i] <- mean(abs(EvaList[[i+1]][[5]])[-which(abs(EvaList[[i+1]][[5]]) > quantile(abs(EvaList[[i+1]][[5]]),prob = 0.75))])
  Err_Dura_MAE[i] <- mean(abs(EvaList[[i+1]][[6]])[-which(abs(EvaList[[i+1]][[6]]) > quantile(abs(EvaList[[i+1]][[6]]),prob = 0.75))])
  Err_Paus_MAE[i] <- mean(abs(EvaList[[i+1]][[7]])[-which(abs(EvaList[[i+1]][[7]]) > quantile(abs(EvaList[[i+1]][[7]]),prob = 0.75))])
}


plot(Err_Dist_MAPE[-which(Err_Dist_MAPE > quantile(Err_Dist_MAPE,probs = 0.75, na.rm = TRUE))]) # remove the outlier in the result
boxplot(Err_Dist_MAPE[-which(Err_Dist_MAPE > quantile(Err_Dist_MAPE,probs = 0.75, na.rm = TRUE))]) # remove the outlier in the result

plot(Err_Dist_MAE[-which(Err_Dist_MAE > quantile(Err_Dist_MAE,probs = 0.75, na.rm = TRUE))])
boxplot(Err_Dist_MAE[-which(Err_Dist_MAE > quantile(Err_Dist_MAE,probs = 0.75, na.rm = TRUE))])

plot(Err_Dura_MAE[-which(Err_Dura_MAE > quantile(Err_Dura_MAE,probs = 0.75, na.rm = TRUE))])
boxplot(Err_Dura_MAE[-which(Err_Dura_MAE > quantile(Err_Dura_MAE,probs = 0.75, na.rm = TRUE))])

plot(Err_Paus_MAE[-which(Err_Paus_MAE > quantile(Err_Paus_MAE,probs = 0.75, na.rm = TRUE))])
boxplot(Err_Paus_MAE[-which(Err_Paus_MAE > quantile(Err_Paus_MAE,probs = 0.75, na.rm = TRUE))])

save.image(file = "01_Set1_Method1_nonparaReg.RData")
