
# Based on Density Estimation to predict the driving distance and departure time

library('GA')
load("02_Set2_Method2_DsEsti_OrigData.RData")
source("Set2_Fittness.R")

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
  rm(Dist_Pause_St)
  
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
  
  # Optimize Bandwidth using GA with training data
  # Parameter of GA
  Pop <- 20
  Generation <- 10
  Localsearch <- 20
  # Apply GA
  Resu <- ga(type ="real-valued",
             fitness = Set2_Fittness,
             min = rep(20,2*length(Dist_Lev.Train)), max = rep(300,2*length(Dist_Lev.Train)),
             population = gaControl("real-valued")$population,
             selection = gaControl("real-valued")$selection,
             crossover = gaControl("real-valued")$crossover,
             mutation = gaControl("real-valued")$mutation,
             popSize = Pop,
             pcrossover = 0.75,
             pmutation = 0.1,
             elitism = base::max(1, round(Pop*0.05)),
             updatePop = FALSE,
             postFitness = NULL,
             maxiter = Generation,
             run = Generation,
             maxFitness = 1.0,
             names = NULL,
             suggestions = rep(60,2*length(Dist_Lev.Train)),
             optim = TRUE,
             optimArgs = list(method = "L-BFGS-B",
                              poptim = 0.1,
                              pressel = 0.8,
                              control = list(fnscale = -1, maxit = Localsearch)),
             keepBest = TRUE,
             parallel = TRUE,
             monitor = if(interactive())
             { if(is.RStudio()) gaMonitor else gaMonitor2 }
             else FALSE,
             seed = NULL)
  # Extract the optimal BW in the results
  OptBW <- Resu@solution[nrow(Resu@solution),]
  # Apply the BW in the test data
  for(EvaNr in seq(1,nEva)) #Loop every test data of this actual vehicle
  {
    for(St in Dist_Lev.Train) #Loop every kind of distance
    {
      # Pause Density
      dfPause <- approxfun(density(Act_Veh.Train$Dist_Pause[which(Act_Veh.Train$Distance == St)],OptBW[which(Dist_Lev.Train == St)]))
      
      # Pause to the last such drive (with the same distance)
      LastFahrtNr <- max(which(Act_Veh.Train$Distance == St))
      TDiff <- (Act_Veh.Eva[EvaNr,]$time_day - Act_Veh.Train$time_day[LastFahrtNr])*24*60
      
      # Density/Probability, how possible is it to drive such a distance now
      PDist[which(Dist_Lev.Train == St)] <- dfPause(TDiff) #??BAYES??????????????
      PWD_Dep[which(Dist_Lev.Train == St)] <- length(which((Act_Veh.Train$WD_Dep == Act_Veh.Eva[EvaNr,]$WD_Dep) & (Act_Veh.Train$Distance == St) ))/length(which(Act_Veh.Train$Distance == St))
      
      # possible departure time, in Min
      dfMin <- approxfun(density(Act_Veh.Train$Min[which(Act_Veh.Train$Distance == St)],OptBW[length(Dist_Lev.Train)+which(Dist_Lev.Train == St)]))
      PMin[which(Dist_Lev.Train == St)] <- dfMin(Act_Veh.Eva[EvaNr,]$Min)
      
      # following sentence is still needed, because some Distance are very little in the TrainData,
      if(is.na(PDist[which(Dist_Lev.Train == St)] )) { PDist[which(Dist_Lev.Train == St)]  <- 0}
      if(is.na(PMin[which(Dist_Lev.Train == St)] )) { PMin[which(Dist_Lev.Train == St)]  <- 0}
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
  # valuable predictions are those with relative error smaller than 0.1
  Eva.Fit_Exp <- length(which(abs(Eva.Err_Dist_Exp_Rel)<0.1)) / length(Eva.Est_Dist_Exp)
  Eva.Fit_Max <- length(which(abs(Eva.Err_Dist_Max_Rel)<0.1)) / length(Eva.Err_Dist_Max_Abs)
  # summarize the evaluation results and save in the list EvaList
  EvaAuto <- data.frame(Eva.Fit_Exp,Eva.Fit_Max, nEva, nTrain = nrow(Act_Veh.Train),Eva.Act_Dist,Eva.Est_Dist_Exp,Eva.Err_Dist_Exp_Abs,Eva.Err_Dist_Exp_Rel,Eva.Est_Dist_Max,Eva.Err_Dist_Max_Abs,Eva.Err_Dist_Max_Rel)
  EvaList <- c(EvaList,list(EvaAuto))  
}

save.image(file = "02_Set2_Method2_DensityGA.RData")
rm(list = ls())
