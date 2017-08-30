Set2_Fittness <- function(BW)
{
  for(EvaNr in seq(1,nEva))
  {
    for(St in Dist_Lev.Train) #Loop distance level
    {
      # Pause Density
      dfPause <- approxfun(density(Act_Veh.Train$Dist_Pause[which(Act_Veh.Train$Distance == St)],BW[which(Dist_Lev.Train == St)]))
      
      # Pause to the last such drive (with the same distance)
      LastDriveNr <- max(which(Act_Veh.Train$Distance == St))
      TDiff <- (Act_Veh.Eva$time_day[EvaNr] - Act_Veh.Train$time_day[LastDriveNr]) *24 *60
      
      # Density/Probability, how possible is it to drive such a distance now
      PDist[which(Dist_Lev.Train == St)] <- dfPause(TDiff)
      PWD_Dep[which(Dist_Lev.Train == St)] <- length(which((Act_Veh.Train$WD_Dep == Act_Veh.Eva$WD_Dep[EvaNr]) & (Act_Veh.Train$Distance == St) ))/length(which(Act_Veh.Train$Distance == St))
      
      # possible departure time, in Min
      dfMin <- approxfun(density(Act_Veh.Train$Min[which(Act_Veh.Train$Distance == St)],BW[length(Dist_Lev.Train)+which(Dist_Lev.Train == St)]))
      PMin[which(Dist_Lev.Train == St)] <- dfMin(Act_Veh.Eva$Min[EvaNr])
      
      #following sentence is still needed, because some Distance are very little in the TrainData,
      if(is.na(PDist[which(Dist_Lev.Train == St)])) { PDist[which(Dist_Lev.Train == St)] <- 0}
      if(is.na(PMin[which(Dist_Lev.Train == St)])) { PMin[which(Dist_Lev.Train == St)] <- 0}
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
    
    Eva.Est_Dist_Exp[EvaNr] <- sum(PDist*Dist_Lev.Train)
    Eva.Err_Dist_Exp_Abs[EvaNr] <- Act_Veh.Eva$Distance[EvaNr] - Eva.Est_Dist_Exp[EvaNr]
    Eva.Err_Dist_Exp_Rel[EvaNr] <- Eva.Err_Dist_Exp_Abs[EvaNr]/Act_Veh.Eva$Distance[EvaNr]
  }
  Set2_Fittness <- length(Eva.Err_Dist_Exp_Rel[which(abs(Eva.Err_Dist_Exp_Rel)<0.1)])/length(Eva.Err_Dist_Exp_Rel)
}

