PauseCalc <- function()
{
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
  
  Act_Veh$Pause_Before <- Pause_Before
  Act_Veh$Pause_After <- Pause_After
}