# This Program is used to preprocess the orginal data from the csv. file.

DriveOrg <- read.csv("REM2030_v2015.csv", header = TRUE, sep="," ) # read the csv file of driving profiles
InfoOrg <- read.csv("REM2030_v2015_car_info.csv",header = TRUE , sep=",") # read the csv file of informations of vehicles
DriveOrg <- na.omit(DriveOrg) # remove the empty rows
InfoOrg <- na.omit(InfoOrg)

# Original data are saved as Backup. The follow operations are all for variables Drive and Info
Drive <- DriveOrg
Info <- InfoOrg

# Abbreviation of the Attributes in the vehicle information
  # city size in Info
Info$city_size <- as.character(Info$city_size)
Info$city_size[which(Info$city_size == "over 100000")] <- "L" # large
Info$city_size[which(Info$city_size == "more than 100000")] <- "L" # large
Info$city_size[which(Info$city_size == "20000 to 100000")] <- "M" # middle
Info$city_size[which(Info$city_size == "under 20000")] <- "S" # small
Info$city_size[which(Info$city_size == "up to 20000")] <- "S" # small
  # company size in Info
Info$company_size <- as.character(Info$company_size)
Info$company_size[which(Info$company_size == "under 10")] <- "XS"     # extra small
Info$company_size[which(Info$company_size == "kleiner 10")] <- "XS"
Info$company_size[which(Info$company_size == "10 to 50")] <- "S"      # small
Info$company_size[which(Info$company_size == "51 to 250")] <- "M"     # middle
Info$company_size[which(Info$company_size == "52 bis 250")] <- "M"  
Info$company_size[which(Info$company_size == "53 bis 250")] <- "M"  
Info$company_size[which(Info$company_size == "54 bis 250")] <- "M"  
Info$company_size[which(Info$company_size == "56 bis 250")] <- "M" 
Info$company_size[which(Info$company_size == "251 to 1000")] <- "L"   # large
Info$company_size[which(Info$company_size == "1001 to 5000")] <- "XL" # extra large
Info$company_size[which(Info$company_size == "over 5000")] <- "XXL"   # xx large
Info$company_size[which(Info$company_size == "")] <- "NG" # not given
  # vehicle utilization
Info$vehicle_utilization <- as.character(Info$vehicle_utilization)
Info$vehicle_utilization[which(Info$vehicle_utilization == "")] <- "NG"              # not given
Info$vehicle_utilization[which(Info$vehicle_utilization == "fleet vehicle")] <- "FV" # fleet vehicle
Info$vehicle_utilization[which(Info$vehicle_utilization == "company car")] <- "CC"   # company car
  # Parking spot
Info$parking_spot <- as.character(Info$parking_spot)
Info$parking_spot[which(Info$parking_spot == "own parking spot on company's estate")] <- "Own" # own parking spot
Info$parking_spot[which(Info$parking_spot == "differing parking spots on company's estate")] <- "ODi" # differing parking spot
Info$parking_spot[which(Info$parking_spot == "no own parking spot on company's estate")] <- "Nop" # No own parking spot
Info$parking_spot[which(Info$parking_spot == "")] <- "NG" # not given
  # number of user
Info$number_of_users <- as.character(Info$number_of_users)
Info$number_of_users[which(Info$number_of_users == "one user" || Info$number_of_users == "only one user")] <- "One" # only one user
Info$number_of_users[which(Info$number_of_users == "several user")] <- "Mul" # several users
Info$number_of_users[which(Info$number_of_users == "")] <- "NG" # not given
  # federal state as character
Info$federal_state <- as.character(Info$federal_state)

# Columen 10 "Comment" has only "None" and is deleted
Info <- Info[,-10]

# Drive Duration
Time_Dep <- ISOdate(Drive$deptyear,Drive$deptmonth, Drive$deptday, Drive$depthour, Drive$deptminute) #extract departure time
Time_Arr <- ISOdate(Drive$arryear,Drive$arrmonth, Drive$arrday, Drive$arrhour, Drive$arrminute)
Duration <- as.numeric(difftime(Time_Arr, Time_Dep, units = "mins")) # Duration in Minute

# Weekday
WD_Dep <- weekdays(Time_Dep, abbreviate = "TRUE")

# Clocktime in Min 0 - 1440 Min (24 h * 60 Min  =  1440)
Clc_Dep <- Drive$depthour * 60 + Drive$deptminute
Clc_Arr <- Drive$arrhour * 60 + Drive$arrminute

# average speed
AveSpd <- Drive$distance/Duration * 60  # km/h   

# summarize in Driving profiles
Drive <- data.frame(Drive, Clc_Dep, Clc_Arr, WD_Dep, Duration, AveSpd)


# Delete the wrong data
  # Driving Profiles with duration of 0 min
Drive <- Drive[-which(Drive$Duration == 0),] # 337(0.388%) Drivings were deleted
  # Driving Profiles with impossible average speed
Drive <- Drive[-which(Drive$AveSpd > 160 | Drive$AveSpd < 1),] #another 156 Profiles were deleted

rm(Time_Arr,Time_Dep,Clc_Arr,Clc_Dep,AveSpd,WD_Dep,Duration)
save.image(file = "00_Set1_Data_AfterProcessing.RData")
