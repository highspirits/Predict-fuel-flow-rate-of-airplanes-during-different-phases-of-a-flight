#Different Phases of Flight 
#http://www.fp7-restarts.eu/index.php/home/root/state-of-the-art/objectives/2012-02-15-11-58-37/71-book-video/parti-principles-of-flight/126-4-phases-of-a-flight
#http://dspace.mit.edu/bitstream/handle/1721.1/35725/FlightOpsICATfinal2.pdf
#0 = Unknown
#1 = Preflight
#2 = Taxi
#3 = Takeoff
#4 = Climb
#5 = Cruise
#6 = Approach
#7 = Rollout

setwd("C:/Sreedhar/Data Science Competitions/Crowd Analytix/Predict fuel flow rate of airplanes during different phases of a flight/Code")

filenames = list.files(path = "C:/Sreedhar/Data Science Competitions/Crowd Analytix/Predict fuel flow rate of airplanes during different phases of a flight/Data/CAX_Train_1", pattern = ".csv")
train1 = do.call(rbind, lapply(paste("C:/Sreedhar/Data Science Competitions/Crowd Analytix/Predict fuel flow rate of airplanes during different phases of a flight/Data/CAX_Train_1/", filenames, sep = ''), read.csv))

#Convert PH column to more readable format
train1$PH[train1$PH == 0] = '0Unknown'
train1$PH[train1$PH == 1] = '1Preflight'
train1$PH[train1$PH == 2] = '2Taxi'
train1$PH[train1$PH == 3] = '3Takeoff'
train1$PH[train1$PH == 4] = '4Climb'
train1$PH[train1$PH == 5] = '5Cruise'
train1$PH[train1$PH == 6] = '6Approach'
train1$PH[train1$PH == 7] = '7Rollout'
#=============================================================================================================
#See how many observations are present for each flight stage
(table(train1$PH)/nrow(train1))*100
#Output:

#0Unknown 1Preflight      2Taxi   3Takeoff     4Climb    5Cruise  6Approach   7Rollout 
#9.5480043  0.8550063 21.9810499  1.4262822 19.4653512 25.4724496 20.8522704  0.3995861

#Result: There are more observations in Taxi(2), Climb(4), Cruise(5), Approach(6). 
#
#Cruise(5)(25)
#Taxi(2)(21)
#Approach(6)(20)
#Climb(4)(19)
#
#Interpretation: 
# 1) Since the observations are sampling/sec, more time spent in a phase will give more samples. 
# 2) Number of samples for Climb and Approach are almost same (as expected)
# 3) Taxi is second in number of samples collected/sec (interestingly)
#=============================================================================================================

#Percentage of fuel spent in each stage of the flight, sorted in decreasing order
sort((tapply(train1$FF, train1$PH, sum)/sum(as.numeric(train1$FF))*100), decreasing = T)
#Result

#4Climb      5Cruise    6Approach     0Unknown        2Taxi     3Takeoff     7Rollout   1Preflight 
#34.740558512 30.934154314 14.981453454  9.687365838  7.147118670  2.366287941  0.135440162  0.007621109

#Interpretation:
# 1)Climbing phase has the highest fuel consumption, followed by Cruise and Approach
#=============================================================================================================
#Will the number of samples taken has any impact on the above observation?
#meaning, if in 100seconds of Cruise phase, fuel spent is 30.93%, what would be the % spent in 1 sec
#if in 150 seconds, fuel spent is 14.98%, what would be the % spent in 1 sec
#This number should give the fuel spent if each phase of the flight journey would be in 1 sec. 
#Will this be any different than the interpretation we had on fuel consumption in each phase, based on Lbs/Hr

#Fuel Spent if there were only one entry per PH type
#This would be same as fuel consumed - (Lbs/Hr)/Sec
sort((tapply(train1$FF, train1$PH, sum))/(table(train1$PH)), decreasing = T)

#Result:
#4Climb   3Takeoff    5Cruise   0Unknown  6Approach   7Rollout      2Taxi 1Preflight 
#7374.71642 6855.40145 5018.08842 4192.41115 2968.73460 1400.57988 1343.54845   36.83152

#Converting the above numbers to %
temp = (tapply(train1$FF, train1$PH, sum))/((table(train1$PH)))
sort((temp/(sum(temp))*100), decreasing = T)

#Result
#4Climb   3Takeoff    5Cruise   0Unknown  6Approach   7Rollout      2Taxi 1Preflight 
#25.2642604 23.4851942 17.1909380 14.3623376 10.1702737  4.7980984  4.6027204  0.1261772

#Interpretation:
# 1)If different phases of flight journey were to be done in 1 second, then Climbing has the highest fuel consumption, followed 
#   by Takeoff and Cruise. 

#Order interms of highest fuel consumed
# 1) Climb
# 2) Cruise
# 3) Approach
# 4) Unknown
# 5) Taxi
# 6) Takeoff

#Order interms of highest fuel consumed/sec (rate of fuel consumption)
# 1) Climb
# 2) Takeoff
# 3) Cruise
# 4) Unknown
# 5) Approach
# 6) Rollout
# 7) Taxi
#=============================================================================================================
#Feature Engineering
#=============================================================================================================

#Tried finding correlation between numeric columns with FF, but got memory exception error
#findCorrelation(train1[sapply(train1, is.numeric)])

#Tried using Boruta package for identifying importance of the features, got memory exception error
#boruta.train1 = Boruta(FF ~ .-ACID -Flight_instance_ID -Year -Month -Day, data = train1)

#Subset train1 data (folder1 all files data) to analyze "4Climb" elements only
t.climb = subset(train1, train1$PH == '4Climb')

#Remove ACID, Flight_instance_ID, Date and Time fields
t.climb = train1.climb

t.climb$ACID = NULL
t.climb$Flight_instance_ID = NULL
t.climb$Year = NULL
t.climb$Month = NULL
t.climb$Day = NULL
t.climb$Hour = NULL
t.climb$Minute = NULL
t.climb$Second = NULL

#Install xgboost package
install.packages("xgboost")
library(xgboost)


  






