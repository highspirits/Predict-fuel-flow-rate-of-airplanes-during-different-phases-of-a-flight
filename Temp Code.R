setwd("C:/Sreedhar/Data Science Competitions/Crowd Analytix/Predict fuel flow rate of airplanes during different phases of a flight/Code")

filenames = list.files(path = "C:/Sreedhar/Data Science Competitions/Crowd Analytix/Predict fuel flow rate of airplanes during different phases of a flight/Data/CAX_Train_1", pattern = ".csv")

train1 = do.call(rbind, lapply(paste("C:/Sreedhar/Data Science Competitions/Crowd Analytix/Predict fuel flow rate of airplanes during different phases of a flight/Data/CAX_Train_1/", filenames, sep = ''), read.csv))

t.climb = subset(train1, train1$PH == '4')

t.climb$PH = NULL
t.climb$ACID = NULL
t.climb$Flight_instance_ID = NULL
t.climb$Year = NULL
t.climb$Month = NULL
t.climb$Day = NULL
t.climb$Hour = NULL
t.climb$Minute = NULL
t.climb$Second = NULL

t.climb = my.rem.zer.var(t.climb)

t.climb.sub = t.climb[sample(nrow(t.climb), 50000), ]

y = t.climb.sub$FF  

t.climb.sub$FF = NULL

test = read.csv("C:/Sreedhar/Data Science Competitions/Crowd Analytix/Predict fuel flow rate of airplanes during different phases of a flight/Data/Test Data/CAX_Test.csv")
test.climb = subset(test, test$PH == 4)

submit <- test.climb[ ,c("id")]
submit.temp <- test.climb[ ,c("id")]

test.climb <- subset(test.climb, select=c(colnames(t.climb.sub)))


source_https <- function(url)
{
  library(RCurl)
  eval(parse(text=getURL(url,followlocation=T,cainfo=system.file("CurlSSL","cacert.pem",package="RCurl"))),envir=.GlobalEnv)
}

install.packages("xgboost")
library(xgboost)

source_https("https://raw.githubusercontent.com/rohanrao91/Models_CV/master/XGBoost.R")

model_xgb_1 <- XGBoost(t.climb.sub,y,test.climb,cv=5,objective="reg:linear",nrounds=500,max.depth=10,eta=0.1,colsample_bytree=0.5,seed=235,metric="rmse",importance=1)


