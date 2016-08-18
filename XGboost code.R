#train1 = do.call(rbind, lapply(paste("C:/Sreedhar/Data Science Competitions/Crowd Analytix/Predict fuel flow rate of airplanes during different phases of a flight/Data/CAX_Train_1/", filenames, sep = ''), read.csv))
#test = read.csv("C:/Sreedhar/Data Science Competitions/Crowd Analytix/Predict fuel flow rate of airplanes during different phases of a flight/Data/Test Data/CAX_Test.csv", stringsAsFactors = F)

#CHANGE
train0 = subset(train1, train1$PH == '1')

train0$PH = NULL
train0$ACID = NULL
train0$Flight_instance_ID = NULL
train0$Year = NULL
train0$Month = NULL
train0$Day = NULL
train0$Hour = NULL
train0$Minute = NULL
train0$Second = NULL

train0 = my.rem.zer.var(train0)

train0.sub = train0[sample(nrow(train0), 50000), ]

y = train0.sub$FF  

train0.sub$FF = NULL

# CHANGE
test0 = subset(test, test$PH == 1)

submit <- test0[ ,c("id")]
submit.temp <- test0[ ,c("id")]

test0 <- subset(test0, select=c(colnames(train0.sub)))

model_xgb_1 <- XGBoost(train0.sub,y,test0,cv=5,objective="reg:linear",nrounds=500,max.depth=10,eta=0.1,colsample_bytree=0.5,seed=235,metric="rmse",importance=1)

test_xgb_1 = model_xgb_1[[2]]

submit = data.frame("id" = submit, "FF" = test_xgb_1$pred_xgb)

write.csv(submit, "./submit.csv", row.names = F)