rm(list=ls(all=T))
setwd("D:/Edwisor/project/Cab Fare Prediction/r codes")

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

## Read the train and test data
train = read.csv("train_cab.csv", header = T)
test = read.csv("test.csv", header = T)


#Verify first five rows of data
head(train)
head(test)


str(train)
str(test)
summary(train) #Statistical summary of the dataset

#fare amount should be converted to numeric
train$fare_amount = as.numeric(train$fare_amount)


#####Visualising the data
#Distribution of Fare Amount
P1 <- ggplot(aes(x =fare_amount), data = train)+ geom_histogram(bins = 100) +
  scale_x_continuous(limits = c(-10,125)) + 
  geom_vline(aes(xintercept = median(fare_amount)), linetype = "dashed", size = 2.5, color = "blue") +
  geom_text(aes(x = 4.5, y = 200, label = "median")) + ggtitle("Distribtion of Fare Amount")
P1

#Distribtion of Passenger Count
P2 <- ggplot(aes(x=factor(passenger_count)), data = train) + geom_bar() +
  geom_text(aes(label =scales::percent(..count../sum(..count..))),stat = 'count',vjust = -0.5) + ggtitle("Distribution of Passenger Count") + labs(x = "passenger Count")


P2

#Distribution of Fare Amount:
#There are some trips that have negative fare amount value
#Median Fare amount is 8.5 - High number of trips have fare amount < 10, fewer number of trips with amounts > 50

#Distribution of Passenger Count:
#69.2% of the rides have just 1 passenger
#There are records with 129 and 208 passengers which is obviously incrorect

############ Missing Value analsis ##############

sum(is.na(train)) ##55 values
train = na.omit(train) # total obseravations now #16012

## covert the negative vaklues to positive.
train$fare_amount = abs(train$fare_amount)

## removing the passenger_count values more than 6.
train = subset(train, passenger_count < 6 ) ## 15690 total observations

#Removing trip records that are not possible or seem to be inaccurate.
train = subset(train, pickup_longitude > -75 & pickup_longitude < -73 & 
                     pickup_latitude > 40 & pickup_latitude < 42)
train = subset(train, dropoff_longitude > -75 & dropoff_longitude < -73 & 
                     dropoff_latitude > 40 & dropoff_latitude < 42)



#Extracting the following variables from the datetime column: Year, month, weekday, day, hour
train$pickup_datetime = strptime(x = as.character(train$pickup_datetime),format = "%Y-%m-%d %H:%M")

#but after splitting date time into the separate column but the data type is still a character so I can handle the same 

train$pickup_datetime = gsub('\\ UTC','',train$pickup_datetime)

# Lets Split Date and time
train$Date = as.Date(train$pickup_datetime) 
train$Year = substr(as.character(train$Date),1,4) 
train$Month = substr(as.character(train$Date),6,7) 
train$Weekday = weekdays(as.POSIXct(train$Date), abbreviate = F)
train$Date = substr(as.character(train$Date),9,10) 
train$time = substr(as.factor(train$pickup_datetime),12,13) 

head(train)


#performing same operations on test data
test$pickup_datetime = strptime(x = as.character(test$pickup_datetime),format = "%Y-%m-%d %H:%M")

#but after splitting date time into the separate column but the data type is still a character so I can handle the same 

test$pickup_datetime = gsub('\\ UTC','',test$pickup_datetime)

# Lets Split Date and time
test$Date = as.Date(test$pickup_datetime) 
test$Year = substr(as.character(test$Date),1,4) 
test$Month = substr(as.character(test$Date),6,7) 
test$Weekday = weekdays(as.POSIXct(test$Date), abbreviate = F)
test$Date = substr(as.character(test$Date),9,10) 
test$time = substr(as.factor(test$pickup_datetime),12,13) 

head(test)
head(train)

#Dropping the datetime variable now

train = train[,-2] #2nd column is pickupdate and time.
test = test[,-1]  #1st column is pickup date and time.

#We will calculte the total distance travelled on the spehere using the 
#haversine distance/ great circle distance.

#Formulas used to calculate(phi = latitudes of the 2 pts, lambda = longitudes): haversine(??) = sin²(??/2)
#a = sin²(??B - ??A/2) + cos ??A * cos ??B * sin²(??B - ??A/2)
#c = 2 * atan2( ???a, ???(1???a) )
#d = R * c

rad = (22/7)/180

#Training dataset distance
phi1_train = train$pickup_latitude * rad
lambda1_train = train$pickup_longitude * rad
phi2_train = train$dropoff_latitude * rad
lambda2_train = train$dropoff_longitude * rad

d_phi_train = phi2_train - phi1_train
d_lambda_train = lambda2_train - lambda1_train

a = (sin(d_phi_train/2))^2 + cos(phi1_train) * cos(phi2_train) * (sin(d_lambda_train/2))^2

c = 2 * atan2(sqrt(a), sqrt(1 - a))
R = 6378.145
d = R * c
d_miles = d * 0.621371
train$distance = d_miles


#Distance for test dataset

phi1_test = test$pickup_latitude * rad
lambda1_test = test$pickup_longitude * rad
phi2_test = test$dropoff_latitude * rad
lambda2_test = test$dropoff_longitude * rad

d_phi_test = phi2_test - phi1_test
d_lambda_test = lambda2_test - lambda1_test

a = (sin(d_phi_test/2))^2 + cos(phi1_test) * cos(phi2_test) * (sin(d_lambda_test/2))^2

c = 2 * atan2(sqrt(a), sqrt(1 - a))
R = 6378.145
d = R * c
d_miles = d * 0.621371
test$distance = d_miles
test$distance

# Fare Amount by Hour dependence

P5 = ggplot(aes( time,fare_amount), data = train) + 
  geom_point(alpha =1, position = position_jitter(h=0), color = 'Orange')+
  ylim(0,400) + 
  geom_line(stat = 'summary', fun.y = median, color = 'blue') +  
  geom_text( aes(x = 12, y = 13, label = "Median")) + ggtitle("Fare Amount by time")
P5

P6=ggplot(train, aes(x= distance,y=fare_amount)) +
  geom_point()
P6

# fare amount increases with increase in the distance.

############Feature selection ###############
#converrt few of the variables to numeric
train$Date = as.numeric(train$Date)
train$Year = as.numeric(train$Year)
train$Month = as.numeric(train$Month)
train$time = as.numeric(train$time)
train$distance = as.numeric(train$distance)

####correlation plot
numeric_index = sapply(train,is.numeric)
corrgram(train[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
numeric_index
##none of the dependent variable show multi collinearity therefore we 
#have to consider all the variables in our model.

####### Modelling #############

#### Linear Correlation


#Divide the data into train and test
set.seed(123)
train_index = sample(1:nrow(train), 0.8 * nrow(train))
train_model = train[train_index,]
test_model = train[-train_index,]

#run regression model
lm_model = lm(fare_amount ~., data = train_model)
#Summary of the model
summary(lm_model)
predictions_LR = predict(lm_model, test_model[,2:12])

#Calculate RMSE
regr.eval(test_model[,1] , predictions_LR , stats = 'rmse')

## RMSE is 166.193 which is very high therefore we need some more models.


############# DECISION TREE MODEL #################

# ##rpart for regression
fit = rpart(fare_amount ~ ., data = train_model, method = "anova")
#Predict for new test cases
predictions_DT = predict(fit, test_model[,-1])
#Calculate RMSE
regr.eval(test_model[,1] , predictions_DT , stats = 'rmse')

###rrmse 133.76 better than linear regression model.

############ Random Forest Regression.###################
Rental_rf=randomForest(fare_amount ~ . , data = train_model)

#Predict for new test cases
predictions_rf = predict(Rental_rf , test_model[,-1])


#Calculate RMSE
regr.eval(test_model[,1] , predictions_DT , stats = 'rmse')

##rmse 90.35 ,, it is better than above two models.

#### thereforre random forest is the best model for the predictions.


