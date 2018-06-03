#Linear Regression model

dataset <- read.csv("C:/Users/gagan/Desktop/Dataset.csv", header = TRUE)

#Checking the linear trend of data
plot(dataset$Year,dataset$DecVol)

#Checking the missing values
is.na(dataset)

#Splitting data into training and testing
set.seed(123)
split=sample.split(dataset$DecVol,SplitRatio = 2/3)
training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split==FALSE)

testing_set <- rbind(testing_set, c(2013,))

#Simple Linear Regression Model
model <- lm(DecVol~Year,data=training_set)

summary(model)

#Predicting the Test set Results
y_pred=predict(model,newdata = testing_set)


#Visualising the Training set results
ggplot()+geom_point(aes(x=training_set$Year,y=training_set$DecVol),
                    colour='red')+
  geom_line(aes(x=training_set$Year,y=predict(model,newdata = training_set)),
            colour='blue')+
  ggtitle('Year VS DecVol(Training_set)')+
  xlab('Years')+
  ylab('DecVol')

##Visualising the Testing set results
ggplot()+geom_point(aes(x=testing_set$Year,y=testing_set$DecVol),
                    colour='red')+
  geom_line(aes(x=training_set$Year,y=predict(model,newdata = training_set)),
            colour='blue')+
  ggtitle('Year VS DecVol(Testing_set)')+
  xlab('Years')+
  ylab('DecVol')