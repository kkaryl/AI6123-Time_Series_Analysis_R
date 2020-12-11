# Title     : NTU Time Series R 
# Objective : Try Logistic Regression model on Titanic data
# Created by: Karyl
# Created on: 5/1/2020

# Import the data "train.csv"
# na.string can help to encode all missing data as NA
training.data.raw<-read.csv('data/train.csv',
                            header=T,na.strings=c(""))

#Check number of missing values for each variable
sapply(training.data.raw,function(x) sum(is.na(x)))

#Check how many unique observations for each corresponding variable
sapply(training.data.raw,function(x) length(unique(x)))

#Delete passengerID, name and ticket as it is not useful in predicting survival
#Delete cabin as cabin contains too many missing values
#Delete ticket as it is not helpful as all passengers have ticket
data = subset(training.data.raw, select=c(2,3,5,6,7,8,10,12))

# Set missing ages as mean to estimate the missing values 
data$Age[is.na(data$Age)]<-mean(data$Age,na.rm=T)

# Delete the 2 rows of the missing Embarked
data<-data[!is.na(data$Embarked),]

#?rownames
rownames(data)<-NULL
train<-data[1:800,]
test<-data[801:889,]
# Can use sample.split to randomly split

# Logistic regression, specify binomial
model<-glm(Survived~.,family=binomial(link='logit'), data=train)
summary(model)
# Looking at Pr values, seems like Pclass, Sex, Age, SibSp are significant
# The rest are not significant as their p-value is bigger than 0.05

# Select the other x values (other than survived)
fitted.results <- predict(model, newdata=subset(test, 
                          select=c(2,3,4,5,6,7,8)), 
                          type='response')
fitted.results <- ifelse(fitted.results>0.5,1,0)
misClasificError <- mean(fitted.results!=test$Survived)
print(paste('Accuracy', 1-misClasificError))



