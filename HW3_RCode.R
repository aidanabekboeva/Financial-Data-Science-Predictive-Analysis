install.packages("tidyverse")
install.packages("ggplot2")
install.packages("car")
library(tidyverse)
library(ggplot2)
library(car)
library(dplyr)
library(MASS)
library(class)

############################### Question 1 ################################

#a) Produce some numerical and graphical summaries

weekly <- read.csv("C:\\Users\\Aidana Bekboeva\\Desktop\\STEVENS\\2. FA 582 - Financial Data Science\\Assignment 3\\HW3_data\\Weekly.csv")

summary(weekly)

hist(weekly$Lag1, xlab = " ", main = "Lag 1")
hist(weekly$Lag2, xlab = " ", main = "Lag 2")
hist(weekly$Today, xlab = " ", main = "Today")

res <- cor(weekly[,-9])
round(res, 2)

ggplot(data = weekly, mapping = aes(x = Volume, y = Year), main = "Volume & Years ggplot") + geom_point()

#b) Logistic regression

weekly$Direction <- as.factor(weekly$Direction)
class(weekly$Direction)

weekly_fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = weekly, 
              family = binomial)
summary(weekly_fit)

#The only variable that was statistically significant was Lag2, 
#at the level of significance 0.05. 
#The other variables fail to reject the null hypothesis.

#c) Confusion matrix

weeklylogprob= predict(weekly_fit, type='response')
pred_weeklylog =rep("Down", length(weeklylogprob))
pred_weeklylog[weeklylogprob > 0.5] = "Up"
table(pred_weeklylog, weekly$Direction)

#d) Fitting the logistic regression model

t = (weekly$Year<2009)
weekly_t <-weekly[!t,]
weekly_fit<-glm(Direction~Lag2, data=weekly,family=binomial, subset=t)
weeklylogprob= predict(weekly_fit, weekly_t, type = "response")
pred_weeklylog = rep("Down", length(weeklylogprob))
pred_weeklylog[weeklylogprob > 0.5] = "Up"
Direction_t = weekly$Direction[!t]
table(pred_weeklylog, Direction_t)
mean(pred_weeklylog == Direction_t)

#e) Repeat d) using LDA.
weeklylda.fit<-lda(Direction~Lag2, data=weekly,family=binomial, subset=t)
weeklylda.pred<-predict(weeklylda.fit, weekly_t)
table(weeklylda.pred$class, Direction_t)
mean(weeklylda.pred$class==Direction_t)

#f) Repeat d) using QDA.
weeklylda.fit<-qda(Direction~Lag2, data=weekly,family=binomial, subset=t)
weeklylda.pred<-predict(weeklylda.fit, weekly_t)
table(weeklylda.pred$class, Direction_t)
mean(weeklylda.pred$class==Direction_t)

#g) Repeat d) using KNN with K = 1.
week_t = as.matrix(weekly$Lag2[t])
weekly_test = as.matrix(weekly$Lag2[!t])
train_Direction = weekly$Direction[t]
set.seed(1)
weekly_knnpred=knn(week_t, weekly_test, train_Direction, k=1)
table(weekly_knnpred, Direction_t)
mean(weekly_knnpred == Direction_t)

#(h) Which of these methods appears to provide the best results on this data?

#The methods that have the highest accuracy rates are the Logistic Regression 
#and Linear Discriminant Analysis; both having rates of 62.5%.

#(i) Experiment with different combinations of predictors

#1. Logistic regression 
weekly_t <-weekly[!t,]
weekly_fit<-glm(Direction~Lag2:Lag5+Lag2, data=weekly,family=binomial, subset=t)
weeklylogprob= predict(weekly_fit, weekly_t, type = "response")
pred_weeklylog = rep("Down", length(weeklylogprob))
pred_weeklylog[weeklylogprob > 0.5] = "Up"
Direction_t = weekly$Direction[!t]
table(pred_weeklylog, Direction_t)
mean(pred_weeklylog == Direction_t)

#2. LDA
weeklylda.fit<-lda(Direction~Lag2:Lag3+Lag2, data=weekly,family=binomial, subset=t)
weeklylda.pred<-predict(weeklylda.fit, weekly_t)
table(weeklylda.pred$class, Direction_t)
mean(weeklylda.pred$class==Direction_t)

#3. QDA
weeklylda.fit<-qda(Direction~Lag2:Lag3+Lag2, data=weekly,family=binomial, subset=t)
weeklylda.pred<-predict(weeklylda.fit, weekly_t)
table(weeklylda.pred$class, Direction_t)
mean(weeklylda.pred$class==Direction_t)

#4. KNN with K = 5, 10, 15
week_t = as.matrix(weekly$Lag2[t])
weekly_test = as.matrix(weekly$Lag2[!t])
train_Direction = weekly$Direction[t]
set.seed(1)
weekly_knnpred=knn(week_t, weekly_test, train_Direction, k=10)
table(weekly_knnpred, Direction_t)
mean(weekly_knnpred == Direction_t)



############################### Question 2 ################################

#a) Create a binary variable, mpg01, ...

dfauto <- read.csv("C:\\Users\\Aidana Bekboeva\\Desktop\\STEVENS\\2. FA 582 - Financial Data Science\\Assignment 3\\HW3_data\\Auto.csv")
summary(dfauto)

dfauto$mpg01 <- ifelse(dfauto$mpg > median(dfauto$mpg), "1", "0")
summary(dfauto)

#b) Explore the data graphically

install.packages("ggpubr")
library("ggpubr")

ggplot(dfauto, 
       aes(x = mpg01, 
           y = cylinders)) +
  geom_boxplot() +
  labs(title = "mpg01 & cylinders")

ggplot(dfauto, 
       aes(x = mpg01, 
           y = displacement)) +
  geom_boxplot() +
  labs(title = "mpg01 & displacement")

ggplot(dfauto, 
       aes(x = mpg01, 
           y = horsepower)) +
  geom_boxplot() +
  labs(title = "mpg01 & horsepower")

ggplot(dfauto, 
       aes(x = mpg01, 
           y = weight)) +
  geom_boxplot() +
  labs(title = "mpg01 & weight")

ggplot(dfauto, 
       aes(x = mpg01, 
           y = acceleration)) +
  geom_boxplot() +
  labs(title = "mpg01 & acceleration")

ggplot(dfauto, 
       aes(x = mpg01, 
           y = year)) +
  geom_boxplot() +
  labs(title = "mpg01 & year")

ggplot(dfauto, 
       aes(x = mpg01, 
           y = origin)) +
  geom_boxplot() +
  labs(title = "mpg01 & origin")

dfauto$mpg01 <- ifelse(dfauto$mpg > median(dfauto$mpg), 1, 0)
res <- cor(dfauto[,-9])
round(res, 2)

#c) Split the data into a training set and a test set
train <- (dfauto$year %% 2 == 0)
train_set <- dfauto[train,]
test_set <- dfauto[-train,]

#d) Perform LDA on the training data 
dfautolda_fit <- lda(mpg01~displacement+horsepower+weight+year+cylinders+origin, data = train_set)
dfautolda_pred <- predict(dfautolda_fit, test_set)
table(dfautolda_pred$class, test_set$mpg01)
mean(dfautolda_pred$class != test_set$mpg01)

#e) Perform QDA on the training data
dfautolda_fit <- qda(mpg01~displacement+horsepower+weight+year+cylinders+origin, data = train_set)
dfautolda_pred <- predict(dfautolda_fit, test_set)
table(dfautolda_pred$class, test_set$mpg01)
mean(dfautolda_pred$class != test_set$mpg01)

#f) Perform logistic regression on the training data
dfauto_fit <- glm(mpg01~displacement+horsepower+weight+year+cylinders+origin, data=train_set,family=binomial)
dfauto_probs = predict(dfauto_fit, test_set, type = "response")
dfauto_pred = rep(0, length(dfauto_probs))
dfauto_pred[dfauto_probs > 0.5] = 1
table(dfauto_pred, test_set$mpg01)
mean(dfauto_pred != test_set$mpg01)

#g) Perform KNN on the training data, K = 5, 10, 15, 50
knn_training <- cbind(dfauto$displacement,dfauto$horsepower,
                      dfauto$weight,dfauto$cylinders,
                      dfauto$year, dfauto$origin)[train,]
knn_test=cbind(dfauto$displacement,dfauto$horsepower,
               dfauto$weight,dfauto$cylinders, 
               dfauto$year, dfauto$origin)[-train,]
set.seed(1)
dfauto_knnpred=knn(knn_training,knn_test,train_set$mpg01,k=50) #k=10, k=15
mean(dfauto_knnpred != test_set$mpg01)








