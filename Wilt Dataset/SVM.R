training <- read.csv('training.csv',header=T)
write.csv(training[1:3000,],file="train.csv",row.names = FALSE)
write.csv(training[3001:4339,],file="cross_val.csv",row.names = FALSE)
train <- read.csv('train.csv',header=T)
cross_validation <- read.csv('cross_val.csv',header=T)
library(e1071)
library(caret)
model2 <- svm(formula = class ~ Mean_Green + Mean_Red + Mean_NIR,data =train)
my_prediction <- predict(model2,cross_val,type='response')
accuracy = ifelse(my_prediction != factor(cross_val$class,levels=c("w","n")),0,1)
mean(accuracy)
#0.9958506224

#Another splitting method

training<-read.csv("training.csv")
set.seed(88)
split <- sample.split(training,SplitRatio = 0.75)
train<-subset(training,split == TRUE)
cross_val <- subset(training,split == FALSE)
library(e1071)
library(caret)
model2 <- svm(formula = class ~ Mean_Green + Mean_Red + Mean_NIR,data =train)
my_prediction <- predict(model2,cross_val,type='response')
accuracy = ifelse(my_prediction != factor(cross_val$class,levels=c("w","n")),0,1)
mean(accuracy)
#0.9923928