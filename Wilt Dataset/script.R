
#train with all data available
#SVM 
#setwd("C:/Users/Utkarsh/Desktop/eg1/SVM")
train<-read.csv("training.csv")
test<-read.csv("testing.csv")
library(e1071)
library(caret)
model2 <- svm(formula = class ~ Mean_Green + Mean_Red + Mean_NIR,data =train)
my_prediction <- predict(model2,test,type='response')
my_solution1 <- data.frame(class=my_prediction,test[,1],test[,2],test[,3],test[,4],test[,5])
write.csv(my_solution,file="my_solution.csv",row.names= FALSE)
accuracy = ifelse(my_prediction != factor(test$class,levels=c("w","n")),0,1)
mean(accuracy)
#0.796


# Final submission
# setwd("C:/Users/Utkarsh/Desktop/eg1/SVM")
train<-read.csv("training.csv")
test<-read.csv("testing.csv")
library(e1071)
library(caret)
model2 <- svm(formula = class ~ Mean_Green + Mean_Red + Mean_NIR,data =train)
my_prediction <- predict(model2,test,type='response')
my_solution <- data.frame(class=my_prediction,GLCM_pan=test[,1],Mean_Green=test[,2],Mean_Red=test[,3],Mean_NIR=test[,4],SD_pan=test[,5])
write.csv(my_solution,file="my_solution.csv",row.names= FALSE)
#0.796

#Random forest 
#setwd("C:/Users/Utkarsh/Desktop/eg1/random forest")
train<-read.csv("training.csv")
test<-read.csv("testing.csv")
library(randomForest)
my_forest <- randomForest(class~Mean_Green + Mean_Red + Mean_NIR,data = train,ntree=1000,importance = TRUE)
my_prediction <- predict(my_forest,test)
accuracy = ifelse(my_prediction != factor(test$class,levels=c("w","n")),0,1)
mean(accuracy)
#0.79

#Logistic Regression
setwd("C:/Users/Utkarsh/Desktop/eg1/logistic Reg")
train<-read.csv("training.csv")
test<-read.csv("testing.csv")
library(caTools)
model <- glm(formula = class ~ Mean_Green + Mean_Red + Mean_NIR,data = train,family = binomial(link = 'logit'))
summary(model)
my_prediction=predict(model,test,type='response')
my_prediction<-ifelse(f>0.5,1,0)
test$binaryclass=ifelse(cross_val$class == 'w',1,0)
error <- mean(f != test$binaryclass)
accuracy = 1- error
accuracy

#setwd("C:/Users/Utkarsh/Desktop")
test <- read.csv("testing.csv")
accuracy = ifelse(factor(test$class,levels=c("w","n")) != factor(result$class,levels=c("w","n")),0,1)
mean(accuracy)
