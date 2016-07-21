#setwd("C:/Users/Utkarsh/Desktop/eg1/random forest")
training <- read.csv('training.csv',header=T)
write.csv(training[1:3000,],file="train.csv",row.names = FALSE)
write.csv(training[3001:4339,],file="cross_validation.csv",row.names = FALSE)
train <- read.csv('train.csv',header=T)
cross_validation <- read.csv('cross_validation.csv',header=T)
library(randomForest)
my_forest <- randomForest(class~Mean_Green + Mean_Red + Mean_NIR,data = train,ntree=1000,importance = TRUE)
my_prediction <- predict(my_forest,cross_validation)
accuracy = ifelse(my_prediction != factor(cross_validation$class,levels=c("w","n")),0,1)
mean(accuracy)
#0.9977595

#For splitting the data

#setwd("C:/Users/Utkarsh/Desktop/eg1/random forest")
training<-read.csv("training.csv")
set.seed(88)
split <- sample.split(training,SplitRatio = 0.75)
train<-subset(training,split == TRUE)
cross_val <- subset(training,split == FALSE)
library(randomForest)
my_forest <- randomForest(class~Mean_Green + Mean_Red + Mean_NIR,data = train,ntree=1000,importance = TRUE)
my_prediction <- predict(my_forest,cross_val)
accuracy = ifelse(my_prediction != factor(cross_val$class,levels=c("w","n")),0,1)
mean(accuracy)
#0.9903181
#Some useful code below:

# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(Survived) ~ Pclass +Sex+Age+SibSp+Parch+Fare+Embarked+Title,data =train,ntree=1000,importance=TRUE)

# Make your prediction using the test set
my_prediction <- predict(my_forest,test)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution,file="my_solution.csv",row.names= FALSE)
