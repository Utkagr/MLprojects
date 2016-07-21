#install.packages('caTools')
library(caTools)
training<-read.csv("training.csv")
set.seed(88)
split <- sample.split(training,SplitRatio = 0.75)
train<-subset(training,split == TRUE)
cross_val <- subset(training,split == FALSE)
model <- glm(formula = class ~ Mean_Green + Mean_Red + Mean_NIR,data = train,family = binomial(link = 'logit'))
summary(model)
f <- predict(model,cross_val,type = 'response')
f <- ifelse(f>0.5,1,0)
cross_val$binaryclass=ifelse(cross_val$class == 'w',1,0)
error <- mean(f != cross_val$binaryclass)
accuracy = 1- error
accuracy
#0.9930844