getwd()
setwd("/home/utkarsh/BITA")
data1 <- read.csv(file.choose()) #ClickStream.csv
data2 <- read.csv(file.choose()) #ProductCategoryData.csv
data3 <- read.csv(file.choose()) #UserProfileData.csv
#View(data1)
#View(data2)
#View(data3)
#levels(data1$URL) #28
#levels(data2$URL) #31
which(match(data1$URL,data2$URL,nomatch=0,incomparables= NULL) == 0) # No values
#In data2,We have product category based on URLs
# We can use that in data1.
data4 = data1

#For determining category related to the URL
count = 0
for(i in match(data4$URL,data2$URL,nomatch = 0,incomparables = NULL))
  {count= count + 1
    data4$Category[count] <- as.character(data2$Category[i])}
#View(data4)
data4$Category <- as.factor(data4$Category)
#levels(data4$Category)
#View(data4)
data4 <- data4[,c(1,2,3,8,4,5,6,7)] #Removing features of slight importance

#For determining Number of friends
#Note that for some users,number of friends are not given.
match(data4$User.ID,data3$User.ID,nomatch = NULL,incomparables = NULL)
count1 = 0
for(i in match(data4$User.ID,data3$User.ID,nomatch = NULL,incomparables = NULL))
  {count1 = count1 + 1
    data4$Number.of.friends[count1] = data3$Number.of.friends[i]
    data4$College.education[count1] = as.character(data3$College.Education[i])
}
#View(data4)

mean(data4$Number.of.friends) # comes out to be NUll
#If we find mean by removing NULL values,i comes out as 91.14595
mean(data4$Number.of.friends,na.rm= T)
#So,We wont be using this value as it is a huge number of friends.
#And Number of friends is a huge factor for determining Cross-Selling opportunities.
#That is because more number of friends allows me to buy combo products which are cheaper.
# So, we will keep the number of friends to be 0 for those whose data are not available
#data4$Number.of.friends[is.na(data4$Number.of.friends)] <- 0

#Introducing new variable
#Number of different product orders

diff_users = levels(data4$IP.Address)
diff_users[1]
for(user in diff_users){
  indexes = which(data4$IP.Address == user)
  data4$diff_products_ordered[indexes] <- nlevels(droplevels(data4$Category[indexes]))}
#View(data4)
# Now,we have the number of different products orered by each user
# Obviously,we would like to cross sell more to the user who orders different products.
data5 <- subset(data4,(!is.na(data4[,9])) & (!is.na(data4[,10])))
#View(data5)
data6 <- subset(data4,(is.na(data4[,9])) & (is.na(data4[,10])))
#View(data6)
#table(data6$diff_products_ordered)
#1    2    3    4    5    6 
#8832 1645  487  351  148   54 
# It seems that there are users who have bought different products(upto 6).
#So,this data is valuable to us
#We cannot just remove it.
# We will give them averages of users of their types
# Like we will take avg friends of those users who buy diff. products 6 and
# assign it to no of friends to the missing values where diff products = 6
avg_frnd_6 = floor(mean(data4$Number.of.friends,trim=0,na.rm = T,data4$diff_products_ordered == 6))
data4$Number.of.friends[is.na(data4[,9])]
#View(data4)
#write.csv(data4,file = "/home/utkarsh/BITA/data4.csv")

# To predict the missing values,it would not be useful to use a predictive model because very few features are related
# Even College.Education and Number.of.friends is not much related.
#summary(data4$Number.of.friends[data4$College.education == "No"])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   82.00  100.00   90.76  123.00  169.00   11520 
#summary(data4$Number.of.friends[data4$College.education == "Yes"])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   89.00  104.00   97.67  122.00  169.00   11517
# We see that even for users who dont go to college have a lot of friends(mean = 90.76)
# Obviously,mean of college going students is greater(97.67) but it would not make sense to use a model to predict the missing values.
# So,we will use the mean values
#View(data4)

data7= data4
#data7$College.education[1]
count=0
for(i in data7$Number.of.friends){
  count = count + 1
  if((is.na(i)) && (!is.na(data7$College.education[count]))){
    if(data7$College.education[count] == "No"){data7$Number.of.friends[i] <- 90
    } else if(data7$College.education[count] == "Yes") {data7$Number.of.friends[i] <- 97}} else if((is.na(i)) && (is.na(data7$College.education[count]))){data7 <- data7[-c(count),]}}
data8 = data7[!with(data7,is.na(data7$Number.of.friends)& is.na(data7$College.education)),]
#View(data8)
#as.factor(data8$College.education)
data8$College.education[with(data8,is.na(data8$College.education))] <- "Unknown"
#View(data8)
#summary(data4$diff_products_ordered[is.na(data4[,9])])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   1.000   1.000   1.392   1.000   6.000
data8 <- data8[!with(data8,is.na(data8$Number.of.friends)),]
#summary(data8)
#write.csv(data8,file = "/home/utkarsh/BITA/data8.csv")
data9 <- unique(data8)
#View(data9)
#write.csv(data9,file = "/home/utkarsh/BITA/data9.csv")
data10 <- data9
data10$Date <- as.Date(data10$Timestamp,format = "%d-%m-%Y")
data10$Time <- sapply(strsplit(as.character(data10$Timestamp)," "),"[",2)
data10$Date[is.na(data10$Date)] <- as.Date(data10$Timestamp[is.na(data10$Date)],format = "%d/%m/%Y")
data10$Date <- as.factor(data10$Date)
#View(data10)
data10 <- data10[,-c(11,12)] # Removing URL nad User_ID

#converts time hh:mm into hours only.
data10$Time <- sapply(strsplit(data10$Time,":"),
       function(x){
        x <- as.numeric(x)
        x[1]+x[2]/60  
        }
)
#View(data10)

#No of products ordered
data10$Num_of_products_ordered <- NA
ip_levels <- levels(data10$IP.Address)
for(ip in ip_levels){
  indexes <- which(data10$IP.Address == ip)
  data10$Num_of_products_ordered[indexes] <- length(indexes)
}
data10$Num_of_products_ordered <- as.numeric(data10$Num_of_products_ordered)
#View(data10)
cor(data10$Number.of.friends,data10$Num_of_products_ordered)
#0.006480331

#num_days_order
data10$num_days_order <- NA
ip_levels <- levels(data10$IP.Address)
for(ip in ip_levels){
  indexes <- which(data10$IP.Address == ip)
  data10$num_days_order[indexes] <- length(levels(factor(data10$Date[indexes])))
}

cor(data10$diff_products_ordered,data10$num_days_order)
#0.5414004

#difference_in_date
#for(date in levels(data10$Date)){
#  date_index <- which(data10$Date == date)
#  ip_date <- levels(factor(data10$IP.Address[date_index]))
#  for(ip in ip_date){
#    
#  }
#}

#Normalizing data
x <- data10$Number.of.friends
x <- (x - min(x))/(max(x)-min(x))
data10$Number.of.friends <- x
View(data10)

x <- data10$diff_products_ordered
x <- (x - min(x))/(max(x)-min(x))
data10$diff_products_ordered <- x

data11 <- data10

x <- data11$num_days_order
x <- (x - min(x))/(max(x)-min(x))
data11$num_days_order <- x

x <- data11$Num_of_products_ordered
x <- (x - min(x))/(max(x)-min(x))
data11$Num_of_products_ordered <- x

#View(data11)
#summary(data11$College.education)

data12 <- data11[,c(2,7,9,12,13)]
#View(data11)
#View(data12)
#cor(data12$diff_products_ordered,data12$Number.of.friends) # -0.01611921
#cor(data12$diff_products_ordered,data12$Num_of_products_ordered) #0.7738822
#cor(data12$diff_products_ordered,data12$num_days_order) #0.5414004

library(cluster)
fit <- kmeans(data12[,-c(2)],6)
#Here,for choosing the number of clusters,I used the number of product categories i.e 6 and it turned out quite good
summary(fit)
library(fpc)
plotcluster(data12[,-c(2)],fit$cluster)
myresult <- data.frame(data11, fit$cluster)
View(myresult)

---------------------------------------------------------------------------------

data9$College.education[data9$College.education == ""] <- "Unknown"
data9$College.education <- as.character(data9$College.education)
data9$College.education <- as.factor(data9$College.education)
levels(data9$College.education)
View(data9)

library(cluster)
fit <- kmeans(data9[,-c(1,3)],6)
summary(fit)

aggregate(data10[,-c(1,3)],by=list(fit$cluster),FUN=mean)
#Group.1 Number.of.friends diff_products_ordered
#      1          5.780598              1.313690
#      2        106.488964              1.263497Diff
mydata <- data.frame(data10[,c(1,3)], fit$cluster)
View(mydata)
summary(data10)
library(rattle)
rattle()
#Lets see the correlation b/w Number of friends and erent products ordered
cor(data10$Number.of.friends,data10$diff_products_ordered)
#  -0.01618106
# We see a negative correlation between them.
# If someone have more friends,they are likely to buy the same type of products.
# This might sound a little weird but the correlation suggests so.
library(cluster)
fit <- kmeans(data11,2)
summary(fit)
library(fpc)
plotcluster(data11,fit$cluster)
