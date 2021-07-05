#The Spark foundation GRIP21
#Author- Rahul Kumar
# By using "R studio"
#Task1-: To predict the percentage score based on study hours=9.25 hrs/day

# Fist we need to install packages
install.packages("tidyverse")# To excute the code we can also used "CLT+ENTER"
#In my pc alreday these packages are indtalled, for me only have to connect with library.
library(tidyverse)

#importing the dataset
df= read.csv(url('http://bit.ly/w-data')) # used 'url' function
df
head(df) # it will give Top 6 rows, starting from 1.

#Check the data type 
str(df)

##Data preprocessing
#1:check the missing values
colSums(is.na(df))

#2.check the outlier
# positive_outlier = Q3+1.5*IQR
# negative_outlier = Q1-1.5*IQR
# IQR = Q3-Q1
boxplot(df$Hours) # not found

#3. Encoding concept:"we do not have any charater data" so we cannot use it.
#4. Feature scalling:feature scaling is also one of the techniques to handle outlier

#################################################################
#Since it is supervise machine learning, hence we have to split the data
# into training and testing for building the model and predicting the test data 

# Randomly splitting the dataset into training and test set
# please split data between 70% and 80% for training dataset
# and 20% to 30% for test dataset

install.packages("caTools")## splitting the data into training and test
library(caTools) # i already installed this packages

set.seed(100)# fixing the random number and please ensure to use number while using set.seed(number)
df1<- sample.split(df$Scores, SplitRatio = 0.80) # Split alway dependent variable
df1 # will give in boolean value : 80% True - training data and 20% False - Test

# Table will give how many True and false there
#True - training data
# false- testing data

table(df1)
#now we will train the model 
training<- subset(df,df1= T)
testing<- subset(df,df1= F)
#For all supervise machine learning, you have to use this
####################################################################

# Building the Linear Regression model with training dataset
# linear regression model function : lm(dv~., data=training)
#dv- dependent variable = Score
#idv_ independent variable= Hours

reg<- lm(Scores~ Hours,data = training)
reg
#by using summary() we will get the p-value
summary(reg)
# Hours idv statically significant because p-value ***(less than 0.05)

#our model accuracy is almost 95%,Adjusted R-squared:  0.9509
######################################################################

#now predict the model
own_pred<- predict(reg,newdata = testing)
own_pred
##Here I am predicting the score for study hours 9.25 

pred<- predict(reg, data.frame(Hours=9.25))
pred

# comapre actucal data and predicted data

df3<- cbind(testing$Scores,own_pred)
df3

# To plot graph and find the correlation between Hours & scores.
install.packages("ggpubr") #package to plot the graph
library(ggpubr)

#graph 
plot<-ggscatter(df, x = "Hours", y = "Scores", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "no.of hours studies", ylab = "perctange score")
plot






