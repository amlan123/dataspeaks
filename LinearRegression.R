# Big Mart Sales Data

#load directory
getwd()

> setwd("C:/Users")

#load data
train <- read.csv("C:/Users/amlan/Documents/RPRAC/Train_UWu5bXk.csv")
test <- read.csv("C:/Users/amlan/Documents/RPRAC/Test_u94Q5KV.csv")

#create a new variable in test file 
test$Item_Outlet_Sales <- 1

#combine train and test data
combi <- rbind(train, test)

#impute missing value in Item_Weight
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)

#impute zero in item_visibility
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility),combi$Item_Visibility)

#rename level in Outlet_Size
levels(combi$Outlet_Size)[1] <- "Other"

#rename levels of Item_Fat_Content
library(plyr)
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,c("LF" = "Low Fat", "reg" = "Regular"))
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))

#create a new column 2012 - Year
combi$Year <- 2012 - combi$Outlet_Establishment_Year

#drop variables not required in modeling
library(dplyr)
combi <- select(combi, -c(Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year))

#divide data set
new_train <- combi[1:nrow(train),]
new_test <- combi[-(1:nrow(train)),]

#linear regression
linear_model <- lm(Item_Outlet_Sales ~ ., data = new_train)
summary(linear_model)
par(mfrow=c(2,2))
plot(linear_model)
linear_model <- lm(log(Item_Outlet_Sales) ~ ., data = new_train)
summary(linear_model)
plot(linear_model)

#check the R square value to know how better your model is.
