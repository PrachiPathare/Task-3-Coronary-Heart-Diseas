options(warn = -1 )
#loading the libraries
library(ggplot2)
library(caret)
library(rpart.plot)
library(DataExplorer)
library(tidyverse)
library(psych)
library(pROC)

#Loading the Data

data = read.csv('Data.csv',row.names = 1,stringsAsFactors = FALSE)

str(data)
summary(data)

#Getting rid of the Missing Values
data$A2[is.na(data$A2)] = 8
data$A15[data$A15 == -99] = 0
data$A16[data$A16 == -99] = 0




#removing outliers
#IV

o_1 =  boxplot(data$IV, plot = T)$out
x = data
x = x[-which(x$IV %in% o_1),]

histogram(x$IV)

#A1

o_2 =  boxplot(data$A1, plot = T)$out
x = x[-which(x$A1 %in% o_2),]

histogram(x$A1)

#A2

o_3 =  boxplot(data$A2, plot = T)$out
x = x[-which(x$A2 %in% o_3),]

histogram(x$A2)
#A3

o_4 =  boxplot(data$A3, plot = T)$out
x = x[-which(x$A3 %in% o_4),]

histogram(x$A3)
#A4

o_5 =  boxplot(data$A4, plot = T)$out
x = x[-which(x$A4 %in% o_5),]

histogram(x$A4)
#A5

o_6 =  boxplot(data$A5, plot = T)$out
x = x[-which(x$A5 %in% o_6),]

histogram(x$A5)
#A6

o_7 =  boxplot(data$A6, plot = T)$out
x = x[-which(x$A6 %in% o_7),]
histogram(x$A6)

#A7

o_8 =  boxplot(data$A7, plot = T)$out
x = x[-which(x$A7 %in% o_8),]
histogram(x$A7)

#A8

o_9 =  boxplot(data$A8, plot = T)$out
x = x[-which(x$A8 %in% o_9),]
histogram(x$A8)

#A9

o_10 =  boxplot(data$A9, plot = T)$out
x = x[-which(x$A9 %in% o_10),]
histogram(x$A9)

#A10
histogram(data$A10)
o_11 =  boxplot(data$A10, plot = T)$out
x = x[-which(x$A10 %in% o_11),]
histogram(x$A10)

#A12

o_13 =  boxplot(data$A12, plot = T)$out
x = x[-which(x$A12 %in% o_13),]
histogram(x$A12)

data = x

#eda univariate

data %>% introduce()
data %>% plot_intro()
data %>% plot_missing()


#studying the continuous features

data %>% plot_histogram()

#for categorial variables

data %>% plot_bar()


#Multivariate EDA
#relationship

corPlot(data)

data$Target = as.factor(data$Target)
#giving level to categorial variable
levels(data$Target) = c('yes','no')


# Model Building
set.seed(8)
train_Indices = createDataPartition(data$Target, p = 0.8, list = FALSE)
data.train = data[train_Indices,] 
data.test = data[-train_Indices,]


#10 Fold cross validation
mycontrol = trainControl(method = "cv", number = 11)



#Decision Tree
set.seed(8)
decision_tree = train(Target~. ,
                      data = data.train,
                      method = "rpart",
                      trControl = mycontrol,
                      tuneLength = 5
                     )
decision_tree

plot(decision_tree)

rpart.plot(decision_tree$finalModel)

#Prediction on test data
prediction = predict(decision_tree, newdata = data.test, type = "raw")
plot(prediction)

prediction_prob = predict(decision_tree, newdata = data.test, type = "prob")
plot(roc(data.test$Target,prediction_prob[,2] ))

cm_dt = confusionMatrix(prediction, data.test$Target)

cm_dt
cm_dt$byClass[7]
cm_dt$byClass[6]
cm_dt$byClass[5]









#Decision tree classification model is giving best accuracy value of 69%

#Prediction on test data



                






  
 









