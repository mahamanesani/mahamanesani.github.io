---
title: "Practical Machine Learning Course Project"
author: "Mahamane Sani Ibrahim"
date: "23/11/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(caret)
library(rpart) # Regressive Partitioning and Regression trees
library(rpart.plot) # Decision Tree plot
library(randomForest)
set.seed(1717)
setwd("L:/FEWSNET/Coursera/Data Science/project developing data models")
Training <- read_csv("pml-Training.csv")
Testing <- read_csv("pml-Testing.csv")


```
# Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it.
In this project, we will use data from accelerometers on the belt, forearm, arm and dumbell of 6 participants to predict the manner in which they did the exercise.

# Data cleansing and Exploratory data analysis
```{r}
dim(Training)
dim(Testing)
# Delete columns with all missing values
Training<-Training[,colSums(is.na(Training)) == 0]
Testing <-Testing[,colSums(is.na(Testing)) == 0]
# Delete variables are irrelevant to our current project: user_name, raw_timestamp_part_1, raw_timestamp_part_,2 cvtd_timestamp, new_window, and  num_window (columns 1 to 7). 
Training   <-Training[,-c(1:7)]
Testing <-Testing[,-c(1:7)]
Training$classe<-as.factor(Training$classe)
# partition the data so that 75% of the Training dataset into Training and the remaining 25% to Testing
traintrainset <- createDataPartition(y=Training$classe, p=0.75, list=FALSE)
TrainTraining <- Training[traintrainset, ] 
TestTraining <- Training[-traintrainset, ]
dim(Training)
ggplot(Training)+aes(x = classe)+stat_count()
```
The graph above shows that each level frequency is within the same order of magnitude of each other. Level A is the most frequent with more than 4000 occurrences while level D is the least frequent with about 2500 occurrences.

# Prediction using decision tree
```{r}
model1 <- rpart(classe ~ ., data=TrainTraining, method="class")
# Predicting:
predict1 <- predict(model1, TestTraining, type = "class")
# Plot of the Decision Tree
rpart.plot(model1, main="Classification Tree", extra=102, under=TRUE, faclen=0)
``` 

```{r}
# Test results on our TestTraining data set:
confusionMatrix(predict1, TestTraining$classe)
```
# Prediction using random forest
```{r}
model2 <- randomForest(classe ~. , data=TrainTraining, method="class")

# Predicting:
predict2 <- predict(model2, TestTraining, type = "class")

# Test results on TestTrainingSet data set:
confusionMatrix(predict2, TestTraining$classe)

```

# Conclusion
The Random Forest algorithm performed better than Decision Trees. It's accuracy was 0.995 (95% CI: (0.9925, 0.9967)) compared to Decision Tree model with 0.762 (95% CI: (0.7499, 0.7739)). So, we choose the Random Forests model. The expected out-of-sample error is estimated at 0.005, or 0.5%.

## Final prediction
Below, we show the result of the final prediction
```{r}
predictfinal <- predict(model2, Testing, type="class")
predictfinal
```