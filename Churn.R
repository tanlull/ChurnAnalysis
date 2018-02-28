setwd("C:/Users/tan/git/ChurnAnalysis");
churnData <- read.csv("Telco-Churn.csv");
head(churnData);
#partner = wife hasbun
#tenure = how low we stay using the service
#dependents = have childerns
summary(churnData);
#total charge -> has NA -> why does it has NA ?  -> find root cause
library(tidyr)
library(dplyr)
#display total charge where is NA  -> tenure (never use srvice before) -> replace with 0 because it never use before
churnData %>% filter(is.na(TotalCharges))
#replace  Total charge with 0
churnData$TotalCharges[is.na(churnData$TotalCharges)] <- 0
summary(churnData); # NA has gone

# change senior citizen to factor (not calculate as number)  , SenoirCitizen  0,1 to Yes,No
# %>%  pipe data  , add  change SenoirCitizen data to Factor then remove customerID
#mutate -> add column 
#remove customerID
#get new data : churnData.1
churnData %>% 
  mutate(
    SeniorCitizen = factor(
        ifelse(SeniorCitizen ==1,"Yes","No")
      )
  ) %>%
  select (-customerID) -> churnData.1

summary(churnData.1)

# find Yes , no 
# monority data to merge 

# find number : distribute 
# 

# #1 check tenure fill with with sex 
library(ggplot2);
ggplot(churnData.1,
        aes(x=tenure, fill = gender)) +
  geom_histogram(bins = 15, color = "black")
       
# check tenure fill with with sex , normalize to 100 -> position = "fill"
library(ggplot2);
ggplot(churnData.1,
       aes(x=tenure, fill = gender)) +
  geom_histogram(bins = 15, color = "black" , position = "fill")

# change sex to Contract -> dependency 
library(ggplot2);
ggplot(churnData.1,
       aes(x=tenure, fill = Contract)) +
  geom_histogram(bins = 15, color = "black" , position = "fill")

# remove fill 
library(ggplot2);
ggplot(churnData.1,
       aes(x=tenure, fill = Contract)) +
  geom_histogram(bins = 15, color = "black")

# add facet grid distribute along with column 
library(ggplot2);
ggplot(churnData.1,
       aes(x=tenure, fill = Contract)) +
  geom_histogram(bins = 15, color = "black") +
  facet_grid(. ~ Contract)


#2 Check distirbution of Monthlycharge
ggplot(churnData.1,
       aes(x=MonthlyCharges)) +
  geom_histogram(bins = 15, color = "black")

# find corelation using lm
fit <- lm(MonthlyCharges ~ ., churnData.1)


#check p-value  Pr(>|t|)  *** , tenure , all usage
summary(fit)

# monthly < 26 has highvalue the explore  -> we get that they use PhoneService only
summary(churnData.1 %>% filter(MonthlyCharges<26))

#(FSelector) -- feature selectio n, require java -> can be used tree instead --> can be skipped 

#install.packages("FSelector");
library(FSelector);
options(scipen = T)
ig <- information.gain(Churn ~ ., churnData.1)
#check attr_importance  , tenure             0.07593293572
ig

#churn rate depend on tenure , the churn is sinificantly decreasing which depend on tenure 
ggplot(churnData.1,
       aes(x = tenure, fill = Churn)) +
  geom_histogram(bins = 20,
                 color = "black", position = "fill")

## NOW we have understand data

#churn not balance  No  :5163 , Yes :1869 
# around 25%
summary(churnData.1)

#data not balance
# 1. Do nothing , then adj treshold 
# 2. sampling data 50/50 then adjust ...

#tree based model: data not balance is OK
#this model 

set.seed(555)
# sample by position 
train_pos <- sample(nrow(churnData.1),0.6*nrow(churnData.1))
#get sampling data position 
train_pos
str(train_pos)
#get subset by row position 
churnData.train <- churnData.1[train_pos,]
churnData.test <- churnData.1[-train_pos,]

#entrophy, gini 
#decision tree : we have to know why we have to use this model --> math
library(rpart)
library(rpart.plot)
tree <- rpart(Churn ~ ., churnData.train)
#write to file 
pdf("tree.pdf")
rpart.plot(tree)
dev.off()
#close write to file

# we are not only consider class : yes/no, we also consider Prob. 
#seelct from high information gain from root ndoe  
rpart.plot(tree, cex=0.6)

# Prob have to compare with root node 
# node : yes 0.63 , is better that root node 0.27 -> around 3 times

tree$where

#Depth First Search ; represent number of node with number of data (node 15 -- 14% is expected to churn )
table(tree$where)

#which will returntrue
cond <- which(tree$where == 15)
#node 15 index data
cond
high_churn_node <- churnData.train[cond,]

#summary high churn data 
#if the value are the same then it will not effect the churn, Tech support , 
summary(high_churn_node)

#add depth tree node
#cp default value is 0.01 , adjust to 0.003

tree1 <- rpart(Churn ~ ., churnData.train,
              control = rpart.control(cp = 0.003))
pdf("tree1.pdf",width=15)
rpart.plot(tree1)
dev.off()

rpart.plot(tree1, cex=0.15)

#get high churn model at Yes 0.89 0.xx% -> use to similality search 

#Prediction 
#library caret : cross validation , tune model , confusion matrics  , 

library(caret)
#node type ->result get prob.  , or class
res <- predict(tree,churnData.test,type="class")
confusionMatrix(res,churnData.test$Churn,
                mode = "prec_recall",
                positive = "Yes"
                )
#Precision / Recall  : high recall =low precision , high precision = low recall 
# filter model -> Recall  
# target customer -> Precision

# F-measure high -> preision sync. with recall 


#Accuracy -> check with  NO Information Rate (random) -> must be better

res.p <- predict(tree, churnData.test)[,"Yes"]
res.p

#compare with random 
#data frame
lift_result <- data.frame(prob = res.p,
  y = churnData.test$Churn)

lift_result

lift_obj <- lift(y ~ prob,
                 data = lift_result,
                 class="Yes")
lift_obj
#select sample size 30% to get predict 60% of churn 
plot(lift_obj, values = 60)


# lift 2: another alternative way 
# evauate performace 
library(ROCR)
pred <- prediction(res.p, churnData.test$Churn)
perf_lift <- performance(pred, "lift", "rpp") #rpp rate of positive preiction
plot(perf_lift)

# lift 3 # anotehr way 
#install.packages("lift")
library(lift)
# top 10% 
TopDecileLift(res.p,
              1-as.integer(churnData.test$Churn))


#decision tree -> auto select features
library(randomForest)

#devide train data to trinn 4 and test 1
train_control = trainControl(method = "cv", number = 5)
metric = "Accuracy"
#package caret use train
model <- train(Churn ~ .,
               churnData.train,
               method = "rf",
               metric = metric,
               trControl = train_control)
model

#Resample , cappa (predict no/yes) , 

#create our model
#in one three use only 2 column 
rf <- randomForest(Churn ~ .,
                   churnData.train,
                   mtry = 2)

res <- predict(rf,
               churnData.test)
confusionMatrix(res,
                churnData.test$Churn,
                mode = "prec_recall",
                positive = "Yes")

#after that segmentation -> by elimiating 2-3 year and focus only less than 1 year 
#focus on spcific group / benefit (if worth then do it)
