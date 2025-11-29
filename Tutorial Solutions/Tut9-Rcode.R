
##############################  SOLUTION


############################## 

library(rpart)
library(rpart.plot)
library(class)
library(ROCR)
library(e1071)

setwd("~/Documents/Data")

# setwd("C:/Data")


# Q1

data = read.csv("diabetes-dataset-1k.csv")


dim(data)
# [1] 1000      7

head(data)

# hypertension: 0 = No; 1 = Yes
# heart_disease: 0= No; 1 = Yes
# diabetes: 0 = No; 1 = Yes

# DECLARE WITH R THE CATEGORICAL INPUTS:

data$hypertension = as.factor(data$hypertension)

data$heart_disease = as.factor(data$heart_disease)

set.seed(1101)

index = sample(1:1000) # randomly mix well a series of numbers from 1 to 1000.

train.set = data[c( index[1:800] ), ] # train data

test.set = data[-c( index[1:800] ), ] # test data

# NOTE: the outcome will be different if you choose to do as below though technically they are not wrong.

# test.set = data[c( index[1:200] ), ] # test data

# train.set = data[-c( index[1:200] ), ] # train data



# Q2

m = 1:50 #   

precision = numeric()

accuracy = numeric()

for (i in m) { 


dt <- rpart(diabetes ~ age + hypertension + heart_disease +
                bmi + HbA1c_level + blood_glucose_level,
                method="class",
                data=train.set,                      
                control=rpart.control(minsplit = i),  
                parms=list(split='information'))

# rpart.plot(dt, type=4, extra=2, varlen=0, faclen=0, clip.right.labs=FALSE)

pred.dt = predict(dt, newdata = test.set, type = "class")  

matrix = table(test.set$diabetes, pred.dt)  

precision = append(precision, matrix[2,2]/sum(matrix[,2] ) ) 

accuracy = append(accuracy, sum(diag(matrix))/sum(matrix)) 

	}

# precision

# accuracy

cbind(m, precision, accuracy)


# Q3

# Consider DT with m = 50

DT <- rpart(diabetes ~ age + hypertension + heart_disease +
                bmi + HbA1c_level + blood_glucose_level,
                method="class",
                data=train.set,
                control=rpart.control(minsplit = 50), 
                parms=list(split='information'))

rpart.plot(DT, type=4, extra=2, varlen=0, faclen=0, clip.right.labs=FALSE) 


pred.dt = predict(DT, test.set, type = "prob")[ ,2]  

pred.dt <- prediction(pred.dt, test.set$diabetes) 

perf.dt = performance(pred.dt, measure="tpr", x.measure="fpr")

plot(perf.dt, col = "black") # in BLACK color

# AUC VALUE of ROC curve for DT 
auc.dt = performance(pred.dt , measure ="auc")

auc.dt = auc.dt@y.values[[1]]; auc.dt  # 0.9753188
# 





#################   - NAIVE BAYES CLASSIFIER 

# Q4 

NB = naiveBayes(diabetes ~ age + hypertension + heart_disease +
                bmi + HbA1c_level + blood_glucose_level, data = train.set)



# Q5 
pred.nb = predict(NB,  test.set, type = "raw")[ ,2]

pred.nb <- prediction(pred.nb, test.set$diabetes)

perf.nb = performance(pred.nb, measure="tpr", x.measure="fpr")

plot(perf.nb, col = "blue") # in BLUE color


# AUC VALUE of ROC curve for NAIVE BAYES
auc.nb = performance(pred.nb , measure ="auc")

auc.nb = auc.nb@y.values[[1]]; auc.nb  # 0.9638009

# 



# Q6
delta <- round (as.numeric(unlist(perf.nb@alpha.values)) ,3)

fpr <- round(as.numeric(unlist(perf.nb@x.values)) ,3)

tpr <- round(as.numeric(unlist(perf.nb@y.values)) ,3)

# adjust margins and plot TPR and FPR
par(mar = c(5 ,5 ,2 ,5))

plot(delta ,tpr , xlab ="Threshold", xlim =c(0 ,1) ,
     ylab = "True positive rate ", type ="l", col = "blue")
     
par(new ="True")

plot(delta, fpr , xlab ="", ylab ="", axes =FALSE, 
                  xlim =c(0 ,1) , type ="l", col = "red" )
                  
axis(side =4) # to create an axis at the 4th side
mtext(side = 4, line = 3, "False positive rate")
text(0.5 ,0.06 , "FPR", col = "red")
text(0.8 ,0.7 , "TPR", col = "blue")




# Q7 
cbind(delta, tpr, fpr)

# It is subjective to choose threshold delta.
# it depends on the context of the study.  
# A suggestion: choose a value of delta such that tpr is high yet fpr is low.
# For example: Tpr is nearly 1, such as 0.923 while fpr is loswest amongst values that tpr = 0.923
# hence, threshold delta = 0.106 is quite reasonable.




# Q8
# when threshold = , then:

threshold = 0.106

pred.nb = predict(NB,  test.set, type = "raw")[ ,2]

# predicted status of test point using delta = 0.025
nb. = ifelse(pred.nb > threshold, 1, 0) 

mean(nb. == test.set$diabetes) # 0.875








#################  -  LOGISTIC REGRESSION MODEL 

# Q9
LR = glm(diabetes ~ age + hypertension + heart_disease +
                bmi + HbA1c_level + blood_glucose_level, 
                data = train.set, family = binomial)

summary(LR)



# Q10

# odds ratio = exp(0.885055) = 2.423118



# Q11
### ROC curve and AUC for LR
pred.logr = predict(LR, test.set, "response")

pred.logr = prediction(pred.logr, test.set$diabetes)

perf.logr = performance(pred.logr, measure="tpr", x.measure="fpr")

plot(perf.logr,  col = "red") # this is to plot the ROC curve in RED COLOR

# AUC VALUE
auc.logr = performance(pred.logr , measure ="auc")

auc.logr = auc.logr@y.values[[1]]; auc.logr  # 0.9744961




#################   - KNN


# # Q12

train.set[ , c(1,4,5,6)] = scale(train.set[ , c(1,4,5,6)])

test.set[ , c(1,4,5,6)] = scale(test.set[ , c(1,4,5,6)])

pred.knn= knn(train.set[,c(1,4,5,6)], test.set[,c(1,4,5,6)], train.set$diabetes, k = 3, prob=TRUE) 

# Q13
winning.prob <- attr(pred.knn, "prob")  
# to get the predicted probability for each test point to be the winning class 

prob = numeric(200) # to store the probability of "disease = 1" for 200 test points

for (i in 1:200){prob[i] = ifelse (pred.knn[i]==1, winning.prob[i], 1- winning.prob[i])}


# Q14
knn. = ifelse(prob > threshold, 1, 0)

mean(knn. == test.set$diabetes) # accuracy = 0.89


######## END #############################################

