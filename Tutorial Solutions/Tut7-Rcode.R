
###### Q1:  TITANIC DATA SET & NAIVE BAYES

# setwd("~/Documents/Data")

setwd("C:/Data")


titanic= read.csv("Titanic.csv")
dim(titanic)
head(titanic)

# attach(titanic)

tab = table(titanic$Survived)

prop.table(tab)

# (a) Compute the probabilities P(Y = 1) (survived) and P(Y = 0) (did not survive).

tab = table(titanic$Survived)

prop.table(tab) # proportion table of the response


# (b) Compute the conditional probabilities P(Xi = xi|Y = 1) and P(Xi = xi|Y = 0):

# RAW FREQUENCY between response and class is
table(titanic[,c("Survived", "Class")]) # Response is in the ROWS

# # ROW-WISE PROPORTIONS
class.pro <- prop.table(table(titanic[,c("Survived", "Class")]), margin = 1) 
class.pro


# RAW FREQUENCY between response and gender is
table(titanic[,c("Survived", "Sex")]) # Response is in the ROWS
# # ROW-WISE PROPORTIONS
gender.pro <- prop.table(table(titanic[,c("Survived", "Sex")]), margin = 1) 
gender.pro


# RAW FREQUENCY between response and gender is
table(titanic[,c("Survived", "Age")]) # Response is in the ROWS
# # ROW-WISE PROPORTIONS
age.pro <- prop.table(table(titanic[,c("Survived", "Age")]), margin = 1) 
age.pro


# (c) Predict survival for an adult female passenger in 2nd class cabin.

prob_survived <- class.pro["Yes","2nd"]* gender.pro["Yes","Female"]* 
  age.pro["Yes","Adult"]* prop.table(tab)["Yes"]


prob_NOT_survived <- class.pro["No","2nd"]* gender.pro["No","Female"]* 
  age.pro["No","Adult"]* prop.table(tab)["No"]


prob_survived
prob_NOT_survived

prob_survived/prob_NOT_survived


# (d) Compare your prediction above with the one performed by the naiveBayes()
#function in package `e1071'

library(e1071)

M1 <- naiveBayes(Survived ~ Class + Sex + Age, titanic)

test <- data.frame(Class="2nd", Sex="Female", Age="Adult")

predicted.label <- predict(M1,test)
predicted.label


predicted.prob <- predict(M1,test, "raw")
predicted.prob


# ratio of two proportional probabilities from MANUAL CALCULATION
prob_survived/prob_NOT_survived


# ratio of two actual probabilities of survived and not survived:
predicted.prob[1,"Yes"]/ predicted.prob[1,"No"]



################# CONCLUDE: SAME ANSWER COMPARED TO MANUAL CALCULATION



####################  TITANIC DATA SET & DECISION TREE



##########   
library("rpart")
library("rpart.plot")


M2<- rpart(Survived ~ Class + Sex + Age,
            method ="class",
            data = titanic,
            control = rpart.control(minsplit = 1),
            parms = list(split ='information'))

# Q2(b)

rpart.plot(M2 , type =4, extra =2, clip.right.labs = FALSE , varlen =0, faclen =0)


pred.M2 = predict(M2, newdata = titanic[,1:3], type = 'class') # ,type = 'prob')

# type = 'class' to get the class labels 
# type = 'prob' to get the probability of winning class


data.frame(pred.M2, titanic$Survived)[1375:1390,] # some 20 ROWS in the middle of the data


#Q2c

############################ ROC & AUC for both NAIVE BAYES & DECISION TREES:
############################


# different classifiers produce different performances
# plot 2 ROC curves in the same plane

library(ROCR)
head(titanic)

# WE WOULD WANT TO CHANGE THE RESPONSE FROM YES/NO TO 1/0 (BUT not a MUST)

titanic$Survived = ifelse(titanic$Survived=="Yes", 1, 0)


####  FOR NAIVE BAYES CLASSIFIER:

# ROC for Naive Bayes classifier
pred.M1 <- predict(M1, titanic[,1:3],type='raw')   # get the probabilities: P(Y =No) and P(Y = Yes)
score <- pred.M1[, 2] # ONLY TAKE THE PROBABILITY OF YES in second column

pred_nb <- prediction(score, titanic$Survived)
roc_nb = performance(pred_nb, measure="tpr", x.measure="fpr")
plot(roc_nb,  col = "red") 

auc1 <- performance(pred_nb , "auc")@y.values[[1]]; auc1
#0.7164944


########  FOR DECISION TREES:


#######################################  
############### 
############### WE FORM THE TREE to GET THE PROBABILITIES
############### INSTEAD OF GETTING THE CLASS LABELS FOR THE OUTCOME


M2<- rpart(Survived ~ Class + Sex + Age, 
            method ="class",
            data = titanic,
            control = rpart.control(minsplit = 1),
            parms = list(split ='information'))


#rpart.plot(M2 , type =4, extra =2, clip.right.labs = FALSE , varlen =0, faclen =0)


#by probabilities

pred.M2 = predict(M2, newdata = titanic[,1:3], type = 'prob') # GET THE PROBABILTIES of Yes and No, NOT THE CLASS


##################### ROC for Decision Trees:


pred.M2 = predict(M2, titanic[,1:3], type='prob') # GET THE PROBABILTIES
score2 = pred.M2[,2] # ONLY TAKE THE PROBABILITY OF YES


pred_dt = prediction(score2, titanic$Survived)
roc_dt = performance(pred_dt, measure="tpr", x.measure="fpr")

plot(roc_nb,  col = "red") # Naive Bayes ROC 
plot(roc_dt, add = TRUE) # Decision tree

legend("bottomright", c("Naive Bayes","Decision Trees"),col=c("red","black"), lty=1)


auc2 = performance(pred_dt , measure ="auc")
auc2@y.values[[1]] 
# 0.7262628


###############  CONCLUDE: DECISION TREE IS slightly BETTER THAN NAIVE BAYES with a bit larger AUC value

# when forming the ROC curve, it's better to use the PROBABILITIES of (Y = Yes), NOT THE CLASS








