
#setwd("C:/Data")

setwd("~/Documents/Data")

# TUTORIAL 9 

library(ROCR)
library(e1071)

################# ON-SITE QUESTIONS = Smarket DATA SET


market = read.csv("Smarket.csv")

#summary(market[,2:10])

head(market)

dim(market)

# (a)
tab = table(market$Direction)

prop.table(tab)

# (b)

market$y = ifelse(market$Direction=="Up", 1, 0)

table(market$y)
prop.table(table(market$y)) # 51.84% of the days are UP

head(market)


M3 <- glm(y~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=market, family=binomial)

summary(M3)


# get the accuracy of the Logistic model above

pred.M3 = predict(M3, newdata = market, type = "response")
# this is the probability of y = 1 (Direction = Up) for each day

# consider 0.5184 as the borderline to predict the direction be up or down

pred.direction = ifelse(pred.M3 >=0.5184, "Up", "Down")

confusion.matrix = table(market$Direction, pred.direction)

accuracy = sum(diag(confusion.matrix))/sum(confusion.matrix)
accuracy



# for ROC curve and get AUC value for model M3

pred.M3 <- predict(M3, market,type='response')
pred <- prediction(pred.M3, market$y)
roc = performance(pred, measure="tpr", x.measure="fpr")
plot(roc, col = "blue") 



auc <- performance(pred , "auc")@y.values[[1]]; auc
# 0.5387341





#################### OFF-SITE QUESTIONS = TITANIC FOR LOGISTIC REGRESSION


titanic= read.csv("Titanic.csv")
dim(titanic)
head(titanic)

# attach(titanic)

table(titanic$Survived)



# (a) Logistic regression

titanic$sur = ifelse(titanic$Survived == 'Yes', 1, 0) # change the response to 0 and 1 to fit the model

# titanic$sur = as.factor(titanic$sur) # OPTIONAL TO RUN THIS LINE OF CODE

head(titanic)

M2 <- glm(sur~ Class + Sex + Age, data=titanic, family=binomial(link= "logit"))

summary(M2)

# (b) write down the fitted model, phat = predicted probability of survival:

# log[phat/(1-phat)] = 2.0438 -1.0181*I(Class = 2nd) -1.7778*I(Class = 3rd) 
#                   -0.8577* I(Class = Crew) -2.4201*I(Sex = Male) + 1.0615*I(Age = Child)

# (c) Interpret the coefficient of variable SEX:

# FEMALE IS REFERENCE. MALE IS INDICATED BY INDICATOR.
# coefficient is estimated = -2.4201. 
# It means, given the same condition on the class and age,
# when comparing to a female, the LOG-ODDS of survival for a male is less than by 2.42.
# It means, the ODDS of survival of a male passenger will be less than that of a female by
# e^2.42 = 11.25 TIMES.


# (d) Interpret the coefficient of variable AGE:
 
# "ADULT" IS CHOSEN AS REFERENCE. CATEGORY "CHILD" IS INDICATED BY AN INDICATOR.
# coefficient is estimated = 1.0615.
# It means, given the same condition on the class and gender,
# when comparing to an adult, the LOG-ODDS of survival of a child is larger by 1.0615.
# That means, the ODDS of survival of a child passenger is larger than that of an adult passenger by 
# e^1.0615 = 2.89 TIMES.


# (e) ROC & AUC
#different classfiers produce different performances
#plot 2 ROC curves in the same plane

library(ROCR)

# ROC for Logistic Regresison:
pred = predict(M2, type="response") # type = response to get the probability of survived
# this pred is a vector of probabilities of success: sur = 1 (Survived = Yes);

pred_log = prediction(pred, titanic$Survived)
roc_log = performance(pred_log, measure="tpr", x.measure="fpr")
plot(roc_log, col = "red")




# ROC for Naive Bayes classifier
library(e1071)

# IN PREVIOUS TUTORIAL, THE CODE WE USED TO FORM NAIVE BAYES IS: 
# M1 <- naiveBayes(Survived ~ Class + Sex + Age, data = titanic)

# HERE, WE USE RESPONSE sur, INSTEAD OF Survived, to match with model M2 above.
M1 <- naiveBayes(sur ~ Class + Sex + Age, data = titanic)

pred.M1 <- predict(M1, titanic[,1:3],type='raw')
# this pred.M1 has two columns: first column is probability of sur = 0 (Survived = 0) = died;
# second column is probability of sur = 1 (Survived = Yes).
pred.M1 <- pred.M1[, 2] # select the second column, probability of survived only.
pred_nb <- prediction(pred.M1, titanic$sur)
roc_nb = performance(pred_nb, measure="tpr", x.measure="fpr")
plot(roc_nb, add = TRUE, col = "blue") #add = TRUE means plot the curve in the existing plot

# MUST HAVE "add = TRUE" to add this curve to the existing plot  before



legend("bottomright", c("logistic regression","naive Bayes"),col=c("red","blue"), lty=1)



auc1 = performance(pred_log , measure ="auc")
auc1@y.values[[1]] # 0.7597259


auc2 <- performance(pred_nb , "auc")@y.values[[1]]
auc2 #0.7164944



