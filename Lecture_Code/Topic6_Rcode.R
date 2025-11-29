
# setwd("C:/Data") # for Windows

setwd("~/Documents/Data") # for Mac

#############  EXAMPLE 1: CLASSIFYING FRUITS

fruit.dat= read.csv("fruit.csv")
#Long: 1 = Yes, 0 = No
#Sweet: 1 = Yes, 0 = No
#Yellow: 1 = Yes, 0 = No
fruit.dat<- data.frame(lapply(fruit.dat, as.factor))
head(fruit.dat)
attach(fruit.dat)

table(Long)
table(Sweet)
table(Yellow)

#Install package 'e1071' first
#install.packages("e1071")
library(e1071)

model <- naiveBayes(Fruit ~ Long+Yellow+Sweet,fruit.dat)


newdata <- data.frame(Long=1,Sweet=1, Yellow=0)
newdata <- data.frame(lapply(newdata, as.factor))

results <- predict (model,newdata,"raw")
results

results <- predict (model,newdata,"class") # default setting
results



######## EXAMPLE 2: EMPLOYEE & ONSITE EDUCALTIONAL PROGRAM


sample <- read.table("sample1.csv",header=TRUE,sep=",")
head(sample)
dim(sample)
sample
# Enrolls = RESPONSE with 2 categories

######### PART 1: MANUAL FORMING NAIVE BAYES CLASSIFIER ######

traindata <- as.data.frame(sample[1:14,])
testdata <- as.data.frame(sample[15,])
testdata


tab <- table(traindata$Enrolls)
tab = prop.table(tab)

# Get P(X = xi|Y = yj): row-wise proportion for feature AGE
ageCounts <- table(traindata[,c("Enrolls", "Age")]);ageCounts


age.pro = prop.table(ageCounts, margin = 1)
age.pro # this is P(Age = xi|Y = yj): row-wise proportion for feature AGE



# Get P(Income = xi|Y = yj): row-wise proportion for feature INCOME
incomeCounts <- table(traindata[,c("Enrolls", "Income")])


income.pro = prop.table(incomeCounts, margin = 1)
income.pro # this is P(Income = xi|Y = yj): row-wise proportion for feature INCOME

# Get P(X = xi|Y = yj): row-wise proportion for feature JOBSATISFACTION
jsCounts <- table(traindata[,c("Enrolls", "JobSatisfaction")])


js.pro = prop.table(jsCounts, margin = 1)
js.pro


# Get P(X = xi|Y = yj): row-wise proportion for feature DESIRE
desireCounts <- table(traindata[,c("Enrolls", "Desire")])


desire.pro = prop.table(desireCounts, margin = 1)
desire.pro


# point 15 (test point) has:
#    Age Income JobSatisfaction Desire Enrolls
#   <=30 Medium             Yes   Fair

# Proportion that point 15 will be "Yes" for the outcome IS PROPORTIONAL TO "prob_yes" defined below:
prob_yes <-
age.pro["Yes",testdata[,c("Age")]]*   # this is row "Yes", column <=30 of age.pro
income.pro["Yes",testdata[,c("Income")]]*  # row "Yes", column Medium of income.pro
js.pro["Yes",testdata[,c("JobSatisfaction")]]*  # row Yes, column Yes of js.pro
desire.pro["Yes",testdata[,c("Desire")]]*   # row Yes, column Desire of desire.pro
tab["Yes"] # proportion of Yes in response of train data.

# Proportion that point 15 will be "No" for the outcome IS PROPORTIONAL TO "prob_no" defined:
prob_no <-
age.pro["No",testdata[,c("Age")]]*
income.pro["No",testdata[,c("Income")]]*
js.pro["No",testdata[,c("JobSatisfaction")]]*
desire.pro["No",testdata[,c("Desire")]]*
tab["No"]

#MAKING DECISION:
prob_yes/prob_no #4.115226. 
# Hence the 15th observation should be classified as YES.



######### PART 2: USE PACKAGE e1071 FORMING NAIVE BAYES CLASSIFIER ######

library(e1071)

model <- naiveBayes(Enrolls ~ Age+Income+JobSatisfaction+Desire, traindata)

results <- predict(model,testdata,"raw"); results

results[2]/results[1] # 4.115226


results <- predict(model,testdata,"class"); results


##### EXTRA: CHECK THE GOODNESS OF FIT OF THE CLASSIFIER by ACCURACY


pred = predict(model, traindata, "class")
pred

data.frame(traindata[,5], pred) 
# for reference to see the difference between real response and predicted response from NB classifier

confusion.matrix = table(traindata[,5], pred) # 10/14 are predicted as Yes

accuracy = sum(diag(confusion.matrix))/sum(confusion.matrix); 

accuracy 


pred = predict(model, traindata, "raw")

# consider threshold delta = 0.7
# which(pred[,2] >=0.7) # only 8/14 are predicted as "Yes"


############### BANK-SAMPLE DATA ==> ROC and AUC


banktrain <- read.csv("bank-sample.csv", header=TRUE)
dim(banktrain)
head(banktrain)

# drop 6 UNNECESSARY columns for the TRANNING DATA SET:

drops <- c("balance", "day", "campaign", "pdays", "previous", "month")

banktrain <- banktrain [,!(names(banktrain) %in% drops )]
# this means we DO NOT TAKE THE COLUMNS THAT HAVE THE SAME HEADER AS IN "drops"

# TESTING DATA SET:

banktest <- read.csv("bank-sample-test.csv")

dim(banktest) # 100 rows and 17 columns

head(banktest) # the columns have the same names/header as in TRAIN set


# drop 6 UNNECESSARY columns for the TEST DATA SET:

banktest <- banktest[,!( names ( banktest ) %in% drops )]

library(e1071)

# build the naive Bayes classifier USING THE TRAIN SET
nb_model <- naiveBayes( subscribed ~ . , data = banktrain) # 

# When forming the model nb_model above, we let response "subscribed" to depend on ALL THE REST COLUMNS in the data set.


# banktest has a total of 11 columns. 
# 11th column is the LAST column, is the RESPONSE

nb_prediction <- predict(nb_model, newdata = banktest[  , -11], type ='raw')

# IT'S RECOMMENDED TO TAKE OUT the response for the test set because the model was formed as: 
# response depends on all the rest columns: subscribed ~.
# if we form the model where we do not use the dot but we specify the names of each regressor explicitly: subscribed ~ age + job + marital + education + default + housing + loan + contact + duration + poutcome
# then we DO NOT NEED TO TAKE OUT the response in the test set in the function predict() above.

nb_prediction
# this is the predicted response for the TEST set which includes the RAW PROBABILITY of no/yes for each point
# the first column is the probability of no (WILL NOT SUBSCRIBE)
# the second column is the probability of yes (WILL SUBSCRIBE)

predicted.response = round(nb_prediction, digits = 3) 
# just rounding the probabilities to 3 decimal places

cbind(predicted.response, banktest[  , ncol(banktest)])
# compare the predicted probability and the REAL RESPONSE


predict(nb_model, newdata = banktest[  , -11], type ='class')




# PLOT ROC CURVE FOR THE NAIVE BAYES CLASSIFIER ABOVE:
#install.packages("ROCR") 
# https://cran.r-project.org/web/packages/ROCR/ROCR.pdf

library(ROCR)
score <- nb_prediction[, c("yes")] 
# score is the conditional prob from Naive Bayes classifier for each test point

actual_class <- banktest$subscribed == 'yes' 
# change the actual response from "no" and "yes" to 0 and 1

pred <- prediction(score , actual_class) 
# this is to "format" the input so that we can use the function in ROCR to get TPR and FPR

perf <- performance(pred , "tpr", "fpr")

plot (perf, lwd =2) # lwd is to specify how thick the curve is
abline (a=0, b=1, col ="blue", lty =3)
# the straight blue line is just a reference line


# COMPUTE AUC FOR NAIVE BAYES CLASSIFIER:
auc <- performance(pred , "auc")@y.values[[1]]

auc
# auc is used to compare between Naive Bayes method with other methods
# such as linear model, logistic model, DT, etc. 
# the one with larger auc value is better.


# VISUALIZE ON HOW THE THRESHOLD CHANGES WILL CHANGE TPR AND FPR:
# threshold is denoted as delta in the lecture slides of DSA1101,
# however, in R settings, it is named as "alpha.values"

threshold <- round (as.numeric(unlist(perf@alpha.values)) ,4) 
fpr <- round(as.numeric(unlist(perf@x.values)) ,4)
tpr <- round(as.numeric(unlist(perf@y.values)) ,4)

# adjust margins and plot TPR and FPR
par(mar = c(5 ,5 ,2 ,5))
# mar = a numerical vector of the form c(bottom, left, top, right) = c(5,4,4,2)
# http://127.0.0.1:14187/library/graphics/html/par.html

plot(threshold ,tpr , xlab ="Threshold", xlim =c(0 ,1) ,
     ylab = "True positive rate ", type ="l", col = "blue")
par( new ="True")
plot(threshold ,fpr , xlab ="", ylab ="", axes =F, xlim =c(0 ,1) , type ="l", col = "red" )
axis(side =4) # to create an axis at the 4th side
mtext(side =4, line =3, "False positive rate")
text(0.4 ,0.05 , "FPR", col = "red")
text(0.6 ,0.35 , "TPR", col = "blue")

cbind(threshold, tpr, fpr) # for reference

