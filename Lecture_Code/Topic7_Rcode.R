
# LOGISTIC CURVE

z = seq ( -10 ,10 ,0.1);
logistic = function (z) {exp(z)/(1+ exp(z))}

plot(z, logistic(z), xlab ="x", ylab ="p", lty =1, type ='l')


##########  DATA SET ON CUSTOMER CHURN

setwd("~/Documents/Data")

data = read.csv("churn.csv")
head(data)

data$Churned = as.factor(data$Churned)
data$Married = as.factor(data$Married)
data= data[,-1] #Remove ID column

attach(data)

table(Churned)
prop.table(table(Churned)) # 22% customers churned in the data


# LOGISTIC MODEL
M1<- glm( Churned ~., data = data,family = binomial)
summary(M1)
# if we don't specify "family = binomial", then a LINEAR model is formed, not logistic model


M2<- glm( Churned ~ Age + Married + Churned_contacts,
 data = data,family = binomial(link ="logit"))
summary(M2)

M3<- glm( Churned ~Age + Churned_contacts,
 data = data,family = binomial(link ="logit"))
summary(M3)

predict(M3, newdata = data.frame(Age = 50, Churned_contacts = 5), type = 'response')
# type = 'response' means we want to get the Pr(Y = 1).
# type = c("link", "response", "terms")
# for glm(), when family = binomial, the default predictions are of log-odds (probabilities on logit scale) 
# and type = "response" gives the predicted probabilities for response Y = 1.





# ROC CURVE FOR LOGISTIC MODEL

library(ROCR)
prob = predict(M3, type ="response")

# above is to predicted probability Pr(Y = 1) for each point in the training data set, using M3
# type = c("link", "response", "terms"). 
# http://127.0.0.1:14187/library/stats/html/predict.glm.html

pred = prediction(prob , Churned ) 
roc = performance(pred , "tpr", "fpr")
auc = performance(pred , measure ="auc")
auc@y.values[[1]]
plot(roc , col = "red", main = paste(" Area under the curve :", round(auc@y.values[[1]] ,4)))



# HOW TPR, FPR CHANGE WHEN THRESHOLD CHANGES:

# extract the alpha(threshold), FPR , and TPR values from roc
alpha <- round (as.numeric(unlist(roc@alpha.values)) ,4)
length(alpha) 
fpr <- round(as.numeric(unlist(roc@x.values)) ,4)
tpr <- round(as.numeric(unlist(roc@y.values)) ,4)

# adjust margins and plot TPR and FPR
par(mar = c(5 ,5 ,2 ,5))

plot(alpha ,tpr , xlab ="Threshold", xlim =c(0 ,1) ,
     ylab = "True positive rate ", type ="l", col = "blue")
par( new ="True")
plot(alpha ,fpr , xlab ="", ylab ="", axes =F, xlim =c(0 ,1) , type ="l", col = "red" )
axis( side =4) # to create an axis at the 4th side
mtext(side =4, line =3, "False positive rate")
text(0.18 ,0.18 , "FPR", col = "red")
text(0.58 ,0.58 , "TPR", col = "blue")


# there are some metrics that can help to choose a threshold: G-mean; Youden’s J statistic; etc

cbind(alpha, tpr,fpr)









#########################. EXTRA






n = nrow(data) # Get the total number of rows in the dataset

test.index = sample(1:n, size = floor(0.2 * n)) # Randomly select 20% of indexes for the test set

# in some cases where 0.2*n is NOT INTEGER, then floor() will take the integer just less than 0.2*n

train.set = data[-test.index, ] # Create the training set by excluding test.index

test.set = data[test.index, ]   # Create the test set using test.index


dim(test.set) # 
dim(train.set) # 


M<- glm( Churned ~., data = train.set,family = binomial)
summary(M)

prob = predict(M, newdata = test.set, type ="response")

pred = prediction(prob , test.set$Churned ) 
roc = performance(pred , "tpr", "fpr")
auc = performance(pred , measure ="auc")
auc@y.values[[1]]
plot(roc , col = "red", main = paste(" Area under the curve :", round(auc@y.values[[1]] ,4)))



