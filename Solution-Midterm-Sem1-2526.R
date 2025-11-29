

setwd("C:/Data")

setwd("~/Documents/Data")

data = read.csv("ford.csv")

head(data)


# DO NOT USE THIS WAY
# data = read.csv("~/Documents/Data/ford.csv") 


# Q1  (2 points) Write code to change all "Other" to "Hybrid" for \textbf{fuelType}

data$fuelType = ifelse(data$fuelType == "Other",  "Hybrid", data$fuelType)



# Q2  (2 points) Write code to remove any data with invalid \textbf{year}
# Report the number of rows in the data

data = data[(data$year >= 1996 & data$year <= 2020),]
# 17964 rows



# Q3  (2 points) Create a contingency table (named \textbf{tab1}) for 
# \textbf{transmission} and \textbf{fuelType}. Report the number of cars that 
# use petrol and also have an automatic transmission.

tab1 = table(data$transmission, data$fuelType); tab1

# 773 cars



# Q4  (2 points) Write code to create box plots of car's price by groups 
# of years for years 2016 to 2020. Give your comments.

boxplot(price ~ year, data = data[data$year>=2016 & data$year<=2020,])

# comments: The group of cars registered in 2020 has HIGHEST MEDIAN PRICE
# with an increasing trend from 2016 to 2020.
# possible association: newer cars have higher price.



# Q5  (3 points) Write code to create a new column for \textbf{data}, 
# named \textbf{priceHL}, which equals to ``high'' if the \textbf{price} 
# of the car is above 12500 and equals to ``low'' otherwise. 
# Create a contingency table (named \textbf{tab2}) for \textbf{priceHL} 
# and \textbf{transmission} but only for ``Automatic'' and ``Manual'' cars.

data$priceHL = ifelse(data$price > 12500,  "high", "low")

tab2 = table(data$transmission[data$transmission!="Semi-Auto"], 
             data$priceHL[data$transmission!="Semi-Auto"]); tab2



# Q6  (3 points) Using \textbf{tab2}, write one command in R to find the two probabilities below. 
# (i) the probability of having high price car in the group of cars with automatic transmission 
# (ii) the probability of having high price car in the group of cars with manual transmission
# Report the difference of the two probabilities above and interpret the meaning of that difference.

prop.table(tab2, 1)
#                high       low
# Automatic 0.6830882 0.3169118
# Manual    0.3621190 0.6378810

# percentage of having high price car in the group of cars with automatic transmission = 0.683
# which is  higher than that among the group of cars with manual transmission = 0.362.
# difference = 0.683 - 0.362 = 0.321

# comments on the difference: the large difference suggests possible strong association between 
# transmission type and high price car, that the automatic transmission cars may have higher price.



################ PART II: Linear Regression (10 points)
# For the questions in this Part II, we would want to form a linear regression 
# using variables \textbf{fuelType}, \textbf{transmission}, \textbf{mileage} 
# and \textbf{mpg} to predict a car's \textbf{price}.



# Q7 (2 points) Write code to find the correlation between \textbf{price} and \textbf{mileage}. 
# Compare with the correlation between \textbf{1/price} and \textbf{mileage}. 
# Give your comments.

cor(data$price, data$mileage)
cor(1/data$price, data$mileage)

# the strength of the correlation increased from 0.531 to 0.579, suggesting that
# the use of 1/price as a response might be better when using mileage as the only
# quantitative variable



# Q8 (2 points) Write code to create a linear model, called \textbf{model1}, which uses 
# \textbf{fuelType}, \textbf{transmission}, \textbf{mileage} and \textbf{mpg} 
# to predict a car's \textbf{price}. 
# Show the coefficients of \textbf{model1}.

data$fuelType = as.factor(data$fuelType)
data$transmission = as.factor(data$transmission)
model1 = lm(price ~ fuelType+transmission+mileage+mpg, data=data)
model1 #or summary(model1)



# Q9 (2 points) Using \textbf{model1}, predict the \textbf{price} of a car which has a manual 
# transmission, runs on petrol at 60 miles per gallon, and has a mileage of 30000. 

predict(model1, newdata = data.frame(transmission="Manual", fuelType="Petrol", mpg=60, mileage=30000))

# 9256.379 pounds



################ PART III: KNN (25 points)

# For the questions in this Part III, we would want to form KNN classifiers using numeric input features 
# \textbf{mileage}, \textbf{tax}, \textbf{mpg} and \textbf{engineSize} which help to predict 
# if a car will be sold as high-price or not. Hence, we would consider \textbf{priceHL} as the response variable 
# and high-price is considered as positive. 

# Q10 (2 points) Write code to create a new data frame, called \textbf{data.KNN} for which its first column 
# is the column of the response; and other columns are
# \textbf{mileage}, \textbf{tax}, \textbf{mpg} and \textbf{engineSize} \textbf{after standardization} 
# for all observations. 

data.KNN = data[, c(10, 5,7,8,9)]; head(data.KNN)

data.KNN[,2:5] = scale(data.KNN[,2:5])     


# Q11 (2 points) Run the command \textbf{set.seed(210)}.

# Then, write R code to randomly split \textbf{data.KNN} into two groups of equal size: 
# one group will be the train set, named as \textbf{train.set} and other group has 
# the rest of rows which will be the test set, named as \textbf{test.set}.

n = dim(data.KNN)[1];n # n = 17964, even number
n/2

set.seed(210)       
index = sample(1:n) 


data.train = data.KNN[index[1:n/2], ] # TRAIN set first       
data.test = data.KNN[-index[1:n/2], ] # the rest belong to TEST set



# Q12 (8 points) Write R code to create a vector of odd values, from 3 to up to 25, named \textbf{K}.

# Use the train set with four standardized features to form the KNN classifiers 
# where each classifier has the value of nearest neighbours $k$ from \textbf{K}. 
# The values of FNR (Type 2 error rate) and accuracy all classifiers are kept in two vectors, 
# named \textbf{fnr} and \textbf{accuracy}, respectively. \label{knn}


K = seq(3, 25,2); K

library(class)

fnr.k= numeric() # TYPE 2 ERROR RATE to be stored here for different value of k
accuracy.k = numeric() # ACCURACY values to be stored here for different value of k

set.seed(210)
for (i in K){ 
  
  pred <- knn(train=data.train[,2:5], test=data.test[,2:5], cl=data.train[,1], k=i) 
  # KNN with k receiving value as in vector K
  
  confusion.matrix = table(data.test[,1], pred) # confusion.matrix 
  
  type2 = confusion.matrix[1,2]/sum(confusion.matrix[1,1], confusion.matrix[1,2])
  acc = sum(diag(confusion.matrix))/sum(confusion.matrix)
  
  fnr.k =append(fnr.k, type2 ) 
  accuracy.k = append(accuracy.k, acc)
  
}

cbind(K, fnr.k, accuracy.k)

# Q13 (4 points) Write R code to find all the values of $k$ the gives FNR value (Type 2 error Rate) 
# smaller than 0.1. 
# Report the matrix that consists those values of $k$ and the corresponding FNR and accuracy values 
# as comments in your code file. Denote that matrix as \textbf{good.fnr}.
# Among the values of $k$ the gives FNR value smaller than 0.1, 
# write code to find the best value of $k$ that has highest accuracy, 
# and name it as \textbf{best.K}.\label{bestk}
# Report (as a comment in your answer file) the value of FNR and accuracy for that \textbf{best.K}.

# matrix of K with FNR <0.1 and accuracy shown: 
good.fnr = cbind(K, fnr.k, accuracy.k)[which(fnr.k < 0.1), ] ; good.fnr 

#       K      fnr.k accuracy.k
# [1,] 13 0.09935987 0.9043643
# [2,] 15 0.09963819 0.9054776
# [3,] 17 0.09546340 0.9069250
# [4,] 19 0.09908155 0.9035849
# [5,] 21 0.09713331 0.9070363
# [6,] 23 0.09657668 0.9057003
# [7,] 25 0.09657668 0.9049210

best = good.fnr[which(good.fnr[,3] == max(good.fnr[,3])) , ]
best.K = best[1]; best.K # 21

# the best value of k and its FNR & accuracy
best
#           K       fnr.k  accuracy.k 
# 21.00000000  0.09713331  0.90703629 



# Q14 (2 points) Write R code to form a KNN classifier using the best $k$ found in Question 13, 
# where the classifier is formed based on \textbf{test.set}; 
# and is used to predict the outcomes for \textbf{train.set}, 
# where the predicted outcomes is named as \textbf{pred.best.K}.\label{out-bestk}


pred.best.K <- knn(train=data.test[,2:5], test=data.train[,2:5], cl=data.test[,1], k=best.K) 

# Q15 (3 points) Write R code to find the value of FNR and accuracy for the prediction in 
# Question 14, and report these values.

confusion.matrix = table(data.train[,1], pred.best.K) # confusion.matrix 

type2 = confusion.matrix[1,2]/sum(confusion.matrix[1,1], confusion.matrix[1,2]); type2
acc = sum(diag(confusion.matrix))/sum(confusion.matrix); acc

cbind(best.K, type2, acc) # 
#   best.K     type2       acc
# K     21 0.1055361 0.9014085



# Q16 (4 points) Use the KNN classifier formed in Question 14 with the best \textbf{k} 
# to predict the label for a used Ford car with information given below.\label{newpoint}
#\textbf{mileage} = 30000, \textbf{tax} = 110, \textbf{mpg} = 60, \textbf{engineSize} = 1.
# \textit{Hint}: You may standardize the values for the new observation using 
# the mean and the standard deviation of all the observations in the given data set.
# Report the output.

# the KNN model built from the TEST SET with the best k found above

new = data.frame(mileage = 30000, tax = 110, mpg = 60, engineSize = 1)

mean = colMeans(data[, c(5,7,8,9)])

sd = apply(data[, c(5,7,8,9)], MARGIN = 2,FUN = sd)

new.standardized = (new - mean)/sd
new.standardized

predict = knn(train=data.test[,2:5], test=new.standardized, cl=data.test[,1], k=best.K)
predict 

# predicted lable = low







########### QUESTION 2 = TOTAL WORTH 15 POINTS


# Q1 (5 points) Using a while loop, calculate the number of months that 
# Adam and Eve will take to pay back their loan. Since housing loans from HDB 
# need to be paid back in 25 years, will their plan be possible?

# General idea: Adam and Eve has a loan of $700,000 
# At the start of the month, they are charged an interest of 2.6/12%
# At the end of the month, they are pay off $2,500 of their loan
# How many months does it take for the loan amount to fall to or below 0?

loan = 700000
payment = 2500
months = 0

while (loan>0){
  loan = loan*(1+0.026/12)
  loan = loan-payment
  months = months+1
}

months
months/12

# It will take them 432 months, or 36 years to pay back the loan.



# Q2 (5 points) Define a function in R, named \textbf{F}, which helps them to 
# calculate the maximum 25-year loan that they can take for a certain monthly 
# payment amount. Using the function, what is the maximum 25-year loan that they
# can take with a monthly payment of \$2,500?

F = function(payment){
  loan = 0
  for (i in 1:300){
    loan = loan+payment
    loan = loan/(1+0.026/12)
  }
  return(loan)
}
F(2500)

# 551061.9



# Q3 (5 points) Using any loop with the function \textbf{F}, find the minimum 
# monthly payment amount needed to get the \$700,000 loan. The minimum monthly 
# payment should be \textbf{rounded UP} to the nearest integer.

# LET i BE THE AMOUNT OF PAYMENT MONTLY 

for (i in 1:1000000){
  if (F(i)>700000){
    break
  }
}
i

i = 0 # amount of payment monthly
while (F(i)<700000){
  i = i+1
}
i

# 3176












