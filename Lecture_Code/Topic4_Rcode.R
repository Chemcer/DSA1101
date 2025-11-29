
#setwd("C:/Data")

setwd("~/Documents/Data")


set.seed(1)
# please google "what is set seed in for" to know what it means by this command


################# STOCK MARKET EXAMPLE

market = read.csv("Smarket.csv")

#summary(market[,2:10])

head(market)

dim(market)

nrow(market)



###################################  PREPARING DATA TO FORM MODEL AND TO TEST MODEL


# to separate the data above to two parts: 
# one part is used to train the model
# another part is to test the model.
# We'll select the rows that belong to the years before 2005 to train model 
# index of the rows before year 2005 is in the vector "index.train":


index.train = which(market$Year <2005)
# indexes of all the rows where Year <2005

# create data that having rows before year 2005:
train.data = market[index.train, ]

# the rest of rows in "market" is for testing model:
test.data  = market[-index.train , ] # take the year 2005 as test set
# take all the rows of "market" where the indexes of those rows are not in "index.train".

dim(train.data)

dim(test.data)

# people normally use a way so that they can split data RANDOMLY. 
# we'll learn about the random way to split data later.
 
# install.packages("class")

library(class) # to call the package "class" be ready for us to use.

# form a SET OF FEATURES for the training (train.x); and for testing (test.x):

train.x = train.data[  ,c("Lag1","Lag2","Lag3","Lag4","Lag5")] # item 1
test.x = test.data[  ,c("Lag1","Lag2","Lag3","Lag4","Lag5")] # item 2 

# WE ONLY USE THE 5 LAGS AS FEATURES WHERE THE MAGNITUDE OF THESE 5 COLUMNS IS THE SAME
# HENCE, THERE IS NO NEED TO STANDARDIZE.
# HOWEVER, IT IS RECOMMENDED TO STANDARDIZE THE QUANTITATIVE COLUMNS WHEN THEIR MAGNITUDE IS NOT THE SAME


# form the RESPONSE for the training (train.y); and for testing (test.y):
train.y = train.data[ ,c("Direction")] # item 3
test.y = test.data[ ,c("Direction")]



###################################  FORMING MODEL = FORMING THE CLASSIFIER

library(class)


knn.pred = knn(train.x,test.x,train.y,k=1)  # KNN with k = 1 # value of k is item 4

knn.pred

# the values in knn.pred is the PREDICTED RESPONSE/CLASS for the test points

# If we want to get the predicted probability for each test point instead of the predicted class, the
# WE SHOULD ADD prob = TRUE

knn.pred = knn(train.x,test.x,train.y,k=1, prob = TRUE) 

prob = attr(knn.pred, "prob") # this will be the predicted probability. HOWEVER....READ BELOW

# # HOWEVER, THE PREDICTED PROBABILITIES ARE FOR THE WINNER CLASS, 
# SUCH AS IF 0 IS THE WINNER, THEN PROBABILTY (Y = 0) IS REPORTED;
# IF 1 IS THE WINNER, THEN PROBABILTY (Y = 1) IS REPORTED
# DO TAKE NOTE OF THIS


knn.pred = knn(train.x,test.x,train.y,k=1)  # get the class label

data.frame(test.y, knn.pred) 
# this can help to compare between the REAL CLASS and the PREDICTED CLASS


table(test.y, knn.pred)
# 55 Down were predicted correctly;
# 75 Ups were predicted correctly.
# the rest (56 + 66) were predicted wrongly by the KNN classifier where k = 1.

############ THE CLASSIFICATION FORMED BY KNN WAS CREATED, named knn.pred





############ TO CHECK HOW GOOD THE CLASSIFER ABOVE IS, WE MAY USE ACCURACY:

confusion.matrix=table(knn.pred, test.y)
confusion.matrix 
sum(diag(confusion.matrix))/sum(confusion.matrix) # 0.515873
# (55+75)/252 ~ 51.59% of the observations are correctly predicted


############ FORM ANOTHER CLASSIFIER BY KNN BUT WITH K = 10:

knn.pred = knn(train.x,test.x,train.y,k=10)  # KNN with k = 10

confusion.matrix=table(knn.pred, test.y)
confusion.matrix
sum(diag(confusion.matrix))/sum(confusion.matrix) # 0.5357143


########  RANDOM WAY TO SPLIT ORIGINAL DATA INTO TRAIN AND TEST with 8:2 ratio


n = nrow(market) # Get the total number of rows in the dataset

test.index = sample(1:n, size = floor(0.2 * n)) # Randomly select 20% of indexes for the test set

# in some cases where 0.2*n is NOT INTEGER, then floor() will take the integer just less than 0.2*n

train.set = market[-test.index, ] # Create the training set by excluding test.index

test.set = market[test.index, ]   # Create the test set using test.index


dim(test.set) # 250 rows randomly be selected as test set
dim(train.set) # 1000 rows randomly be selected as train set




########################## N-FOLD CROSS VALIDATION 

#  A small example on dividing whole data set into n folds  #################
n_folds=3
Number_of_datapoints=12 # sample size
index=rep(1:n_folds,length.out = Number_of_datapoints)
s = sample(index); s
table(s) 
# dataset of 12 points is devided into 3 folds randomly, each fold has 4 points.
# the 4 points for each of 3 folds are selected from the dataset following s. For example,
# s = 3 1 1 3 2 2 2 2 1 3 1 3
# then, the first data point belongs to 3rd fold. The next 2 points belong to 1st fold, etc.
##################################################

## 5-fold Cross-Validation for KNN with k=1, 5, 10, etc. for the data set Smarket.csv

X=market[,c("Lag1","Lag2","Lag3","Lag4","Lag5")] # columns of explanatories/features
Y=market[,c("Direction")] # response

dim(market) # 1250 data points/observations and 10 columns

dim(market)[1] # 1250

n_folds=5

folds_j <- sample(rep(1:n_folds, length.out = dim(market)[1] ))  
# group names from 1 to 5 in a random order

table(folds_j)

#err=numeric(n_folds) # vector to store the error rate of each fold

acc=numeric(n_folds) # vector to store the accuracy of each fold in the loop below

for (j in 1:n_folds) {
	test_j <- which(folds_j == j) # get the index of the points that will be in the test set
	pred <- knn(train=X[ -test_j, ], test=X[test_j, ], cl=Y[-test_j ], k=1) # KNN with k = 1, 5, 10, etc

	#err[j]=mean(Y[test_j] != pred) 
	acc[j]=mean(Y[test_j] == pred) 
      # this acc[j] = sum(diag(confusion.matrix))/sum(confusion.matrix), where confusion.matrix=table(Y[test_j],pred)

}

#err
acc

#error=mean(err); error
accur=mean(acc); accur # this is the average accuracy over 5 folds



