setwd("C:/Users/yaote/DSA1101/data")

data<-read.csv('crab.csv')#, header=T)
head(data)
data$spine = as.factor(data$spine)
table(data$spine)
attach(data)

plot(width,weight, col = spine)# type = "n")
points(width[which(spine==1)],weight[which(spine==1)],pch = 20, col = "black")
points(width[which(spine==2)],weight[which(spine==2)],pch = 6, col = "red")
points(width[which(spine==3)],weight[which(spine==3)],pch = 10, col= "blue")
legend(22, 5, legend = c("Spine = 1", "Spine = 2", "Spine = 3"), 
col = c("black", "red", "blue"), pch = c(20, 6, 10))


M = lm(weight ~ width + spine, data = data)
summary(M)


# Q2
#(FPR, TPR) = (0.4, 1), (0.4, 0.4), (0, 0.4)

x = c(0.4, 0.4, 0)
y = c(1, 0.4, 0.4)
plot(x,y, type = "n", xlab = "FPR", ylab = "TPR", ylim = c(0,1), xlim = c(0,1))
points(0.4,1, pch = 10, col = "red") # sigma = 0.3
points(0.4,0.4, pch = 10, col = "blue") # sigma = 0.6
points(0,0.4, pch = 10, col = "black") # sigma = 0.8
legend(0.6, 0.8, legend = c("sigma = 0.3", "sigma = 0.6", "sigma = 0.8"), 
col = c("red", "blue","black"), pch = c(10, 10, 10))




# Q3

library("class")

caravan = read.csv("Caravan.csv")
head(caravan)
nrow(caravan)
dim(caravan) # 87 columns, the first column can be ignored
#description: 85 predictors to measure demographic characteristics.
# Response = Purchase, indicates whether or not an individual purchase a caravan insurance policy.
caravan[87]

#(a)
table(caravan$Purchase)
table(caravan$Purchase)[2]/sum(table(caravan$Purchase)) #6%
# dataset has 6% of people purchased caravan insurance

#(b)
caravan=caravan[,-1] # remove the first column since it's of no information

#SCALING THE INPUT FEATURES
standardized.X= scale(caravan[,-86]) # scaling all the features, except the last column = RESPONSE


# (c)

n = dim(caravan)[1] # sample size = 5822

set.seed (5)
test = sample(1:n, 2000) # sample a random set of 2000 INDEXES, ranging from 1:n.
train.X=standardized.X[-test ,] #training set of features
test.X =standardized.X[test ,]  # test set of features

train.Y=caravan$Purchase[-test] # response for training set
test.Y =caravan$Purchase[test] # response for test set
train.Y
test.Y
# (d) 


knn.pred = knn(train.X,test.X,train.Y,k=1) # KNN with k = 1
confusion.matrix=table(test.Y,knn.pred)
confusion.matrix # Yes is in the second column/row  = POSITIVE
precision = confusion.matrix[2,2]/sum(confusion.matrix[,2])
precision


# KNN with k = 3
knn.pred = knn(train.X,test.X,train.Y,k=3) # KNN with k = 3
confusion.matrix=table(test.Y,knn.pred)
confusion.matrix # Yes is in the second column/row = POSITIVE
precision = confusion.matrix[2,2]/sum(confusion.matrix[,2])
precision


# KNN with k = 5
knn.pred = knn(train.X,test.X,train.Y,k=5) # KNN with k = 5
confusion.matrix=table(test.Y,knn.pred)
confusion.matrix
precision = confusion.matrix[2,2]/sum(confusion.matrix[,2])
precision


# KNN with k = 10
knn.pred = knn(train.X,test.X,train.Y,k=10) # KNN with k = 10
confusion.matrix=table(test.Y,knn.pred)
confusion.matrix

#        knn.pred
# test.Y   No  Yes
# No      1880   0
# Yes     120    0

# k = 10 is an even number, that might cause some ties when doing classification. 
# Hence, it could be the reason that causes some wrong classification that leads
# to the prediction Yes be all 0.