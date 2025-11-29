setwd("C:/Users/yaote/DSA1101/data")
##########  GERMAN CREDIT DATA SET

library(class)

credit = read.csv("German_credit.csv")
dim(credit)
head(credit)

#library(base)

dim(credit)[1]

# Qb
# scaling data

credit[ ,2:5] = scale(credit[ ,2:5])

# credit[ ,2:5] = apply(credit[ ,2:5], margin = 2, scale )# margin = 2 for columns, 1 = for rows
# OR another way as below
# credit[ ,2:5] = lapply(credit[,2:5], scale)


# Qc
set.seed (1)
train = sample(1:1000 , 800); #randomly sample a set of 800 indexes in the range from 1:1000
train.data = credit[train, ] # 800 data points for the train set
test.data = credit[-train, ] # 200 data points for the test set


train.x = train.data[ ,2:5]
test.x = test.data[ ,2:5]
train.y = train.data[ ,1]
test.y = test.data[ ,1]

# Qd

knn.pred = knn(train.x, test.x, train.y,k=1)
confusion.matrix = table(knn.pred , test.y)
confusion.matrix
accuracy = sum(diag(confusion.matrix))/sum(confusion.matrix); accuracy


# Qe
n_folds=5 # each fold has 200 data points

set.seed (1)
folds <- sample(rep(1:n_folds, length.out = dim(credit)[1] )) 


X = credit[,2:5] # input features
Y = credit[,1] # response

acc=numeric(n_folds) # to store the accuracy for each iteration of n-fold CV

for (j in 1:n_folds) {
	test.index <- which(folds == j) # get the index of the points that will be in the test set
	pred <- knn(train=X[ -test.index, ], test=X[test.index, ], cl=Y[-test.index ], k=1) # KNN with k = 1, 5, 10, etc

	acc[j]=mean(Y[test.index] == pred) 
      # this acc[j] = sum(diag(confusion.matrix))/sum(confusion.matrix), where confusion.matrix=table(Y[test.index],pred)
      }

acc
mean(acc)



# Qf

K = 100 # can try KNN with k = 1,2,...K.

accuracy=numeric(K) # to store the average accuracy from n-fold CV of each k.

acc=numeric(n_folds) # to store the accuracy for each iteration of n-fold CV

for (i in 1:K){
    
    for (j in 1:n_folds) {
	test.index <- which(folds == j) # get the index of the points that will be in the test set
	pred <- knn(train=X[ -test.index, ], test=X[test.index, ], cl=Y[-test.index ], k=i) # KNN with k = 1, 5, 10, etc

	acc[j]=mean(Y[test.index] == pred) 
      # this acc[j] = sum(diag(confusion.matrix))/sum(confusion.matrix), where confusion.matrix=table(Y[test.index],pred)
      }
accuracy[i] = mean(acc)
}

max(accuracy)
sort(accuracy)[98:100] # the three largest accuracy
index = which(accuracy == max(accuracy)) ; index # give index which is also the value of k.

plot(x=1:100, accuracy, xlab = "K")
abline(v = index, col = "red", )


##################  IRIS DATA SET

#install.packages("rpart")
#install.packages("rpart.plot")

library("rpart")
library("rpart.plot")

iris = read.csv("iris.csv")
head(iris)
names(iris) = c("Sepal.Length", "Sepal.Width","Petal.Length","Petal.Width","Species")



fit.iris <- rpart(Species ~ .,
method = "class", data =iris, control = rpart.control( minsplit =1),
parms = list( split ='gini'))
#split = 'information' or 'gini' 


rpart.plot(fit.iris , type =4, extra =2, clip.right.labs =FALSE , varlen =0, faclen =0)

# EXTRA: predict if a new observation that has measurement of:
# sepal.length = 5.1, sepal.width = 3.3, petal.length = 1.6, petal.width = 0.2
# 

#code:
predict(fit.iris, newdata = data.frame(Sepal.Length = 5.1, Sepal.Width = 3.3, 
                 Petal.Length = 1.6, Petal.Width = 0.2), type = 'class')
#answer: that observation should be Iris-setosa.


# predict for this observation:
#sepal.length = 5.1, sepal.width = 3.3, petal.length = 3, petal.width = 1.8

predict(fit.iris, newdata = data.frame(Sepal.Length = 5.1, Sepal.Width = 3.3, 
          Petal.Length = 3, Petal.Width = 1.8), type = 'prob') # get probability

#answer:   Iris-setosa Iris-versicolor Iris-virginica
#   1           0      0.02173913      0.9782609







# EXTRA VISUALIZATION by ggplot2 package
library(ggplot2)
library(magrittr)
# sepal width vs. sepal length
ggplot(iris , aes(x=Sepal.Length, y=Sepal.Width, color =Species)) +
geom_point ()+
labs (x = "sepal length")+ labs (y = "sepal width")

# petal width vs. petal length
ggplot(iris , aes(x=Petal.Length , y=Petal.Width , color =Species)) +
geom_point ()+
labs(x = "petal length")+ labs (y = "petal width")


# OR using a usual way:

# sepal width vs. sepal length

attach(iris)
plot(Sepal.Length, Sepal.Width, type = "n")
points(Sepal.Length[Species=="Iris-setosa"],Sepal.Width[Species=="Iris-setosa"], pch = 20, col = "darkgreen")
points(Sepal.Length[Species=="Iris-versicolor"],Sepal.Width[Species=="Iris-versicolor"], pch = 20, col = "red")
points(Sepal.Length[Species=="Iris-virginica"],Sepal.Width[Species=="Iris-virginica"], pch = 20, col = "blue")
legend(6.3,4.48, legend=c("setosa", "versicolor", "virginica"), col = c("darkgreen", "red", "blue"), pch = c(20,20,20))



# petal width vs. petal length

plot(Petal.Length, Petal.Width, type = "n")
points(Petal.Length[Species=="Iris-setosa"],Petal.Width[Species=="Iris-setosa"], pch = 20, col = "darkgreen")
points(Petal.Length[Species=="Iris-versicolor"],Petal.Width[Species=="Iris-versicolor"], pch = 20, col = "red")
points(Petal.Length[Species=="Iris-virginica"],Petal.Width[Species=="Iris-virginica"], pch = 20, col = "blue")
legend(5,0.6, legend=c("setosa", "versicolor", "virginica"), col = c("darkgreen", "red", "blue"), pch = c(20,20,20))