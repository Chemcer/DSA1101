

setwd("C:/Data")

setwd("~/Documents/Data")

#################  ON-SITE QUESTIONS



# K MEANS FOR IRIS FLOWER DATASET


data2 = read.csv("iris.csv") # 
names(data2)


# FOR THIS DATASET, IT IS NOT COMPULSORY TO STANDARDIZE THE VARIABLES SINCE
# THE MAGNITUDE OF VARIABLES ARE SIMILAR.
# HOWEVER, IT'S RECOMMENDED THAT WE SHOULD STANDARDIZE THE VARIABLES BEFORE APPLYING K-MEANS.


# Q1: cluster the observations into $k$ groups where $k = 1, 2, 3, ...,10$. 
# For each value of $k$, obtain the value of $WSS$ - the within sum of squares.

K = 10 

wss <- numeric(K)
test = numeric(K)

for (k in 1:K) { 
  wss[k] <- sum(kmeans(data2[,1:4],centers=k, nstart = 20)$withinss )

}

# nstart = 1 is by default.
# we change it to 20 or other larger integer to stablize the plot of WSS below.



# Q2: obtain the plot of $WSS$ against $k$. 
# Which value of $k$ would you choose as the number of clusters 
# for all the observations in the data set? Explain.

plot(1:K, wss, col = "red", type="b", xlab="Number of Clusters",  ylab="Within Sum of Squares")


# choose k= 3 where WSS is stable after it.

# 


# Q3: With the value of $k$ chosen above, 
# report the centroids of all the clusters and 
# the number of the observations in each cluster.

kout <- kmeans(data2[,1:4],centers=3, nstart = 2) # k = 3

#plot(data2$petal.width, data2$petal.length, col = kout$cluster)


kout$centers
#  sepal.length sepal.width petal.length petal.width
#1     5.901613    2.748387     4.393548    1.433871
#2     6.850000    3.073684     5.742105    2.071053
#3     5.006000    3.418000     1.464000    0.244000


kout$size
#[1] 62 38 50


#









################# OFF-SITE QUESTIONS


# Q1
x1 = c(1, 1.5, 3, 3.5, 4.5)
x2 = c(1,2,4,5,5)

plot(x1, x2, pch = 20, col = "blue")

text(1.1,1.1,"A")
text(1.6, 2.2, 'B')
text(3.1, 4.1, 'C')
text(3.63, 5, 'D')
text(4.35, 5, 'E')

# Adding the starting centroids 
points(2,2, pch = 2, col = 'red')
text(2.2, 2.1, 'C-P')
points(4,4, pch = 10, col = 'darkgreen')
text(4,3.8, 'C-Q')

# Adding the new centroids after the first iteration:
points(1.25, 1.5, col = 'red', pch = 2)
text(1.35, 1.4, 'C-P-new')
points(11/3, 14/3, col = 'darkgreen', pch = 10)
text(11/3, 4.5, 'C-Q-new')



data = data.frame(x1, x2)
data
kout = kmeans(data, centers = 2)
kout$withinss
kout$tot.withinss

# Q2

data = read.csv("hdb-2012-to-2014.csv")

dim(data)
names(data)

attach(data)

plot(floor_area_sqm, resale_price, pch = 20)




#########  IT IS RECOMMENDED TO STANDARDIZE THE INPUT FEATURES BEFORE K-MEANS

data.X = scale(data[,c("floor_area_sqm","resale_price")] )

# we standardize only 2 columns we use for K-means only



# PLOT WSS vs K TO PICK OPTIMAL K:

K = 15 
wss <- numeric(K)

for (k in 1:K) { 
   wss[k] <- kmeans(data.X, centers=k, nstart = 50)$tot.withinss
}


plot(1:K, wss, col = "blue", type="b", xlab="Number of Clusters",  ylab="Within Sum of Squares")

# k=3 might be a good choice.


# k = 3 groups; ONE MIGHT PREFER = 4

kout <- kmeans(data.X,centers=3, nstart = 50)

# visualize the 3 groups:

plot(data$floor_area_sqm, 
     data$resale_price, 
     col=kout$cluster)

# THE CENTROIDS OF THE THREE CLUSTERS:

kout$centers

#  floor_area_sqm resale_price
#1      0.2619077   0.09266398
#2      1.6425666   1.98456430
#3     -1.0531087  -0.90274280


# THE SIZE (NUMBER OF POINTS) IN EACH CLUSTER:

kout$size
#[1] 3297  754 1996
