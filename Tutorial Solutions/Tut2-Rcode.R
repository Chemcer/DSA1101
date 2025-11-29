#R code for Tutorial 2 Solution

setwd("C:/Users/yaote/OneDrive/桌面/DSA1101/data")


##### PART 1: ON-SITE

hdb = read.csv("hdbresale_reg.csv")

names(hdb)

attach(hdb)

#(2)
dim(hdb) # 6055 rows and 11 columns


# (3)-(a)
hist(resale_price)


# (3) - (b)

boxplot(resale_price)
outliers = boxplot(resale_price)$out

length(outliers)





##### PART 2: OFF-SITE

#### FEV dataset

fev = read.csv("FEV.csv")
names(fev)
attach(fev)

#Qb
hist(FEV, col = 10, freq= FALSE)

#Qc
boxplot(FEV, col = 10, ylab = "FEV", main = "Boxplot of FEV")

#outlier values
out = boxplot(FEV, col = 10, ylab = "FEV", main = "Boxplot of FEV")$out

# get the index of the outliers:
index = which(FEV %in% c(out)) 
index

#information of all the outliers:
fev[c(index),]



#Qd
qqnorm(FEV, pch = 20)
qqline(FEV, col = "red")


#Qe

female = FEV[which(Sex==0)] # or FEV[Sex==0]

male = FEV[which(Sex==1)] # or FEV[Sex==1]

opar <- par(mfrow=c(1,2)) #arrange a figure which has 1 row and 2 columns (to contain the 2 hitograms)

hist(female, col = 2, freq= FALSE, main = "Histogram of Female FEV", ylim = c(0,0.52))

hist(male, col = 4, freq= FALSE, main = "Histogram of Male FEV", ylim = c(0,0.52))

par(opar)

median(female)
IQR(female)
summary(female)
var(female)

median(male)
summary(male)
IQR(male)
var(male)


#Qf-g
plot(height, FEV)

# EXTRA 1: classifying the points by gender:
# (adding color for the points by its gender)
plot(FEV ~ height, col = as.factor(Sex)) 


# EXTRA 2: ADD COLOR AND LEGEND:
plot(height, FEV, type = "n") # to create the frame for the plot/figure. type = "n" means add NO POINT TO THE PLOT
points(female ~ height[which(Sex==0)], col = "red", pch = 20)
points(male ~ height[which(Sex==1)], col = "darkblue", pch = 20)
legend(1.2, 5, legend = c("Female", "Male"), col = c("red","darkblue"), pch=c(20,20))

cor(FEV, height)

market = read.csv("Smarket.csv", header=TRUE, sep=',')
market1= market[market$Year<2005,]
market2 = market[market$Year == 2005,]

features = c("Lag1", "Lag2", "Lag3", "Lag4","Lag5")
response = c('Direction')

pred = knn(market1[,features], market2[,features], market1[,response], k=5)

head(pred)

