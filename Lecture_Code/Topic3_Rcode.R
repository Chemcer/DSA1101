
########## TOPIC 3 - LINEAR REGRESSION ###########

setwd("C:/Data")

setwd("~/Documents/Data")



resale = read.csv("hdbresale_reg.csv")
head(resale[ ,2:7]) # 1st column indicates ID of flats

head(resale[ ,8:11])

########## MODELLING

x = c( -1, 3, 5)
y = c( -1, 3.5 , 3)
lm(y~x)

########## PREDICTING

M = lm(y~x)             # M = name of the fitted model
new = data.frame(x = 2) # create dataframe of new point
predict(M, newdata = new)



########## MODEL FOR HDB RESALE FLATS

price = resale$resale_price
area = resale$floor_area_sqm
lm(price~area)$coef # coefficients of the model

########## RSE
#Calculating RSE:
sqrt(sum((y - M$fitted)^2)/(length(y) - 2))

summary(M) # Take the Residual standard error

########## R^2

TSS = var(y)*(length(y) -1) # or
TSS = sum((y- mean (y)) ^2)

RSS =sum((y- M$fitted )^2)

R2 = 1 - RSS/TSS; R2


summary(M)$r.squared

########## MULTIPLE LINEAR MODEL

set.seed(520)
x1 = rnorm(100) 
x2 = rnorm(100) 
y = 1 + 2*x1 -5*x2+ rnorm(100)
lm(y~x1+x2)

#instal.packages("rgl") # THIS IS OPTIONAL, NOT COMPULSORY TO HAVE 3D PLOT BELOW
library(rgl)
M.2 = lm(y~x1+x2)
# 3D plot to illustrate the data points
plot3d (x1 , x2 , y, xlab = "x1", ylab = "x2", zlab = "y",
       type = "s", size = 1.5 , col = "red")

coefs = coef(M.2)
a <- coefs[2] # coef of x1
b <- coefs[3] # coef of x2
c <- -1       # coef of y in the equation: ax1 + bx2 -y + d = 0.
d <- coefs[1] # intercept
planes3d (a, b, c, d, alpha = 0.5) # the plane is added to the plot3d above.


########### MLR for HDB RESALE FLATS

resale = read.csv("hdbresale_reg.csv")

resale$age = 2024 - resale$lease_commence_date # AGE OF THE FLAT to 2024
# create a new column for "resale" to calculate the age of the flat till year 2024


M1 = lm(resale_price ~ floor_area_sqm, data = resale)
summary(M1)

M2 = lm(resale_price ~ floor_area_sqm + age, data = resale)
summary(M2)




#############  DIVIDING FULL DATASET INTO TRAIN SET AND TEST SET RANDOMLY

n = dim(resale)[1] # total number of rows/observations

# first, mix up all the indexes of the full data
# then, take the first 80% of those mixed indexes as indexes of train data



index.train = sample(1:n)[1:(0.8*n)] 

train.data = resale[index.train, ]


test.data = resale[ - index.train, ]




# forming model based on the train set:

M3 = lm(resale_price ~ floor_area_sqm + age, data = train.data)
summary(M3)


# predict the response for test set:

prediction = predict(M3, test.data)
prediction

cbind(prediction, test.data$resale_price)
# the prediction using model M3 is the first column
# second column is the actual price





##########  ADDING CATEGORICAL VARIABLE TO THE MLR MODEL
# FLAT TYPE has 5 categories


M4 = lm(resale_price ~ floor_area_sqm + age + flat_type, data = resale)
summary(M4)

# R chooses "2 ROOM" as reference category
# coefficient 26753.48 of "3 ROOM" means:
# fixing other variables in the model, compared to a 2-ROOM flat then
# on average, a 3-ROOM flat is more expensive by $26753.48





######## CRAB.CSV DATASET

df = read.csv("crab.csv")

head(df)

# color & spine are categorical variables

# color: 2 = light; 3 = medium, 4 = dark, and 5 = darker

attach(df)

color

levels(color) # NULL

color = as.factor(color)

# when we form a model for satell based on color and weight form data frame df, color should have 3 coefficients

M = lm(satell ~ weight + color, data = df)
summary(M)  # why color only has 1 coef ?

## however, if...

M = lm(satell ~ weight + color)

summary(M)

############################ 

df$color = as.factor(df$color)

M = lm(satell ~ weight + color, data = df)

summary(M)





######### FITTED VALUES OF A LINEAR MODEL

# MODEL M2
M2$fitted.values
# these are all the prediction for the data set used to form the model


# MODEL M3
M3$fitted.values






############ ASSUMPTIONS OF RESPONSE TO FORM A LINEAR MODEL
# response should be symmetric (1); variability of response is stable when regressors change (2)
# how to check for (1) and how to check for (2)?
# At our level, checking the symmetricity in (1) by histogram would be enough

 hist(resale$resale_price) # if we plan to use full data to form model

 hist(train.data$resale_price) # if we plan to use train.data to form the model

# in any case, the response is NOT symmetric, very right skewed.
# hence, it's NOT SUITABLE to fit a linear model for resale price.

# TRANSFORMATION IS BETTER, such as taking log-e, or sqrt.


hist(log(resale$resale_price)) # slightly better, more symmetric

# hence, fitting a linear model for the log-e of the price is better than 
# fitting a LM for the price itself.




# HOW TO CHECK FOR (2) = THE VARIABILITY OF RESPONSE BE STABLE WHEN REGRESSORS CHANGE?
# using scatter plot of y vs quantitative x
# AND CHECK IF THE RANGE OF RESPONSE IS STABLE WHEN X CHANGES.



plot(resale$floor_area_sqm, resale$resale_price)

plot(resale$age, resale$resale_price)

# THE RANGE OF PRICE IS NOT STABLE WHEN FLOOR AREA CHANGES.












