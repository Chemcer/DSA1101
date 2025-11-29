setwd("C:/Users/yaote/DSA1101/data")

##### PART 1: ON-SITE

house = read.csv("house_selling_prices_FL.csv")
names(house) # names of columns
dim(house) # 100 observations and 9 columns
house$NW = as.factor(house$NW) # to declare that NW is categorical
price = house$price
attach(house)

#(a)
cor(price, size)
# 0.76

#(b)
plot(size, price, pch = 20)

#(c)
M1 = lm(price ~ size, data = house)

summary(M1)


#(d)
M2 = lm(price ~ size + NW, data = house)
summary(M2)


#(e)
predict(M2, newdata=data.frame(size=4000, NW = "1"))
# since we have declared NW as a factor, we have to put number 1 in quote signs.







##### PART 2  OFF-SITE QUESTIONS

###############. OFF-SITE QUESTIONS


#####   Q1
simple <- function(x , y) {
beta_1 <- (sum(x*y)- mean (y)* sum (x ))/( sum(x^2)- mean(x)* sum(x));
beta_0 <- mean(y)- beta_1* mean(x) ;
return(c( beta_0 , beta_1)) ;
}


# Note: if you use sum(x)/length(x) instead of mean(x), then please note that
# sum(x)*sum(x) /length(x) might produce "integer overflow" error.
# to fix it, you should add a set of brackets as: sum(x) * (sum(x)/length(x))

dat= read.table("Colleges.txt",header =TRUE,sep= "\t")
names(dat)
head(dat)

# Compare outputs 
simple(x = dat$SAT, y = dat$Acceptance )

lm(Acceptance ~ SAT , data =dat )

# both functions "simple" and "lm" give the same set of coefficients.



#####   Q2 OFF-SITE
hdb = read.csv("hdbresale_reg.csv")

names(hdb)

simple(x = hdb$floor_area_sqm, y = hdb$resale_price)


lm(resale_price ~ floor_area_sqm, data = hdb)

# both functions "simple" and "lm" give the same set of coefficients.


##########. Q3

hdb= read.csv("hdbresale_reg.csv",header =TRUE)
names(hdb)
head(hdb)

dim(hdb)


# (a)
hist(hdb$resale_price) 

# RIGHT SKEWED --> NOT SUITABLE TO BE THE RESPONSE FOR A LINEAR MODEL
# ---> SHOULD BE TRANSFORMED BY TAKING LOG_e.


# (b)

hist(log(hdb$resale_price))
# somehow more symmetric -- > more suitable to be the response of a linear model


# (c)
# we then create a new column inside data frame "resale" for the log of the price 

hdb$log.price = log(hdb$resale_price)
attach(hdb)

plot(log.price ~ floor_area_sqm, col = 2)



# (d)
M = lm(log.price ~ floor_area_sqm + flat_type, data = hdb)
summary(M)

# R chooses "2 ROOM" as reference category
# coefficient 0.119 of "flat_type 3 ROOM" means:
# fixing other variables in the model, compared to a 2-ROOM flat then
# on average, a 3-ROOM flat is HAVING LOG-PRICE LARGER BY 0.119.


# (f)
new = data.frame(floor_area_sqm = 100, flat_type = "4 ROOM")
log.predict = predict(M, newdata = new)
exp(log.predict)


# (g)
summary(M)$r.squared # 


###############################




