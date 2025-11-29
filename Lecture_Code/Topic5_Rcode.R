
# setwd("C:/Data")

setwd("~/Documents/Data")

bankdata = read.csv("bank-sample.csv", header=TRUE)
head(bankdata)
head(bankdata[,2:8])
head(bankdata[,c(9,16,17)])

#### Some SUMMARIES TO UNDERSTAND THE DATA SET  ###
table(bankdata$job)
table(bankdata$marital)

table(bankdata$education)

table(bankdata$default)


table(bankdata$housing)

table(bankdata$loan)

table(bankdata$contact)

table(bankdata$poutcome)


#######  FITTING A DECISION TREE USING THE WHOLE DATA SET GIVEN AS THE TRAIN SET

#install.packages("rpart")
#install.packages("rpart.plot")

library("rpart")
library("rpart.plot")

fit <- rpart(subscribed ~job + marital + education+default + 
housing + loan + contact+poutcome,
method="class",
data=bankdata,
control=rpart.control(minsplit=50),
parms=list(split='information')
)

# there are few other arguments instead of minsplit in rpart.control: cp, maxdepth;
# smaller values of cp correspond to decision trees of larger sizes, 
# and hence more complex decision surfaces.
# argument "maxdepth" also could be used.
# minslpit = 1: one stem is created when data have at least one observation in that stem


fit <- rpart(subscribed ~job + marital + education+default + 
housing + loan + contact+poutcome,
method="class",
data=bankdata,
control=rpart.control(maxdepth = 4),    ##### change the maxdepth  = 2, 3, or 4 and plot the tree to see the difference
parms=list(split='information')
)




# method = "anova", "poisson", "class" or "exp"
# If response is a survival object, then method = "exp" is assumed, 
# if response has 2 columns then method = "poisson" is assumed, 
# if response is a factor then method = "class" is assumed, 
# otherwise method = "anova" is assumed
# split = 'information' or 'gini'



# To plot the fitted tree:
rpart.plot(fit, type=4, extra=2)# can try with extra = 4 to see the difference


rpart.plot(fit, type=4, extra=2, varlen=0, faclen=0, clip.right.labs=FALSE)

#varlen = length of variable's name,varlen = 0 means full name of input variables is shown
#faclen = length of category's name, faclen = 0 means full name of categories
#clip.right.labs: TRUE means don't print the name of variable for the right stem
# You can try with varlen = 4 to see the difference compared to varlen = 0.
# type = 0, 1, ..., 5
# extra = 0, 1,..., 11

# https://cran.r-project.org/web/packages/rpart.plot/rpart.plot.pdf



length(bankdata$poutcome)
table(bankdata$poutcome)



### Calculating conditional entropy when 'poutcome' is splitted as
# as x1 = failure, other, unknown and x2 = success

x1=which(bankdata$poutcome!="success") # index of the rows where poutcome = x1
length(x1) # 1942 rows that the value of poutcome = x1.

x2=which(bankdata$poutcome=="success") # index of the rows where poutcome = x2
length(x2) # 58 rows that the value of poutcome = x2 = success

table(bankdata$subscribed[x1]) 
# counting how many "yes" and how many "no" for Subscribed among those with poutcome = x1
# among 1942 customers with poutcome = x1, 179 subscribed (179 yes), and 1763 no.

table(bankdata$subscribed[x2]) 
# counting how many "yes" and how many "no" for Subscribed among those with poutcome = x2
# among 58 customers with poutcome = x2, 32 subscribed (32 yes), and 26 no.




### Calculating conditional entropy when 'poutcome' is splitted as
# as x1 = success, other, unknown and x2 = failure
x1=which(bankdata$poutcome!="failure")
x2=which(bankdata$poutcome=="failure")
table(bankdata$subscribed[x1])
table(bankdata$subscribed[x2])




###### DIAGNOSTICS OF THE FITTED TREE
# 	check how good the fitted tree is, using the 5 metrics mentioned in Topic 4

# FOR EXAMPLE, WE CONSIDER THE ACCURACY. YOU MAY CONSIDER OTHER METRICS.

# REAL RESPONSE IS bankdata$subscribed

# THE TREE WAS FITTED USING THE WHOLE DATA SET:

fit <- rpart(subscribed ~job + marital + education+default + 
housing + loan + contact+poutcome,
method="class",
data=bankdata,
control=rpart.control(maxdepth = 1),    ##### change the maxdepth  = 2, 3, or 4 and plot the tree to see the difference
parms=list(split='information')
)

rpart.plot(fit, type=4, extra=2, varlen=0, faclen=0, clip.right.labs=FALSE)

# NOW, WE CHECK HOW THE TREE ABOVE PREDICTS THE RESPONSE FOR THOSE POINTS IN THE WHOLE DATA SET


pred = predict(fit, newdata = bankdata[, c("job" , "marital" , "education", "default", "housing" , "loan", "contact" , "poutcome")],
 type = "class")


data.frame(bankdata$subscribed, pred)

confusion.matrix = table(bankdata$subscribed, pred)
confusion.matrix

accuracy = sum(diag(confusion.matrix))/sum(confusion.matrix)
accuracy # 





############  PLAYING GOLF EXAMPLE

library("rpart") # load libraries
library("rpart.plot")

play_decision <- read.table("DTdata.csv",header=TRUE,sep=",")
head(play_decision)

fit <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
method="class",
data=play_decision,
control=rpart.control(minsplit=1),
parms=list(split='information'))

rpart.plot(fit, type=4, extra=2)


# PREDICT THE DECISION FOR TWO DAYS:

newdata <- data.frame(Outlook= c("rainy", "sunny"), Temperature= c("mild","hot"),
Humidity=c("high", "normal"), Wind=c(FALSE, TRUE))
newdata

predict(fit,newdata=newdata,type="prob") # get the probability
# to get the probabilioty in numeric, we use:
p = predict(fit,newdata=newdata,type="prob")
as.numeric(paste(p))


predict(fit,newdata=newdata,type="class") # getthe class

pred = predict(fit, newdata = play_decision[,-1], type = "class")

data.frame(play_decision$Play, pred)

confusion.matrix = table(play_decision$Play, pred)

confusion.matrix

# accuracy = 100%

######





