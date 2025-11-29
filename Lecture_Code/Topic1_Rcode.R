

##############  TOPIC 1 - INTRODUCTION TO R

# creating a vector of numbers:
number<-c(2,4,6,8,10); number 

# creating a vector of strings/characters:
string<-c("weight", "height", "gender"); string

# creating a Boolean vector (T/F):
logic<- c(T, T, F, F, T); logic


### FUNCTION numeric()

number.2<-numeric(3)
number.2

### APPENDING TWO VECTORS
c(number, number.2)


### FUNCTION rep()

# rep(a,b): replicate the item a by b times where a could be a number or a vectora
number.3<-rep(2,3); number.3

number.3<-rep(c(1,2),3)
number.3

rep(string,2)


### FUNCTION seq()

seq(from=2, to=10, by=2)

seq(2,10,2)

seq(from=2, to=10, length = 5)

seq(10) # a sequence from 1 up to 10, distance by 1







### FUNCTION matrix()

v <- c(1:6)
v

m <- matrix(v, nrow=2, ncol=3); 
m

#to fill the matrix by rows:
m <- matrix(v, nrow=2, ncol=3, byrow=T)
m



### FUNCTION rbind()
a <- c(1,2,3,4)
b <- c(5,6,7,8)

ab_row <- rbind(a,b)
ab_row

### FUNCTION cbind()

ab_col = cbind(a,b)
ab_col



ab_col.try <- cbind(ab_row, c(9,10))
ab_col.try




### LIST IN R

list.1 <- list(10.5, 20, TRUE, "Daisy")
list.1

x = c(2,4,6,8) # length 4
y = c(T, F, T) # length 3

list.2 = list(A = x, B = y) # assign names to list members
list.2

list.2[1] # reference by index

list.2$A # reference by name

list.1[1] # this is NOT numeric

list.1[1]*2

list.1[[1]]*2



###### DATAFRAME IN R


## MANUALLY CREATE A DATA FRAME

height = c(1.6, 1.8, 1.7) # height of 3 people in meter
weight = c(46, 75, 68) # weight of them in kg

HW = data.frame(height, weight)
HW





## IMPORTING A FILE IN TO R AS A DATAFRAME

# setwd("C:/Data")

# setwd("~/Documents/Data")

data1<-read.csv("crab.txt", sep = "", header = FALSE) # this will include the header as first data row
data1[1:8,] #first 8 rows

names(data1) # names of columns 


data1<-read.csv("crab.txt",sep = "", header = TRUE)
data1[1:8,] #first 8 rows. The output is below.

varnames <- c("Subject", "Gender", "CA1", "CA2", "HW")
data2<-read.table("C:/Data/ex_1.txt", header = FALSE,
                  col.names = varnames)

data2

data3<-read.csv("C:/Data/ex_1_comma.txt",sep = ",", header = FALSE)


data3<-read.table("C:/Data/ex_1_name.txt", header = TRUE)
data3


attach(data3)
CA1

data3[,1] # first column

data3[,2:4] # all columns from 2 up to 4

data3[1:2,] # row 1 to row 2

data3[3,3] # value at 3rd row & 3rd column

data3[3,4] # value at 3rd row & 4th column

# all the rows (observations) whose gender = M:
data3[Gender == "M",]

#all the rows (observations) whose gender = M and CA2>85
data3[Gender == "M" & CA2 > 85,]




#############  WHILE LOOP

x = 1
while(x<=3) {print("x is less than 4")
             x = x+1}


# Find the sum of first 10 integers:
x<-0; S<-0
while(x<=10) {S<- S+ x
              x<-x+1}

S


#############  FOR LOOP

# Example: find the sum of first 10 integers
S<-0
for(i in 1:10){S <-S+i}
S

# Find the mean of vector x
x = c(2, 4, 3, 8, 10)
l = length(x)
S = 0
for (i in 1:l){S = S + x[i]}
ave = S/l; ave

#Find the sum of all even numbers from 1 up to 100.
x = c(1:100) 
S = 0
for (i in 1:length(x)){
 if(x[i]%%2 ==0){S = S + x[i]} else {S = S}
 }
print(S)



#############  CONDITIONS WITH if()... else if()... else()

x = c(1:10); # a vector of numbers from 1 to 10
# we want to divide this vector into 3 subsets: 
# a set of all small numbers from 1 to 3
# a set of all medium numbers, from 4 to 7
# a set of large numbers from 8 to 10

S = numeric(0)
M = numeric(0)
L = numeric(0)
for (i in 1:length(x)){
 if (x[i] <=3){S = append(S, x[i])} else if (x[i]< 8)
 {M = append(M, x[i])} else {L = append(L, x[i])}
 }
print(S)

print(M)

print(L)


### FUNCTION ifelse()

x = c(1:8);x
x = ifelse(x%%2 == 0, "even", "odd")
x




############  REPEAT LOOP # for self learning, not tested.

# EXAMPLE: print the first five integers
i <-1
repeat {
  print (i)
  if(i ==5) { break }
  i <- i+1
 }

# Example: obtain the sum of first 5 integers
S = 0

i <-1
repeat {
 S <-S+i;
 if(i ==5) { break }
 i <- i+1
 }
S







########### USER-DEFINED FUNCTION IN R


## STEP 1
find.sum = function(x) { 
  s = sum(x)
  return(s)
  
}

find.sum(x = 10) # PARAMETER IS A NUMBER

find.sum(x = 1:100) # PARAMETER IS A VECTOR

find.sum(x = matrix(1:6, 2,3)) # PARAMETER IS A MATRIX

find.sum() # error # PARAMETER IS NOT GIVEN




# STEP 2: function with default value for parameter
find.sum = function(x = 1) { 
  s = sum(x)
  return(s)
}

find.sum(x = 10)

find.sum() # if parameter is not given, then the default value of it is used.



# STEP 3: function with many parameters
find.sum = function(x = 1, y) { 
  s = sum(x)
  
  return(s*y )
  
}

find.sum( x = 1:100, y = 2)

find.sum(y = 5)



# STEP 4: OUTPUT OF A FUNCTION IS A VECTOR, OR A DATAFRAME, OR A LIST.
# self exploring



#Function for finding odds ratio in general for a matrix x of 2x2:

OR<-function(x){
  if(any(x==0)) {x<-x+0.5}
  odds.ratio<-x[1,1]*x[2,2]/(x[2,1]*x[1,2])
  
  return(odds.ratio) }


table = matrix (1:4, 2,2)
OR(table) # apply function OR to the object "table".


# Can you form a function that help to find the number of even elements in a vector?











































