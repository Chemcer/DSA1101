


###########  SOLUTION OF TUTORIAL 1 #######

############################### PART 1: ON-SITE QUESTIONS ########

##### Q1
x <- numeric(30) # create a vector with all 30 elements, all zero.

#x[1] <- 0 # first element of the serie is zero

x[2] <- 1  # second element is 1

# from the 3rd element onwards, we need to use the formula to find
# let index i runs from 3rd to 30th:

#Q1(a)
for(i in 3:30){ x[i] <- 2*x[i-1] - x[i-2] + 5 }

x[30] # 2059

x  #vector x with the 30 elements


#Q1(b)

which(x >=1000) # find the index of elements in x that is >=1000

# all elements from x_22 onwards are larger than 1000.
# n = 22 is the smallest




##### Q2

# following the idea as in Q1 (though it's not the best way)

y = numeric(100) # one might create a vector with more than 100 elements.

y[1] = 2800 + 1.02*10000

for(i in 2:100){ y[i] <- 2800 + 1.02*y[i-1]}


which(y>=300000) # find the index of elements in y that is >=300000

# all elements from 55th onwards are larger than 300000.
# Hence, the smallest n is 55.

y[55] # the value at 55th position


# SECOND WAY TO SOLVE Q2, ON-SITE PART:

y = numeric() # generate a vector with no element inside yet.

y = append(y, 2800 + 1.02*10000) # this is the first value of vector y


while(max(y) <300000){ y = append(y, 2800 + 1.02*max(y) )}

length(y) # 55

y[55]




############################### PART 2 ########



#################Q1(a) SALARY IS NOT CHANGED OVER THE YEARS

price = 1200000 # House's price
cost = price*0.25 # down payment amount

r= 0.02 # percentage of monthly return from investment

portion_save = 0.4 # portion of salary for saving, every month

# salary is the monthly salary

#### FIRST PERSON WITH salary = 7000

salary = 7000
  
saved <- 10000 # initial savings that parents give
  
month <- 0
  
while(saved < cost){
    month = month +1
    saved = saved+ portion_save *salary + saved*r
print(saved)
  }
print(month)


# when salary = 7000 # answer should be 55 months



#### SECOND PERSON WITH salary = 10000

salary = 10000
  
saved <- 10000 # initial savings that parents give
  
month <- 0
  
while(saved < cost){
    month = month +1
    saved = saved+ portion_save *salary + saved*r
  }
print(month)


# when salary = 10000 # answer should be 44 months

# Extra question: 
# Can you think of a way which can make the code be easy if we have 10 persons with different salary?
# Hint: Which part of the code above for 2 persons is repeated? Use FOR loop

####### SHORTER CODE WITH FOR LOOP

price = 1200000 # House's price
cost = price*0.25 # down payment amount

r= 0.02 # percentage of monthly return from investment

portion_save = 0.4 # portion of salary for saving, every month

sal = c(7000,10000) # vector of salary for two persons

total.month = numeric(length(sal)) # vector to record the output - number of months

print(cbind(sal,total.month)) # we'll let the code to update the second column

for (i in 1:length(sal)){
  
salary = sal[i]
saved <- 10000 # initial savings that parents give

month <- 0

while(saved < cost){
  month = month +1
  saved = saved+ portion_save *salary + saved*r
}
total.month[i] = month
}

print(cbind(sal,total.month))


#####################################

###################Q1(b) THERE IS A RAISE IN THE SALARY EVERY 4 MONTHS:

# rate is the raise in salary per 4 months, change from person to person

price = 1200000 # House's price

cost = price*0.25 # down payment amount

r= 0.02 #percentage of monthly return from investment

portion_save = 0.4 # portion of salary for saving, every month



# FIRST PERSON: SALAPRY = 7000 & rate = 0.02

salary = 7000

rate = 0.02

  
saved <- 10000 # savings given by parents initially
  
month <- 0

while(saved < cost){
      
    month = month +1
    
    saved = saved+ portion_save *salary + saved*r
    
    if (month%%4 ==0){salary = salary*(1+rate)} # increase the salary per 4 months
  }

print(month)


# when salary = 7000, & rate = 0.02,  answer: 52 months


# SECOND PERSON: SALAPRY = 10000 & rate = 0.01

salary = 10000

rate = 0.01

  
saved <- 10000 # savings given by parents initially
  
month <- 0

while(saved < cost){
      
    month = month +1
    
    saved = saved+ portion_save *salary + saved*r
    
    if (month%%4 ==0){salary = salary*(1+rate)} # increase the salary per 4 months
  }

print(month)


# when salary = 10000, & rate = 0.01, answer: 43 months

#################3


#Extra question: Can you use FOR loop to write a shorter code for Q1(b)? Answer is given below


price = 1200000 # House's price

cost = price*0.25 # down payment amount

r= 0.02 #percentage of monthly return from investment

portion_save = 0.4 # portion of salary for saving, every month

sal = c(7000, 10000) # salary of 2 persons
Rate = c(0.02,0.01) # rate of increasement of 2 persons

total.month = numeric(length(sal)) # vector to record the output - number of months


print(cbind(sal, Rate,total.month))

for (i in 1:length(sal)){
  
  salary = sal[i]
  rate = Rate[i]
  saved <- 10000 # initial savings that parents give
  
  month <- 0
  
  while(saved < cost){
    month = month +1
    saved = saved+ portion_save *salary + saved*r
    if (month%%4 ==0){salary = salary*(1+rate)} # increase the salary per 4 months
  }
  total.month[i] = month
}

print(cbind(sal,Rate,total.month))




###################### Q2

price = 1200000 # House's price
cost = price*0.25 # down payment amount
r= 0.02 #monthly rate return from investment
portion_save = 0.4 # portion of salary for saving, every month
### salary = monthly salary is the argument of F1
F1 = function(salary){ 
  saved <- 10000
  month <- 0
  while(saved < cost){
    month = month +1
    saved = saved+ portion_save *salary + saved*r
  }
  return(month)
}
###  Test F1 for the two cases:
F1(7000) # answer should be 55 months
F1(10000) # answer should be 44 months


###############Q3
F2 <- function(salary, price = 1200000, rate = 0.02, portion_save = 0.4) {
  r = 0.02 #monthly rate return from investment
  saved <- 10000 # savings given by parents initially
  month <- 0
  cost = 0.25*price
  while(saved < cost){
    month = month +1
    saved = saved+ portion_save *salary + saved*r
    if (month%%4 ==0){salary = salary*(1+rate)} 
  }
  return(month)
}
### Test function F2
F2(salary = 7000, rate = 0.02) # answer: 52
F2(salary = 10000, rate = 0.01) # answer: 43

# NOTE: this question asks for two arguments: salary and rate. They are compulsory. 
# there is NO NEED to set default value for rate, it's optional.
# other arguments like price, portion_save are extra and not compulsory. We add these two
# just to make F2 be more flexible, when different individuals have different portion_save
# and different dream house.














