
######  ASSOCIATION RULES 


#install.packages('arules')
#install.packages('arulesViz')

# documentation of package 'arules'
# https://cran.r-project.org/web/packages/arules/arules.pdf

library('arules')
library('arulesViz')

data(Groceries)

?Groceries

######################## UNDERSTANDING THE DATASET

summary(Groceries)

# this link below is helpful to understand this special data
# https://www.jdatalab.com/data_science_and_data_mining/2018/10/10/association-rule-transactions-class.html#:~:text=The%20Transactions%20Class,-The%20arules%20package&text=The%20Groceries%20data%20set%20contains,to%20read%20the%20Groceries%20data

inspect(head(Groceries)) # the first 6 transactions

inspect(head(Groceries, 10)) # the first 10 transactions



Groceries@itemInfo[1:10,]
Groceries@data[,100:110] # 11 columns and 169 rows. 
#169 rows for 169 categories of items and 11 columns for 11 transactions

Groceries@data[,1:6]
# the first 6 transactions in sparse matrix


# the items for first 5 transactions:
apply(Groceries@data[,1:5], 2,
      function(r) paste(Groceries@itemInfo[r,"labels"], collapse=", "))

# the items for 100th to 105-th transactions:
apply(Groceries@data[,100:105], 2,
      function(r) paste(Groceries@itemInfo[r,"labels"], collapse=", "))



###############  GETTING THE FREQUENT 1-ITEMSETS:

itemsets.1 <- apriori(Groceries, parameter=list(minlen=1, maxlen=1,
           support=0.02, target="frequent itemsets"))

summary(itemsets.1)

# minlen = 1: frequent itemset has at least 1 item
# maxlen = 1: frequent itemset has max = 1 item
# set both 'minlen = 1' and 'maxlen = 1' means we want frequent itemset that has only 1 item.


# list the most 10 frequent 1-itemsets:
inspect(head(sort(itemsets.1, by = "support"), 10))

# list all the 59 frequent 1-itemsets:
inspect(sort(itemsets.1, by ="support"))



###############  GETTING THE FREQUENT 2-ITEMSETS:

itemsets.2 <- apriori(Groceries, parameter=list(minlen=2, maxlen=2,
          support=0.02, target="frequent itemsets"))

summary(itemsets.2)

# list all the frequent 2-itemsets:
inspect(sort(itemsets.2, by ="support"))

# list of most 10 frequent 2-itemsets:
inspect(head(sort(itemsets.2, by = "support"), 10))



###############  GETTING THE FREQUENT 3-ITEMSETS:


itemsets.3 <- apriori(Groceries, parameter=list(minlen=3, maxlen=3,
          support=0.02, target="frequent itemsets"))

summary(itemsets.3)
inspect(sort(itemsets.3, by ="support"))

# only TWO frequent itemsets that meets the minimum support of 0.02.


###############  GETTING THE FREQUENT 4-ITEMSETS:

itemsets.4 <- apriori(Groceries, parameter=list(minlen=4, maxlen=4,
    support=0.02, target="frequent itemsets"))

summary(itemsets.4)

inspect(sort(itemsets.4, by ="support")) # nothing

# no 4-itemset satisfies the minimum support of 0.02. 
# If we lower down the minimum support to 0.007 then....

itemsets.4 <- apriori(Groceries, parameter=list(minlen=4, maxlen=4,
    support=0.007, target="frequent itemsets"))

summary(itemsets.4)
inspect(sort(itemsets.4, by ="support"))

# there are three frequent 4-itemsets if the minimum support is 0.007.


## # IF THE PARAMETER MAXLEN is not specified, then....

itemsets<- apriori( Groceries , parameter = list( minlen=1,
            support =0.02 , target ="frequent itemsets"))

summary( itemsets )
# this summarizes that: there are 59 frequent 1-itemsets; 
# 61 frequent 2-itemsets; and 2 frequent 3-itemsets

inspect(sort( itemsets , by ="support")) 
# this will rank the itemsets by their support, regardless of itemsets with 1 item or 2 items.
# row 17: {other vegetables, whole milk}  with support = 0.07483477




###############  GETTING THE RULES instead of  FREQUENT ITEMSETS

rules <- apriori(Groceries, parameter=list(support=0.001,
         confidence=0.6, target = "rules"))

plot(rules) # scatter plot of all 2918 rules

# Scatter plot with customize-able measures and can add limiting the plot to the top 100 rules with the 
# largest value for the shading measure. 
plot(rules, measure = c("support", "confidence"), shading = "lift", limit = 100)#, col = "darkblue", limit = 100)






# PLOT SOME TOP RULES FOR VISUALZATION:

# the top 3 rules sorted by LIFT:
inspect(head(sort(rules, by="lift"), 3))

# the top 5 rules sorted by LIFT
inspect(head(sort(rules, by="lift"), 5))
highLiftRules <- head(sort(rules, by="lift"), 5)

########. PLOT THE TOP 5 RULES WITH HIGHEST LIFT FOR VISUALIZATION:

plot(highLiftRules, method="graph") # this is simple and a bit difficult to see the links

# more parameters added, plot looks better (easy for seeing the links):
plot(highLiftRules, method = "graph", engine = "igraph",
     edgeCol = "blue", alpha = 1)
# alpha = c(0,1)
# the size of the node/circle is sorted by the support.
# the darkness of the node's color represents the change in lift


plot(highLiftRules, method = "graph", engine = "igraph",
  nodeCol = "red", edgeCol = "blue", alpha = 1)
# this will fix the color be "red" for all lift values, 
# only the size of the node is sorted by the support.



#some common choices for 'method':
# matrix, mosaic, doubledecker, graph, paracoord, scatterplot, grouped matrix, two-key plot, matrix3D

