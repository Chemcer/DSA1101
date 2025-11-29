library(arules)
library(arulesViz)

data(Epub)

?Epub

######################## UNDERSTANDING THE DATASET

summary(Epub)

inspect(head(Epub)) # the first 6 transactions

inspect(head(Epub, 10)) # the first 10 transactions



Epub@itemInfo
Epub@data[,100:101] # 2 columns and 936 rows. 
# 936 rows for 936 categories of items and 2 columns for 2 transactions/downloads.

Epub@data[,1:6]
# the first 6 transactions/downloads in sparse matrix


# the items for first 5 transactions/downloads:
apply(Epub@data[,1:5], 2,
      function(r) paste(Epub@itemInfo[r,"labels"], collapse=", "))

# the items for 100th to 105-th transactions:
apply(Epub@data[,100:105], 2,
      function(r) paste(Epub@itemInfo[r,"labels"], collapse=", "))



###############  GETTING THE FREQUENT 1-ITEMSETS:

itemsets.1 <- apriori(Epub, parameter=list(minlen=1, maxlexn=1,
           support=0.005, target="frequent itemsets"))

summary(itemsets.1)
# Set support = 0.005 will give us 67 frequent 1-itemset

# minlen = 1: frequent itemset has at least 1 item
# maxlen = 1: frequent itemset has max = 1 item
# set both 'minlen = 1' and 'maxlen = 1' means we want frequent itemset that has only 1 item.


# list the most 10 frequent 1-itemsets:
inspect(head(sort(itemsets.1, by = "support"), 5))

# list all the 67 frequent 1-itemsets:
inspect(sort(itemsets.1, by ="support"))




###############  GETTING THE FREQUENT 2-ITEMSETS:

itemsets.2 <- apriori(Epub, parameter=list(minlen=2, maxlen=2,
          support=0.002, target="frequent itemsets"))

summary(itemsets.2)

# Setting support = 0.005 will give 0 frequent 2-itemset.
# Setting support = 0.002 will give 6.

# list all the frequent 2-itemsets:
inspect(sort(itemsets.2, by ="support"))







## # IF THE PARAMETER MAXLEN is not specified, then....

itemsets<- apriori( Epub , parameter = list( minlen=1,
            support = 0.002 , target ="frequent itemsets"))

summary( itemsets )
# this summarizes that: there are 233 frequent 1-itemsets; 
# 6 frequent 2-itemsets; That's all.

inspect(sort( itemsets , by ="support")) 
# this will rank the itemsets by their support, regardless of itemsets with 1 item or 2 items.




###############  GETTING THE RULES instead of  FREQUENT ITEMSETS

rules <- apriori(Epub, parameter=list(support=0.001,
         confidence=0.3, target = "rules"))

plot(rules) # scatter plot of all 30 rules

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

