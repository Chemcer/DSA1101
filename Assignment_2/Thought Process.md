EDA:
It's like every other EDA in kaggle and other statistical report.

Didn't include data cleaning because there's no need for it.

Process:

	0. What are the features of the dataset?
	
	1. What is the distribution of independent variable like? 
	
	2. Association and correlation of independent variables to response variable.
	
	3. Data visualisation.

EDA sets the background of data and the direction for the tuning of hyperparameter.
Example:
Binary feature => exclude from k-NN
High correlation value between 2 features => manipulate logistic regression equation.

Data preparation:

	0. Because there are 100k rows in the dataset, it is computationally expensive to tune hyperparameter.
	
	1. There is a clear imbalanced class ratio. Over- and under- sampling methods, and stratified folds are out of the syllabus scope but probably good to implement.
	
	2. There are 8 features and some might not be significant, consider dropping certain features when training. Read: Curse of Dimensionality and Principal Component Analysis
	
	3.If the usual (what is taught in the mod) 5 fold of the subdata approach is used, ensure each fold has a positive diabetes response

Note: probably best to under/over-sample to not overfit the models. Provide your own justification behind taking this tradeoff

k-NN:

	0. Remember to exclude binary features and scale the data

	1.  As usual, 5 folds, store a particular evaluative metric and find the average 
	
	2. Consider weighted vs unweighted K-NN because some variables have more impact on diabetes reduction + (depends on data preparation) presence of class imbalance
	 
Decision Tree:
	
     Remember to factor the ordinal data.
	 
     Recommend to not tune max depth
	 
     Idk what else to comment
     
Logistic Regression:
	
     Take note of variables with high collinearity or might show one due to the context (in this case, blood sugar level and HbA1cn) 

Model Evaluation:
Same old, evaluate based on whatever metric you think is relevant to the context and all 
