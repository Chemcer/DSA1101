# ==============================================================================
# DSA1101 cheat sheet by Yao Teck
# Consolidated from Tutorials, Midterms, and Finals
# ==============================================================================

# 0. SETUP & LIBRARIES ---------------------------------------------------------
library(class)       # KNN
library(e1071)       # Naive Bayes
library(rpart)       # Decision Trees
library(rpart.plot)  # Tree plotting
library(ROCR)        # ROC/AUC
library(arules)      # Association Rules
library(arulesViz)   # Rules visualization
library(ggplot2)     # Plotting, not needed

# ==============================================================================
# 1. DATA MANIPULATION & CLEANING
# ==============================================================================

# Inspection
dim(data); names(data); str(data); summary(data)

# Preprocessing
data$col = as.factor(data$col) # Factor conversion
data$risk = ifelse(data$val > 100, "High", "Low") # Binning
data = data[data$year >= 1996, ] # Filtering rows
data$fuel = ifelse(data$fuel == "Other", "Hybrid", data$fuel) # Renaming values

# Scaling (Crucial for KNN & K-Means)
data_scaled = data
data_scaled[, 2:5] = scale(data[, 2:5]) # Scale specific numerical cols

# ==============================================================================
# 2. EXPLORATORY DATA ANALYSIS (EDA)
# ==============================================================================

# Histogram with Normal Density Overlay
hist(data$val, prob = TRUE, main = "Density Plot")
x_seq = seq(min(data$val), max(data$val), length.out = 100)
lines(x_seq, dnorm(x_seq, mean(data$val), sd(data$val)), col = "red")

# Boxplots & Outliers
bp = boxplot(data$val ~ data$group)
outlier_vals = bp$out # Extract values
outlier_idx = which(data$val %in% outlier_vals) # Extract indices

# Contingency Tables & Odds Ratio
tab = table(data$cat1, data$cat2)
prop.table(tab, 1) # Row proportions
OR = (tab[1,1] * tab[2,2]) / (tab[1,2] * tab[2,1]) # Odds Ratio

# ==============================================================================
# 3. SUPERVISED LEARNING MODELS
# ==============================================================================

# --- A. LINEAR REGRESSION ---
lm_model = lm(response ~ ., data = data)
summary(lm_model)
pred_lm = predict(lm_model, newdata = data) #

# --- B. LOGISTIC REGRESSION ---
glm_model = glm(response ~ ., data = data, family = binomial)
exp(coef(glm_model)) # Interpret Odds Ratio
prob_glm = predict(glm_model, newdata = data, type = "response") # Probabilities
pred_glm = ifelse(prob_glm > 0.5, "Yes", "No") # Thresholding

# --- C. NAIVE BAYES ---
nb_model = naiveBayes(response ~ ., data = data)
pred_nb = predict(nb_model, data, type = "class") # Labels

# --- D. DECISION TREES ---
dt_model = rpart(response ~ ., data = data, method = "class",
                  control = rpart.control(minsplit = 20, cp = 0.01),
                  parms = list(split = 'information'))
rpart.plot(dt_model, type = 4, extra = 2) #

# --- E. KNN (K-NEAREST NEIGHBORS) ---
# Requires X and Y separation (See Section 7 for splitting logic)
train_x = scale(data[, 1:4])
train_y = data$response
set.seed(1) 
knn_pred = knn(train = train_x, test = train_x, cl = train_y, k = 5)

# ==============================================================================
# 4. MODEL EVALUATION
# ==============================================================================

# Confusion Matrix
cm = table(Actual = data$response, Predicted = pred_glm)
accuracy = sum(diag(cm)) / sum(cm) #

# ROC Curve & AUC
pred_obj = prediction(predictions = prob_glm, labels = data$response)
perf = performance(pred_obj, "tpr", "fpr")
plot(perf, col = "blue") #
auc = performance(pred_obj, "auc")@y.values[[1]] #

# Plotting TPR/FPR vs Threshold
alpha = perf@alpha.values[[1]]; tpr = perf@y.values[[1]]; fpr = perf@x.values[[1]]
plot(alpha, tpr, type = "l", col = "blue")
lines(alpha, fpr, col = "red") #

# ==============================================================================
# 5. UNSUPERVISED LEARNING
# ==============================================================================

# K-Means (Elbow Method)
wss = numeric(10)
set.seed(123)
for(k in 1:10){ wss[k] = kmeans(scale(data[,1:4]), centers = k)$tot.withinss }
plot(1:10, wss, type = "b") #

# Association Rules
rules = apriori(data, parameter = list(supp = 0.01, conf = 0.5))
inspect(head(sort(rules, by = "lift"), 5)) #

# ==============================================================================
# . VALIDATION & DATA SPLITTING STRATEGIES
# ==============================================================================

# --- A. SIMPLE TRAIN-TEST SPLIT (DATAFRAME) ---
# Used for GLM, Decision Trees, Naive Bayes
set.seed(1)
n = nrow(data)
train_idx = sample(1:n, size = floor(0.8 * n)) # 80% indices

train_data = data[train_idx, ]
test_data  = data[-train_idx, ]

# --- B. XY SPLIT VALIDATION (FOR KNN) ---
# KNN requires separating Predictors (X) and Response (Y)
# 1. Create X matrix (Predictors) and Y vector (Response)
X_all = data[, c("age", "salary", "height")] # Numeric only
Y_all = data[, "response"]

# 2. Scale features (Required for KNN)
X_scaled = scale(X_all)

# 3. Split using indices
train_x = X_scaled[train_idx, ]
test_x  = X_scaled[-train_idx, ]

train_y = Y_all[train_idx]
test_y  = Y_all[-train_idx]

# 4. Fit KNN
pred_knn = knn(train = train_x, test = test_x, cl = train_y, k = 5)

# --- C. N-FOLD CROSS-VALIDATION LOOP ---
# Used to check model stability or tune hyperparameters
n_folds = 
set.seed(1)
# Create random fold assignments (1 to 5) for every row
folds = sample(rep(1:n_folds, length.out = nrow(data)))

acc_storage = numeric(n_folds) # Store metrics here

for (j in 1:n_folds) {
    # 1. Identify Test Indices for this fold
    test_idx = which(folds == j)
    
    # 2. Split Data
    cv_train_x = X_scaled[-test_idx, ]
    cv_test_x  = X_scaled[test_idx, ]
    cv_train_y = Y_all[-test_idx]
    cv_test_y  = Y_all[test_idx]
    
    # 3. Train & Predict (Example: KNN)
    pred = knn(train = cv_train_x, test = cv_test_x, cl = cv_train_y, k = 5)
    
    # 4. Calculate Metric (Accuracy)
    acc_storage[j] = mean(pred == cv_test_y)
}

# Average Performance
mean(acc_storage)