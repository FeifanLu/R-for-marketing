
path1 <- file.choose()

loan_data <- readRDS(path1)


####################################Exploring the data

# View the structure of loan_data
str(loan_data)


# Load the gmodels package 
library(gmodels)

# Call CrossTable() on loan_status
CrossTable(loan_data$loan_status)

# Call CrossTable() on grade and loan_status
CrossTable(loan_data$grade, loan_data$loan_status, pro.r=TRUE, prop.c=FALSE,
           prop.t=FALSE, prop.chisq=FALSE)

summary(loan_data)


#################################################Histgram
# Create histogram of loan_amnt: hist_1
hist_1 <- hist(loan_data$loan_amnt)

# Print locations of the breaks in hist_1
hist_1$breaks

# Change number of breaks and add labels: hist_2
hist_2 <- hist(loan_data$loan_amnt, breaks = 200, xlab = "Loan amount", 
               main = "Histogram of the loan amount")


# Plot the age variable
plot(loan_data$age, ylab = "Age")


#################################################Outliers
# Save the outlier's index to index_highage
index_highage <- which(loan_data$age > 122)

# Create data set new_data with outlier deleted
new_data <- loan_data[-index_highage, ]

# Make bivariate scatterplot of age and annual income
plot(loan_data$age, loan_data$annual_inc, xlab = "Age", ylab = "Annual Income")



#######################################Option 1 Deleting missing data

# Look at summary of loan_data
summary(loan_data$int_rate)

# Get indices of missing interest rates: na_index
na_index <- which(is.na(loan_data$int_rate))

# Remove observations with missing interest rates: loan_data_delrow_na
loan_data_delrow_na <- loan_data[-na_index, ]

# Make copy of loan_data
loan_data_delcol_na <- loan_data

# Delete interest rate column from loan_data_delcol_na
loan_data_delcol_na$int_rate <- NULL

##########################################Option2  replace missing values

# Compute the median of int_rate
median_ir <- median(loan_data$int_rate, na.rm = TRUE)

# Make copy of loan_data
loan_data_replace <- loan_data

# Replace missing interest rates with median
loan_data_replace$int_rate[na_index] <- median_ir




#########################################Option3 Keep missing data by doing coarse classification

# Make the necessary replacements in the coarse classification example below 
loan_data$ir_cat <- rep(NA, length(loan_data$int_rate))

loan_data$ir_cat[which(loan_data$int_rate <= 8)] <- "0-8"
loan_data$ir_cat[which(loan_data$int_rate > 8 & loan_data$int_rate <= 11)] <- "8-11"
loan_data$ir_cat[which(loan_data$int_rate > 11 & loan_data$int_rate <= 13.5)] <- "11-13.5"
loan_data$ir_cat[which(loan_data$int_rate > 13.5)] <- "13.5+"
loan_data$ir_cat[which(is.na(loan_data$int_rate))] <- "Missing"

loan_data$ir_cat <- as.factor(loan_data$ir_cat)

# Look at your new variable using plot()
plot(loan_data$ir_cat)



#########################################Splitting the data set


# Set seed of 567
set.seed(567)

# Store row numbers for training set: index_train
index_train <- sample(1:nrow(loan_data), 2 / 3 * nrow(loan_data))

# Create training set: training_set
training_set <- loan_data[index_train, ]

# Create test set: test_set
test_set <- loan_data[-index_train, ]


##########################################create a model and make predict


#########################################Create confusion matrix
conf_matrix <- table(test_set$loan_status, model_pred)
conf_matrix
# Compute classification accuracy
(6092 + 349) / nrow(test_set)

# Compute sensitivity
349 / 1037





#####################################################Basic logistic model
path2 <- file.choose()

loan_status <- readRDS(path2)
str(loan_status)
summary(loan_status)

# Create training set: training_set
training_set <- loan_status[index_train, ]

# Create test set: test_set
test_set <- loan_status[-index_train, ]

# Build a glm model with variable ir_cat as a predictor
log_model_cat <- glm(formula = loan_status ~ ir_cat, family = "binomial",
                     data = training_set)

# Print the parameter estimates 
log_model_cat

table(loan_status$ir_cat)



#####################################Multiple variables in a logistic regression model
# Build the logistic regression model
log_model_multi <- glm(loan_status ~ age + ir_cat + grade + loan_amnt +
                         annual_inc , family = "binomial", data = training_set)

# Obtain significance levels using summary()
summary(log_model_multi)


#########################################Predicting the probability of default

# Build the logistic regression model
log_model_small <- glm(loan_status ~ age + ir_cat, family="binomial", data=training_set)
log_model_small

#without type=response, will get the linear predictor produced by linear formula
predictions_all_small <- predict(log_model_small, newdata = test_set, type = "response")

# Look at the range of the object "predictions_all_small"
range(predictions_all_small)


########################################Making more discriminative models


# Build the logistic regression model
log_model_full <- glm(loan_status ~ ., family = "binomial", data = training_set)

# Make PD-predictions for all test set elements using the the full logistic regression model
predictions_all_full <- predict(log_model_full, newdata = test_set, type = "response")

# Look at the predictions range
range(predictions_all_full)




###################################Specifying a cut-off


# The code for the logistic regression model and the predictions is given below
log_model_full <- glm(loan_status ~ ., family = "binomial", data = training_set)
predictions_all_full <- predict(log_model_full, newdata = test_set, type = "response")
predictions_all_full
# Make a binary predictions-vector using a cut-off of 15%
pred_cutoff_15 <- ifelse(predictions_all_full > 0.15, 1, 0)

# Construct a confusion matrix
table(test_set$loan_status, pred_cutoff_15)


#######################################Comparing link functions for a given cut-off

# Fit the logit, probit and cloglog-link logistic regression models
#logit has fatter tail so makes better prediciton on outliers , but takes more computational power
log_model_logit <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                       family = binomial(link = logit), data = training_set)
log_model_probit <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                        family =  binomial(link = probit), data = training_set)
log_model_cloglog <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                         family = binomial(link = cloglog), data = training_set)

# Make predictions for all models using the test set
predictions_logit <- predict(log_model_logit, newdata = test_set, type='response')
predictions_probit <- predict(log_model_probit, newdata = test_set,type='response')
predictions_cloglog <- predict(log_model_cloglog, newdata = test_set,type='response')
head(predictions_logit)
head(predictions_probit)
head(predictions_cloglog)
# Use a cut-off of 14% to make binary predictions-vectors
cutoff <- 0.14
class_pred_logit <- ifelse(predictions_logit > cutoff, 1, 0)
class_pred_probit <- ifelse(predictions_probit > cutoff, 1, 0)
class_pred_cloglog <- ifelse(predictions_cloglog  > cutoff, 1, 0)

# Make a confusion matrix for the three models
true_val <- test_set$loan_status
tab_class_logit <- table(true_val, class_pred_logit)
tab_class_probit <- table(true_val, class_pred_probit)
tab_class_cloglog <- table(true_val, class_pred_cloglog)
tab_class_logit
tab_class_probit
tab_class_cloglog
# Compute the classification accuracy for all three models
acc_logit <- sum(diag(tab_class_logit)) / nrow(test_set)
acc_probit <- sum(diag(tab_class_probit)) / nrow(test_set)
acc_cloglog <- sum(diag(tab_class_cloglog)) / nrow(test_set)

acc_logit
acc_probit
acc_cloglog


#########################################Computing the gain for a tree

# The Gini-measure of the root node is given below
gini_root <- 2 * 89 / 500 * 411 / 500

# Compute the Gini measure for the left leaf node
gini_ll <- 2*(401/446)*(45/446)

# Compute the Gini measure for the right leaf node
gini_rl <- 2*(10/54)*(44/54)

# Compute the gain
gain <- gini_root - 446 / 500 * gini_ll - 54/500 * gini_rl

# compare the gain-column in small_tree$splits with our computed gain, multiplied by 500, and assure they are the same

improve <- gain * 500
improve

######################################Deal with imbalanced dataset

######################################Option 1 Undersampling the training set


# Load package rpart in your workspace.
library(rpart)

# Change the code provided in the video such that a decision tree is constructed using the undersampled training set. Include rpart.control to relax the complexity parameter to 0.001.

library(ROSE)
CrossTable(training_set$loan_status)

undersampled_training_set <- ovun.sample(formula = loan_status ~ ., data = training_set,
                                    method = "under", N = 2124/0.3 , seed = 123, na.action=na.pass)
class(undersampled_training_set)
#get the data from the ovun.sample class
undersampled_training_set <- undersampled_training_set$data
tree_undersample <- rpart(loan_status ~ ., method = "class",
                          data =  undersampled_training_set,
                          control = rpart.control(cp = 0.001))

# Plot the decision tree

plot(tree_undersample, uniform = TRUE)

# Add labels to the decision tree
text(tree_undersample)


library(partykit)
plot(as.party(tree_undersample),type = "extended",gp = gpar(fontsize = 7))



#####################################Option2 Changing the prior probabilities

# Change the code below such that a tree is constructed with adjusted prior probabilities.
tree_prior <- rpart(loan_status ~ ., method = "class",
                    data = training_set, parms = list(prior = c(0.7, 0.3)),
                    control = rpart.control(cp = 0.001))

# Plot the decision tree
plot(tree_prior, uniform = TRUE)

# Add labels to the decision tree
text(tree_prior)


#######################################Option3  Including a loss matrix

str(training_set)
# Change the code provided in the video such that a decision tree is constructed using a loss matrix penalizing 10 times more heavily for misclassified defaults.
tree_loss_matrix  <- rpart(loan_status ~ ., method = "class", data = training_set,
                           parms = list(loss = matrix(c(0, 10, 1, 0), ncol = 2)),
                           control = rpart.control(cp = 0.001))

# Plot the decision tree
plot(tree_loss_matrix, uniform = TRUE)

# Add labels to the decision tree
text(tree_loss_matrix)

####################################################Pruning the tree with changed prior probabilities


# tree_prior is loaded in your workspace

# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree_prior)

# Use printcp() to identify for which complexity parameter the cross-validated error rate is minimized
printcp(tree_prior)

# Create an index for of the row with the minimum xerror
index <- which.min(tree_prior$cptable[, "xerror"])
index
# Create tree_min
tree_min <- tree_prior$cptable[index, "CP"]

#  Prune the tree using tree_min
ptree_prior <- prune(tree_prior, cp = tree_min)

# Use prp() to plot the pruned tree
library(rpart.plot)
prp(ptree_prior)


################################################Pruning the tree with the loss matrix

# set a seed and run the code to construct the tree with the loss matrix again
set.seed(345)
tree_loss_matrix  <- rpart(loan_status ~ ., method = "class", data = training_set,
                           parms = list(loss=matrix(c(0, 10, 1, 0), ncol = 2)),
                           control = rpart.control(cp = 0.001))

# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree_loss_matrix)
printcp(tree_loss_matrix)
index2 <- which.min(tree_loss_matrix$cptable[, "xerror"])
index2

tree_min2 <- tree_loss_matrix$cptable[index2, "CP"]
tree_min2
# Prune the tree using cp = 0.0012788
ptree_loss_matrix <- prune(tree_loss_matrix, cp = 0.0012788)
ptree_loss_matrix2 <- prune(tree_loss_matrix, cp = tree_min2)
# Use prp() and argument extra = 1 to plot the pruned tree
prp(ptree_loss_matrix, extra=1)
prp(ptree_loss_matrix2, extra=1)


####################################################One final tree using more options

# set a seed and run the code to obtain a tree using weights, minsplit and minbucket
set.seed(345)


case_weights <- ifelse(training_set$loan_status == 1, 3, 1)
summary(case_weights)

#when data is imbalanced, normally needs to lower the minsplit and minbucket
tree_weights <- rpart(loan_status ~ ., method = "class",
                      data = training_set, weights = case_weights,
                      control = rpart.control(minsplit = 5, minbucket = 2, cp = 0.001))

# Plot the cross-validated error rate for a changing cp
plotcp(tree_weights)
printcp(tree_weights)
# Create an index for of the row with the minimum xerror
index3 <- which.min(tree_weights$cp[ , "xerror"])
index3
# Create tree_min
tree_min3 <- tree_weights$cp[index, "CP"]

#  Prune the tree using tree_min
ptree_weights <- prune(tree_weights, tree_min3)

# Plot the pruned tree using the rpart.plot()-package
prp(ptree_weights, extra = 1)

#########################################################Confusion matrices and accuracy of our final trees

# Make predictions for each of the pruned trees using the test set.
pred_undersample <- predict(tree_undersample, newdata = test_set,  type = "class")
pred_prior <- predict(tree_prior, newdata = test_set, type = "class")
pred_loss_matrix <- predict(tree_loss_matrix, newdata = test_set, type = "class")
pred_weights <- predict(tree_weights, newdata = test_set, type = "class")




# Construct confusion matrices using the predictions.
confmat_undersample <- table(test_set$loan_status, pred_undersample)
confmat_prior <- table(test_set$loan_status, pred_prior)
confmat_loss_matrix <- table(test_set$loan_status, pred_loss_matrix)
confmat_weights <- table(test_set$loan_status, pred_weights)
confmat_undersample 
confmat_prior 
confmat_loss_matrix
confmat_weights

# Compute the accuracies
acc_undersample <- sum(diag(confmat_undersample)) / nrow(test_set)
acc_prior <- sum(diag(confmat_prior)) / nrow(test_set)
acc_loss_matrix <- sum(diag(confmat_loss_matrix)) / nrow(test_set)
acc_weights <- sum(diag(confmat_weights)) / nrow(test_set)
#loss matrix with 10:1(10 for default predicted as non-default, 1 for non-default predicted as default)
#has the lowest accuracy, but did the best on correctly predicting default,and minimizing false negative
acc_undersample
acc_prior
acc_loss_matrix
acc_weights


#########################################################Computing a bad rate given a fixed acceptance rate
# Make predictions for the probability of default using the pruned tree and the test set.
#first column is the prob for 0, second is the prob for 1
prob_default_prior <- predict(tree_prior, newdata = test_set)[ ,2]
predict(tree_prior, newdata = test_set)
# Obtain the cutoff for acceptance rate 80%
#this cutoff give me the 80% quantile for predicted defaults
cutoff_prior <- quantile(prob_default_prior, 0.8)
cutoff_prior
# Obtain the binary predictions.
# will make 20% predicted defaults as labeled defaults, other 80% predicted defaults will be labeled
#as non-default
bin_pred_prior_80 <- ifelse(prob_default_prior > cutoff_prior, 1, 0)

# Obtain the actual default status for the accepted loans
# identify those labeled non-default observation's actual value in test_set
accepted_status_prior_80 <- test_set$loan_status[bin_pred_prior_80 == 0]

# Obtain the bad rate for the accepted loans
# those being labeled non-default observations will have some actual default values,
# calculate the percentage rate. 
# which is our bad rate
sum(accepted_status_prior_80) / length(accepted_status_prior_80)




##############################################The strategy table and strategy curve
# iterate through all of accepting_rate, from 1 - 0
strategy_bank <-function(prob_of_def){
  cutoff=rep(NA, 21)
  bad_rate=rep(NA, 21)
  accept_rate=seq(1,0,by=-0.05)
  for (i in 1:21){
    cutoff[i]=quantile(prob_of_def,accept_rate[i])
    pred_i=ifelse(prob_of_def> cutoff[i], 1, 0)
    pred_as_good=test_set$loan_status[pred_i==0]
    bad_rate[i]=sum(pred_as_good)/length(pred_as_good)}
  table=cbind(accept_rate,cutoff=round(cutoff,4),bad_rate=round(bad_rate,4))
  return(list(table=table,bad_rate=bad_rate, accept_rate=accept_rate, cutoff=cutoff))
}




# Have a look at the function strategy_bank
strategy_bank

predictions_cloglog <- predict(log_model_cloglog, newdata = test_set,type='response')
predictions_loss_matrix <- predict(tree_loss_matrix, newdata = test_set)[,2]
predictions_logit <- predict(log_model_logit, newdata = test_set, type='response')
# Apply the function strategy_bank to both predictions_cloglog and predictions_loss_matrix
strategy_cloglog <- strategy_bank(predictions_cloglog)
strategy_logit <- strategy_bank(predictions_logit)
strategy_loss_matrix <- strategy_bank(predictions_loss_matrix)

# Obtain the strategy tables for both prediction-vectors
strategy_cloglog$table
strategy_loss_matrix$table
strategy_logit$table

logtable <- as.data.frame(strategy_cloglog$table)
treetable <- as.data.frame(strategy_loss_matrix$table)
logtable
treetable

logtable[c(4,12), ]

treetable[c(4,12), ]
# Draw the strategy functions
# Acceptance rate is connected with the cutoff point
# Acceptance rate = 0.8 means you will accept 80% predicted default, and rule out only 20%
par(mfrow = c(1,2))
plot(strategy_cloglog$accept_rate, strategy_cloglog$bad_rate, 
     type = "l", xlab = "Acceptance rate", ylab = "Bad rate", 
     lwd = 2, main = "logistic regression")

plot(strategy_loss_matrix$accept_rate, strategy_loss_matrix$bad_rate, 
     type = "l", xlab = "Acceptance rate", 
     ylab = "Bad rate", lwd = 2, main = "tree")

#when cut off = 0, cloglog logistic regression will has 0 bad rate and 1 sensitivity
#However decision tree will sill has 0.0667 bad rate, and it's sensitivity is (1-0.0667)
#Why?
#because logistic model's cutoff directly decides the prediction output will be 1 or 0
#but decision tree's cut off only decides the prediction output of predicted default.
#those defaults already predicted as non-default, the cut-off could not impact

#######################################################


#############################################ROC-curves for comparison of logistic regression models

# Load the pROC-package
library(pROC)
par(mfrow = c(1,1))
# Construct the objects containing ROC-information
ROC_logit <- roc(test_set$loan_status, predictions_logit)
ROC_probit <- roc(test_set$loan_status, predictions_probit)
ROC_cloglog <- roc(test_set$loan_status, predictions_cloglog)
ROC_all_full <- roc(test_set$loan_status, predictions_all_full)

# Draw all ROCs on one plot
plot(ROC_logit)
lines(ROC_probit, col = "blue")
lines(ROC_cloglog, col = "red")
lines(ROC_all_full, col = "green")

# Compute the AUCs
auc(ROC_logit)
auc(ROC_probit)
auc(ROC_cloglog)
auc(ROC_all_full)


##########################################################ROC-curves for comparison of tree-based models

#we wants the probability not the 0,1 class, so delete the type='class' parameter
#and choose the second column because it is the probability of being 1
predictions_undersample <- predict(tree_undersample, newdata = test_set)[,2]
predictions_prior <- predict(tree_prior, newdata = test_set)[,2]
predictions_loss_matrix <- predict(tree_loss_matrix, newdata = test_set)[,2]
predictions_weights <- predict(tree_weights, newdata = test_set)[,2]
predictions_ptree_loss_matrix <- predict(ptree_loss_matrix, newdata = test_set)[,2]
# Construct the objects containing ROC-information
ROC_undersample <- roc(test_set$loan_status, predictions_undersample)
ROC_prior <- roc(test_set$loan_status, predictions_prior)
ROC_loss_matrix <- roc(test_set$loan_status, predictions_loss_matrix)
ROC_weights <- roc(test_set$loan_status, predictions_weights)
ROC_ptree_loss_matrix <- roc(test_set$loan_status, predictions_ptree_loss_matrix)
# Draw the ROC-curves in one plot
plot(ROC_undersample)
lines(ROC_prior, col="blue")
lines(ROC_loss_matrix, col="red")
lines(ROC_weights, col="green")
lines(ROC_ptree_loss_matrix, col='pink')

# Compute the AUCs
auc(ROC_undersample)
auc(ROC_prior)
auc(ROC_loss_matrix)
auc(ROC_weights)
auc(ROC_ptree_loss_matrix)



###################################################Another round of pruning based on AUC

# Build four models each time deleting one variable in log_3_remove_ir
log_4_remove_amnt <- glm(loan_status ~ grade + annual_inc + emp_cat, 
                         family = binomial, data = training_set) 
log_4_remove_grade <- glm(loan_status ~ loan_amnt  + annual_inc + emp_cat, 
                          family = binomial, data = training_set)
log_4_remove_inc <- glm(loan_status ~ loan_amnt + grade  + emp_cat , 
                        family = binomial, data = training_set)
log_4_remove_emp <- glm(loan_status ~ loan_amnt + grade + annual_inc, 
                        family = binomial, data = training_set)

# Make PD-predictions for each of the models
pred_4_remove_amnt <- predict(log_4_remove_amnt, newdata = test_set, type = "response")
pred_4_remove_grade <- predict(log_4_remove_grade, newdata = test_set, type = "response")
pred_4_remove_inc <- predict(log_4_remove_inc, newdata = test_set, type = "response")
pred_4_remove_emp <- predict(log_4_remove_emp, newdata = test_set, type = "response")

# Compute the AUCs
auc(test_set$loan_status, pred_4_remove_amnt)
auc(test_set$loan_status, pred_4_remove_grade)
auc(test_set$loan_status, pred_4_remove_inc)
auc(test_set$loan_status, pred_4_remove_emp)


######################################Further model reduction?

# Build three models each time deleting one variable in log_4_remove_amnt
log_5_remove_grade <- glm(loan_status ~ annual_inc + emp_cat, family = binomial, data = training_set)  
log_5_remove_inc <- glm(loan_status ~ grade  + emp_cat , family = binomial, data = training_set)
log_5_remove_emp <- glm(loan_status ~ grade + annual_inc, family = binomial, data = training_set)

# Make PD-predictions for each of the models
pred_5_remove_grade <- predict(log_5_remove_grade, newdata = test_set, type = "response")
pred_5_remove_inc <- predict(log_5_remove_inc, newdata = test_set, type = "response")
pred_5_remove_emp <- predict(log_5_remove_emp, newdata = test_set, type = "response")

# Compute the AUCs
auc(test_set$loan_status, pred_5_remove_grade)
auc(test_set$loan_status, pred_5_remove_inc)
auc(test_set$loan_status, pred_5_remove_emp)

# Plot the ROC-curve for the best model here
plot(roc(test_set$loan_status,pred_4_remove_amnt))




