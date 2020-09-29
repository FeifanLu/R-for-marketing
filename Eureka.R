library(caret)
library(randomForest)
library(lift)
library(ROCR)
library(tidyverse)
library(ggplot2)


######################################Load Data

data<-read.csv(file.choose(), na.strings=c(""," ","NA"), header=TRUE, stringsAsFactors = FALSE)
glimpse(data)
sum(is.na(data))
head(data)
summary(data)


## Fixing incorrectly classified data types:
#identify the reponse variable 
#turn it into a binary classification problem

turn_to_one<-function(vector){
  for (i in 1:length(vector)){
    if(vector[i]>1){
      print(vector[i])
      vector[i]<- 1
      
    }
  }
  return(vector)
}

a <- cbind(1,1,1,2,2,3)
turn_to_one(a)

turn_to_one(data$converted_in_7days)
data$converted_in_7days <- turn_to_one(data$converted_in_7days)
summary(data$converted_in_7days)



data$converted_in_7days <- as.factor(data$converted_in_7days)

max(data$air_purifier_page_top)

class(data[,1])
ncol(data)
#custom function 
#if a column is numeric and its max value is 1, turn this column into factors
transform_to_factor<-function(dataframe){
  dataframe.na.omit <- na.omit(dataframe)
  for(i in 1:ncol(dataframe.na.omit)){
    if(class(dataframe.na.omit[,i]) =='numeric'){
      if(max(dataframe.na.omit[,i]) ==1){
        dataframe[,i] <- as.factor(dataframe[,i])
      }
    }
  }
  return(dataframe)
}

data <- transform_to_factor(data)

glimpse(data)
str(data)
summary(data)
###################################deal with factors with many levels
#deal with the data
summary(data$date)

table(sapply(data,class))

#deal with client id
data$client_id <- as.numeric(as.character(data$client_id))
data$client_id

#deal with dsls



#deal with region




#deal with sourceMedium


#examine the integer 


select_if(data, is.integer)
new.factors <- colnames(select_if(data, is.integer))
new.factors
#we have dsls, newUser and paid
#transform all of them to factors. Dsls means day since last session
data$dsls <- as.factor(data$dsls)
data$newUser <- as.factor(data$newUser)
data$paid <- as.factor(data$paid)
table(sapply(data,class))
knitr::kable(table(is.na(data)))
#######################################EDA

#understand the imnbalanced dataset

prop.table(table(data$converted_in_7days))

#turn it into a binary classification problem


#split the dataset

set.seed(123)
glimpse(data)
inTrain <- createDataPartition(y = data$converted_in_7days,
                               p = 0.7, list = FALSE)
training <- data[ inTrain,]
testing <- data[ -inTrain,]


library(ROSE)
########################################Undersampling
# Calculate the required number of cases in the over-sampled dataset
table(training$converted_in_7days)
n_new <- (1981)/0.4

# Under-sample
#if do not set na.action = na.pss,
#it will automatically omit the na which cause the size of dataset shrink
undersampling_result <- ovun.sample(formula = converted_in_7days ~ ., data = training,
                                    method = "under", N = n_new, seed = 123, na.action=na.pass)
undersampling_result
# Verify the Class-balance of the under-sampled dataset
undersampled_data <- undersampling_result$data
glimpse(undersampled_data)
prop.table(table(undersampled_data$converted_in_7days))
table(undersampled_data$converted_in_7days)





#######################################Upsampling 














#######################################Decision Tree

library(rpart)
library(partykit)
set.seed(123)


CART_cp = rpart.control(cp = 0.0005) #set cp to a small number to "grow" a large tree

rpart_tree<-rpart(converted_in_7days~.,data=undersampled_data, method="class", control=CART_cp) #"Grow" a tree on training data

prunned_rpart_tree<-prune(rpart_tree, cp=0.045) #Prun the tree. Play with cp to see how the resultant tree changes
plot(as.party(prunned_rpart_tree), type = "extended",gp = gpar(fontsize = 7)) #Plotting the tree (adjust fontsize if needed)

# Understand the relationship between the cross-validated error, size of the tree and cp.
plotcp(rpart_tree) # Use printcp(rpart_tree) to print the values. As a rule of thumb pick up the largest cp which does not give a substantial drop in error

rpart_prediction_class<-predict(prunned_rpart_tree,newdata=testing, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class,testing$converted_in_7days,positive = "1") #Display confusion matrix

rpart_probabilities_testing <-predict(prunned_rpart_tree,newdata=testing,type = "prob") #Predict probabilities
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], testing$converted_in_7days) #Calculate errors
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing,"auc") #Create AUC data
rpart_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing #Display AUC value

plotLift(rpart_prediction_class,  testing$converted_in_7days, cumulative = TRUE, n.buckets = 10)



######################################Random Forest
#https://stackoverflow.com/questions/8370455/how-to-use-random-forests-in-r-with-missing-values

str(undersampled_data)
model_forest <- randomForest(converted_in_7days~ ., data=undersampled_data, 
                             na.action = na.roughfix,
                             type="classification",
                             importance=TRUE,
                             ntree = 500,           # hyperparameter: number of trees in the forest
                             mtry = 10,             # hyperparameter: number of random columns to grow each tree
                             nodesize = 10,         # hyperparameter: min number of datapoints on the leaf of each tree
                             maxnodes = 10,         # hyperparameter: maximum number of leafs of a tree
                             cutoff = c(0.5, 0.5)   # hyperparameter: how the voting works; (0.5, 0.5) means majority vote
) 
