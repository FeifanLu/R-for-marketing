library(tidyverse)

path <- file.choose()

###################################################### Build a random forest model for bike rentals

# bikesJuly is in the workspace
str(bikesJuly)

# Random seed to reproduce results
seed <- 123

# the outcome column
(outcome <- "cnt")

# The input variables
(vars <- c("hr", "holiday", "workingday", "weathersit", "temp", "atemp", "hum", "windspeed"))

# Create the formula string for bikes rented as a function of the inputs
(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))
fmla
# Load the package ranger
library(ranger)

# Fit and print the random forest model.
#set respect.unordered.factors = 'order', allow algorithem skips some expensive dummies encoding, treat ordered value
#as numbers such as label encoding. saving time and faster
#
bike_model_rf <- ranger(fmla, 
                         bikesJuly, 
                         num.trees = 500, 
                         respect.unordered.factors = 'order', 
                         seed = seed)

(bike_model_rf2 <- ranger(fmla, 
                        bikesJuly, 
                        num.trees = 500, 
                        respect.unordered.factors = 'ignore', 
                        seed = seed))

(bike_model_rf3 <- ranger(fmla, 
                        bikesJuly, 
                        num.trees = 500, 
                        respect.unordered.factors = TRUE, 
                        seed = seed))


(bike_model_rf4 <- ranger(fmla, 
                        bikesJuly, 
                        num.trees = 500, 
                        respect.unordered.factors = FALSE, 
                        seed = seed))
#################################################################Predict bike rentals with the random forest model
# bikesAugust is in the workspace
str(bikesAugust)

# bike_model_rf is in the workspace
bike_model_rf

# Make predictions on the August data
bikesAugust$pred <- predict(bike_model_rf, bikesAugust)$predictions

# Calculate the RMSE of the predictions
bikesAugust %>% 
  mutate(residual = cnt - pred)  %>%        # calculate the residual
  summarize(rmse  = sqrt(mean(residual^2))) # calculate rmse

# Plot actual outcome vs predictions (predictions on x-axis)
ggplot(bikesAugust, aes(x = pred, y = cnt)) + 
  geom_point() + 
  geom_abline()



####################################################Visualize random forest bike model predictions

first_two_weeks <- bikesAugust %>% 
  # Set start to 0, convert unit to days
  mutate(instant = (instant - min(instant)) / 24) %>% 
  # Gather cnt and pred into a column named value with key valuetype
  gather(key = valuetype, value = value, cnt, pred) %>%
  # Filter for rows in the first two
  filter(instant < 14) 

# Plot predictions and cnt by date/time 
randomforest_plot <- ggplot(first_two_weeks, aes(x = instant, y = value, color = valuetype, linetype = valuetype)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous("Day", breaks = 0:14, labels = 0:14) + 
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("Predicted August bike rentals, Random Forest plot")




##################################################vtreat on a small example


# create dframe 
a <- c('b', 'r', 'r', 'r', 'r', 'b','r', 'g', 'b','b')
b <- c(13, 11, 15, 14, 13, 11,  9, 12,  7, 12)
c <- c(1.0785088, 1.3956245, 0.9217988, 1.2025453, 1.0838662, 0.8043527, 1.1035440,
       0.8746332, 0.6947058, 0.8832502)
dframe <- as.data.frame(cbind(a,b,c))
dframe$b <- as.numeric(dframe$b)
dframe$c <- as.numeric(dframe$c)
str(dframe)

colnames(dframe) <- c('color', 'size', 'probability')
dframe

# Load the package vtreat
library(vtreat)
library(magrittr)
#use_series equals $, but can be used in pipeline.
# Create a vector of variable names
(vars <- c("color", "size"))
vars
# Create the treatment plan
treatplan <- designTreatmentsZ(dframe, vars)
treatplan
# Examine the scoreFrame
(scoreFrame <- treatplan%>%
    use_series(scoreFrame) %>%
    select(varName, origName, code))


class(treatplan$scoreFrame)
scoreFrame_other_way <- treatplan
scoreFrame_other_way <- scoreFrame_other_way$scoreFrame %>%select(varName, origName, code)
scoreFrame_other_way
# We only want the rows with codes "clean" or "lev"
(newvars <- scoreFrame %>%
    filter(code %in% c("clean", "lev")) %>%
    use_series(varName))
class(newvars)
# Create the treated training data
(dframe.treat <- prepare(treatplan, dframe, varRestriction = newvars))

###############################################Novel levels

# treatplan is in the workspace
summary(treatplan)

# newvars is in the workspace
newvars

# Print dframe and testframe
dframe
testframe

a2 <- c('g', 'g', 'y', 'g', 'g', 'y', 'b', 'g', 'g', 'r')
b2 <- c( 7,  8, 10, 12,  6,  8, 12, 12, 12,  8)
c2 <- c(0.9733920, 0.9122529, 1.4217153, 1.1905828, 0.9866464, 1.3697515, 1.0959387,
 0.9161547, 1.0000460, 1.3137360)
testframe <- as.data.frame(cbind(a2,b2,c2))
str(testframe)
testframe$b2 <- as.numeric(testframe$b2)
testframe$c2 <- as.numeric(testframe$c2)
str(testframe)

colnames(testframe) <- c('color', 'size', 'probability')
testframe
# Use prepare() to one-hot-encode testframe
(testframe.treat <- prepare(treatplan, testframe, varRestriction = newvars))


####Good work! As you saw, vtreat encodes novel colors like yellow that were not present in the data as all zeros: 
#'none of the known colors'. This allows downstream models to accept these novel values without crashing.

############################################vtreat the bike rental data


# The outcome column
summary(bikesJuly)
(outcome <- "cnt")

# The input columns
(vars <- c("hr", "holiday", "workingday", "weathersit", "temp", "atemp", "hum", "windspeed"))

# Load the package vtreat
library(vtreat)

# Create the treatment plan from bikesJuly (the training data)
#Set the flag verbose=FALSE to prevent the function from printing too many messages.
treatplan <- designTreatmentsZ(bikesJuly, vars, verbose = FALSE)
treatplan
# Get the "clean" and "lev" variables from the scoreFrame
(newvars <- treatplan %>%
    use_series(scoreFrame) %>%               
    filter(code %in% c("clean", "lev")) %>%  # get the variables you care about
    use_series(varName))                     # get the varName column
newvars
# Prepare the training data
bikesJuly.treat <- prepare(treatplan, bikesJuly,  varRestriction = newvars)

# Prepare the test data
bikesAugust.treat <- prepare(treatplan, bikesAugust, varRestriction = newvars)

# Call str() on the treated data
str(bikesJuly.treat) 
str(bikesAugust.treat)


##########################################Find the right number of trees for a gradient boosting machine


# The July data is in the workspace
ls()

# Load the package xgboost
library(xgboost)

# Run xgb.cv
cv <- xgb.cv(data = as.matrix(bikesJuly.treat), 
             label = bikesJuly$cnt,
             nrounds = 100,
             nfold = 5,
             objective = "reg:linear",
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10,
             verbose = 0   # silent
)

# Get the evaluation log
elog <- cv$evaluation_log
elog
# Determine and print how many trees minimize training and test error
elog %>% 
  summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(test_rmse_mean))    # find the index of min(test_rmse_mean)
ntrees <- which.min(elog$test_rmse_mean)
ntrees

####################################Fit an xgboost bike rental model and predict

# Examine the workspace
ls()

# The number of trees to use, as determined by xgb.cv
ntrees

# Run xgboost
bike_model_xgb <- xgboost(data = as.matrix(bikesJuly.treat), # training data as matrix
                          label = bikesJuly$cnt,  # column of outcomes
                          nrounds = ntrees,       # number of trees to build
                          objective = "reg:linear", # objective
                          eta = 0.3,
                          depth = 6,
                          verbose = 0  # silent
)

# Make predictions
bikesAugust$pred <- predict(bike_model_xgb, as.matrix(bikesAugust.treat))

# Plot predictions vs actual bike rental count
ggplot(bikesAugust, aes(x = pred, y = cnt)) + 
  geom_point() + 
  geom_abline()


#########################################Evaluate the xgboost bike rental model

# bikesAugust is in the workspace
str(bikesAugust)

# Calculate RMSE
bikesAugust %>%
  mutate(residuals = cnt - pred) %>%
  summarize(rmse = sqrt(mean(residuals^2)))
#####################################################Visualize the xgboost bike rental model

# Print randomforest_plot
randomforest_plot

# Plot predictions and actual bike rentals as a function of time (days)
bikesAugust %>% 
  mutate(instant = (instant - min(instant))/24) %>%  # set start to 0, convert unit to days
  gather(key = valuetype, value = value, cnt, pred) %>%
  filter(instant < 14) %>% # first two weeks
  ggplot(aes(x = instant, y = value, color = valuetype, linetype = valuetype)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous("Day", breaks = 0:14, labels = 0:14) + 
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("Predicted August bike rentals, Gradient Boosting model")



