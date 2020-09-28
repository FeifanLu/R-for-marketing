
################################################Introducing the GBSG2 dataset

# Check out the help page for this dataset
help(GBSG2, package = "TH.data")

# Load the data
data(GBSG2, package = "TH.data")

# Look at the summary of the dataset
summary(GBSG2)

##############################################Digging into the GBSG2 dataset 1
# Count censored and uncensored data
num_cens <- table(GBSG2$cens)
num_cens

# Create barplot of censored and uncensored data
barplot(num_cens)

# Use help() to look at cens
help(GBSG2, package = "TH.data")

GBSG2$time


#########################################Using the Surv() function for GBSG2
# Create Surv-Object
sobj <- Surv(GBSG2$time, GBSG2$cens)

# Look at 10 first elements
sobj[1:10]

# Look at summary
summary(sobj)


###############################################The UnempDur dataset
# Load the UnempDur data
data(UnempDur, package = "Ecdat")

# Count censored and uncensored data
cens_employ_ft <- table(UnempDur$censor1)
cens_employ_ft
str(UnempDur)
# Create barplot of censored and uncensored data
barplot(cens_employ_ft)

# Create Surv-Object
sobj <- Surv(UnempDur$spell, UnempDur$censor1)

# Look at 10 first elements
sobj[1:10]

###################################################First Kaplan-Meier estimate
#########Correct! Surv() is used to define the time-to-event outcome, 
#survreg() can be used to estimate a Weibull model (see upcoming lessons), 
#and with survfit() you can estimate survival curves, e.g. with the Kaplan-Meier technique.



# Create time and event data
time <- c(5, 6, 2, 4, 4)
event <- c(1, 0, 0, 1, 1)

# Compute Kaplan-Meier estimate
km <- survfit(Surv(time, event) ~ 1)
km

# Take a look at the structure
str(km)

# Create data.frame
data.frame(time = km$time, n.risk = km$n.risk, n.event = km$n.event,
           n.censor = km$n.censor, surv = km$surv)


##############################################Exercise ignoring censoring

# Create dancedat data
dancedat <- data.frame(
  name = c("Chris", "Martin", "Conny", "Desi", "Reni", "Phil", 
           "Flo", "Andrea", "Isaac", "Dayra", "Caspar"),
  time = c(20, 2, 14, 22, 3, 7, 4, 15, 25, 17, 12),
  obs_end = c(1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0))

# Estimate the survivor function pretending that all censored observations are actual observations.
km_wrong <- survfit(Surv(time) ~ 1, data = dancedat)

# Estimate the survivor function from this dataset via kaplan-meier.
km <- survfit(Surv(time, obs_end) ~ 1, data = dancedat)
Surv(dancedat$time)
Surv(dancedat$time, dancedat$obs_end)
km
km_wrong

# Plot the two and compare
#See how ignoring censoring underestimates your friends' dancing stamina? 
#The correct analysis (red curve) shows that your friends actually dance longer than the incorrect blue curve suggests.
# survival function = 1-cdf
#however we need to identify the censored data, and do not extract them from the suvival probability
#because the event has not happened yet for them.
#km_wrong is the example 1 - cdf without identifying the censored data and treat them differently
ggsurvplot_combine(list(correct = km, wrong = km_wrong))



####################################Estimating and visualizing a survival curve

# Kaplan-Meier estimate
km <- survfit(Surv(time, cens) ~ 1, data = GBSG2)

# plot of the Kaplan-Meier estimate
ggsurvplot(km)
km
# add the risk table to plot
ggsurvplot(km, risk.table = TRUE)
# add a line showing the median survival time
ggsurvplot(km, risk.table = TRUE, surv.median.line = "hv")

#####################################Estimating median survival from a Weibull model
# Weibull model
wb <- survreg(Surv(time, cens) ~ 1, data = GBSG2)


# Compute the median survival from the model
# the median of survival anlysis is 1693.93 days
predict(wb, type = "quantile", p = 0.5, newdata = data.frame(1))



##################################Survival curve quantiles from a Weibull model

# Weibull model
wb <- survreg(Surv(time, cens) ~ 1, data = GBSG2)

# 70 Percent of patients survive beyond time point...
predict(wb, type = "quantile", p = 1 - 0.7, newdata = data.frame(1))


#############################################Estimating the survival curve with survreg()

# Weibull model
wb <- survreg(Surv(time, cens) ~ 1, data = GBSG2)

# Retrieve survival curve from model probabilities 
surv <- seq(.99, .01, by = -.01)

# Get time for each probability
t <- predict(wb, type = "quantile", p = 1 - surv, newdata = data.frame(1))

# Create data frame with the information
surv_wb <- data.frame(time = t, surv = surv)

# Look at first few lines of the result
head(surv_wb)

#########################################Comparing Weibull model and Kaplan-Meier estimate

# Weibull model
wb <- survreg(Surv(time, cens) ~ 1, data = GBSG2)
wb

# Retrieve survival curve from model
surv <- seq(.99, .01, by = -.01)

# Get time for each probability
t <- predict(wb, type = "quantile", p = 1 - surv, newdata = data.frame(1))

# Create data frame with the information needed for ggsurvplot_df
#ggsurvplot only works for survfit which is a step estimation made from Kaplan-Meier
surv_wb <- data.frame(time = t, surv = surv, 
                      upper = NA, lower = NA, std.err = NA)

# Plot
ggsurvplot_df(fit = surv_wb, surv.geom = geom_line)

#####################################Interpreting coefficients

# Look at the data set
str(dat)

# Estimate a Weibull model
#wbmod <- survreg(Surv(time, status) ~ sex, data = dat)
#coef(wbmod)

##################################Compute Weibull model

# Weibull model
wbmod <- survreg(Surv(time, cens) ~ horTh, data = GBSG2)
coef(wbmod)

# Retrieve survival curve from model
surv <- seq(.99, .01, by = -.01)
t_yes <- predict(wbmod, type = "quantile", p = 1 - surv,
                 newdata = data.frame(horTh = "yes"))

# Take a look at survival curve
str(t_yes)
##############################################Computing a Weibull model and the survival curves
# Weibull model
wbmod <- survreg(Surv(time, cens) ~ horTh + tsize, data = GBSG2)

# Imaginary patients
newdat <- expand.grid(
  horTh = levels(GBSG2$horTh),
  tsize = quantile(GBSG2$tsize, probs = c(0.25, 0.5, 0.75)))
newdat


# Compute survival curves
surv <- seq(.99, .01, by = -.01)
t <- predict(wbmod, type = "quantile", p = 1 - surv,
             newdata = newdat)

# How many rows and columns does t have?
dim(t)
newdat
t

###############################################Visualising a Weibull model

#There are two sets of methods that are explained below:
  
#gather() and spread() from the tidyr package. This is a newer interface to the reshape2 package.
#melt() and dcast() from the reshape2 package.

# Use cbind() to combine the information in newdat with t
library(reshape2)
surv_wbmod_wide <- cbind(newdat, t)
surv_wbmod_wide
head(surv_wbmod_wide) #99 columns , 6 rows
# Use melt() to bring the data.frame to long format
surv_wbmod <- melt(surv_wbmod_wide, id.vars = c("horTh", "tsize"), variable.name = "surv_id", value.name = "time")
surv_wbmod# 6 * 99 rows, 4 columns
str(surv)
# Use surv_wbmod$surv_id to add the correct survival probabilities surv
as.numeric(surv_wbmod$surv_id)
surv[1]
surv[2]
surv_wbmod$surv <- surv[as.numeric(surv_wbmod$surv_id)]
head(surv_wbmod)


# Add columns upper, lower, std.err, and strata to the data.frame
surv_wbmod[, c("upper", "lower", "std.err", "strata")] <- NA

# Take a look at the structure of the object
str(surv_wbmod)

# Plot the survival curves
ggsurvplot_df(surv_wbmod, surv.geom = geom_line,
              linetype = "horTh", color = "tsize", legend.title = NULL)


#################################################Computing a Weibull and a log-normal model

# Weibull model
wbmod <- survreg(Surv(time, cens) ~ horTh, data = GBSG2)

# Log-Normal model
lnmod <- survreg(Surv(time, cens) ~ horTh, data = GBSG2, dist = "lognormal")

# Newdata
newdat <- data.frame(horTh = levels(GBSG2$horTh))
newdat
# Surv
surv <- seq(.99, .01, by = -.01)

# Survival curve from Weibull model and log-normal model
wbt <- predict(wbmod, type = "quantile", p = 1 - surv, newdata = newdat)
lnt <- predict(lnmod, type = "quantile", p = 1 - surv, newdata = newdat)

################################Comparing Weibull and Log-Normal Model I
wdata <- cbind(newdat, wbt)
ldata <- cbind(newdat, lnt)
head(wdata)
head(ldata)

dist <- c('weibull', 'weibull','lognormal', 'lognormal')
newdat <- rbind(wdata, ldata)
newdat <- cbind(dist, newdat)
surv_wide <- newdat

# Melt the data.frame into long format.
surv_long <- melt(surv_wide, id.vars = c("horTh", "dist"), variable.name = "surv_id", value.name = "time")
surv_long
# Add column for the survival probabilities
surv_long$surv <- surv[as.numeric(surv_long$surv_id)]

# Add columns upper, lower, std.err, and strata contianing NA values
surv_long[, c("upper", "lower", "std.err", "strata")] <- NA


# Plot the survival curves
ggsurvplot_df(surv_long, surv.geom = geom_line, 
              linetype = "horTh", color = "dist", legend.title = NULL)


######################################Computing a Cox model
#semi parametric model. no assumption on distribution. 
#no estimation on intercept, negative value stands for positive impact
# Compute Cox model

#????????????????????????,???Kaplan-Meier survival estimate,??????????????????(univariable analysis),????????????????????????,???????????????????????????????????????????????????????????????????????????????????????(??????????????????multi-variables????????????????????????????????????????????????????????????????????????????????????,???????????????????????????????????????,???????????????????????????????????? ???/??? ?????????????????????)

#??????,Kaplan-Meier??????????????????????????????(??????A vs ??????B,??? vs ???),???????????????????????????????????????????????????

#??????????????????????????????,Cox????????????????????????(Cox proportional hazards regression model)?????????????????????
cxmod <- coxph(Surv(time, status) ~ performance, data = dat)
dat
# Show model coefficient
coef(cxmod)

#################Computing the survival curve from a Cox model

# Cox model
cxmod <- coxph(Surv(time, cens) ~ horTh + tsize, data = GBSG2)

# Imaginary patients
newdat <- expand.grid(
  horTh = levels(GBSG2$horTh),
  tsize = quantile(GBSG2$tsize, probs = c(0.25, 0.5, 0.75)))
rownames(newdat) <- letters[1:6]

# Inspect newdat
head(newdat)
newdat


# Compute survival curves
#survfit for non or semi parametric models, it use the model to fits the data
#survreg is the survival regression, is used for parametric models
cxsf <- survfit(cxmod, data = GBSG2, newdata = newdat, conf.type = "none")
head(cxsf$surv)


summary(cxsf)
# Look at first 6 rows of cxsf$surv and time points
head(cxsf$surv)
head(cxsf$time)


# Compute data.frame needed for plotting
surv_cxmod0 <- surv_summary(cxsf)
surv_cxmod0

# Get a character vector of patient letters (patient IDs)
pid <- as.character(surv_cxmod0$strata)
pid
newdat['a',]
# Multiple of the rows in newdat so that it fits with surv_cxmod0
m_newdat <- newdat[pid, ]
m_newdat
# Add patient info to data.frame
surv_cxmod <- cbind(surv_cxmod0, m_newdat)

# Plot
ggsurvplot_df(surv_cxmod, linetype = "horTh", color = "tsize",
              legend.title = NULL, censor = FALSE)



###########################################Capstone: The Cox model
lung
# Compute Cox model and survival curves
head(lung)
 
cxmod <- coxph(Surv(time, status) ~ pat.karno, data = lung)
new_lung <- data.frame(pat.karno = c(60, 70, 80, 90))
cxsf <- survfit(cxmod, data = lung, newdata = new_lung, conf.type = "none")
# Use the summary of cxsf to take a vector of patient IDs
surv_cxmod0 <- surv_summary(cxsf)
pid <- as.character(surv_cxmod0$strata)

# Duplicate rows in newdat to fit with surv_cxmod0 and add them in
m_newdat <- new_lung[pid, , drop = FALSE]
surv_cxmod <- cbind(surv_cxmod0, m_newdat)


# Plot
ggsurvplot_df(surv_cxmod, color = "pat.karno", legend.title = NULL, censor = FALSE)



# Compute Kaplan-Meier curve
km <- survfit(Surv(time, status) ~ 1, data = lung)

# Compute Cox model
cxmod <- coxph(Surv(time, status) ~ pat.karno, data = lung)


# Compute Cox model survival curves
new_lung <- data.frame(pat.karno = c(60, 70, 80, 90))
cxsf <- survfit(cxmod, data = lung, newdata = new_lung, conf.type = "none")

# Plot Kaplan-Meier curve
ggsurvplot(km, conf.int = FALSE)
# Plot Cox model survival curves
ggsurvplot(cxsf, censor = FALSE)
