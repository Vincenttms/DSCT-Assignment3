#Libraries
require(dplyr)
require(tidyr)
require(ggplot2)
require(bigmemory)
require(biganalytics)
require(ROCR)
require(ggfortify)
require(MASS)
require(igraph)
require(rgl)
require(caret)
require(e1071)

library(dplyr)
library(tidyr)
library(ggplot2)
library(pastecs)
library(bigmemory)
library(biganalytics)
library(ROCR)
library(ggfortify)
library(MASS)
library(igraph)
library(rgl)
library(caret)
library(e1071)


#Import Data from CSV
gcd <- read.csv("C:\\Users\\vincenttanms\\Documents\\DSCT Participants\\Assignment 3\\DSCT-Logistic-Regression-master\\german_credit_data.csv", header = TRUE, sep=";")

#------------------------1) Assess the data and ensure that there are no defects in it. --------------------

#Check for each variable how many have missing records
sapply(gcd, function(x) sum(is.na(x)))

#Check for each variable how many unique values are there
sapply(gcd, function(x) length(unique(x)))

# Summary of the Data
# view a summary of the raw data
head(gcd)

# looking at summary statistics for the entire data set
summary(gcd)

# using the pastecs package to generate descriptive statistics
stat.desc(gcd)

#Clean data
#Remove outliers
gcd <- subset(gcd, Personal != 7 )
gcd <- subset(gcd,Years.with.present.employer != 9)
gcd <- subset(gcd, Credit.asked.for..X.100. != 0)
gcd <- subset(gcd, Other.installment.plans != 4)





#------2) Determine if any categorical variables will need to be transformed, combined or split up---------
#Transform Catergorical data
#The contrast specifies how the levels of the factors will be coded into a family
# of numeric dummy variables
rgcd <- gcd
rgcd$Status.of.account <- as.factor(gcd$Status.of.account)
levels(rgcd$Status.of.account)

rgcd$Credit.history <- as.factor(gcd$Credit.history)
levels(rgcd$Credit.history)

rgcd$purpose.of.loan <- as.factor(gcd$purpose.of.loan)
levels(rgcd$purpose.of.loan)

rgcd$Savings.account <- as.factor(gcd$Savings.account)
levels(rgcd$Savings.account)

rgcd$Years.with.present.employer <- as.factor(gcd$Years.with.present.employer)
levels(rgcd$Years.with.present.employer)

rgcd$Personal <- as.factor(gcd$Personal)
levels(rgcd$Personal)

rgcd$other.debtors.guarantors <- as.factor(gcd$other.debtors.guarantors)
levels(rgcd$other.debtors.guarantors)

rgcd$Assets <- as.factor(gcd$Assets)
levels(rgcd$Assets)

rgcd$Other.installment.plans <- as.factor(gcd$Other.installment.plans)
levels(rgcd$Other.installment.plans)

rgcd$Housing <- as.factor(gcd$Housing)
levels(rgcd$Housing)

rgcd$Job.Classification <- as.factor(gcd$Job.Classification)
levels(rgcd$Job.Classification)


#The contrast specifies how the levels of the factors will be coded into a family
# of numeric dummy variables
contrasts(rgcd$Status.of.account)
contrasts(rgcd$Credit.history)
contrasts(rgcd$purpose.of.loan)
contrasts(rgcd$Savings.account)
contrasts(rgcd$Years.with.present.employer)
contrasts(rgcd$Personal)
contrasts(rgcd$other.debtors.guarantors)
contrasts(rgcd$Assets)
contrasts(rgcd$Other.installment.plans)
contrasts(rgcd$Housing)
contrasts(rgcd$Job.Classification)

#Create New gcd data.frame to store all the dichotomous variables
ngcd <- data.frame(gcd$Loan.Duration,
                   gcd$Credit.asked.for..X.100.,
                   gcd$Age,
                   gcd$No.of.dependents,
                   gcd$Have.telephone,
                   gcd$Foreign.Worker)


#Transformation on Status.of.account
ngcd$Status.of.account.1[gcd$Status.of.account != 1] <- 1
ngcd$Status.of.account.1[gcd$Status.of.account == 1] <- 2

ngcd$Status.of.account.2[gcd$Status.of.account != 2] <- 1
ngcd$Status.of.account.2[gcd$Status.of.account == 2] <- 2

ngcd$Status.of.account.3[gcd$Status.of.account != 3] <- 1
ngcd$Status.of.account.3[gcd$Status.of.account == 3] <- 2

ngcd$Status.of.account.4[gcd$Status.of.account != 4] <- 1
ngcd$Status.of.account.4[gcd$Status.of.account == 4] <- 2

sapply(ngcd, function(x) sum(is.na(x)))
#Transformation on Credit.history
ngcd$Credit.history.0[gcd$Credit.history != 1] <- 1
ngcd$Credit.history.0[gcd$Credit.history == 1] <- 2

ngcd$Credit.history.1[gcd$Credit.history != 2] <- 1
ngcd$Credit.history.1[gcd$Credit.history == 2] <- 2

ngcd$Credit.history.2[gcd$Credit.history != 3] <- 1
ngcd$Credit.history.2[gcd$Credit.history == 3] <- 2

ngcd$Credit.history.3[gcd$Credit.history != 4] <- 1
ngcd$Credit.history.3[gcd$Credit.history == 4] <- 2

ngcd$Credit.history.4[gcd$Credit.history != 4] <- 1
ngcd$Credit.history.4[gcd$Credit.history == 4] <- 2

#Transformation on purpose.of.loan
ngcd$purpose.of.loan.0[gcd$purpose.of.loan != 0] <- 1
ngcd$purpose.of.loan.0[gcd$purpose.of.loan == 0] <- 2

ngcd$purpose.of.loan.1[gcd$purpose.of.loan != 1] <- 1
ngcd$purpose.of.loan.1[gcd$purpose.of.loan == 1] <- 2

ngcd$purpose.of.loan.2[gcd$purpose.of.loan != 2] <- 1
ngcd$purpose.of.loan.2[gcd$purpose.of.loan == 2] <- 2

ngcd$purpose.of.loan.3[gcd$purpose.of.loan != 3] <- 1
ngcd$purpose.of.loan.3[gcd$purpose.of.loan == 3] <- 2

ngcd$purpose.of.loan.4[gcd$purpose.of.loan != 4] <- 1
ngcd$purpose.of.loan.4[gcd$purpose.of.loan == 4] <- 2

ngcd$purpose.of.loan.5[gcd$purpose.of.loan != 5] <- 1
ngcd$purpose.of.loan.5[gcd$purpose.of.loan == 5] <- 2

ngcd$purpose.of.loan.6[gcd$purpose.of.loan != 6] <- 1
ngcd$purpose.of.loan.6[gcd$purpose.of.loan == 6] <- 2

ngcd$purpose.of.loan.8[gcd$purpose.of.loan != 8] <- 1
ngcd$purpose.of.loan.8[gcd$purpose.of.loan == 8] <- 2

ngcd$purpose.of.loan.9[gcd$purpose.of.loan != 9] <- 1
ngcd$purpose.of.loan.9[gcd$purpose.of.loan == 9] <- 2

ngcd$purpose.of.loan.10[gcd$purpose.of.loan != 10] <- 1
ngcd$purpose.of.loan.10[gcd$purpose.of.loan == 10] <- 2

#Transformation on Savings.account
ngcd$Savings.account.1[gcd$Savings.account != 1] <- 1
ngcd$Savings.account.1[gcd$Savings.account == 1] <- 2

ngcd$Savings.account.2[gcd$Savings.account != 2] <- 1
ngcd$Savings.account.2[gcd$Savings.account == 2] <- 2

ngcd$Savings.account.3[gcd$Savings.account != 3] <- 1
ngcd$Savings.account.3[gcd$Savings.account == 3] <- 2

ngcd$Savings.account.4[gcd$Savings.account != 4] <- 1
ngcd$Savings.account.4[gcd$Savings.account == 4] <- 2

ngcd$Savings.account.5[gcd$Savings.account != 5] <- 1
ngcd$Savings.account.5[gcd$Savings.account == 5] <- 2

#Transformation on Years.with.present.employer
ngcd$Years.with.present.employer.1[gcd$Years.with.present.employer != 1] <- 1
ngcd$Years.with.present.employer.1[gcd$Years.with.present.employer == 1] <- 2

ngcd$Years.with.present.employer.2[gcd$Years.with.present.employer != 2] <- 1
ngcd$Years.with.present.employer.2[gcd$Years.with.present.employer == 2] <- 2

ngcd$Years.with.present.employer.3[gcd$Years.with.present.employer != 3] <- 1
ngcd$Years.with.present.employer.3[gcd$Years.with.present.employer == 3] <- 2

ngcd$Years.with.present.employer.4[gcd$Years.with.present.employer != 4] <- 1
ngcd$Years.with.present.employer.4[gcd$Years.with.present.employer == 4] <- 2

ngcd$Years.with.present.employer.5[gcd$Years.with.present.employer != 5] <- 1
ngcd$Years.with.present.employer.5[gcd$Years.with.present.employer == 5] <- 2

#Transformation on Personal(ngcd)
ngcd$sex[gcd$Personal == 1] <- 1
ngcd$sex[gcd$Personal == 2] <- 2
ngcd$sex[gcd$Personal == 3] <- 1
ngcd$sex[gcd$Personal == 4] <- 1

ngcd$status[gcd$Personal == 1] <- 2
ngcd$status[gcd$Personal == 2]<-2
ngcd$status[gcd$Personal == 3] <- 1
ngcd$status[gcd$Personal == 4]<-2

#Transformation on Personal(rgcd)
rgcd$sex[gcd$Personal == 1] <- 1
rgcd$sex[gcd$Personal == 2] <- 2
rgcd$sex[gcd$Personal == 3] <- 1
rgcd$sex[gcd$Personal == 4] <- 1

rgcd$status[gcd$Personal == 1] <- 2
rgcd$status[gcd$Personal == 2]<-2
rgcd$status[gcd$Personal == 3] <- 1
rgcd$status[gcd$Personal == 4]<-2

#Transformation on Personal(gcd)
gcd$sex[gcd$Personal == 1] <- 1
gcd$sex[gcd$Personal == 2] <- 2
gcd$sex[gcd$Personal == 3] <- 1
gcd$sex[gcd$Personal == 4] <- 1

gcd$status[gcd$Personal == 1] <- 2
gcd$status[gcd$Personal == 2]<-2
gcd$status[gcd$Personal == 3] <- 1
gcd$status[gcd$Personal == 4]<-2

#Transformation on other.debtors.guarantors
ngcd$other.debtors.guarantors.1[gcd$other.debtors.guarantors != 1] <- 1
ngcd$other.debtors.guarantors.1[gcd$other.debtors.guarantors == 1] <- 2

ngcd$other.debtors.guarantors.2[gcd$other.debtors.guarantors != 2] <- 1
ngcd$other.debtors.guarantors.2[gcd$other.debtors.guarantors == 2] <- 2

ngcd$other.debtors.guarantors.3[gcd$other.debtors.guarantors != 3] <- 1
ngcd$other.debtors.guarantors.3[gcd$other.debtors.guarantors == 3] <- 2

#Transformation on Assets
ngcd$Assets.1[gcd$Assets != 1] <- 1
ngcd$Assets.1[gcd$Assets == 1] <- 2

ngcd$Assets.2[gcd$Assets != 2] <- 1
ngcd$Assets.2[gcd$Assets == 2] <- 2

ngcd$Assets.3[gcd$Assets != 3] <- 1
ngcd$Assets.3[gcd$Assets == 3] <- 2

ngcd$Assets.4[gcd$Assets != 4] <- 1
ngcd$Assets.4[gcd$Assets == 4] <- 2

#Transformation on Other.installment.plans
ngcd$Other.installment.plans.1[gcd$Other.installment.plans != 1] <- 1
ngcd$Other.installment.plans.1[gcd$Other.installment.plans == 1] <- 2

ngcd$Other.installment.plans.2[gcd$Other.installment.plans != 2] <- 1
ngcd$Other.installment.plans.2[gcd$Other.installment.plans == 2] <- 2

ngcd$Other.installment.plans.3[gcd$Other.installment.plans != 3] <- 1
ngcd$Other.installment.plans.3[gcd$Other.installment.plans == 3] <- 2

#Transformation on Housing
ngcd$Housing.1[gcd$Housing != 1] <- 1
ngcd$Housing.1[gcd$Housing == 1] <- 2

ngcd$Housing.2[gcd$Housing != 2] <- 1
ngcd$Housing.2[gcd$Housing == 2] <- 2

ngcd$Housing.3[gcd$Housing != 3] <- 1
ngcd$Housing.3[gcd$Housing == 3] <- 2

#Transformation on Job.Classification
ngcd$Job.Classification.1[gcd$Job.Classification != 1] <- 1
ngcd$Job.Classification.1[gcd$Job.Classification == 1] <- 2

ngcd$Job.Classification.2[gcd$Job.Classification != 2] <- 1
ngcd$Job.Classification.2[gcd$Job.Classification == 2] <- 2

ngcd$Job.Classification.3[gcd$Job.Classification != 3] <- 1
ngcd$Job.Classification.3[gcd$Job.Classification == 3] <- 2

ngcd$Job.Classification.4[gcd$Job.Classification != 4] <- 1
ngcd$Job.Classification.4[gcd$Job.Classification == 4] <- 2

#Transformation on purpose.of.loan
ngcd$purpose.of.loan.0[gcd$purpose.of.loan == 0] <- 1
ngcd$purpose.of.loan.0[gcd$purpose.of.loan != 0] <- 0
ngcd$purpose.of.loan.1[gcd$purpose.of.loan == 1] <- 1
ngcd$purpose.of.loan.1[gcd$purpose.of.loan != 1] <- 0
ngcd$purpose.of.loan.2[gcd$purpose.of.loan == 2] <- 1
ngcd$purpose.of.loan.2[gcd$purpose.of.loan != 2] <- 0
ngcd$purpose.of.loan.3[gcd$purpose.of.loan == 3] <- 1
ngcd$purpose.of.loan.3[gcd$purpose.of.loan != 3] <- 0
ngcd$purpose.of.loan.4[gcd$purpose.of.loan == 4] <- 1
ngcd$purpose.of.loan.4[gcd$purpose.of.loan != 4] <- 0
ngcd$purpose.of.loan.5[gcd$purpose.of.loan == 5] <- 1
ngcd$purpose.of.loan.5[gcd$purpose.of.loan != 5] <- 0
ngcd$purpose.of.loan.6[gcd$purpose.of.loan == 6] <- 1
ngcd$purpose.of.loan.6[gcd$purpose.of.loan != 6] <- 0
ngcd$purpose.of.loan.8[gcd$purpose.of.loan == 8] <- 1
ngcd$purpose.of.loan.8[gcd$purpose.of.loan != 8] <- 0
ngcd$purpose.of.loan.9[gcd$purpose.of.loan == 9] <- 1
ngcd$purpose.of.loan.9[gcd$purpose.of.loan != 9] <- 0
ngcd$purpose.of.loan.10[gcd$purpose.of.loan == 10] <- 1
ngcd$purpose.of.loan.10[gcd$purpose.of.loan != 10] <- 0

#Transformation on Credit.offered(ngcd)
ngcd$Credit.offered.[gcd$Credit.offered. == 1] <- 0
ngcd$Credit.offered.[gcd$Credit.offered. == 2] <- 1

#Transformation on Credit.offered(rgcd)
rgcd$Credit.offered.[gcd$Credit.offered. == 1] <- 0
rgcd$Credit.offered.[gcd$Credit.offered. == 2] <- 1

#Transformation on Credit.offered(gcd)
gcd$Credit.offered.[gcd$Credit.offered. == 1] <- 0
gcd$Credit.offered.[gcd$Credit.offered. == 2] <- 1

#-----------3) Determine if there are any natural grouping of the credit applications using---------------
#------------------------------clustering techniques (k-means / MDS)--------------------------------------
#K-Means
# Important to scale first before doing cluster analysis
ngcd<-na.omit(ngcd)
sgcd <- as.data.frame(scale(ngcd))

#Check for each variable how many have missing records
sapply(sgcd, function(x) sum(is.na(x)))

# using standard k-means in R (scaled)
km <- kmeans(sgcd, 3, iter.max = 25, nstart = 5, algorithm="Hartigan-Wong")
autoplot(km, data = sgcd)

#Check for each variable how many unique values are there
sapply(sgcd, function(x) length(unique(x)))


#K-Means
# Important to scale first before doing cluster analysis
ngcd<-na.omit(ngcd)
sgcd <- as.data.frame(scale(gcd))

#Check for each variable how many have missing records
sapply(sgcd, function(x) sum(is.na(x)))

# using standard k-means in R (scaled)
km <- kmeans(sgcd, 3, iter.max = 25, nstart = 5, algorithm="Hartigan-Wong")
autoplot(km, data = sgcd)

#Check for each variable how many unique values are there
sapply(sgcd, function(x) length(unique(x)))



#Multi-Dimensional Scaling
# first need to convert the data into a distance matrix
# euclidean distances between the rows
d <- dist(gcd, method = "canberra", diag=TRUE)

# Apply classical MDS 
fit <- cmdscale(d, eig=TRUE, k=2) # k is the number of dim
fit # view results

# visualise in 2D using labels
autoplot(fit, shape = FALSE, label.colour = 'blue', label.size = 3)

# visualise using ggplot
ggplot(as.data.frame(fit$points), aes(fit$points[,1], -fit$points[,2], label = rownames(fit$points))) +
  geom_text(check_overlap = TRUE, size=3) +
  #xlab('Log10(V1)') + ylab('Log10(V2)') +
  #scale_x_continuous(breaks = NULL, trans='log2') +
  #scale_y_continuous(breaks = NULL, trans='log2')
  xlab('V1') + ylab('V2')


# visualise in 3D
isoFit <- isoMDS(d, k=3) # k is the number of dim
isoFit # view results

plot3d(isoFit$points, size=5)

g <- graph.full(nrow(fit$points))
V(g)$label <- rownames(fit$points)
layout <- layout.mds(g, dist = as.matrix(d))
plot(g,
     layout = layout,
     vertex.size = 4,
     vertex.label.font=1,
     vertex.label.family='sans',
     vertex.label.cex=0.75,
     vertex.label.dist=-0.5)






#-------------------4) Formulate and construct a suitable regression model--------------------------------
#create a separate model training and testing set (typically 80/20)
train <- gcd[1:800,]
test <- gcd[801:1001,]

# create a logistic regression model with the training data
# target is Credit.offered., predictor variables are all values
model <- glm(Credit.offered. ~.,family=binomial(link='logit'),data=train)
# review the model
summary(model)

#----------------------------------5) Step through your model and----------------------------------------- 
#--- determine if there is any multicollinearity in the predictors and adjust your model accordingly.-----
step <- stepAIC(model, direction="both")
step$anova # display results
summary(step)

model2 <- glm(Credit.offered. ~ Loan.Duration +
                Status.of.account  +
                Credit.history   + 
                Savings.account, 
              family=binomial(link='logit'), data=train)
summary(model2)

# odds ratios and 95% CI
exp(cbind(OR = coef(model), confint(model)))

# reviewing the model using chisq test across each predictor in sequence
# to determine the relative importance of each predictor
anova(model, test="Chisq")

#-------------7) Assess the predictive ability of the model and comment on your assessment.---------------
# use the model to predict for cases 2 - 8 in the test set
results <- predict(model2,newdata=test[-21],type='response')
# the results of the model is a probability value and must be mapped to 1 or 0
fitted.results <- ifelse(results > 0.5, 1, 0)

# determine the mis-classification error
missedClassified <- sum(fitted.results != test$Credit.offered.)/nrow(test)
# calculate the accuracy
print(paste('Accuracy',1-missedClassified))

#---8) Construct a ROC curve and calculate the AUC to evaluate the regression model that you have built.--
# Is the model you have constructed effective?
predict <- prediction(results, test$Credit.offered.)
perf <- performance(predict, measure = "tpr", x.measure = "fpr")
plot(perf, main="ROC Curve", colorize = TRUE)

auc<- performance(predict, measure = "auc")
auc<-auc@y.values[[1]]
auc

#and then a lift chart
perf<-performance(predict, "lift","rpp")
plot(perf,main="Lift Curve",colorize=T)


confMat <- confusionMatrix(data = fitted.results, reference = test$Credit.offered., positive = "1")









#-------------------4) Formulate and construct a suitable regression model--------------------------------
#create a separate model training and testing set (typically 80/20)
rtrain <- rgcd[1:800,]
rtest <- rgcd[801:1001,]

# create a logistic regression model with the training data
# target is Credit.offered., predictor variables are all values
rmodel <- glm(Credit.offered. ~.,family=binomial(link='logit'),data=rtrain)
# review the model
summary(rmodel)

#----------------------------------5) Step through your model and----------------------------------------- 
#--- determine if there is any multicollinearity in the predictors and adjust your model accordingly.-----
step <- stepAIC(rmodel, direction="both")
step$anova # display results
summary(step)

rmodel2 <- glm(Credit.offered. ~ Status.of.account +
                 Loan.Duration + 
                 purpose.of.loan +
                 Credit.history, 
               family=binomial(link='logit'), data=rtrain)
summary(rmodel2)

# odds ratios and 95% CI
exp(cbind(OR = coef(rmodel2), confint(rmodel2)))

# reviewing the model using chisq test across each predictor in sequence
# to determine the relative importance of each predictor
anova(rmodel2, test="Chisq")


#-------------7) Assess the predictive ability of the model and comment on your assessment.---------------
# use the model to predict for cases 2 - 8 in the test set
rresults <- predict(rmodel2,newdata=rtest[-21],type='response')
# the results of the model is a probability value and must be mapped to 1 or 0
rfitted.results <- ifelse(rresults > 0.5, 1, 0)

# determine the mis-classification error
rmissedClassified <- sum(rfitted.results != rtest$Credit.offered.)/nrow(rtest)
# calculate the accuracy
print(paste('Accuracy',1-rmissedClassified))

#---8) Construct a ROC curve and calculate the AUC to evaluate the regression model that you have built.--
# Is the model you have constructed effective?
rpredict <- prediction(rresults, rtest$Credit.offered.)
perf <- performance(rpredict, measure = "tpr", x.measure = "fpr")
plot(perf, main="ROC Curve", colorize = TRUE)

auc<- performance(rpredict, measure = "auc")
auc<-auc@y.values[[1]]
auc

#and then a lift chart
perf<-performance(rpredict, "lift","rpp")
plot(perf,main="Lift Curve",colorize=T)


confMat <- confusionMatrix(data = rfitted.results, reference = rtest$Credit.offered., positive = "1")







#-------------------4) Formulate and construct a suitable regression model--------------------------------
#create a separate model training and testing set (typically 80/20)
Ntrain <- ngcd[1:800,]
Ntest <- ngcd[801:1001,]

# create a logistic regression model with the training data
# target is Credit.offered., predictor variables are all values
Nmodel <- glm(Credit.offered. ~.,family=binomial(link='logit'),data=Ntrain)
# review the model
summary(Nmodel)



#----------------------------------5) Step through your model and----------------------------------------- 
#--- determine if there is any multicollinearity in the predictors and adjust your model accordingly.-----
step <- stepAIC(Nmodel2, direction="both")
step$anova # display results
summary(step)

Nmodel2 <- glm(Credit.offered. ~ gcd.Loan.Duration + 
                 Status.of.account.1  + 
                 Status.of.account.2   +
                 Credit.history.3 +
                 purpose.of.loan.0 +
                 purpose.of.loan.6  +
                 status  +
                 Years.with.present.employer.4+
                 Savings.account.1 +
                 Credit.history.1, 
              family=binomial(link='logit'), data=Ntrain)
summary(Nmodel2)

# odds ratios and 95% CI
exp(cbind(OR = coef(Nmodel2), confint(Nmodel2)))

# reviewing the model using chisq test across each predictor in sequence
# to determine the relative importance of each predictor
anova(Nmodel, test="Chisq")



#-------------7) Assess the predictive ability of the model and comment on your assessment.---------------
# use the model to predict for cases 2 - 8 in the test set
Nresults <- predict(Nmodel2,newdata = Ntest[-55],type='response')
# the results of the model is a probability value and must be mapped to 1 or 0
Nfitted.results <- ifelse(Nresults > 0.5, 1, 0)

# determine the mis-classification error
missedClassified <- sum(Nfitted.results != Ntest$Credit.offered.)/nrow(Ntest)
# calculate the accuracy
print(paste('Accuracy',1-missedClassified))

#---8) Construct a ROC curve and calculate the AUC to evaluate the regression model that you have built.--
# Is the model you have constructed effective?
Npredict <- prediction(results, Ntest$Credit.offered.)
perf <- performance(Npredict, measure = "tpr", x.measure = "fpr")
plot(perf, main="ROC Curve", colorize = TRUE)

auc<- performance(Npredict, measure = "auc")
auc<-auc@y.values[[1]]
auc

#and then a lift chart
perf<-performance(Npredict, "lift","rpp")
plot(perf,main="Lift Curve",colorize=T)

confMat <- confusionMatrix(data = Nfitted.results, reference = Ntest$Credit.offered., positive = "1")