# *------------------------------------------------------------------
# | PROGRAM NAME: Analysis of MIST recruitment data
# | DATE: 25/05/2017
# | CREATED BY:  Ankur Shrivastava
# | PROJECT FILE: Recruit1.R             
# *----------------------------------------------------------------
# | PURPOSE: The purpose of this file is to analyze the various               
# |          factors responsible for a candidate joining MIST
# *------------------------------------------------------------------
# | COMMENTS:               
# |
# |  1:  
# |*------------------------------------------------------------------
# | DATA USED:  MIST recruitment data             
# |
# |
# |*------------------------------------------------------------------

install.packages("devtools", dependencies = TRUE)
install.packages("Rcpp", dependencies = TRUE)
install.packages("readxl", dependencies = TRUE)
install.packages("caTools", dependencies = TRUE)
install.packages("rpart", dependencies = TRUE)
install.packages("rpart.plot", dependencies = TRUE)
install.packages("ROCR", dependencies = TRUE)

library(devtools)
library(Rcpp)
library(readxl)

#Load the recruitment dataset.
recruit <- read_excel("Recruit1.xlsx", 1)

#First look at data in R
str(recruit)
summary(recruit)

# replace text "NA" with R's NA
for(x in 1:nrow(recruit)){
  for(y in 1:ncol(recruit)){
    if(!is.na(recruit[x,y])){
      if(recruit[x,y] == "NA"){
        recruit[x,y] <- NA
      }
    }
  }
}

#Converting categorical variables read in as text to factor variables
recruit$gender <- as.factor(recruit$gender)
recruit$hqual <- as.factor(recruit$hqual)
recruit$gate <- as.factor(recruit$gate)
recruit$pPhd <- as.factor(recruit$pPhd)
recruit$working <- as.factor(recruit$working)
recruit$org <- as.factor(recruit$org)
recruit$same_city <- as.factor(recruit$same_city)
recruit$np <- as.factor(recruit$np)
recruit$dept <- as.factor(recruit$dept)
recruit$offered <- as.factor(recruit$offered)
recruit$joined <- as.factor(recruit$joined)
recruit$csal <- as.numeric(recruit$csal)
recruit$esal <- as.numeric(recruit$esal)
recruit$osal <- as.numeric(recruit$osal)

#Removing the name column as that won't be needed for analysis
recruit <- subset(recruit, select = -c(name))

#looking at the data again
str(recruit)
summary(recruit)

library(ggplot2)
#Plot of actual numbers who joined
ggplot(recruit, aes(x = joined)) + geom_bar(fill = "red") + xlab("Candidates joined") + ylab("Number of Candidates")

#Plot of actual numbers who joined by each department
ggplot(recruit, aes( x = joined, fill = dept)) + geom_bar(position = "dodge") + xlab("Department") + ylab("Number of Candidates joined")

#Running chi squre tests 
chisq.test(recruit$joined, recruit$dept)

#Percentage of people joined by each department
ggplot(recruit, aes( x = dept, fill = joined)) + geom_bar(position = "fill") + xlab("Department") + ylab("Percentage of Candidates joined")

#Percentage of people joined by gender
ggplot(recruit, aes( x = gender, fill = joined)) + geom_bar(position = "fill") + xlab("Gender") + ylab("Percentage of Candidates joined")

#Running chi squre tests 
chisq.test(recruit$joined, recruit$gender)

#Percentage of people joined by working or not
ggplot(recruit, aes( x = working, fill = joined)) + geom_bar(position = "fill") + xlab("Currently Working") + ylab("Percentage of Candidates joined")

#Running chi squre test
chisq.test(recruit$joined, recruit$working)

#Number of people joined by years of experience
ggplot(recruit, aes( x = joined, y = exp)) + geom_boxplot() + xlab("Candidates joined") + ylab("Years of experience")

#Running one-way anova
exp.aov <- aov(recruit$exp ~ recruit$joined)
summary(exp.aov)

#Percentage of people joined by GATE qualified
ggplot(recruit, aes( x = gate, fill = joined)) + geom_bar(position = "fill") + xlab("GATE qualified") + ylab("Percentage of Candidates joined")

#Running chi squre test
chisq.test(recruit$joined, recruit$gate)

#Percentage of people joined by Highest qualification
ggplot(recruit, aes( x = hqual, fill = joined)) + geom_bar(position = "fill") + xlab("Highest Qualification") + ylab("Percentage of Candidates joined")

#Running chi squre test
chisq.test(recruit$joined, recruit$hqual)

#Percentage of people joined by pursuing Phd
ggplot(recruit, aes( x = pPhd, fill = joined)) + geom_bar(position = "fill") + xlab("Pursuing Phd") + ylab("Percentage of Candidates joined")

#Running chi squre test
chisq.test(recruit$joined, recruit$pPhd)

#Percentage of people joined by same_city
ggplot(recruit, aes( x = same_city, fill = joined)) + geom_bar(position = "fill") + xlab("Residing in same city") + ylab("Percentage of Candidates joined")

#Running chi squre test
chisq.test(recruit$joined, recruit$same_city) 

#Percentage of people joined by notice period
ggplot(recruit, aes( x = np, fill = joined)) + geom_bar(position = "fill") + xlab("Notice Period in months") + ylab("Percentage of Candidates joined")

#Running chi squre test
chisq.test(recruit$joined, recruit$np) 

#Plot of people joining by current salary
ggplot(recruit, aes( x = joined, y = csal)) + geom_boxplot() + xlab("Candidates joined") + ylab("Current Salary")

#Running one-way anova
csal.aov <- aov(recruit$csal ~ recruit$joined)
summary(csal.aov)

#Plot of people joining by expected salary
ggplot(recruit, aes( x = joined, y = esal)) + geom_boxplot() + xlab("Candidates joined") + ylab("Expected Salary")

#Running one-way anova
esal.aov <- aov(recruit$csal ~ recruit$joined)
summary(esal.aov)

#Correlation between current salary and expected salary
cor(recruit$csal, recruit$esal, use = "pairwise.complete.obs")

#----------------------------Modeling Section---------------------------------------------

table(recruit$joined)
#baseline accuracy
29/49

#Remove unnecessary variables
recruit1 <- subset(recruit, select = -c(org, offered, remarks, osal))

library(caTools)
# Randomly split data
set.seed(2017)
split = sample.split(recruit1$joined, SplitRatio = 0.7)
split

# Create training and testing sets
train = subset(recruit1, split == TRUE)
test = subset(recruit1, split == FALSE)

# Logistic Regression Model
logmodel = glm(joined ~  hqual + gate + pPhd +exp +working + same_city + np + dept + csal , data=train, family=binomial)
summary(logmodel)

#Building Decision Trees model
library(rpart)
library(rpart.plot)
treemodel = rpart(joined ~ ., data=train, minbucket = 2)
summary(treemodel)
#Plot the Decision tree.
prp(treemodel)

#----------Test and evaluation---------------------------------------------------

#Since, the number of levels are different in training and testing datasets
logmodel$xlevels[["hqual"]] <- union(logmodel$xlevels[["hqual"]], levels(test$hqual))
#Prediction using the predict function
predictlog = predict(logmodel, newdata = test, type = "response")
table(test$joined, predictlog > 0.5)
(3+6)/15

predicttrainlog = predict(logmodel, type = "response")
#Load ROCR package for plotting ROC curve
library(ROCR)

# Prediction function
ROCRtrainlogpred = prediction(predicttrainlog, train$joined)

# Performance function
ROCRtrainlogperf = performance(ROCRtrainlogpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRtrainlogperf, colorize=TRUE)

performance(ROCRtrainlogpred, "auc")@y.values


#----------------Similarly for CART model---------------------------

predictCART = predict(treemodel, newdata = test, type = "class")
table(test$joined, predictCART)
(8+2)/(15)

predicttrainCART = predict(treemodel)

# Prediction function
ROCRtrainCARTpred = prediction(predicttrainCART[,2], train$joined)

# Performance function
ROCRtrainCARTperf = performance(ROCRtrainCARTpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRtrainCARTperf, colorize=TRUE)

performance(ROCRtrainCARTpred, "auc")@y.values

#-------------------End of File-------------------------------------------