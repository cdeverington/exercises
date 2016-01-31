##################################
# RTI Data Science Exercise 1    #
# By: Cory Everington            #
# Date: 1/26/16                  #
# Data: flatten.csv              #
##################################

# Libraries Imported
library(lattice)
library(caret)
library(Hmisc)
library(partykit)
library(vcd)
library(PerformanceAnalytics)
library(rms)
library(ROCR)
library(epiR)
library(vcdExtra)
library(ggplot2)
library(verification)
library(brewerpal)

# Read in the csv file
file <- "/Users/CoryEverington/Google Drive/NCSUAnalytics/Fall/AA 504/JobSearch/RTI/Exercise/flatten.csv"
census <- read.csv(file)

# Makes the Database Searchable
attach(census)
names(census)

# Initial Data Exploration
# Checking how often the target variable occurs
OVER_50K <- factor(over_50k, levels=c(0,1), labels=c("Below 50k","Above 50k"))
table(OVER_50K)
histogram(OVER_50K, main="Distribution of Income", xlab="") # About 20% Above #

# Looking at the Distribution of other Variables #
histogram(age, main="Distribution of Age", xlab="") # Right Skewed #
histogram(capital_gain, main="Distribution of Capital Gain", xlab="") # Heavily Right Skewed - Large number of missing #
histogram(capital_loss, main="Distribution of Capital Loss", xlab="") # Large number missing #
histogram(capital_loss[!capital_loss == 0], main="Distribution of Capital Loss", xlab="") # Close up on histogram (excluding 0s)
histogram(hours_week, main="Distribution of Hours Per Week Worked", xlab="") # Large peak at 40hrs
histogram(Country, main="Distribution of Native Country", xlab="")  # Missing Values Present #
histogram(EdLevel, main="Distribution of Education Level", xlab="")
histogram(education_num, main="Distribution of Education Level (Continous)", xlab="")
histogram(MaritalStatus, main="Distribution of Marital Status", xlab="")
histogram(Occupation, main="Distribution of Occupation", xlab="") # Missing Values Present #
histogram(Race, main="Distribution of Race", xlab="")
histogram(RelationshipStatus, main="Distribution of Relationship Status", xlab="")
histogram(Sex, main="Distribution of Sex", xlab="")
histogram(WorkClass, main="Distribution of Work Class Occupation", xlab="") # Missing Values Present

#---------------------#
# Subsetting the Data #
#---------------------#

# Splitting the Data into Training (60%), Testing (20%), and Validation (20%)
# Used a seed to make sure same samples generated each time
set.seed(12345); ind <- sample(3, nrow(census), replace=TRUE, prob=c(0.6,0.2,0.2))
histogram(ind, main="Distribution of Split", xlab="")
train.census <- census[ind==1,]
test.census <- census[ind==2,]
valid.census <- census[ind==3,]

#---------------------#
# Data Exploration    #
#---------------------#

# Generating Summary Statistics
summary(train.census)
describe(train.census)

#----------------------------#
#   Transforming Variaables  #
#----------------------------#

# Age #
train.census$Age_SqRt <- sqrt(train.census$age)
train.census$Age_log <- log(train.census$age)
histogram(sqrt(age), main="Square root of Age", xlab="")
histogram(log(age), main="Log of Age", xlab="") # Log of Age has best correlation #

# Hours Per Week # Decided on No Transformation
hours <- data.frame(train.census$over_50k,train.census$hours_week )
hours$log <- log(train.census$hours_week)
hours$sqrt <- sqrt(train.census$hours_week)
hours$cube <- (train.census$hours_week)*(train.census$hours_week)*(train.census$hours_week)
hours$sq <- (train.census$hours_week)*(train.census$hours_week)
corr <- hours
cor(corr, method="pearson")
chart.Correlation(corr)

# Changed Country of Origin to Binary #
train.census$Country_BIN <- ifelse(train.census$Country == "United-States",1,0) 
train.census$Country_BIN <- as.factor(train.census$Country_BIN)
histogram(train.census$Country_BIN , main="Country Bins", xlab="")

# Looking at Correlation between Variables
corr <- train.census[, c(11,12,13,15)]
cor(corr, method="pearson")
chart.Correlation(corr)

corr <- train.census[, c(15,2,24,25)]
cor(corr, method="pearson")
chart.Correlation(corr)

train.census$education_num <- as.numeric(train.census$education_num)

corr <- train.census[, c(5,15)]
cor(corr, method="pearson")
chart.Correlation(corr)


# Binning Capital Gains and Losses #
# Using Decision Tree to find Splits

# Binning for Capital Gains
bintree1 <- ctree(over_50k ~ capital_gain, data=train.census)
plot(bintree1, type="simple", gp = gpar(fontsize=8))
plot(bintree1, type="extended", gp = gpar(fontsize=8))

# Bin for 0, <= 5060, 5060-6849, >6849
train.census$CapGain_Bin <- ifelse(train.census$capital_gain == 0,"0",
                                   ifelse(train.census$capital_gain <= 5060, "1-5060",
                                          ifelse(train.census$capital_gain <= 6849, "5060-6849",">6849")))
train.census$CapGain_Bin <- as.factor(train.census$CapGain_Bin)
histogram(train.census$CapGain_Bin , main="Capital Gain Bins", xlab="")

# Binning for Capital Losses
bintree2 <- ctree(over_50k ~ capital_loss, data=train.census, control=ctree_control(minprob=.0001, maxdepth = 4))
plot(bintree2, type="simple", gp = gpar(fontsize=8))
plot(bintree2, type="extended", gp = gpar(fontsize=8))
# Bin 0-1564, 1564-1816, 1816-1977, 1977-2163, 2163-2352, 2352-2377

train.census$CapLoss_Bin <- ifelse(train.census$capital_loss== 0,"0",
                                   ifelse(train.census$capital_loss <= 1564, "1-1564",
                                          ifelse(train.census$capital_loss <= 1816, "1564-1816",
                                                 ifelse(train.census$capital_loss <= 1977, "1816-1977",
                                                        ifelse(train.census$capital_loss <= 2377, "1977-2377",">2377")))))
train.census$CapLoss_Bin <- as.factor(train.census$CapLoss_Bin)
histogram(train.census$CapLoss_Bin , main="Capital Loss Bins", xlab="")

# Collapsing Some Levels of the Work Class and Education Level Variable 
# Needed to stabilize the parameter estimates in the model (counts were too low)

# Work Class (Without-Pay and Never-Worked)
crossWC <- table(WorkClass, over_50k)
histogram(~over_50k | WorkClass, train.census)
mosaic(table(WorkClass, over_50k))
assocstats(crossWC)
crossWC

train.census$WorkClass2 <- train.census$WorkClass
train.census$WorkClass2[train.census$WorkClass2 == "Without-pay"] <- "?"
train.census$WorkClass2[train.census$WorkClass2 == "Never-worked"] <- "?"
train.census$WorkClass2 <- factor(train.census$WorkClass2)
histogram(~over_50k | train.census$WorkClass2)
crossWC <- table(train.census$WorkClass2, train.census$over_50k)
crossWC

# Education Level - combining Preschool and 1st-4th
train.census$EdLevel2 <- train.census$EdLevel
train.census$EdLevel2[train.census$EdLevel2 == "Preschool"] <- "1st-4th"
train.census$EdLevel2 <- factor(train.census$EdLevel2)
histogram(~over_50k | train.census$EdLevel2)
crossEd <- table(train.census$EdLevel2, train.census$over_50k)
crossEd

# Looking at Relationship w/ Target
cross <- table(train.census$CapGain_Bin, train.census$over_50k)
histogram(~ train.census$over_50k|train.census$CapGain_Bin, main="Histogram of Over 50k by Capital Gain Bins",xlab="")
mosaic(table(train.census$CapGain_Bin, train.census$over_50k))
assocstats(cross)

cross <- table(train.census$CapLoss_Bin, train.census$over_50k)
histogram(~ train.census$over_50k|train.census$CapLoss_Bin, main="Histogram of Over 50k by Capital Gain Bins",xlab="")
mosaic(table(train.census$CapLoss_Bin, train.census$over_50k))
assocstats(cross)

cross <- table(MaritalStatus, over_50k)
histogram(~ over_50k|MaritalStatus, main="Histogram of Over 50k by Marital Status",xlab="")
mosaic(table(MaritalStatus, over_50k))
assocstats(cross)

cross2 <- table(Sex, over_50k)
histogram(~ over_50k|Sex, main="Histogram of Over 50k by Sex",xlab="")
mosaic(table(Sex, over_50k))
assocstats(cross2)

cross3 <- table(EdLevel, over_50k)
histogram(~ over_50k|EdLevel, main="Histogram of Over 50k by Education Level",xlab="")
mosaic(table(EdLevel, over_50k))
assocstats(cross3)

cross4 <- table(Race, over_50k)
histogram(~ over_50k|Race, main="Histogram of Over 50k by Race",xlab="")
mosaic(table(Race, over_50k))
assocstats(cross4)

cross5 <- table(RelationshipStatus, over_50k)
histogram(~ over_50k|RelationshipStatus, main="Histogram of Over 50k by Relationship Status",xlab="")
mosaic(table(RelationshipStatus, over_50k))
assocstats(cross5)

#-----------------------------#
#   Looking at Interactions   #
#-----------------------------#
# Detecting Interactions #
table(train.census$Race, train.census$over_50k, train.census$RelationshipStatus)
histogram(~train.census$over_50k|train.census$Race + train.census$Sex, main="Distribution of Income by Race and Sex", xlab="Income (Over 50k)")
CMHtest(table(train.census$Race, train.census$over_50k, train.census$Sex),data=train.census)
mantelhaen.test(table(Race, over_50k, Sex))

table(train.census$Race, train.census$over_50k, train.census$RelationshipStatus)
histogram(~over_50k|Race + RelationshipStatus, data=train.census, main="Distribution of Income by Race and Sex", xlab="Income (Over 50k)")
CMHtest(table(Race, over_50k, RelationshipStatus),data=train.census)
mantelhaen.test(table(Race, over_50k, RelationshipStatus))

Logit.Model <- lrm(over_50k ~ Age_log*Sex + CapGain_Bin*Sex + CapLoss_Bin*Sex + hours_week*Sex +
                     Country_BIN*Sex + EdLevel2*Sex + MaritalStatus*Sex + 
                     Race*Sex + Sex + WorkClass2*Sex, data=train.census)
Logit.Model

Logit.Model <- lrm(over_50k ~ Age_log*Country_BIN + CapGain_Bin*Country_BIN + CapLoss_Bin*Country_BIN + 
                     hours_week*Country_BIN + EdLevel2*Country_BIN + 
                     Country_BIN + WorkClass2*Country_BIN, data=train.census)
Logit.Model

Logit.Model <- lrm(over_50k ~ EdLevel2*RelationshipStatus, data=train.census) # Not significant
Logit.Model

# Possible Significant Interactions:
# Age_SqRt*CapGain_Bin, Age_SqRt*MaritalStatus

#---------------------#
#      Modeling       #
#---------------------#

#------ Decision Tree ------#

# Using a Decision Tree as a Baseline Model
census.tree <- ctree(over_50k ~  Age_log + CapGain_Bin + CapLoss_Bin + hours_week +
                       Country_BIN + EdLevel2 + MaritalStatus + Occupation + education_num +
                       Race + RelationshipStatus + Sex + WorkClass2, data=train.census)
plot(census.tree)
plot(census.tree, type="simple", gp = gpar(fontsize=8))
plot(census.tree, type="extended", gp = gpar(fontsize=8))

# ROC Curve
pred.tree <- predict(census.tree)
roc_pred.tree <- prediction(pred.tree, train.census$over_50k)
perf.tree <- performance(roc_pred.tree, measure="tpr", x.measure="fpr")
plot(perf.tree, col="blue", lty=3, lwd=3)
# Calculating AUC
auc.tree <- performance(roc_pred.tree,"auc")
# now converting S4 class to vector
auc.tree <- unlist(slot(auc.tree, "y.values"))
# adding ROC AUC to the center of the plot
minauc<-min(round(auc.tree, digits = 2))
minauct.tree <- paste(c("AUC Decision Tree  = "),minauc,sep="")
legend(0.4,0.4,c(minauct.tree))
abline(a=0,b=1)

#------ Logistic Regression using LRM ------#
Logit.Model <- lrm(over_50k ~ Age_log + CapGain_Bin + CapLoss_Bin + hours_week + EdLevel2 +
                    MaritalStatus + Occupation  + RelationshipStatus + Sex +
                     Age_log*CapGain_Bin + Age_log*MaritalStatus + WorkClass2,
                    data=train.census) # Took out Race and Country_Bin - doesn't significantly improve model
Logit.Model 
pred.lrm <- predict(Logit.Model)
roc_pred.lrm <- prediction(pred.lrm, train.census$over_50k)
perf.lrm <- performance(roc_pred.lrm, measure="tpr", x.measure="fpr")
plot(perf.lrm, col="red", lty=3, lwd=3, add=TRUE)

# Calculating AUC and Adding to Label on ROC Plot
auc.lrm <- performance(roc_pred.lrm,"auc")
auc.lrm <- unlist(slot(auc.lrm, "y.values")) # now converting S4 class to vector
minauc.lrm<-min(round(auc.lrm, digits = 2))
minauct.lrm <- paste(c("AUC LRM = "),minauc,sep="")
abline(a=0,b=1)
legend(0.4,0.5,c(minauct.tree,minauct.lrm), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("blue","red"))
# C Statistic: 0.919 ~same as decision Tree #

#------ Modeling using GLM ------#

Logit.Model2 <- glm(over_50k ~ Age_log + CapGain_Bin + CapLoss_Bin + hours_week + EdLevel2 +
                      MaritalStatus + Occupation + Race + RelationshipStatus + Sex + Country_BIN
                      + Age_log*CapGain_Bin + Age_log*MaritalStatus + education_num,
                      data=train.census, family=binomial(logit))
summary(Logit.Model2) 
# Model Selection Techniques
# Backwards Selection
Backwards.GLM <- drop1(Logit.Model2, test = "LRT")
Backwards.GLM # Suggested we drop Race #
Logit.Backwards <- glm(over_50k ~ Age_log + CapGain_Bin + CapLoss_Bin + hours_week + EdLevel2 +
                       MaritalStatus + Occupation + RelationshipStatus + Sex + Country_BIN +
                       Age_log*CapGain_Bin + Age_log*MaritalStatus,
                       data=train.census, family=binomial(logit))
# AIC - Stepwise
Logit.Model3 <- glm(over_50k ~ Age_log + CapGain_Bin + CapLoss_Bin + hours_week + EdLevel2 +
                         MaritalStatus + Occupation + RelationshipStatus + Sex + Country_BIN +
                         Age_log:CapGain_Bin + Age_log:MaritalStatus,
                         data=train.census, family=binomial(logit))
Logit.AIC <- step(Logit.Model3, direction="both", trace=1)
summary(Logit.AIC)

pred.glm <- predict(Logit.Model3)
roc_pred.glm <- prediction(pred.glm, train.census$over_50k)
perf.glm <- performance(roc_pred.glm, measure="tpr", x.measure="fpr")
plot(perf.glm, col="green", lty=3, lwd=3, add=TRUE)

# Calculating AUC and Adding to Label on ROC Plot
auc.glm <- performance(roc_pred.glm,"auc")
auc.glm <- unlist(slot(auc.glm, "y.values")) # now converting S4 class to vector
minauc.glm<-min(round(auc.glm, digits = 2))
minauct.glm <- paste(c("AUC GLM = "),minauc,sep="")
abline(a=0,b=1)
legend(0.4,0.5,c(minauct.tree,minauct.lrm,minauct.glm), lty=c(1,1,1), 
       lwd=c(2.5,2.5,2.5), col=c("blue","red","green"))

# Minimum number of Variables
Logit.Age <- lrm(over_50k ~ Age_log, data=train.census) #R2 .110
Logit.Gain <-lrm(over_50k ~ CapGain_Bin, data=train.census) #R2 .180
Logit.Loss <-lrm(over_50k ~ CapLoss_Bin, data=train.census) #R2 .073
Logit.US_Bin <-lrm(over_50k ~ Country_BIN, data=train.census) # Can probably drop Country bin, while significant, it explains vary litte
Logit.hours <- lrm(over_50k ~ hours_week, data=train.census) #R2 .075
Logit.Ed <- lrm(over_50k ~ EdLevel2, data=train.census)   #R2 .180
Logit.Mar  <- lrm(over_50k ~ MaritalStatus, data=train.census)  #R2 .297
Logit.Occ <- lrm(over_50k ~ Occupation, data=train.census) #R2 .182
Logit.Rel <- lrm(over_50k ~ RelationshipStatus, data=train.census) #R2 .310
Logit.Sex <- lrm(over_50k ~ Sex, data=train.census) #R2 .076

# Smaller Model Using the Least Number of Variables, while maintaining a high AUC
Small.Model <- lrm(over_50k ~ RelationshipStatus + Occupation + EdLevel2 + CapGain_Bin + 
                     Age_log + CapLoss_Bin, data=train.census)
Small.Model # LRM has better summary statistics #

Small.Model <- glm(over_50k ~ RelationshipStatus + Occupation + EdLevel2 + CapGain_Bin + 
                  Age_log + CapLoss_Bin, data=train.census) # Used GLM to calculate Odds Ratio
summary(Small.Model)
pred.small <- predict(Small.Model)
roc_pred.small <- prediction(pred.small, train.census$over_50k)
perf.small <- performance(roc_pred.small, measure="tpr", x.measure="fpr")
plot(perf.small, col="green", lty=3, lwd=3, add=FALSE)
auc.small <- performance(roc_pred.small,"auc")
auc.small <- unlist(slot(auc.small, "y.values"))

#-------------------------------#
#  Calculating the Odds-Ratios  #
#-------------------------------#
OR <- exp(coef(Small.Model)[-1])
OR
OR.CI <- exp(cbind(OR = coef(Small.Model), confint(Small.Model)))[-1,]
OR.CI

Insig <- OR.CI[,2]<1 & OR.CI[,3]>1
OR.CI <- cbind(OR.CI, Insig)
sort.OR.CI <- OR.CI[order(OR)]
sort.OR.CI

# Plotting the Odds Ratios
par(mar = c(15,4,4,2))
plot(1:nrow(OR.CI), OR.CI[,1], ylim = c(.2, 1.6), type='n', xaxt='n', xlab="", 
     ylab="Odds Ratios", main="Odds Ratios with 95% Confidence Limits", frame.plot=TRUE)
segments(1:nrow(OR.CI), OR.CI[,2], 1:nrow(OR.CI), OR.CI[,3], lwd=2, col=factor(Insig))
points(1:nrow(OR.CI), OR.CI[,1], pch=19, cex=0.8)
xLabels <- names(OR)
axis(1, seq(1, nrow(OR.CI), by=1), labels=xLabels, las=2)
abline(h=1.0, lty=2)
par(mar=c(5.1, 4.1, 4.1, 2.1))
#--------------------------------------------#
#  Final Models for Testing and Validation   #
#--------------------------------------------#
# Model 1. Decision Tree #
census.tree <- ctree(over_50k ~  Age_log + CapGain_Bin + CapLoss_Bin + hours_week +
                     Country_BIN + EdLevel2 + MaritalStatus + Occupation + education_num +
                     Race + RelationshipStatus + Sex + WorkClass2, data=train.census)
# Model 2. LRM with Interactions #
train.lrm <- lrm(over_50k ~ Age_log + CapGain_Bin + CapLoss_Bin + hours_week + EdLevel2 +
                 MaritalStatus + Occupation  + RelationshipStatus + Sex +
                 Age_log*CapGain_Bin + Age_log*MaritalStatus + WorkClass2, data=train.census)
# Model 3. GLM_AIC with Interactions #
train.GLM_AIC <- glm(over_50k ~ Age_log + CapGain_Bin + CapLoss_Bin + hours_week + EdLevel2 +
                     MaritalStatus + Occupation + RelationshipStatus + Sex + Country_BIN +
                     Age_log:CapGain_Bin + Age_log:MaritalStatus, data=train.census, family=binomial(logit))
# Model 4. Minimum Variables Model (LRM) #
train.min <- lrm(over_50k ~ RelationshipStatus + Occupation + EdLevel2 + CapGain_Bin + 
                     Age_log + CapLoss_Bin, data=train.census)

#---------------------------#
#     Testing Data Set      #
#---------------------------#
# Performing Variable Transformations
# Age #
test.census$Age_log <- log(test.census$age)
histogram(log(age), main="Log of Age", xlab="") 

# Changed Country of Origin to Binary #
test.census$Country_BIN <- ifelse(test.census$Country == "United-States",1,0) 
test.census$Country_BIN <- as.factor(test.census$Country_BIN)
histogram(test.census$Country_BIN , main="Country Bins", xlab="")

# Capital Gains - Bin for 0, <= 5060, 5060-6849, >6849
test.census$CapGain_Bin <- ifelse(test.census$capital_gain == 0,"0",
                                   ifelse(test.census$capital_gain <= 5060, "1-5060",
                                          ifelse(test.census$capital_gain <= 6849, "5060-6849",">6849")))
test.census$CapGain_Bin <- as.factor(test.census$CapGain_Bin)
histogram(test.census$CapGain_Bin , main="Capital Gain Bins", xlab="")

# Binning for Capital Losses: Bin 0-1564, 1564-1816, 1816-1977, 1977-2163, 2163-2352, 2352-2377
test.census$CapLoss_Bin <- ifelse(test.census$capital_loss== 0,"0",
                                   ifelse(test.census$capital_loss <= 1564, "1-1564",
                                          ifelse(test.census$capital_loss <= 1816, "1564-1816",
                                                 ifelse(test.census$capital_loss <= 1977, "1816-1977",
                                                        ifelse(test.census$capital_loss <= 2377, "1977-2377",">2377")))))
test.census$CapLoss_Bin <- as.factor(test.census$CapLoss_Bin)
histogram(test.census$CapLoss_Bin , main="Capital Loss Bins", xlab="")

# Work Class (Without-Pay and Never-Worked)
test.census$WorkClass2 <- test.census$WorkClass
test.census$WorkClass2[test.census$WorkClass2 == "Without-pay"] <- "?"
test.census$WorkClass2[test.census$WorkClass2 == "Never-worked"] <- "?"
test.census$WorkClass2 <- factor(test.census$WorkClass2)
histogram(~over_50k | test.census$WorkClass2)
crossWC <- table(test.census$WorkClass2, test.census$over_50k)

# Education Level - combining Preschool and 1st-4th
test.census$EdLevel2 <- test.census$EdLevel
test.census$EdLevel2[test.census$EdLevel2 == "Preschool"] <- "1st-4th"
test.census$EdLevel2 <- factor(test.census$EdLevel2)
histogram(~over_50k | test.census$EdLevel2)
table(test.census$EdLevel2, test.census$over_50k)

# -- Testing the Models --#
# 1. Decision Tree
test.pred.tree <- predict(census.tree, newdata = test.census)
test.roc_pred.tree <- prediction(test.pred.tree, test.census$over_50k)
test.perf.tree <- performance(test.roc_pred.tree, measure="tpr", x.measure="fpr")
plot(test.perf.tree, col="blue", lty=1, lwd=3)
test.auc.tree <- performance(test.roc_pred.tree,"auc")
test.auc.tree <- unlist(slot(test.auc.tree, "y.values"))

# 2. Logistic Regression Model
test.pred.lrm <- predict(train.lrm, newdata=test.census)
test.roc_pred.lrm <- prediction(test.pred.lrm, test.census$over_50k)
test.perf.lrm <- performance(test.roc_pred.lrm, measure="tpr", x.measure="fpr")
plot(test.perf.lrm, col="red", lty=2, lwd=3, add=TRUE)
test.auc.lrm <- performance(test.roc_pred.lrm,"auc")
test.auc.lrm <- unlist(slot(test.auc.lrm, "y.values"))

# 3. GLM - AIC Stepwise
test.pred.glm <- predict(train.GLM_AIC, newdata=test.census)
test.roc_pred.glm <- prediction(test.pred.glm, test.census$over_50k)
test.perf.glm <- performance(test.roc_pred.glm, measure="tpr", x.measure="fpr")
plot(test.perf.glm, col="purple", lty=3, lwd=3, add=TRUE)
test.auc.glm <- performance(test.roc_pred.glm,"auc")
test.auc.glm <- unlist(slot(test.auc.glm, "y.values")) 

# 4. Minimal Variables Model
test.pred.min <- predict(train.min, newdata=test.census)
test.roc_pred.min <- prediction(test.pred.min, test.census$over_50k)
test.perf.min <- performance(test.roc_pred.min, measure="tpr", x.measure="fpr")
plot(test.perf.min, col="orange", lty=4, lwd=3, add=TRUE)
test.auc.min <- performance(test.roc_pred.min,"auc")
test.auc.min <- unlist(slot(test.auc.min, "y.values")) # now converting S4 class to vector

#---------------------------#
#   Validation Data Set     #
#---------------------------#

# Performing Variable Transformations
# Age #
valid.census$Age_log <- log(valid.census$age)
histogram(log(age), main="Log of Age", xlab="") 

# Changed Country of Origin to Binary #
valid.census$Country_BIN <- ifelse(valid.census$Country == "United-States",1,0) 
valid.census$Country_BIN <- as.factor(valid.census$Country_BIN)
histogram(valid.census$Country_BIN , main="Country Bins", xlab="")

# Capital Gains - Bin for 0, <= 5060, 5060-6849, >6849
valid.census$CapGain_Bin <- ifelse(valid.census$capital_gain == 0,"0",
                                  ifelse(valid.census$capital_gain <= 5060, "1-5060",
                                         ifelse(valid.census$capital_gain <= 6849, "5060-6849",">6849")))
valid.census$CapGain_Bin <- as.factor(valid.census$CapGain_Bin)
histogram(valid.census$CapGain_Bin , main="Capital Gain Bins", xlab="")

# Binning for Capital Losses: Bin 0-1564, 1564-1816, 1816-1977, 1977-2163, 2163-2352, 2352-2377
valid.census$CapLoss_Bin <- ifelse(valid.census$capital_loss== 0,"0",
                                  ifelse(valid.census$capital_loss <= 1564, "1-1564",
                                         ifelse(valid.census$capital_loss <= 1816, "1564-1816",
                                                ifelse(valid.census$capital_loss <= 1977, "1816-1977",
                                                       ifelse(valid.census$capital_loss <= 2377, "1977-2377",">2377")))))
valid.census$CapLoss_Bin <- as.factor(valid.census$CapLoss_Bin)
histogram(valid.census$CapLoss_Bin , main="Capital Loss Bins", xlab="")

# Work Class (Without-Pay and Never-Worked)
valid.census$WorkClass2 <- valid.census$WorkClass
valid.census$WorkClass2[valid.census$WorkClass2 == "Without-pay"] <- "?"
valid.census$WorkClass2[valid.census$WorkClass2 == "Never-worked"] <- "?"
valid.census$WorkClass2 <- factor(valid.census$WorkClass2)
histogram(~over_50k | valid.census$WorkClass2)
crossWC <- table(valid.census$WorkClass2, valid.census$over_50k)

# Education Level - combining Preschool and 1st-4th
valid.census$EdLevel2 <- valid.census$EdLevel
valid.census$EdLevel2[valid.census$EdLevel2 == "Preschool"] <- "1st-4th"
valid.census$EdLevel2 <- factor(valid.census$EdLevel2)
histogram(~over_50k | valid.census$EdLevel2)
table(valid.census$EdLevel2, valid.census$over_50k)

# -- Testing the Models -- VALIDATION DATA SET --#
# 1. Decision Tree
valid.pred.tree <- predict(census.tree, newdata = valid.census)
valid.roc_pred.tree <- prediction(valid.pred.tree, valid.census$over_50k)
valid.perf.tree <- performance(valid.roc_pred.tree, measure="tpr", x.measure="fpr")
plot(valid.perf.tree, col="blue", lty=1, lwd=3)
valid.auc.tree <- performance(valid.roc_pred.tree,"auc")
valid.auc.tree <- unlist(slot(valid.auc.tree, "y.values"))

# 2. Logistic Regression Model
valid.pred.lrm <- predict(train.lrm, newdata=valid.census)
valid.roc_pred.lrm <- prediction(valid.pred.lrm, valid.census$over_50k)
valid.perf.lrm <- performance(valid.roc_pred.lrm, measure="tpr", x.measure="fpr")
plot(valid.perf.lrm, col="red", lty=2, lwd=3, add=TRUE)
valid.auc.lrm <- performance(valid.roc_pred.lrm,"auc")
valid.auc.lrm <- unlist(slot(valid.auc.lrm, "y.values"))

# 3. GLM - AIC Stepwise
valid.pred.glm <- predict(train.GLM_AIC, newdata=valid.census)
valid.roc_pred.glm <- prediction(valid.pred.glm, valid.census$over_50k)
valid.perf.glm <- performance(valid.roc_pred.glm, measure="tpr", x.measure="fpr")
plot(valid.perf.glm, col="purple", lty=3, lwd=3, add=TRUE)
valid.auc.glm <- performance(valid.roc_pred.glm,"auc")
valid.auc.glm <- unlist(slot(valid.auc.glm, "y.values")) 

# 4. Minimal Variables Model
valid.pred.min <- predict(train.min, newdata=valid.census)
valid.roc_pred.min <- prediction(valid.pred.min, valid.census$over_50k)
valid.perf.min <- performance(valid.roc_pred.min, measure="tpr", x.measure="fpr")
plot(valid.perf.min, col="orange", lty=4, lwd=3, add=TRUE)
valid.auc.min <- performance(valid.roc_pred.min,"auc")
valid.auc.min <- unlist(slot(valid.auc.min, "y.values")) # now converting S4 class to vector

#------------------------------#
# Generating Graph For Report  #
#------------------------------#

# Plotting Two of the Lines (Minimal Model and LRM with all significant factors)
rd.min <- data.frame(x=valid.perf.min@x.values[[1]],y=valid.perf.min@y.values[[1]])
rd.lrm <- data.frame(x=valid.perf.lrm@x.values[[1]],y=valid.perf.lrm@y.values[[1]])
rd.tree <- data.frame(x=valid.perf.tree@x.values[[1]],y=valid.perf.tree@y.values[[1]])
len <- dim(rd.min)[1]
len2 <- dim(rd.lrm)[1]
len3 <- dim(rd.tree)[1]
desc.part1 <- rep("Minimal Variables",len)
desc.part2 <- rep("Logistic Regression Model",len2)
desc.part3 <- rep("Decision Tree",len3)
Key <- c(desc.part1, desc.part2, desc.part3)
dif.col = c("navyblue","turquoise3","tomato1")
rd <- rbind(rd.min, rd.lrm, rd.tree)
rd$num <- Key
p <- ggplot(rd,aes(x=x,y=y,colour=Key)) + geom_path(size=1.5) + scale_colour_manual(values=dif.col)
p <- p + geom_segment(aes(x=0,y=0,xend=1,yend=1),colour="lightblue", linetype= 2)
p <- p + geom_text(aes(x=1, y= 0, hjust=1, vjust=0, label=paste(sep = "", "Min. Var. AUC = ",round(valid.auc.min,3))),colour="black",size=8)
p <- p + geom_text(aes(x=1, y= .10, hjust=1, vjust=0, label=paste(sep = "", "LRM AUC = ",round(valid.auc.lrm,3))),colour="black",size=8)
p <- p + geom_text(aes(x=1, y= .05, hjust=1, vjust=0, label=paste(sep = "", "Tree AUC = ",round(valid.auc.tree,3))),colour="black",size=8)
p <- p + scale_x_continuous(name= "False positive rate")
p <- p + scale_y_continuous(name= "True positive rate")

fte_theme <- function() {
  # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color="black", size=18, vjust=1.25)) +
    theme(axis.text.x=element_text(size=17,color="black")) +
    theme(axis.text.y=element_text(size=17,color="black")) +
    theme(axis.title.x=element_text(size=17,color="black", vjust=0)) +
    theme(axis.title.y=element_text(size=17,color="black", vjust=1.25))
    theme(
      panel.background = element_rect(fill="grey94"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour="white"),
      text = element_text(size=18)
    )
}
  
p + fte_theme() + ggtitle("ROC Curves for 1996 US Census Models")
