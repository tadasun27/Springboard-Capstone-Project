#############################################################################################
# Capstone_Project_Pumpitup.R
# By : Tadaaki Sun
# Cleaned and perfromed data analysis for "Pump it Up:Data Mining the Water Table" 
# competition on drivendata.org.
# I dropped, reduced the number of levels, and added features to create a predictive model.
#############################################################################################

# Load libraries
library(ggplot2)
library(dplyr)
library(verification)
library(rpart)
library(rpart.plot)
library(caTools)
library(ROCR)

#library(e1071)
#library(googleVis)
#library(caret)
#library(randomForest)

# Set seed
set.seed(27)

########################### Uploading Data ###########################
# Define train and test values
train_values_url <- "http://s3.amazonaws.com/drivendata/data/7/public/4910797b-ee55-40a7-8668-10efd5c1b960.csv"
train_labels_url <- "http://s3.amazonaws.com/drivendata/data/7/public/0bf8bc6e-30d0-4c50-956a-603fc693d966.csv"
test_values_url <- "http://s3.amazonaws.com/drivendata/data/7/public/702ddfc5-68cd-4d1d-a0de-f5f566f76d91.csv"

# Import train and test values
train_values <- read.csv(train_values_url)
train_labels <- read.csv(train_labels_url)
test_values <- read.csv(test_values_url)

# Merge data frames
train <- merge(train_labels, train_values)
test = test_values
test$status_group <- ""

# Optional cleaning environment
rm(train_values)
rm(train_labels)
rm(test_values)

# Quick glance at the data and the number of pumps in status_group (Preliminary Data Analysis)
summary(train)

### Need investigation ###
# amount_tsh - 1st quartile and median is 0, need investigation
# longitude - Min is 0 where the rest range from 33-40, need investigation
# region_code - shouldn't have max 99 when min to 3rd quartile is within 1-20
# district_code - shouldn't have max 80 when min to 3rd quartile is within 0-5
# population - investigate as there shouldn't be waterpoints where there is no population
# construction_year - Investigate the 0's.

### Take a look at ###
# funder - approximately 39000 funders out of 59400, worth investigating?
# installer - approximately 33000 funders out of 59400, worth investigating?
# region - 33000 out of 59400, decrease levels? about the same number as installer...
# public_meeting - 3000 missing. Decide what to do.
# scheme_name - 28000 missing values and 29000 levels. Watch.
# permit - Watch the 3000 missing values. Otherwise good amount.

### Reduce Levels ###
# basin - Approximately 12000 out of 59400. Reduce levels?

### Remove ###
# wpt_name - 52000 unique names out of 59400. Not significant to include in model?
# num_private - Min to 3rd quartile is 0. Remove from model?
# subvillage - 57000/59400. Too unique. Remove.
# lga - 51000/59400. Not siginificant? Remove.
# ward - 58000/59400. Not siginificant? Remove.
# recorded_by - Only GeoData Consultants. Definitely Remove.

### Choose ###
# extraction_type, extraction_type_group, extraction_type_class - seems like duplicate. Choose one.
# scheme_management and management - Similar results(vwc, wug , etc.). Choose One.
# payment_type and payment - Similar. Choose one.
# water_quality and quality_group - Similar. Choose one.
# quantity and quantity_group - Similar. Choose one.
# source and source_type - Similar. Choose one.
# waterpoint_type and waterpoint_type_group - Similar. Choose one.

### Keep but still investigate ###
#source_class, scheme_management, latitude, gps_height, date_recorded

# Merging "functional needs repair" with "non functional" 
train$status_group <- replace(train$status_group, train$status_group == "functional needs repair", "non functional")
train$status_group <- factor(train$status_group)
summary(train$status_group)

# Calculating the baseline performance
32259/59400 
# Baseline Performance = functional water points/total = 0.5430808

############################# Preliminary Exploration ###########################

### Need investigation ###
# amount_tsh - 1st quartile and median is 0, need investigation
hist(train$amount_tsh)
hist(train$amount_tsh[train$amount_tsh>10000])
nrow(train[train$amount_tsh==0,]) #41639 values with 0

ggplot(subset(train, amount_tsh == 0), aes(x = amount_tsh)) +
  geom_histogram(bins = 20) + 
  facet_grid( ~ status_group) # Approximately 20,000 functional without water

ggplot(subset(train, amount_tsh > 1), aes(x = amount_tsh)) +
  geom_histogram(bins = 5) + 
  facet_grid( ~ status_group) # Amongst logically correct info, 12000 F + 5000 NF

59400-41639 #17761 functional, worth noting that there are a lot of 0's

# Let model decide if the 0's are significant.
train$tsh0 <- 0
train$tsh0[train$amount_tsh==0]<-1
test$tsh0 <- 0
test$tsh0[test$amount_tsh==0]<-1

# Remove as too many many illogical data where water amount is 0.
train$amount_tsh <- NULL
test$amount_tsh <- NULL

# longitude - Min is 0 where the rest range from 33-40, need investigation. Check latitude as well.

summary(train$longitude)
hist(train$longitude)
nrow(train[train$longitude==0,]) #1812 values with 0, leave.

summary(train$latitude)
hist(train$latitude)
length(train$latitude[train$latitude>=0])
length(train$latitude[train$latitude<=-12])

#longitude and latitude is ommited as logistic regression is used
train$longitude <- NULL
test$longitude <- NULL

train$latitude <- NULL
test$latitude <- NULL

# region_code - shouldn't have max 99 when min to 3rd quartile is within 1-20

summary(train$region_code)
hist(train$region_code)
hist(train$region_code[train$region_code>50])

#Remove as their may be chance of skewedness. Choose Region Name instead.
train$region_code <- NULL
test$region_code <- NULL

# district_code - shouldn't have max 80 when min to 3rd quartile is within 0-5
summary(train$district_code)
hist(train$district_code)
hist(train$district_code[train$district_code>10])
nrow(train[train$district_code==0,]) #value 0 = 23

# Remove - same reason as region_code.
train$district_code <- NULL
test$district_code <- NULL

# population - investigate as there shouldn't be waterpoints where there is no population
summary(train$population)
hist(train$population)
nrow(train[train$population==0,])

ggplot(subset(train, population == 0), aes(x = population)) +
  geom_histogram(bins = 20) + 
  facet_grid( ~ status_group) # Approximately 10,000 functional in non-populated areas

ggplot(subset(train, population > 1), aes(x = population)) +
  geom_histogram(bins = 5) + 
  facet_grid( ~ status_group) 

# Worth checking if the 0 population is the cause of the fault.
train$population <- NULL
test$population <- NULL

train$population0 <- NULL
test$population0 <- NULL


# construction_year - Investigate the 0's. -> Perhaps add 'age' feature instead of 'construction_year'
summary(train$construction_year)
hist(train$construction_year)
nrow(train[train$construction_year==0,]) #20,709, 0's.

# Let model decide if the 0's are significant.
train$const0 <- 0
train$const0[train$construction_year==0]<-1
test$const0 <- 0
test$const0[test$construction_year==0]<-1

train$construction_year <- NULL
test$construction_year <- NULL

train$const0 <- NULL
test$const0 <- NULL

### Take a look at ###

# funder - approximately 39000 funders out of 59400, worth investigating?
str(train$funder) #1898 levels of funders
funder_levels = 10
fundertop10 <- names(summary(train$funder)[1:funder_levels])
train$funder <- factor(train$funder, levels=c(fundertop10, "Other"))

summary(train$funder)
train$funder <- NULL
test$funder <- NULL

# installer - approximately 33000 funders out of 59400, worth investigating?
str(train$installer)
# Make installer lowercase, take first 3 letters as a sub string
train$install_2 <- substr(tolower(train$installer),1,3)
train$install_2[train$install_2 %in% c(" ", "", "0", "_", "-")] <- "other"

# Take the top 20 substrings from above by occurance frequency
install_top_10 <- names(summary(as.factor(train$install_2)))[1:10]
train$install_2[!(train$install_2 %in% install_top_10)] <- "other"
train$install_2 <- as.factor(train$install_2)

# Create install_2 for the test set using same top 20 from above
test$install_2 <- substr(tolower(test$installer),1,3)
test$install_2[test$install_2 %in% c(" ", "", "0", "_", "-")] <- "other"
test$install_2[!(test$install_2 %in% install_top_20)] <- "other"
test$install_2 <- as.factor(test$install_2)

#installer - Removed as new feature was created.
train$installer <- NULL
test$installer <- NULL

# Table of the install_2 variable vs the status of the pumps
table(train$install_2, train$status_group)
prop.table(table(train$install_2, train$status_group), margin = 1)

train$install_2 <- NULL
test$install_2 <- NULL

# region - 33000 out of 59400, decrease levels? about the same number as installer...
str(train$region) #21 levels is good amount, keep.

# public_meeting - 3000 missing. Decide what to do.
str(train$public_meeting) #keep

# scheme_name - 28000 missing values and 29000 levels. Watch.
str(train$scheme_name)

# permit - Watch the 3000 missing values. Otherwise good amount.
str(train$permit)

### Reduce Levels ###
# basin - Approximately 12000 out of 59400. Reduce levels?
summary(train$basin)

### Remove ###
# wpt_name - 52000 unique names out of 59400. Not significant to include in model? Removed as name does not help predict the faults.
train$wpt_name <- NULL
test$wpt_name <- NULL

# num_private - Min to 3rd quartile is 0. Remove from model?
# num_private - Removed as most of them are 0. Would choose wpt_name instead.
train$num_private <- NULL
test$num_private <- NULL

# subvillage - 57000/59400. Too unique. Remove.
train$subvillage <- NULL
test$subvillage <- NULL

# lga - 51000/59400. Not siginificant? Remove.
train$lga <- NULL
test$lga <- NULL

# ward - 58000/59400. Not siginificant? Remove.
train$ward <- NULL
test$ward <- NULL

# recorded_by - Only GeoData Consultants. Definitely Remove.
train$recorded_by <- NULL
test$recorded_by <- NULL

### Choose ###
# extraction_type, extraction_type_group, extraction_type_class - seems like duplicate. Choose one.
# extraction_type - Removed as it is a duplicate to extraction_type_group.
train$extraction_type  <- NULL
test$extraction_type  <- NULL

# extraction_type class- Removed.
levels(train$extraction_type_class)
train$extraction_type_class  <- NULL
test$extraction_type_class  <- NULL

#extraction_type_group
prop.table((table(train$extraction_type_group)))
train$extraction_type_group[train$extraction_type_group=="rope pump"] <- "other"
train$extraction_type_group[train$extraction_type_group=="wind-powered"] <- "other"
train$extraction_type_group<-droplevels(train$extraction_type_group)
prop.table((table(train$extraction_type_group)))

test$extraction_type_group[test$extraction_type_group=="rope pump"] <- "other"
test$extraction_type_group[test$extraction_type_group=="wind-powered"] <- "other"
test$extraction_type_group<-droplevels(test$extraction_type_group)

# scheme_management and management - Similar results(vwc, wug , etc.). Choose One.
# management - Removed as it is a duplicate to scheme_management.
train$management <- NULL
test$management <- NULL

# payment_type and payment - Similar. Choose one.
# payment - Removed as it is a duplicate to payment_type.
train$payment  <- NULL
test$payment  <- NULL

# water_quality and quality_group - Similar. Choose one.
# quality_group - Removed as it is a duplicate to water_quality.
train$quality_group  <- NULL
test$quality_group  <- NULL

# quantity and quantity_group - Similar. Choose one.
# quantity_group - Removed as it is a duplicate to quantity.
train$quantity_group  <- NULL
test$quantity_group  <- NULL

# source and source_type - Similar. Choose one.
# source_type - Removed as it is a duplicate to source.
train$source_type <- NULL
test$source_type <- NULL

# waterpoint_type and waterpoint_type_group - Similar. Choose one.
# waterpoint_type_group - Removed as it is a duplicate to waterpoint_type.
train$waterpoint_type_group  <- NULL
test$waterpoint_type_group  <- NULL

#waterpoint_type
prop.table(table(train$waterpoint_type))
train$waterpoint_type[train$waterpoint_type=="cattle trough"] <- "other"
train$waterpoint_type[train$waterpoint_type=="dam"] <- "other"
train$waterpoint_type[train$waterpoint_type=="improved spring"] <- "other"
train$waterpoint_type<-droplevels(train$waterpoint_type)
prop.table((table(train$waterpoint_type)))

test$waterpoint_type[test$waterpoint_type=="cattle trough"] <- "other"
test$waterpoint_type[test$waterpoint_type=="dam"] <- "other"
test$waterpoint_type[test$waterpoint_type=="improved spring"] <- "other"
test$waterpoint_type<-droplevels(test$waterpoint_type)

### Keep but still investigate ###
#source_class, scheme_management, latitude, gps_height, date_recorded
# source_class - Removed as we need higher level info like source.
train$source_class <- NULL
test$source_class <- NULL

#gps_height
hist(train$gps_height)
nrow(train[train$gps_height==0,])

ggplot(train, aes(x = gps_height)) +
  geom_histogram(bins = 20) + 
  facet_grid( ~ status_group)

train$gps0 <- 0
train$gps0[train$gps_height==0]<-1
table(train$gps0)

test$gps0 <- 0
test$gps0[test$gps_height==0]<-1

#gps_height - how many values were missing?
gpsFill <- rpart(gps_height ~ latitude+longitude_updated
                 , data=train[(train$gps_height!=0),]
                 , method="anova")
train$gps_height[train$gps0==1] <- predict(gpsFill,train[(train$gps0==1),])
hist(train$gps_height)
hist(train$gps_height[train$gps0 ==1])

gpsFill2 <- rpart(gps_height ~ latitude+longitude_updated
                  , data=test[(test$gps_height!=0),]
                  , method="anova")
train$gps_height[test$gps0==1] <- predict(gpsFill2,test[(test$gps0==1),])

#date_recorded changes to just month as years are scattered apart
date_recorded_month <- factor(format(as.Date(train$date_recorded), "%b"))
summary(date_recorded_month)
train$date_recorded <- NULL
date_recorded_month <- factor(format(as.Date(test$date_recorded), "%b"))
test$date_recorded <- NULL
train <- cbind(train, date_recorded_month)
test <- cbind(test, date_recorded_month)


###
train$Size <- NULL
test$Size <- NULL

train$tsh0 <- NULL
test$tsh0 <- NULL

train$latlong <- NULL
test$latlong <- NULL

train$date_recorded_month <- NULL
test$date_recorded_month <- NULL

###

# Does quantity of water show a good 
table(train$quantity,train$status_group)
prop.table(table(train$quantity, train$status_group), margin = 1)

#Categorical variables
# Bar plot for quantity - significant reason for failure (dry)
qplot(quantity, data=train, geom="bar", fill=status_group) + 
  theme(legend.position = "top")

# Bar plot for water_quality - significant reason for failure (salty and unknown)
qplot(water_quality, data=train, geom="bar", fill=status_group) + 
  theme(legend.position = "top")

# Bar plot for waterpoint_type - significant reason for failure (other)
qplot(waterpoint_type, data=train, geom="bar", fill=status_group) + 
  theme(legend.position = "top") + 
  theme(axis.text.x=element_text(angle = -20, hjust = 0))

# Bar plot for extraction_type_group - no significant reason for failure
qplot(extraction_type_group, data=train, geom="bar", fill=status_group) + 
  theme(legend.position = "top")

# Bar plot for payment - no significant reason for failure
qplot(payment, data=train, geom="bar", fill=status_group) + 
  theme(legend.position = "top")

# Bar plot for source - no significant reason for failure
qplot(source, data=train, geom="bar", fill=status_group) + 
  theme(legend.position = "top") + 
  theme(axis.text.x=element_text(angle = -20, hjust = 0))

#Continuous variable
# Histogram when construction_year is larger than 0
ggplot(subset(train, construction_year > 0), aes(x = construction_year)) +
  geom_histogram(bins = 20) + 
  facet_grid( ~ status_group)

#######Exploratory Data Analysis#######
#Method 1 - Logistic Regression

glm(formula = status_group ~ basin + region + public_meeting + scheme_management + permit + extraction_type_group + management_group + payment_type + source + water_quality + quantity + waterpoint_type + gps0,  
    family = "binomial"(link="logit"), data = train) 
# removed gps_height, scheme_name 
PumpLog <- glm(formula = status_group ~ basin + region + public_meeting + scheme_management + permit + extraction_type_group + management_group + payment_type + source + water_quality + quantity + waterpoint_type + gps0,  
               family = "binomial"(link="logit"), data = train) # AIC:60630
PumpLog <- glm(formula = status_group ~ basin + region + public_meeting + scheme_management + extraction_type_group + payment_type + source + water_quality + quantity + waterpoint_type + gps0,  
               family = "binomial"(link="logit"), data = train) # AIC:60808
summary(PumpLog)
coef(summary(PumpLog))

#Finalized Model
predict(PumpLog,train, type="response")
pred_lr_train <- predict(PumpLog,train, type="response")
summary(pred_lr_train)
table(train$status_group, pred_lr_train)

tapply(pred_lr_train, train$status_group,mean) # 0.315 Functional, 0.6245433 Non Functional 
table(train$status_group, pred_lr_train>0.5)
27074+5185
27074/32259 # specifity = 0.83
16658+10483
16658/27141 # sensitivity = 0.61

table(train$status_group, pred_lr_train>0.7)
31124+1135
31124/32259 # specifity = 0.96
11574/27141 # sensivity = 0.42

ROCRpred = prediction(pred_lr_train, train$status_group)
as.numeric(performance(ROCRpred,"auc")@y.values) #AUC = 0.81
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))

# Prediction
predict(PumpLog, type="response", newdata = test)
predictTest = predict(PumpLog, type="response", newdata = test)
p.test = round(predictTest)
test$status_group <- p.test
test$status_group <- factor(test$status_group) 
summary(test$status_group)

levels(test$status_group) <- c("functional", "non functional")
tapply(predictTest, test$status_group,mean) # 0.278 Functional, 0.77 Non-functional
table(test$status_group, predictTest>0.5)
ROCRpred = prediction(predictTest, test$status_group)
as.numeric(performance(ROCRpred,"auc")@y.values) # AUC = 

ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))

pump = data.frame(id=test$id, test = p.test)
colnames(pump) <- c("id", "status_group")
write.csv(pump, "submission.csv", row.names=FALSE)

# Report results of the test set

### Recomendation and Conclusion
# Create submission data frame
# You can continue using this method by changing the parameters of the model
# Compare using other models
# Combine the two datasets since there are many duplicate codes

###Reduce variables, find meaning in them and exploratory analysis will be much interesting
###have a good skeleton, but need 3 hourse each day to come up with good story 