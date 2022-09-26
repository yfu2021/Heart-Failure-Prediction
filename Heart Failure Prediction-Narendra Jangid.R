################## Heart Failure Prediction ##############
###### HarvardX Data Science Professional Certificate ####
################## Narendra Kumar Jangid #################


#############################################################
###################### Data Preparation ####################
############################################################


#### Install Required Packages #### 
# Install packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("naivebayes", repos = "http://cran.us.r-project.org")
if(!require(funModeling)) install.packages("funModeling", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
# Load required libraries
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(knitr)
library(kableExtra)
library(scales)
library(stringr)
library(ggpubr)
library(naivebayes)
library(funModeling)
library(ggcorrplot)
library(randomForest)

#### Create Heart Failure Data Set #### 
# Read the Heart Failure Data set
heart_failure_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv")
# Modify data set into having more descriptive values
heart_failure_data <- heart_failure_data %>% mutate(anaemia = ifelse(anaemia == 0,"No","Yes"), 
                                                    diabetes = ifelse(diabetes == 0,"No","Yes"),
                                                    high_blood_pressure = ifelse(high_blood_pressure == 0,"No","Yes"),
                                                    sex = ifelse(sex == 0, "Female", "Male"),
                                                    smoking = ifelse(smoking == 0, "No","Yes"),
                                                    DEATH_EVENT = ifelse(DEATH_EVENT == 0, "No","Yes"))
# Convert required data set fields to factor and integer
column_factor <- c("anaemia", "diabetes", "high_blood_pressure", "sex", "smoking", "DEATH_EVENT")
heart_failure_data[column_factor] <- lapply(heart_failure_data[column_factor], as.factor)
column_int <- c("age", "creatinine_phosphokinase", "ejection_fraction", "platelets", "serum_sodium", "time")
heart_failure_data[column_int] <- lapply(heart_failure_data[column_int], as.integer)


#### Create Train and Validation Sets ####
# Splitting the data into Train and Validation Set
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = heart_failure_data$DEATH_EVENT, times = 1, p = 0.1, list = FALSE)
edx <- heart_failure_data[-test_index,]
validation <- heart_failure_data[test_index,]
# Drop unnecessary variables and data set
rm(test_index, column_factor, column_int)


############################################################
############## Data Exploration and Analysis ###############
############################################################


#### Data Overview ####
class(edx)   # Class of edx set
nrow(edx)    # Number of Rows in edx data set
ncol(edx)    # Number of Columns in edx data set
head(edx,5)  # Top 5 Rows in edx set


#### Death Event####
# Plotting Death Event Distribution
ggarrange(edx %>% group_by(DEATH_EVENT) %>% summarise(count = n()) %>% ggplot(aes(x = DEATH_EVENT, y = count, fill= DEATH_EVENT)) + geom_bar(stat = "identity") + labs(x = "Death Event",y = "Count") + geom_text(aes(label = count), nudge_y = 5) + theme(legend.position="bottom") +  scale_fill_discrete(name='Death Event'),
          edx %>% group_by(DEATH_EVENT) %>% summarise(count = n()) %>% mutate(percentage_death_event = round(100*count/sum(count),1)) %>% ggplot(aes(x = DEATH_EVENT, y = percentage_death_event, fill= DEATH_EVENT)) + geom_bar(stat = "identity") + labs(x = "Death Event",y = "% Death Event") + geom_text(aes(label = round((percentage_death_event),0)), nudge_y = 2) + theme(legend.position="bottom") +  scale_fill_discrete(name='Death Event'),
          common.legend = TRUE, align = "h", legend = "bottom")


#### Age ####
# Plotting Age Distribution
ggarrange(edx %>% ggplot(aes(x=age, fill=DEATH_EVENT)) + geom_density(alpha=0.2)+ labs(x = "Age",y = "Density") + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          edx %>% ggplot(aes(age, color = DEATH_EVENT)) + stat_ecdf(geom = "step") + labs(x = "Age",y = "Cumulative Density")+ theme(legend.position="bottom"),
          common.legend = TRUE, align = "h", legend = "bottom")


#### Anaemia ####
# Plotting Anaemia Distribution
ggarrange(edx %>% group_by(anaemia, DEATH_EVENT) %>% summarise(count = n()) %>% ggplot(aes(x= anaemia, y=count, fill=DEATH_EVENT))+ geom_bar(stat="identity") + labs(x = "Anaemia",y = "Count") + geom_text(aes(label = count), nudge_y = -10) + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          edx %>% group_by(anaemia, DEATH_EVENT) %>% summarise(count = n()) %>% mutate(percent_death_event = round(100*count/sum(count),1)) %>% ggplot(aes(x= anaemia, y= percent_death_event, fill=DEATH_EVENT))+ geom_bar( stat="identity") + labs(x = "Anaemia",y = "Death Event %") + geom_text(aes(label = round(percent_death_event,0)), nudge_y = -10) + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          common.legend = TRUE, align = "h", legend = "bottom")


#### Creatinine Phosphokinase ####
# Plotting Creatinine Phosphokinase Distribution
ggarrange(edx %>% ggplot(aes(x=creatinine_phosphokinase, fill=DEATH_EVENT)) + geom_density(alpha=0.2)+ labs(x = "Creatinine Phosphokinase",y = "Density") + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          edx %>% ggplot(aes(creatinine_phosphokinase, color = DEATH_EVENT)) + stat_ecdf(geom = "step") + labs(x = "Creatinine Phosphokinase",y = "Cumulative Density")+ theme(legend.position="bottom"),
          common.legend = TRUE, align = "h", legend = "bottom")


#### Diabetes ####
# Plotting Diabetes Distribution
ggarrange(edx %>% group_by(diabetes, DEATH_EVENT) %>% summarise(count = n()) %>% ggplot(aes(x= diabetes, y=count, fill=DEATH_EVENT))+ geom_bar(stat="identity") + labs(x = "Diabetes",y = "Count") + geom_text(aes(label = count), nudge_y = -10) + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          edx %>% group_by(diabetes, DEATH_EVENT) %>% summarise(count = n()) %>% mutate(percent_death_event = round(100*count/sum(count),1)) %>% ggplot(aes(x= diabetes, y= percent_death_event, fill=DEATH_EVENT))+ geom_bar( stat="identity") + labs(x = "Diabetes",y = "Death Event %") + geom_text(aes(label = round(percent_death_event,0)), nudge_y = -10) + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          common.legend = TRUE, align = "h", legend = "bottom")


#### Ejection Fraction ####
# Plotting Ejection Fraction Distribution
ggarrange(edx %>% ggplot(aes(x=ejection_fraction, fill=DEATH_EVENT)) + geom_density(alpha=0.2)+ labs(x = "Ejection Fraction",y = "Density") + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          edx %>% ggplot(aes(ejection_fraction, color = DEATH_EVENT)) + stat_ecdf(geom = "step") + labs(x = "Ejection Fraction",y = "Cumulative Density")+ theme(legend.position="bottom"),
          common.legend = TRUE, align = "h", legend = "bottom")


#### High Blood Pressure ####
# Plotting High Blood Pressure Distribution
ggarrange(edx %>% group_by(high_blood_pressure, DEATH_EVENT) %>% summarise(count = n()) %>% ggplot(aes(x= high_blood_pressure, y=count, fill=DEATH_EVENT))+ geom_bar(stat="identity") + labs(x = "High Blood Pressure",y = "Count") + geom_text(aes(label = count), nudge_y = -10) + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          edx %>% group_by(high_blood_pressure, DEATH_EVENT) %>% summarise(count = n()) %>% mutate(percent_death_event = round(100*count/sum(count),1)) %>% ggplot(aes(x= high_blood_pressure, y= percent_death_event, fill=DEATH_EVENT))+ geom_bar( stat="identity") + labs(x = "High Blood Pressure",y = "Death Event %") + geom_text(aes(label = round(percent_death_event,0)), nudge_y = -10) + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          common.legend = TRUE, align = "h", legend = "bottom")


#### Platelets ####
# Plotting Platelets Distribution
ggarrange(edx %>% ggplot(aes(x=platelets, fill=DEATH_EVENT)) + geom_density(alpha=0.2)+ scale_x_log10() + labs(x = "Platelets (log10)",y = "Density") + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          edx %>% ggplot(aes(platelets, color = DEATH_EVENT)) + stat_ecdf(geom = "step") + scale_x_log10() + labs(x = "Platelets (log10)",y = "Cumulative Density")+ theme(legend.position="bottom"),
          common.legend = TRUE, align = "h", legend = "bottom")


#### Serum Creatinine ####
# Plotting Serum Creatinine Distribution
ggarrange(edx %>% ggplot(aes(x=serum_creatinine, fill=DEATH_EVENT)) + geom_density(alpha=0.2)+ labs(x = "Serum Creatinine",y = "Density") + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          edx %>% ggplot(aes(serum_creatinine, color = DEATH_EVENT)) + stat_ecdf(geom = "step") + labs(x = "Serum Creatinine",y = "Cumulative Density")+ theme(legend.position="bottom"),
          common.legend = TRUE, align = "h", legend = "bottom")


#### Serum Sodium ####
# Plotting Serum Sodium Distribution
ggarrange(edx %>% ggplot(aes(x=serum_sodium, fill=DEATH_EVENT)) + geom_density(alpha=0.2)+ labs(x = "Serum Sodium",y = "Density") + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          edx %>% ggplot(aes(serum_sodium, color = DEATH_EVENT)) + stat_ecdf(geom = "step") + labs(x = "Serum Sodium",y = "Cumulative Density")+ theme(legend.position="bottom"),
          common.legend = TRUE, align = "h", legend = "bottom")


#### Sex ####
# Plotting Sex Distribution
ggarrange(edx %>% group_by(sex, DEATH_EVENT) %>% summarise(count = n()) %>% ggplot(aes(x= sex, y=count, fill=DEATH_EVENT))+ geom_bar(stat="identity") + labs(x = "Sex",y = "Count") + geom_text(aes(label = round(count,0)), nudge_y = -10) + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          edx %>% group_by(sex, DEATH_EVENT) %>% summarise(count = n()) %>% mutate(percent_death_event = round(100*count/sum(count),1)) %>% ggplot(aes(x= sex, y= percent_death_event, fill=DEATH_EVENT))+ geom_bar( stat="identity") + labs(x = "Sex",y = "Death Event %") + geom_text(aes(label = round(percent_death_event,0)), nudge_y = -10)  + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          common.legend = TRUE, align = "h", legend = "bottom")


#### Smoking ####
# Plotting Smoking Distribution
ggarrange(edx %>% group_by(smoking, DEATH_EVENT) %>% summarise(count = n()) %>% ggplot(aes(x= smoking, y=count, fill=DEATH_EVENT))+ geom_bar(stat="identity") + labs(x = "Smoking",y = "Count") + geom_text(aes(label = round(count,0)), nudge_y = -10) + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          edx %>% group_by(smoking, DEATH_EVENT) %>% summarise(count = n()) %>% mutate(percent_death_event = round(100*count/sum(count),1)) %>% ggplot(aes(x= smoking, y= percent_death_event, fill=DEATH_EVENT))+ geom_bar( stat="identity") + labs(x = "Smoking",y = "Death Event %") + geom_text(aes(label = round(percent_death_event,0)), nudge_y = -10) + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          common.legend = TRUE, align = "h", legend = "bottom")


#### Time ####
# Plotting Time Distribution
ggarrange(edx %>% ggplot(aes(x=time, fill = DEATH_EVENT)) + geom_density(alpha=0.2)+ labs(x = "Time",y = "Density") + theme(legend.position="bottom") + scale_fill_discrete(name='Death Event'),
          edx %>% ggplot(aes(time, color = DEATH_EVENT)) + stat_ecdf(geom = "step") + labs(x = "Time",y = "Cumulative Density")+ theme(legend.position="bottom"),
          common.legend = TRUE, align = "h", legend = "bottom")


#### Variable Importance and Correlation ####
# Variable Importance basis Information Gain
ggplot(var_rank_info(edx, "DEATH_EVENT"), aes(x = reorder(var, gr), y = gr, fill = var)) + geom_bar(stat = "identity") + coord_flip() + theme_bw() + xlab("") + ylab("Variable Importance") + guides(fill = FALSE)
# Variable Correlation
round(cor(edx[c(1,3,5,7,8,9,12)]),3) %>% ggcorrplot(hc.order = TRUE, outline.color = "white") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


############################################################
############# Model Development and Results ################
############################################################


# Exclude least important features from edx and validation set
edx <- edx %>% select(-sex, -diabetes, -smoking)
validation <- validation %>% select(-sex, -diabetes, -smoking)


#### Splitting edx Data into Train and Test Sets ####
# Split edx data into test and train sets
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$DEATH_EVENT, times = 1, p = 0.1, list = FALSE)
edx_train_set <- edx[-test_index,]
edx_test_set <- edx[test_index,]
# Drop unnecessary variables and data set
rm(test_index) 


#### Model Development, Validation and Results ####


## Generalized Linear Model (glm) ##
# Model Training and Testing
set.seed(1, sample.kind = "Rounding")
# Model Training
glm_train <- train(DEATH_EVENT~.,data = edx_train_set, method = "glm")
# Model Testing
glm_test <- predict(glm_train, edx_test_set)
# Model Accuracy On Test Set using Confusion Matrix
glm_test_cfm <- confusionMatrix(glm_test, edx_test_set$DEATH_EVENT, positive = "Yes")
glm_test_accuracy <- glm_test_cfm$overall["Accuracy"]
glm_test_sensitivity <- glm_test_cfm$byClass["Sensitivity"]
glm_test_specificity <- glm_test_cfm$byClass["Specificity"]
Model_test_Accuracy <- tibble(Model = "Generalized Linear Model (glm)", Accuracy = glm_test_accuracy, Sensitivity = glm_test_sensitivity, Specificity = glm_test_specificity)
# Model Validation
glm_validation <- predict(glm_train, newdata = validation)
# Model Accuracy On Validation Set using Confusion Matrix
glm_validation_cfm <- confusionMatrix(glm_validation, validation$DEATH_EVENT, positive = "Yes")
glm_validation_accuracy <- glm_validation_cfm$overall["Accuracy"]
glm_validation_sensitivity <- glm_validation_cfm$byClass["Sensitivity"]
glm_validation_specificity <- glm_validation_cfm$byClass["Specificity"]
Model_validation_Accuracy <- tibble(Model = "Generalized Linear Model (glm)", Accuracy = glm_validation_accuracy, Sensitivity = glm_validation_sensitivity, Specificity = glm_validation_specificity)
# Remove redundant variables
rm(glm_train, glm_test,glm_test_cfm, glm_test_accuracy, glm_test_sensitivity, glm_test_specificity, glm_validation, glm_validation_cfm, glm_validation_accuracy, glm_validation_sensitivity, glm_validation_specificity)

## Naive Bayes (naive_bayes) ##
# Model Training and Testing
set.seed(1, sample.kind = "Rounding")
# Model Training
naive_bayes_train <- train(DEATH_EVENT~.,data = edx_train_set, method = "naive_bayes")
# Model Testing
naive_bayes_test <- predict(naive_bayes_train, edx_test_set)
# Model Accuracy On Test Set using Confusion Matrix
naive_bayes_test_cfm <- confusionMatrix(naive_bayes_test, edx_test_set$DEATH_EVENT, positive = "Yes")
naive_bayes_test_accuracy <- naive_bayes_test_cfm$overall["Accuracy"]
naive_bayes_test_sensitivity <- naive_bayes_test_cfm$byClass["Sensitivity"]
naive_bayes_test_specificity <- naive_bayes_test_cfm$byClass["Specificity"]
Model_test_Accuracy <- bind_rows(Model_test_Accuracy, tibble(Model = "Naive Bayes (naive_bayes)", Accuracy = naive_bayes_test_accuracy, Sensitivity = naive_bayes_test_sensitivity, Specificity = naive_bayes_test_specificity))
# Model Validation
naive_bayes_validation <- predict(naive_bayes_train, newdata = validation)
# Model Accuracy On Validation Set using Confusion Matrix
naive_bayes_validation_cfm <- confusionMatrix(naive_bayes_validation, validation$DEATH_EVENT, positive = "Yes")
naive_bayes_validation_accuracy <- naive_bayes_validation_cfm$overall["Accuracy"]
naive_bayes_validation_sensitivity <- naive_bayes_validation_cfm$byClass["Sensitivity"]
naive_bayes_validation_specificity <- naive_bayes_validation_cfm$byClass["Specificity"]
Model_validation_Accuracy <- bind_rows(Model_validation_Accuracy, tibble(Model = "Naive Bayes (naive_bayes)", Accuracy = naive_bayes_validation_accuracy, Sensitivity = naive_bayes_validation_sensitivity, Specificity = naive_bayes_validation_specificity))
# Remove redundant variables
rm(naive_bayes_train, naive_bayes_test,naive_bayes_test_cfm, naive_bayes_test_accuracy, naive_bayes_test_sensitivity, naive_bayes_test_specificity, naive_bayes_validation, naive_bayes_validation_cfm, naive_bayes_validation_accuracy, naive_bayes_validation_sensitivity, naive_bayes_validation_specificity)

## Decision Tree (rpart) ##
# Model Training and Testing
set.seed(1, sample.kind = "Rounding")
# Model Training
rpart_train <- train(DEATH_EVENT~.,data = edx_train_set, method = "rpart")
# Model Testing
rpart_test <- predict(rpart_train, edx_test_set)
# Model Accuracy On Test Set using Confusion Matrix
rpart_test_cfm <- confusionMatrix(rpart_test, edx_test_set$DEATH_EVENT, positive = "Yes")
rpart_test_accuracy <- rpart_test_cfm$overall["Accuracy"]
rpart_test_sensitivity <- rpart_test_cfm$byClass["Sensitivity"]
rpart_test_specificity <- rpart_test_cfm$byClass["Specificity"]
Model_test_Accuracy <- bind_rows(Model_test_Accuracy, tibble(Model = "Decision Tree (rpart)", Accuracy = rpart_test_accuracy, Sensitivity = rpart_test_sensitivity, Specificity = rpart_test_specificity))
# Model Validation
rpart_validation <- predict(rpart_train, newdata = validation)
# Model Accuracy On Validation Set using Confusion Matrix
rpart_validation_cfm <- confusionMatrix(rpart_validation, validation$DEATH_EVENT, positive = "Yes")
rpart_validation_accuracy <- rpart_validation_cfm$overall["Accuracy"]
rpart_validation_sensitivity <- rpart_validation_cfm$byClass["Sensitivity"]
rpart_validation_specificity <- rpart_validation_cfm$byClass["Specificity"]
Model_validation_Accuracy <- bind_rows(Model_validation_Accuracy, tibble(Model = "Decision Tree (rpart)", Accuracy = rpart_validation_accuracy, Sensitivity = rpart_validation_sensitivity, Specificity = rpart_validation_specificity))
# Remove redundant variables
rm(rpart_train, rpart_test,rpart_test_cfm, rpart_test_accuracy, rpart_test_sensitivity, rpart_test_specificity, rpart_validation, rpart_validation_cfm, rpart_validation_accuracy, rpart_validation_sensitivity, rpart_validation_specificity)

## Random Forest (rf) ##
# Model Training and Testing
set.seed(1, sample.kind = "Rounding")
# Model Training
tuning <- data.frame(mtry = 4)
rf_train <- train(DEATH_EVENT~.,data = edx_train_set, method = "rf", tuneGrid = tuning, importance = TRUE)
# Model Testing
rf_test <- predict(rf_train, edx_test_set)
# Model Accuracy On Test Set using Confusion Matrix
rf_test_cfm <- confusionMatrix(rf_test, edx_test_set$DEATH_EVENT, positive = "Yes")
rf_test_accuracy <- rf_test_cfm$overall["Accuracy"]
rf_test_sensitivity <- rf_test_cfm$byClass["Sensitivity"]
rf_test_specificity <- rf_test_cfm$byClass["Specificity"]
Model_test_Accuracy <- bind_rows(Model_test_Accuracy, tibble(Model = "Random Forest (rf)", Accuracy = rf_test_accuracy, Sensitivity = rf_test_sensitivity, Specificity = rf_test_specificity))
# Model Validation
rf_validation <- predict(rf_train, newdata = validation)
# Model Accuracy On Validation Set using Confusion Matrix
rf_validation_cfm <- confusionMatrix(rf_validation, validation$DEATH_EVENT, positive = "Yes")
rf_validation_accuracy <- rf_validation_cfm$overall["Accuracy"]
rf_validation_sensitivity <- rf_validation_cfm$byClass["Sensitivity"]
rf_validation_specificity <- rf_validation_cfm$byClass["Specificity"]
Model_validation_Accuracy <- bind_rows(Model_validation_Accuracy, tibble(Model = "Random Forest (rf)", Accuracy = rf_validation_accuracy, Sensitivity = rf_validation_sensitivity, Specificity = rf_validation_specificity))
# Remove redundant variables
rm(rf_train, rf_test,rf_test_cfm, rf_test_accuracy, rf_test_sensitivity, rf_test_specificity, tuning, rf_validation, rf_validation_cfm, rf_validation_accuracy, rf_validation_sensitivity, rf_validation_specificity)

## Support Vector Machines with Linear Kernel (svmLinear2) ##
# Model Training and Testing
set.seed(1, sample.kind = "Rounding")
# Model Training
svmLinear2_train <- train(DEATH_EVENT~.,data = edx_train_set, method = "svmLinear2")
# Model Testing
svmLinear2_test <- predict(svmLinear2_train, edx_test_set)
# Model Accuracy On Test Set using Confusion Matrix
svmLinear2_test_cfm <- confusionMatrix(svmLinear2_test, edx_test_set$DEATH_EVENT, positive = "Yes")
svmLinear2_test_accuracy <- svmLinear2_test_cfm$overall["Accuracy"]
svmLinear2_test_sensitivity <- svmLinear2_test_cfm$byClass["Sensitivity"]
svmLinear2_test_specificity <- svmLinear2_test_cfm$byClass["Specificity"]
Model_test_Accuracy <- bind_rows(Model_test_Accuracy, tibble(Model = "Support Vector Machines with Linear Kernel (svmLinear2)", Accuracy = svmLinear2_test_accuracy, Sensitivity = svmLinear2_test_sensitivity, Specificity = svmLinear2_test_specificity))
# Model Validation
svmLinear2_validation <- predict(svmLinear2_train, newdata = validation)
# Model Accuracy On Validation Set using Confusion Matrix
svmLinear2_validation_cfm <- confusionMatrix(svmLinear2_validation, validation$DEATH_EVENT, positive = "Yes")
svmLinear2_validation_accuracy <- svmLinear2_validation_cfm$overall["Accuracy"]
svmLinear2_validation_sensitivity <- svmLinear2_validation_cfm$byClass["Sensitivity"]
svmLinear2_validation_specificity <- svmLinear2_validation_cfm$byClass["Specificity"]
Model_validation_Accuracy <- bind_rows(Model_validation_Accuracy, tibble(Model = "Support Vector Machines with Linear Kernel (svmLinear2)", Accuracy = svmLinear2_validation_accuracy, Sensitivity = svmLinear2_validation_sensitivity, Specificity = svmLinear2_validation_specificity))
# Remove redundant variables
rm(svmLinear2_train, svmLinear2_test,svmLinear2_test_cfm, svmLinear2_test_accuracy, svmLinear2_test_sensitivity, svmLinear2_test_specificity, svmLinear2_validation, svmLinear2_validation_cfm, svmLinear2_validation_accuracy, svmLinear2_validation_sensitivity, svmLinear2_validation_specificity)