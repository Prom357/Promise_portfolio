#Diabetes data variables

###Independent variables######

#Age:                 Age of patients both Male and Female
#Gender:              Male are classified as (0) and Female are classified as (1)
#Polyuria:            Uriates more that usual (if YES it's classified as =1, if NO it is classified as =0)
#Polydipsia:          Excessive thirst  (if YES it's classified as =1, if NO it is classified as =0)
#Sudden weight loss:  Weight loss (if YES it's classified as =1, if NO it is classified as =0)
#Weakness:            Loosing strenght (if YES it's classified as =1, if NO it is classified as =0)
#Polyphagia:          Excessive eating or appetite (if YES it's classified as =1, if NO it is classified as =0)
#Genital thrush:      Yeast infections (if YES it's classified as =1, if NO it is classified as =0)
#Visual blurring:     Difficulty focusing eyesight (if YES it's classified as =1, if NO it is classified as =0)
#Itching:             Skin irritation (if YES it's classified as =1, if NO it is classified as =0)
#Irritability:        Feeling frustrated or angry (if YES it's classified as =1, if NO it is classified as =0)
#Delayed healing:     Delay in Wound healing (if YES it's classified as =1, if NO it is classified as =0)
#Partial paresis:     Weakness of muscle (if YES it's classified as =1, if NO it is classified as =0)
#Muscle stiffness:    Pain the the muscles due to muscle tightness (if YES it's classified as =1, if NO it is classified as =0)
#Alopecia:            Sudden hair loss (if YES it's classified as =1, if NO it is classified as =0)
#Obesity:             Overweight (if YES it's classified as =1, if NO it is classified as =0)

######## Dependent variable#####
## Class: The class indicates "Positive" or "Negative" of newly diabetic or would be diabetic patient
##(i.e the indepented variebles listed above) Where the "Positive" represent (1) and the " Negative" represent (0)

####

#-------Section 01------------------------###(Logistic regression)

#### Set working directory

setwd(dirname(file.choose()))
getwd()

######import the csv file###########

diabetes = read.csv(file.choose())
head(diabetes)

str(diabetes)
summary(diabetes)


####### Data exploration############


## check for missing data

apply(diabetes, MARGIN = 2, FUN = function(x) sum(is.na(x)))
install.packages("Amelia")
library(Amelia)
missmap(diabetes, col = c("black", "blue"), legend = FALSE)
diabetes <- na.omit(diabetes)


# Boxplot all variables
boxplot(diabetes, col = "Bisque")
cor(diabetes, method = "pearson")

# Test dependent variable for normality # Lilliefors (Kolmogorov-Smirnov) normality test###
lillie.test(diabetes$class)

# Change categorical dependent variable to factor
diabetes$class = as.factor(diabetes$class)

# check correlations between variables
install.packages("polycor")
library(polycor)
diabetes.cor <- hetcor(diabetes[-1])
diabetes.cor$type
round(diabetes.cor$correlations, 2)

############################ Start Logistic regression#########
str(diabetes)
table(diabetes$class)


######### Data partioning#############

set.seed(222)
indep1 = sample(2,nrow(diabetes), replace = T, prob = c(0.7, 0.3))

# Train dataset & Test dataset

train = diabetes[indep1==1,]
test = diabetes[indep1==2,]

#train the logistic regression model
model1 = glm(class~., data = train, family ="binomial")
summary(model1)

#remove the indpendent variables that is not related to the dependent variable
model2 = glm(class~ Gender+Polyuria+Polydipsia+Genital.thrush+Itching+Irritability, data=train, family = "binomial")
summary(model2)

model3 = glm(class~ Gender+Polyuria+Polydipsia+Itching+Irritability, data=train, family = "binomial")
summary(model3)

model4 = glm(class~ Gender+Polyuria+Polydipsia+Irritability, data=train, family = "binomial")
summary(model4)


attributes(model4)
plot(model4$residuals)

#predict with the test set
predict1 = predict(model4, test, type="response")
predict1


#creating a confussion matrix
p = ifelse(predict1>0.5, 1,0)
p

Table1 = table(Actual = test$class, Predicted = p)
Table1

##accuracy
(Table1[1,1]+ Table1[2,2])/sum(Table1)



######### Running the code again after balancing the data using oversampling#######

install.packages("caret")
library(caret)

table(train$class)
utrain = upSample(train, train$class)
table(utrain$class)

model5 = glm(class~ Gender+Polyuria+Polydipsia+Irritability, data=utrain, 
             family = "binomial")
summary(model5)
attributes(model5)
plot(model5$residuals)

#predict using the balance data with the test set
predict2 = predict(model5, test, type="response")
predict2

#creating a confussion matrix with the balance data
p = ifelse(predict1>0.5, 1,0)
p

Table2 = table(Actual = test$class, Predicted = p)
Table2

##accuracy
(Table2[1,1]+ Table2[2,2])/sum(Table2)




##########-------Section 02------------------------#########(Randdom forest)

#### Random forest#######
str(diabetes)
diabetes$class <- as.factor(diabetes$class)
table(diabetes$class)

####Data Partition####
set.seed(222)
indep2 <- sample(2,nrow(diabetes),replace = TRUE,prob=c(0.7,0.3))

#######Train dataset & Test dataset###
train2 <- diabetes[indep2==1,]
test2 <- diabetes[indep2==2,]

#### Uisng the random Forest Model############

###Install package
install.packages("randomForest")

##### Load library
library(randomForest)
set.seed(222)
rf <- randomForest(class~., data=train2)
print(rf)
attributes(rf)

###### Prediction & Confusion Matrix- Train
library(caret)
pred3 <- predict(rf, train2)
confusionMatrix(pred3, train2$class)

######## Prediction & Confusion Matrix - Test
pred4 <- predict(rf, test2)
confusionMatrix(pred4, test2$class)


###### Error rate of random forest
plot(rf)


###No. of Nodes for the trees
hist(treesize(rf),
     main = "No, of Nodes for the Trees",
     col = "green")



###### Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - variable Importanc")

importance(rf)
varUsed(rf)

#####Partial dependence plot
partialPlot(rf, train2, Polyuria, "1")


### Extract SingleTress
getTree(rf, 1, labelVar = TRUE)


######## check again accuracy again

class_Pred = predict(rf, test2)
test2$class_Pred = class_Pred



rf = table(Actual = test2$class, test2$class_)
rf


##accuracy
Classification_Accuracy = sum(diag(rf)/sum(rf))
Classification_Accuracy



############## Section-----03---------------######################(K- nearest neighbor)
#####install

install.packages("caret")
install.packages("pROC")
install.packages("mlbench")

###load libaries
library(caret)
library(pROC)
library(mlbench)

##Importing diabetes2 dataset again to explor using KNN###
diabetes2 <- read.csv(file.choose(), header = T)

####### Check for type of data again (classification structure)
str(diabetes2)
diabetes2$class[diabetes2$class ==0] <-"No"
diabetes2$class[diabetes2$class ==1] <-"Yes"
str(diabetes2)
diabetes2$class <- factor(diabetes2$class)
str(diabetes2)

######Data Partition#####

set.seed(1234)
indep3 <- sample(2, nrow(diabetes2), replace = T, prob = c(0.7, 0.3))
train3 <- diabetes2[indep3 ==1,]
test3 <- diabetes2[indep3 == 2,]

###### start the K-Nearest Neigbor Method####

#### Training data set (developing the method -used the following "repeatedcv")

trControl <- trainControl(method = "repeatedcv",
                    number = 10, 
                    repeats = 3,)
                          
###### Normalized the data using preProc ###### 

set.seed(222)
fit <- train(class ~., 
            data =train3,
            method = "knn",
            tuneLength = 20,
            trControl =trControl,
            preProc = c("center", "scale"))
                          
#### Model Performance#####
fit
plot(fit)
varImp(fit)
pred5 <- predict(fit, newdata = test3)
confusionMatrix(pred5, test3$class)
#### 66/71 (cal accuracy val)
                          
############################
###### using other method instead of accuracy#####


set.seed(1234)
indep4 <- sample(2, nrow(diabetes2), replace = T, prob = c(0.7, 0.3))
train4 <- diabetes2[indep4 ==1,]
test4 <- diabetes2[indep4 == 2,]
                          
######K-Nearest Neigbors Method
trControl <- trainControl(method = "repeatedcv",
              number = 10, repeats = 3,
              classProbs = TRUE,
              summaryFunction = twoClassSummary)
                                                    
set.seed(222)
fit <- train(class~., data =train4,
                      method = 'knn',
                      tuneLength = 20,
                      trControl =trControl,
             preProc = c("center", "scale"),
             metric = 'ROC',
             tuneGrid =expand.grid(k = 1:60))
                                                    
#### check for model parformace again ############
fit
plot(fit)
varImp(fit)
pred6 <- predict(fit, newdata = test4)
confusionMatrix(pred6, test4$class)


                                                    
#-----End-------------------------------------------
# remove all variables from the environment
rm(list=ls())                                                  
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    