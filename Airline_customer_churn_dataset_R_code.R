#### Set working directory

setwd(dirname(file.choose()))
getwd()

######import the csv file###########
airlinedata = read.csv(file.choose())

# Install and load the required package
install.packages("caret")
install.packages("randomForest")
install.packages("kknn")
install.packages("naivebayes")
install.packages("glmnet")
install.packages("stats")
install.packages("pROC")
install.packages("PRROC")
install.packages("ROSE")
install.packages("reshape2")
install.packages("psych")
install.packages("dplyr")
install.packages("rpart")
install.packages("class")
install.packages("rpart.plot")
install.packages("pander")
# Load required libraries
library(ggplot2)       # For data visualization
library(caret)         # For machine learning
library(randomForest)  # For Random Forest algorithm
library(kknn)          # For KNN algorithm
library(naivebayes)    # For Naive Bayes algorithm
library(glmnet)        # For logistic regression (if needed)
library(stats)
library(pROC)
library(PRROC)
library(ROSE)
library(reshape2)
library(psych)
library(dplyr)
library(rpart)
library(caret)
library(class)
library(rpart.plot)
library(pander) 
install.packages("nortest")
library(nortest)
# read in data from csv file
airlinedata <- read.csv("airlinedata.csv", stringsAsFactors = FALSE)

# Inspect top rows of the data
head(airlinedata)
str(airlinedata)

# List of column names of categorical variables
categorical_vars <- c("Sat", "Gender", "Cust_Type", "T_Travel", "Class",
                      "Seat_C", "Depart_Arrival_C", "Food_drink",
                      "Gate_loc", "wifi_service", "Inflight_ent", "Online_suprt",
                      "Online_BK", "Onboard_service", "Leg._room", "Baggage_h",
                      "Checkin", "Cleanliness", "Online_boarding")

# Convert the categorical variables to factors
airlinedata[categorical_vars] <- lapply(airlinedata[categorical_vars], factor)
summary(airlinedata)



####Data distribution visualization ##############

# Create a bar plot of gender distribution
ggplot(airlinedata, aes(x = Gender, fill = factor(Gender, labels = c("Male", "Female")))) +
  geom_bar() +
  scale_fill_manual(values = c("blue", "red")) +  # Custom colors for Male and Female
  labs(title = "Gender Distribution of Passengers",
       x = "Gender",
       y = "Count") +
  theme_minimal()



#################Customer type#######

# Create a bar plot of customer type distribution
ggplot(airlinedata, aes(x = Cust_Type, fill = factor(Cust_Type, labels = c("Disloyal", "Loyal")))) +
  geom_bar() +
  scale_fill_manual(values = c("red", "blue")) +  # Custom colors for Disloyal and Loyal
  labs(title = "Customer Type Distribution",
       x = "Customer Type",
       y = "Count") +
  theme_minimal()


#############Create a histogram of passenger ages######
ggplot(airlinedata, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Histogram of Passenger Ages",
       x = "Age",
       y = "Frequency")

# Create a bar plot of customer type distribution
ggplot(airlinedata, aes(x = T_Travel, fill = factor(T_Travel, labels = c("Personal", "Business")))) +
  geom_bar() +
  scale_fill_manual(values = c("orange", "purple")) +  # Custom colors for Personal and Business
  labs(title = "Type of Travel Distribution",
       x = "Type of Travel",
       y = "Count") +
  theme_minimal()

# Create a bar plot of class distribution
ggplot(airlinedata, aes(x = Class, fill = factor(Class, labels = c("Business", "Eco", "Eco Plus")))) +
  geom_bar() +
  scale_fill_manual(values = c("purple", "blue", "green")) +  # Custom colors for Business, Eco, and Eco Plus
  labs(title = "Class Distribution",
       x = "Class",
       y = "Count") +
  theme_minimal()

# Create a scatter plot of Flight Distance vs. Departure Delay vs. Arrival Delay
ggplot(airlinedata, aes(x = Flight_DS, y = Depart_Delay_Min, color = Arrival_Delay_Min)) +
  geom_point() +
  labs(x = "Flight Distance (km)", y = "Departure Delay (min)", color = "Arrival Delay (min)") +
  theme_minimal()




##### Dependent variable plot######

# Bar plot for Satisfaction variabl

# Apply SMOTE to balance the class distribution
balanced_data <- ovun.sample(Sat ~ ., data = airlinedata, method = "both", p = 0.5, N = 10000)$data

# Bar plot of Satisfaction in balanced_data
ggplot(balanced_data, aes(x = Sat, fill = Sat)) +
  geom_bar() +
  labs(x = "Satisfaction", y = "Count", title = "Bar Plot of Satisfaction in Balanced Data") +
  theme_minimal()
##############dele###
# Create a bar plot of satisfaction distribution
ggplot(airlinedata, aes(x = Sat, fill = factor(Sat, labels = c("Dissatisfied", "Satisfied")))) +
  geom_bar() +
  scale_fill_manual(values = c("red", "green")) +  # Custom colors for Dissatisfied and Satisfied
  labs(title = "Satisfaction Distribution",
       x = "Satisfaction",
       y = "Count") +
  theme_minimal()

# Create a bar plot of satisfaction distribution for balanced data
ggplot(balanced_data, aes(x = Sat, fill = factor(Sat, labels = c("Dissatisfied", "Satisfied")))) +
  geom_bar() +
  scale_fill_manual(values = c("red", "green")) +  # Custom colors for Dissatisfied and Satisfied
  labs(title = "Satisfaction Distribution of Balanced Data",
       x = "Satisfaction",
       y = "Count") +
  theme_minimal()


ggplot(balanced_data, aes(x = Sat, fill = factor(Sat, labels = c("Dissatisfied", "Satisfied")))) +
  geom_bar() +
  scale_fill_manual(values = c("green", "red"),
                    labels = c("Dissatisfied", "Satisfied")) +  # Set legend labels
  labs(title = "Satisfaction Distribution of Balanced Data",
       x = "Satisfaction",
       y = "Count") +
  theme_minimal()

######Visualize the distribution Plot Satisfaction dataset score by service###########

# Define satisfaction-related variables########
satisfaction_vars <- c("Seat_C", "Depart_Arrival_C", "Food_drink", "Gate_loc",
                       "wifi_service", "Inflight_ent", "Online_suprt", "Online_BK",
                       "Onboard_service", "Leg._room", "Baggage_h", "Checkin", 
                       "Cleanliness",
                       "Online_boarding")

# Melt the data
melted_data <- melt(balanced_data, id.vars = "Sat", measure.vars = satisfaction_vars, 
                    variable.name = "Service", value.name = "Satisfaction_Score")

# Create box plots using facet_wrap
ggplot(melted_data, aes(x = Service, y = Satisfaction_Score, fill = factor(Sat))) +
  geom_boxplot() +
  facet_wrap(~Service, scales = "free") +
  labs(x = "Service", y = "Satisfaction Score", title = "Box Plot of Satisfaction Scores by Service") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "green"), labels = c("Dissatisfied", "Satisfied"),
                    name = "Satisfaction")


# Create the plot
ggplot(melted_data, aes(x = Service, y = Satisfaction_Score, fill = factor(Sat))) +
  geom_boxplot() +
  facet_wrap(~Service, scales = "free") +
  labs(x = "Service", y = "Satisfaction Score", title = "Box Plot of Satisfaction Scores by Service") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "green"), 
                    labels = c("Dissatisfied", "Satisfied"),
                    breaks = c(0, 1),  # Specify the order of the breaks
                    name = "Satisfaction")
####### Correlation Analysis Section################################

# Define the categorical variables
categorical_vars <- c("Sat", "Gender", "Cust_Type", "T_Travel", "Class",
                      "Seat_C", "Depart_Arrival_C", "Food_drink",
                      "Gate_loc", "wifi_service", "Inflight_ent", "Online_suprt",
                      "Online_BK", "Onboard_service", "Leg._room", "Baggage_h",
                      "Checkin", "Cleanliness", "Online_boarding")

# Convert categorical variables to numeric
airlinedata[categorical_vars] <- lapply(airlinedata[categorical_vars], as.numeric)

# Check the updated structure of the dataset
str(airlinedata)


# Select only numeric variables for correlation calculation
numeric_vars <- c("Sat", "Age", "Flight_DS", "Seat_C", "Depart_Arrival_C", "Food_drink",
                  "Gate_loc", "wifi_service", "Inflight_ent", "Online_suprt",
                  "Online_BK", "Onboard_service", "Leg._room", "Baggage_h",
                  "Checkin", "Cleanliness", "Online_boarding", "Depart_Delay_Min",
                  "Arrival_Delay_Min")


correlation_matrix <- cor(airlinedata[, numeric_vars])
print(correlation_matrix)
# Convert the correlation matrix to a data frame
cor_df <- as.data.frame(as.table(correlation_matrix))

# Create a heatmap
ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "yellow", na.value = "white") +
  labs(title = "Correlation Heatmap",
       x = "Variable 1",
       y = "Variable 2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###################Plot distribution of the variable that has strong negative correlation with Sat##

# Create a scatter plot for balanced customer satisfaction based on Departure Delay and Arrival Delay
ggplot(balanced_data, aes(x = Depart_Delay_Min, y = Arrival_Delay_Min, color = factor(Sat))) +
  geom_point() +
  labs(x = "Departure Delay in min", y = "Arrival Delay in min", title = "Balanced Customer Satisfaction based on Departure and Arrival Delays") +
  scale_color_manual(name = "Satisfaction",
                     values = c("0" = "red", "1" = "green"),
                     labels = c("Satisfied", "Dissatisfied")) +
  theme_minimal()

################Plot distribution of the variable that has strong positive correlation with balance data ###

# Few strong positive correlation plot
factors_to_plot <- c("Inflight_ent", "Seat_C", "Online_suprt", "Onboard_service", "Leg._room")


# Function to plot factor vs. satisfaction
plot_factor_vs_satisfaction <- function(factor_var) {
  ggplot(balanced_data, aes_string(x = factor_var, fill = "Sat")) +
    geom_bar() +
    labs(x = factor_var, y = "Count", title = paste("Customer Satisfaction based on", factor_var)) +
    scale_fill_manual(name = "Satisfaction",
                      values = c("0" = "red", "1" = "green"),
                      labels = c("Satisfied", "Dissatisfied")) +
    theme_minimal()
}

# Create and display plots for each factor
for (factor_var in factors_to_plot) {
  print(plot_factor_vs_satisfaction(factor_var))
}


################### Modelling Section###########################################


##################Data Partition#########
# Change categorical dependent variable to factor

airlinedata$Sat = as.factor(airlinedata$Sat)
set.seed(123)
indep2 <- sample(2, nrow(airlinedata),replace = TRUE,prob=c(0.7,0.3))

#######Train data set & Test data set###
train <- airlinedata[indep2==1,]
test <- airlinedata[indep2==2,]

#Train the logistic regression model
model = glm(Sat~ Seat_C + Depart_Arrival_C +Food_drink +
              Gate_loc + wifi_service + Inflight_ent + Online_suprt 
            + Online_BK + Onboard_service + Leg._room + Baggage_h +
              Checkin + Cleanliness + Online_boarding + Depart_Delay_Min +
              Arrival_Delay_Min 
            + Gender + Age + Cust_Type + T_Travel + Class + Flight_DS,
            data = train, family ="binomial")


summary(model)
########

model2 = glm(Sat~ Seat_C + Depart_Arrival_C +Food_drink +
               Gate_loc + Inflight_ent + Online_suprt 
             + Online_BK + Onboard_service + Leg._room + Baggage_h +
               Checkin + Cleanliness + Online_boarding + Arrival_Delay_Min 
             + Gender + Age + Cust_Type + T_Travel + Class + Flight_DS,   
             data = train, family ="binomial")
summary(model2)

###### Prediction & Confusion Matrix- Train
predict <- predict(model, train)
predict <- factor(ifelse(predict >0.5, 1,0), levels = c(0,1))
predict

# Now, both predict1 and train$Sat have the same levels
confusionMatrix(predict, train$Sat)

###### Prediction & Confusion Matrix- Test
predict1 <- predict(model, test)
predict1 <- factor(ifelse(predict1 >0.5, 1,0), levels = c(0,1))
predict1
confusionMatrix(predict1, test$Sat)
######################################

###### Prediction & Confusion Matrix- Test
predict2 <- predict(model2, test)
predict2 <- factor(ifelse(predict2 >0.5, 1,0), levels = c(0,1))
predict2 

# Calculate the confusion matrix for the test set
confusionMatrix(predict2, test$Sat)

##PLOT THE LOGISTIC REGRESSION PERFORMANCE EVALUATION###############
# Assuming you have true labels (actual) and predicted labels (predicted)

# Create the confusion matrix and statistics data frame
confusion_data <- data.frame(
  Metric = c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Accuracy"),
  Value = c( 0.8837, 0.7970, 0.7835, 0.8918, 0.8364)
)

# Create the bar plot with a descriptive title
ggplot(confusion_data, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.4f", Value)), vjust = -0.3) +
  labs(title = "Logistic Regression Model Evaluation Performance", y = "Value") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend






################ K nearest neighbor###########################

##### Feature Scaling################ 

# Select numeric independent variables for KNN
# Train the KNN model
##### Feature Scaling################ 

# Select numeric independent variables for KNN
predictor_vars <- c("Seat_C", "Depart_Arrival_C", "Food_drink",
                    "Gate_loc", "wifi_service", "Inflight_ent", "Online_suprt",
                    "Online_BK", "Onboard_service", "Leg._room", "Baggage_h",
                    "Checkin", "Cleanliness", "Online_boarding" , "Arrival_Delay_Min",
                    "Gender", "Age", "Cust_Type", "T_Travel", "Class", "Flight_DS")


# Scale the predictor features in the training and test sets
train_scaled <- as.data.frame(scale(train[, predictor_vars]))
test_scaled <- as.data.frame(scale(test[, predictor_vars]))

# Train the KNN model
k <- 5  # Number of neighbors to consider
knn_model <- knn(train = train_scaled, test = test_scaled, cl = train$Sat, k = k)

# Convert predictions to factors
knn_predictions <- factor(knn_model, levels = c("0", "1"))

# Calculate the confusion matrix for the test set
confusionMatrix(knn_predictions, test$Sat)

###########comapre
predictor_vars2 <- c("Seat_C", "Depart_Arrival_C", "Food_drink",
                     "Gate_loc", "wifi_service", "Inflight_ent", "Online_suprt",
                     "Online_BK", "Onboard_service", "Leg._room", "Baggage_h",
                     "Checkin", "Cleanliness", "Online_boarding" , "Arrival_Delay_Min",
                     "Gender", "Depart_Delay_Min", "Age", "Cust_Type", "T_Travel", "Class", "Flight_DS")


# Scale the predictor features in the training and test sets
train_scaled <- as.data.frame(scale(train[, predictor_vars2]))
test_scaled <- as.data.frame(scale(test[, predictor_vars2]))

# Train the KNN model
k <- 9  # Number of neighbors to consider
knn_model2 <- knn(train = train_scaled, test = test_scaled, cl = train$Sat, k = k)

# Convert predictions to factors
knn_predictions2 <- factor(knn_model2, levels = c("0", "1"))

# Calculate the confusion matrix for the test set
confusionMatrix(knn_predictions2, test$Sat)



###### KNN Model Training and Prediction###########################

# Replace these values with the actual values from your KNN evaluation
knn_metrics <- c("Accuracy", "Sensitivity", "Specificity", "Precision", "Negative Predictive Value")
knn_values <- c(0.9267, 0.9402, 0.9154, 0.9024, 0.9485 )

# Create a data frame for KNN
knn_data <- data.frame(metrics = knn_metrics, values = knn_values)

# Create a bar plot for KNN using ggplot2
knn_plot <- ggplot(knn_data, aes(x = metrics, y = values, fill = metrics)) +
  geom_bar(stat = "identity") +
  labs(title = "K-Nearest Neighbors Model Performance",
       x = "Metrics",
       y = "Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(values, 4)), vjust = -0.2, color = "black")

# Display the KNN plot
print(knn_plot)



################Navie baye####################################


#################
# Load necessary libraries


# Read your data (replace 'your_data.csv' with your actual file)
airlinedata <- read.csv("airlinedata.csv", stringsAsFactors = FALSE)

# Convert the target variable to a factor with two levels
airlinedata$Sat <- factor(airlinedata$Sat, levels = c("0", "1"))

# Split the data into training and test sets (70% train, 30% test)
set.seed(123)
indep2 <- sample(2, nrow(airlinedata), replace = TRUE, prob = c(0.7, 0.3))
train <- airlinedata[indep2 == 1, ]
test <- airlinedata[indep2 == 2, ]

# Select predictor variables for the model
predictor_vars <- c("Seat_C", "Depart_Arrival_C", "Food_drink",
                    "Gate_loc", "wifi_service", "Inflight_ent", "Online_suprt",
                    "Online_BK", "Onboard_service", "Leg._room", "Baggage_h",
                    "Checkin", "Cleanliness", "Online_boarding",
                    "Arrival_Delay_Min", "Depart_Delay_Min",
                    "Gender", "Age", "Cust_Type", "T_Travel", "Class", "Flight_DS")
# Select predictor variables for the model

# Train a Naive Bayes model
nb_model <- naive_bayes(Sat ~ ., data = train[, c("Sat", predictor_vars)])

# Predict using the Naive Bayes model
nb_predictions <- predict(nb_model, newdata = test)

# Evaluate the Naive Bayes model
confusionMatrix(nb_predictions, test$Sat)

##########################################

predictor_vars2 <- c("Seat_C", "Depart_Arrival_C", "Food_drink",
                     "Gate_loc", "Inflight_ent", "Online_suprt",
                     "Online_BK", "Onboard_service", "Leg._room", "Baggage_h",
                     "Checkin", "Cleanliness", "Online_boarding",
                     "Arrival_Delay_Min",
                     "Gender", "Age", "Cust_Type", "T_Travel", "Class", "Flight_DS")

# Train a Naive Bayes model
nb_model2 <- naive_bayes(Sat ~ ., data = train[, c("Sat", predictor_vars2)])

# Predict using the Naive Bayes model
nb_predictions2 <- predict(nb_model2, newdata = test)

# Evaluate the Naive Bayes model
confusionMatrix(nb_predictions2, test$Sat)

#######Naive Bayes plot######################################

# Replace these values with the actual values from your Naive Bayes evaluation
metrics <- c("Accuracy", "Sensitivity", "Specificity", "Precision", "Negative Predictive Value")
values <- c(0.8289, 0.8155, 0.8401, 0.8092, 0.8456)

# Create a data frame
data <- data.frame(metrics, values)

# Create a bar plot using ggplot2
plot <- ggplot(data, aes(x = metrics, y = values, fill = metrics)) +
  geom_bar(stat = "identity") +
  labs(title = "Naive Bayes Model Performance",
       x = "Metrics",
       y = "Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(values, 3)), vjust = -0.2, color = "black")

# Display the plot
print(plot)




###################### Decision Tree################


# Load the required library for decision tree

# Train a Decision Tree model
dt_model <- rpart(Sat ~ ., data = train, method = "class")
# Predict using the Decision Tree model
dt_predictions <- predict(dt_model, newdata = test, type = "class")
# Evaluate the Decision Tree model
conf_matrix_dt <- confusionMatrix(dt_predictions, test$Sat)
conf_matrix_dt

######plot decision tree#############
rpart.plot(dt_model)





###################### Random Forest################
# Load the required library for random forest

# Train a Random Forest model
rf_model <- randomForest(Sat ~ ., data = train, ntree = 100)
# Predict using the Random Forest model
rf_predictions <- predict(rf_model, newdata = test)
# Evaluate the Random Forest model
conf_matrix_rf <- confusionMatrix(rf_predictions, test$Sat)
conf_matrix_rf
# View feature importance
varImpPlot(rf_model)

# Plot confusion matrix - Random Forest##############
plot(conf_matrix_rf$table, col = conf_matrix_rf$byClassColor, main = "Confusion Matrix")

# Calculate ROC curve
roc_curve_rf <- roc(test$Sat, as.numeric(rf_predictions))

# Plot ROC curve
plot(roc_curve_rf, main = "ROC Curve for Random Forest Model")

# Train a Random Forest model
rf_model <- randomForest(Sat ~ ., data = train, ntree = 100)

# Visualize the first tree in the forest
plot(rf_model, main = "Random Forest - First Tree Visualization")


#######Compare each models performance###########


