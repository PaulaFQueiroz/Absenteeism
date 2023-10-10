df= read.csv("Absenteeism_at_work.csv",sep=";",header=T,na.strings = "?")

abs = df

summary(abs)
names(abs)
attach(abs)

head(abs)




# call reason.type rep("NA, num of rows in abs), for each i from 1 to num of rows
#look in each reason for absence from 1 to num of rows, call reasin each reason.for ansence in 
# if reason(num of reasin.for.absence) is an element of c()then assign it to reason.type = G1

reason_type <- rep("NA",nrow(abs))
for (i in 1:nrow(abs))
{
  reason <- abs$Reason.for.absence[i]
  if (is.element(reason,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,17,18,19,20,21, 27)))
    reason_type[i]="sick"
  if (is.element(reason,c(15,16)))
    reason_type[i]="pragnant"
 # if (is.element(reason,c(19,20)))
  #  reason_type[i]="doctor_visit"
  if (is.element(reason,c(22:25)))
    reason_type[i]="consultation"
  if (is.element(reason,c(26,0)))
    reason_type[i]="unjustified"
  if (is.element(reason,c(28)))
    reason_type[i]="dentist" 
  
}


#add reason.type to abs dataset and delete Reason.for.absence
abs$reason_type=reason_type
abs$Reason.for.absence=NULL
summary(abs)


#delete columns we dont want
abs_data <- abs[,-c(1,9,10,11,17,18)]
names(abs_data)




#make it all categorical
cat.col <- c(1,2,3,8,10,11,15) #month, week, season,edu,drink, smoke,reason
for (i in cat.col)
{
  abs_data[,i] <- as.factor(as.character(abs_data[,i]))
}

summary(abs_data)

# Load the caret package
library(caret)

# Define the formula for creating dummy variables for "Month.of.absence"
formula <- as.formula("~ Month.of.absence")
# Use dummyVars to create dummy variables
dummy_data <- dummyVars(formula, data = abs_data)
# Apply the transformation to your original data
month <- predict(dummy_data, newdata = abs_data)
as.data.frame(month)
month=month[,-1]
# Combine the original data frame (abs_data) with the month dummy variables (month)
abs_data <- cbind(abs_data, month)
# Drop the original "Month.of.absence" variable
abs_data <- abs_data[, -which(names(abs_data) == "Month.of.absence")]


# View the updated data frame
head(abs_data)
names(abs_data)



# Define the formula for creating dummy variables for "Month.of.absence"
formula <- as.formula("~ Day.of.the.week")
# Use dummyVars to create dummy variables
dummy_data <- dummyVars(formula, data = abs_data)
# Apply the transformation to your original data
days <- predict(dummy_data, newdata = abs_data)
as.data.frame(days)
# Combine the original data frame (abs_data) with the month dummy variables (month)
abs_data <- cbind(abs_data, days)
# Drop the original "Month.of.absence" variable
abs_data <- abs_data[, -which(names(abs_data) == "Day.of.the.week")]


# View the updated data frame
head(abs_data)
names(abs_data)


# Define the formula for creating dummy variables for "Month.of.absence"
formula <- as.formula("~ Seasons")
# Use dummyVars to create dummy variables
dummy_data <- dummyVars(formula, data = abs_data)
# Apply the transformation to your original data
seasons <- predict(dummy_data, newdata = abs_data)
as.data.frame(seasons)
# Combine the original data frame (abs_data) with the month dummy variables (month)
abs_data <- cbind(abs_data, seasons)
# Drop the original "Month.of.absence" variable
abs_data <- abs_data[, -which(names(abs_data) == "Seasons")]


# View the updated data frame
head(abs_data)
names(abs_data)


# Define the formula for creating dummy variables for "Month.of.absence"
formula <- as.formula("~ Education")
# Use dummyVars to create dummy variables
dummy_data <- dummyVars(formula, data = abs_data)
# Apply the transformation to your original data
education <- predict(dummy_data, newdata = abs_data)
as.data.frame(education)
# Combine the original data frame (abs_data) with the month dummy variables (month)
abs_data <- cbind(abs_data, education)
# Drop the original "Month.of.absence" variable
abs_data <- abs_data[, -which(names(abs_data) == "Education")]


# View the updated data frame
head(abs_data)
names(abs_data)


# Define the formula for creating dummy variables for "Month.of.absence"
formula <- as.formula("~ Social.drinker")
# Use dummyVars to create dummy variables
dummy_data <- dummyVars(formula, data = abs_data)
# Apply the transformation to your original data
drinker <- predict(dummy_data, newdata = abs_data)
as.data.frame(drinker)
# Combine the original data frame (abs_data) with the month dummy variables (month)
abs_data <- cbind(abs_data, drinker)
# Drop the original "Month.of.absence" variable
abs_data <- abs_data[, -which(names(abs_data) == "Social.drinker")]


# View the updated data frame
head(abs_data)
names(abs_data)


# Define the formula for creating dummy variables for "Month.of.absence"
formula <- as.formula("~ Social.smoker")
# Use dummyVars to create dummy variables
dummy_data <- dummyVars(formula, data = abs_data)
# Apply the transformation to your original data
smoker <- predict(dummy_data, newdata = abs_data)
as.data.frame(smoker)
# Combine the original data frame (abs_data) with the month dummy variables (month)
abs_data <- cbind(abs_data, smoker)
# Drop the original "Month.of.absence" variable
abs_data <- abs_data[, -which(names(abs_data) == "Social.smoker")]


# View the updated data frame
head(abs_data)
names(abs_data)



# Define the formula for creating dummy variables for "Month.of.absence"
formula <- as.formula("~ reason_type")
# Use dummyVars to create dummy variables
dummy_data <- dummyVars(formula, data = abs_data)
# Apply the transformation to your original data
Reason <- predict(dummy_data, newdata = abs_data)
as.data.frame(Reason)
# Combine the original data frame (abs_data) with the month dummy variables (month)
abs_data <- cbind(abs_data, Reason)
# Drop the original "Month.of.absence" variable
abs_data <- abs_data[, -which(names(abs_data) == "reason_type")]


# View the updated data frame
head(abs_data)
names(abs_data)

#############################################################################Viz
#reason
library(ggplot2)
ggplot(abs_data, aes(x=reorder(reason_type, reason_type, function(x)-length(x)))) +
  geom_bar(fill='maroon') +
  labs(x='reason_type')

#week 2mon 3tues 4wed 5thu 6fri

ggplot(abs_data, aes(x=reorder(Day.of.the.week, Day.of.the.week, function(x)-length(x)))) +
  geom_bar(fill='maroon') +
  labs(x='days of the week')

#season 1_summer, 2_fall, 3_wonter, 4_spring
ggplot(abs_data, aes(x=reorder(Seasons , Seasons , function(x)-length(x)))) +
  geom_bar(fill='maroon') +
  labs(x='seanson')

#high_school_1 , 2_undergrad, 3_post_undergrad,  4_grad
ggplot(abs_data, aes(x=reorder(Education , Education , function(x)-length(x)))) +
  geom_bar(fill='maroon') +
  labs(x='education')

hist(abs_data$Son)
son_counts <- table(abs_data$Son)

hist(abs_data$Pet)
hist(abs_data$Pet)

pet_counts <- table(abs_data$Pet)



ggplot(abs_data, aes(x=reorder(Social.drinker , Social.drinker , function(x)-length(x)))) +
  geom_bar(fill='maroon') +
  labs(x='Social.drinker')

#high_school_1 , 2_undergrad, 3_post_undergrad,  4_grad
ggplot(abs_data, aes(x=reorder(Social.smoker , Social.smoker , function(x)-length(x)))) +
  geom_bar(fill='maroon') +
  labs(x='Social.smoker')

# Load the hexbin package
library(ggplot2)
plot(abs_data$Body.mass.index, abs_data$Absenteeism.time.in.hours , 
     col = "blue", 
     xlab = "BMI",
     ylab = "Abs",
     main = "BMI by abs")

# Create a 2D histogram plot
ggplot(abs_data, aes(x = Body.mass.index, y = Absenteeism.time.in.hours)) +
  geom_bin2d(bins = 20, aes(fill = stat(count)), alpha = 0.6) +
  labs(title = "2D Histogram Plot", x = "X-axis label", y = "Y-axis label")

###########################################################################

##############################################################################try random forest: 


# Load necessary libraries
library(randomForest)

library(caret)


#train and test
set.seed(1)


train <- sample(1:nrow(abs_data),0.8*nrow(abs_data))
training_data <- abs_data[train,]
testing_data <- abs_data[-train,]

# Define the row indices for training and testing
train_indices <- sample(1:nrow(abs_data), nrow(abs_data) / 2)
test_indices <- setdiff(1:nrow(abs_data), train_indices)

# Create training and testing data
training_data <- abs_data[train_indices, ]
testing_data <- abs_data[test_indices, ]



names(abs_data)
names(training_data)
dim(abs_data)
dim(testing_data)


#######################################################had problems with model
# Check for missing values in training_data
any(is.na(training_data))

# Check for missing values in training_data
any(is.na(training_data))

# Verify variable names
names(training_data)

# Verify data types
sapply(training_data, class)

# Check the dimensions of training_data
dim(training_data)

########################################################Solution

# Identify categorical variables
categorical_vars <- names(training_data)[sapply(training_data, is.factor)]


# One-hot encode categorical variables and store as data frames
training_data <- as.data.frame(model.matrix(Absenteeism.time.in.hours ~ ., data = training_data))
testing_data <- as.data.frame(model.matrix(Absenteeism.time.in.hours ~ ., data = testing_data))



training_data <- training_data[, -1]
testing_data <- testing_data[, -1]

class(training_data)
class(testing_data)

colnames(training_data)
colnames(testing_data)

# Add the target variable back to the data frames
training_data$Absenteeism.time.in.hours <- abs_data[train_indices, ]$Absenteeism.time.in.hours
testing_data$Absenteeism.time.in.hours <- abs_data[test_indices, ]$Absenteeism.time.in.hours


###############################################Model again: 
dim(training_data)
dim(testing_data)
dim(abs_data)


# Check if the target variable is present in both data frames
"Absenteeism.time.in.hours" %in% colnames(training_data)
"Absenteeism.time.in.hours" %in% colnames(testing_data)




# Build a Random Forest model using one-hot encoded predictors
rf_model <- randomForest(Absenteeism.time.in.hours ~ ., data = training_data)


# Make predictions on the testing dataset
predictions <- predict(rf_model, newdata = testing_data)


# Ensure testing_data is a data frame (assuming it's not already)
testing_data <- as.data.frame(testing_data)

# Evaluate the model (calculate RMSE)
rmse <- sqrt(mean((predictions - testing_data$Absenteeism.time.in.hours)^2))




print(paste("Root Mean Squared Error (RMSE):", rmse))

# Extract feature importance scores
importance_scores <- rf_model$importance

library(ggplot2)


attach(importance_df)

# Create a data frame with the importance scores
importance_df <- data.frame(Features = rownames(importance_scores),
                            Importance_Score = importance_scores)

names(importance_df)

barplot(importance_df$IncNodePurity)


# Sort the data frame by Importance_Score in descending order
importance_df <- importance_df[order(importance_df$IncNodePurity), ]

# Create a bar plot of feature importance scores
library(ggplot2)

# Create the bar plot
ggplot(importance_df, aes(x = reorder(Features, IncNodePurity), y = IncNodePurity)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Feature Importance Scores", x = "Features", y = "Importance Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

##############################################################



#train and test
set.seed(1)


train <- sample(1:nrow(abs_data),0.8*nrow(abs_data))
train_data <- abs_data[train,]
test_data <- abs_data[-train,]

summary(train_data)


########################################Linear regression fit and predict.

lm_fit <- lm(Absenteeism.time.in.hours~.,train_data)
lm_pred <- predict(lm_fit,newdata=test_data)
summary(lm_fit)
lm_MSE <-mean((lm_pred-test_data$Absenteeism.time.in.hours)^2)#test MSE

lm_MSE




library(ISLR)
library(glmnet)






#model.matrix()automatically transforms qualitat var into dummy var #glmnet() can only take numerical inputs.
x=model.matrix(Absenteeism.time.in.hours~.,abs_data)[,-1]
y=abs_data$Absenteeism.time.in.hours
dim(x)

#training and testing sets
set.seed(1)
train <- sample(1:nrow(abs_data),0.8*nrow(abs_data))
test<-(-train)
y.test=y[test]



#generating grid lambidas
grid=10^seq(10,-2,length=100) 
ridge_mod=glmnet(x,y,alpha=0,lambda=grid)


################################Ridge Regression
ridge_mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12) # thresh controls coordinate descent convergence 

#we can use CV to choose the tuning parameter lambda
set.seed(1)
cv_out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv_out)#cross validation errors  (y) for all lambidas(x)
bestlam=cv_out$lambda.min #yelds best lambida
bestlam
ridge.pred=predict(ridge_mod,s=bestlam,newx=x[test,])  
ridge_MSE= mean((ridge.pred-y.test)^2)
ridge_MSE
#refit on the full data set, using the "best" lambda
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:43,] 
#note:none of beta.hat are zero!

################## The Lasso  #####################

lasso_mod=glmnet(x[train,],y[train],alpha=1,lambda=grid, thresh=1e-12) # thresh controls coordinate descent convergence 

#we can use CV to choose the tuning parameter lambda
set.seed(1)
cv_out_lasso=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv_out_lasso)#cross validation errors  (y) for all lambidas(x)
bestlam=cv_out_lasso$lambda.min #yelds best lambida
bestlam
lasso_pred=predict(lasso_mod,s=bestlam,newx=x[test,])  
lasso_MSE= mean((lasso_pred-y.test)^2)
lasso_MSE

#refit on the full data set, using the "best" lambda
out=glmnet(x,y,alpha=1)
predict(out,type="coefficients",s=bestlam)[1:43,] 
#note:none of beta.hat are zero!

lasso_coef=predict(out,type="coefficients",s=bestlam)[1:43,]
lasso_coef
lasso_coef[lasso_coef!=0]


##################################################################################



