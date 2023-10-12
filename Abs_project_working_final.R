df= read.csv("Absenteeism_at_work.csv",sep=";",header=T,na.strings = "?")

abs = df

summary(abs)
names(abs)
attach(abs)

head(abs)



################################################################################Grouping Reason type
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
#names(abs_data)



################################################################################Change data type
#make it all categorical
cat.col <- c(1,2,3,8,10,11,15) #month, week, season,edu,drink, smoke,reason
for (i in cat.col)
{
  abs_data[,i] <- as.factor(as.character(abs_data[,i]))
}


df = abs_data


#summary(abs_data)
################################################################################Adding Dummies
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

################################################################################Viz
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

#################################################################################Random forest: 
# Load necessary libraries
library(randomForest)

################################################################################train and test
set.seed(1)

train <- sample(1:nrow(abs_data),0.8*nrow(abs_data))
training_data <- abs_data[train,]
testing_data <- abs_data[-train,]



################################################################################Quality check
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
class(testing_data)

################################################################################Build Random Forest model
rf_model <- randomForest(Absenteeism.time.in.hours ~ ., data = training_data)


# Make predictions on the testing dataset
predictions <- predict(rf_model, newdata = testing_data)


# Ensure testing_data is a data frame (assuming it's not already)
testing_data <- as.data.frame(testing_data)

################################################################################ Evaluate the model (calculate RMSE)
rmse <- sqrt(mean((predictions - testing_data$Absenteeism.time.in.hours)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse)) #10.3545

################################################################################ Extract feature importance scores
importance_scores <- rf_model$importance


# Create a data frame with the importance scores
importance_df <- data.frame(Features = rownames(importance_scores),
                            Importance_Score = importance_scores)


# Sort the data frame by Importance_Score in descending order
importance_df <- importance_df[order(importance_df$IncNodePurity), ]

# Create a bar plot of feature importance scores
library(ggplot2)

# Create the bar plot
ggplot(importance_df, aes(x = reorder(Features, IncNodePurity), y = IncNodePurity)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Feature Importance Scores", x = "Features", y = "Importance Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

################################################################################ Training and testing seed(1)

set.seed(1)

train <- sample(1:nrow(df),0.8*nrow(df))
training_data <- df[train,]
testing_data <- df[-train,]
dim(testing_data)

########################################Linear regression fit and predict.
library(tidyr)
library(dplyr)


#do not take out Transport expense, age, Education,Social.smoke
# Fit a GLM model for linear regression
model <- glm(Absenteeism.time.in.hours ~. -Education - Social.smoker - Month.of.absence -Pet -Age - Transportation.expense -Social.drinker  -Service.time -Seasons -Body.mass.index
            , data = training_data)
summary(model)



# Create a null model (intercept-only model)
null_model <- glm(Absenteeism.time.in.hours ~ 1, data = training_data)


fullmodel <- glm(Absenteeism.time.in.hours ~. , data = training_data)
summary(fullmodel)


# Use anova to compare the two models
anova_result <- anova(fullmodel, model, test = "Chisq")
print(anova_result)




################################################################################Goodness of fit


# Use anova to compare the two models
anova_result <- anova(null_model, model, test = "Chisq")
print(anova_result)



# Assuming you already have your model and test_data
predictions <- predict(model, newdata = testing_data)
residuals <- testing_data$Absenteeism.time.in.hours - predictions
mse <- mean(residuals^2)
rmse <- sqrt(mse)


cat("Root Mean Squared Error (RMSE):", rmse, "\n")


anova(model)

# Load the 'car' package for VIF calculation
library(car)

# Calculate VIF for the model
vif_result <- car::vif(model)

# View the VIF values
print(vif_result)
#Based on VIS results we cab drop service time and season

################################################################################ checking assumptions

# Check normality of residuals
residuals <- testing_data$Absenteeism.time.in.hours - predictions
hist(residuals, main = "Histogram of Residuals")

#Residuals vs. Fitted Values Plot:
plot(model, which = 1)  #do not see a pronounced curve with the exception of some outliers

plot(model, which = 4)  # obs that indicate non linearity: 324, 421, 324, 623

plot(model, which = 3)    #heterostadiscity

obs_row <- abs_data[623, ]


qqnorm(residuals)
qqline(residuals)
#not notmal

shapiro.test(residuals)
#This indicates strong evidence against the null hypothesis that the residuals are normally distributed. 


# Create scatterplots for visual inspection
par(mfrow = c(3, 3))  # To arrange plots in a grid, change the dimensions as needed

names(df)
# Replace the variable names with your actual column names
plot( df$Son, df$Absenteeism.time.in.hours,
     main = "Scatterplot: Absenteeism vs. distance")

plot(data$Absenteeism.time.in.hours, data$Distance.from.Residence.to.Work, 
     main = "Scatterplot: Absenteeism vs. Distance from Residence to Work")

# Continue plotting for other independent variables...

par(mfrow = c(1, 1))  # Reset to default plot layout





lm_fit <- lm(Absenteeism.time.in.hours~.,training_data)
lm_pred <- predict(lm_fit,newdata=testing_data)
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



