#### Linear Regression for R script ####
#### By El Mostafa Bendriss ####
#### November 2, 2021 ####


# Installing packages
install.packages("tidyverse")
install.packages('caret', dependencies = TRUE)

# Loading and attaching packages
library(tidyverse)
library(caret)

# Reading the csv dataset
s_scores <- read.csv("student_scores.csv")

# Plotting the distribution of scores
ggplot(data = s_scores) +geom_point(mapping = aes(x = Hours, y = Scores))


# Creating training set indices of data
inTrain <- createDataPartition(y = s_scores$Scores, p = 0.8, list = FALSE)

# Subsetting s_scores data to training
training <- s_scores[inTrain,]

# Subsetting the rest to testing
testing  <- s_scores[-inTrain,]

# Fitting linear regression model
my_lm <- train(Scores~Hours, data=training, method = "lm", preProc = c("center", "scale"))



# Predicting using the test data
testing_var <- data.frame(Hours=testing$Hours)
pred <- predict(my_lm, testing_var)

# Testing with my own data
hours_var <- 9.25
own_pred <- predict(my_lm,data.frame(Hours=hours_var))
own_pred


# Evaluating the model

SSE = sum((testing[,2] -pred)^2)
SST = sum((testing[,2] - mean(training[,2]))^2)
R_square = 1 - SSE/SST
message('R_squared on the test data:')
round(R_square, 2)
RMSE = sqrt(SSE/length(pred))
message("Root mean square error on the test data: ")
round(RMSE, 2)

