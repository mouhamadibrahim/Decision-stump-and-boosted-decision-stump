#import data
library(ISLR)
library(MASS)

data(Boston)

#set the seed
set.seed(0804)

#getting the columns we want to work with
first_column = Boston$lstat
second_column = Boston$rm

#combining them
X_data <- cbind(first_column, second_column)

#getting the y column data
label <- Boston$medv

#getting number of rows of the data
number_of_rows = nrow(Boston)

#splitting the data in half
split_data <- sample(number_of_rows,number_of_rows/2)

#splitting data into training and testing
X_train <- X_data[split_data,]
y_train <- as.matrix(label[split_data])
X_test <- X_data[-split_data,]
y_test <- as.matrix(label[-split_data])

#creating a sequence vector
first_sequence <- seq(2.7,43.2,0.1)
second_sequence <- seq(5,11.2,0.1)

#numer of trees we want to loop through and eta value
eta <- 0.01
B <- 1000


#main for decision stump
decision_stump_result <- Decision_Stump(X_train,y_train,first_sequence,second_sequence)
prediction_value_for_decision_stump <- prediction_for_decision_stump(decision_stump_result,X_test)
test_MSE_for_DS <- mean((prediction_value_for_decision_stump-y_test)^2)

#main for boosted decision stump
boosted_decision_stump <- BDS(X_train,y_train,first_sequence,second_sequence,eta,B)
prediction_for_boosted <- prediction_for_BDS(boosted_decision_stump,X_test,eta,B)
test_MSE_for_boosted <- mean((prediction_for_boosted-y_test)^2)

#final values
print(paste0("the test MSE for decision stump: ", test_MSE_for_DS))
print(paste0("the test MSE for boosted decision stump: ", test_MSE_for_boosted))

BSequence <- seq(300,6000,300)
BDSforFunctionOfB(X_train,y_train,X_test,y_test,first_sequence,second_sequence,eta,BSequence)