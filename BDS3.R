BDSforFunctionOfB <- function(X_train,y_train,X_test,y_test,first_sequence,second_sequence,eta,stopping_points){

  test_MSE <- c()
  for (i in 1:length(stopping_points)) {
    points <- stopping_points[i]
    boosted_value <- BDS(X_train,y_train,first_sequence,second_sequence,eta,points)
    prediction <- prediction_for_BDS(boosted_value,X_test,eta,points)
    test_MSE[i] <- mean((prediction-y_test)^2)
  }
  plot(stopping_points,test_MSE)
}