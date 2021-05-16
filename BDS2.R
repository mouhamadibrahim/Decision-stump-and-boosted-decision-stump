BDS <- function(X_train, y_train, first_sequence, second_sequence, eta, B){
  
  decision_stump_value <- c()
  
  final_value <- matrix(,B,4)
  
  for (index in 1:B) {
    
    decision_stump_value <- Decision_Stump(X_train, y_train, first_sequence, second_sequence)
    final_value[index,] <- decision_stump_value
    
    first_value <- decision_stump_value[1]
    
    first_comparison <- X_train[,1]<decision_stump_value[2]
    second_comparison <- X_train[,2]<decision_stump_value[2]
    
    
    if (first_value == 1) {
      min_value <- first_comparison
    }
    if (decision_stump_value[1]==2) {
      min_value <- second_comparison
    }
    
    ds_3 <- decision_stump_value[3]
    ds_4 <- decision_stump_value[4]
    
    y_train[min_value] <- y_train[min_value] - eta* ds_3
    y_train[!min_value] <- y_train[!min_value] - eta* ds_4
  }
  return(final_value)
}


prediction_for_BDS <- function(final_value, X_test, eta, B){
  
  number_of_rows <- nrow(X_test)
  
  prediction <- matrix(0,number_of_rows,1)
  
  for (index in 1:B) {
    prediction <- prediction + eta*prediction_for_decision_stump(final_value[index,],X_test)
  }
  
  return(prediction)
}
