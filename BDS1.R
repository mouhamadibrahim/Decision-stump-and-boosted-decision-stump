Decision_Stump <- function(X_train, y_train, first_sequence, second_sequence){
  
  number_of_columns = ncol(X_train)
  
  length_of_first_sequence = length(first_sequence)
  length_of_second_sequence = length(second_sequence)
  
  RSS_value <- matrix(Inf,number_of_columns,max(length_of_first_sequence,length_of_second_sequence))
  
  for (index in 1:number_of_columns) {
    if (index == 1){  
      sequence <- first_sequence
    }
    else 
    {
      sequence <- second_sequence
    }
    for (j in 1:length(sequence)) {
      comparision <- (X_train[,index] < sequence[j])
      minimum_value_to_be <- comparision
      bellow <- y_train[minimum_value_to_be]
      over <- y_train[!minimum_value_to_be]
      sum_below = (bellow-mean(bellow))^2
      sum_over = (over-mean(over))^2
      RSS_value[index,j] <- sum(sum_below) + sum(sum_over)
    }
  }
  
  min_RSS <- which(RSS_value==min(RSS_value),arr.ind=TRUE)
  
  RSS_specific_value1 <- min_RSS[1,1]
  RSS_specific_value2 <- min_RSS[1,2] 
  
  if (RSS_specific_value1 == 1) { 
    minimum_value_to_be <- (X_train[,1] < first_sequence[RSS_specific_value2])
    final_result <- c(as.integer(RSS_specific_value1),first_sequence[RSS_specific_value2],mean(y_train[minimum_value_to_be]),mean(y_train[!minimum_value_to_be]))
  }
  
  if (RSS_specific_value1== 2){
    minimum_value_to_be <- X_train[,2]<second_sequence[RSS_specific_value2]
    final_result <- c(as.integer(RSS_specific_value1),second_sequence[RSS_specific_value2],mean(y_train[minimum_value_to_be]),mean(y_train[!minimum_value_to_be]))
  }
  
  return(final_result)
}


prediction_for_decision_stump <- function(decision_stump_result, X_test){
  
  ds_1 <- decision_stump_result[1] 
  ds_2 <- decision_stump_result[2]
  ds_3 <- decision_stump_result[3]
  ds_4 <- decision_stump_result[4]
  
  test_value_1 <- X_test[,1] 
  test_value_2 <- X_test[,2]
  
  number_of_rows <- nrow(X_test)
  
  prediction <- matrix(rep(ds_4,number_of_rows),number_of_rows,1)
  
  if (ds_1==1) {
    prediction[test_value_1 < ds_2,] <- ds_3
  }
  if (ds_1==2) {
    prediction[test_value_2 < ds_2,] <- ds_3
  }
  return(prediction)
}

