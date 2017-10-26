


for (e in seq_len(epochs)) {
  # divide the data into random partitions
  batches <- Batch(data, batch_size)
  
  # perform stochastic gradient descent on each batch to update weights
  result <- parallel::mclapply(batches, function(b){
    SGD(b, weights)
  })
  
  # average the weights across batches
  weights <- AvgWeightFun(result)
  
  # record the current value of the cost function
  test_net[[ 2 ]] <- cbind(1, s(test_net[[ 1 ]] %*% weights[[ 1 ]]))
  
  test_net[[ 3 ]] <- s(test_net[[ 2 ]] %*% weights[[ 2 ]])
  
  
  cost_history[ e ] <- cost(y = Y_test, yhat = test_net[[ 3 ]])
  
}
