rm(list = ls())

data(iris)
x <- iris[ , 1:4 ]
y <- iris$Species # as.numeric(iris$Species == "setosa")
nodes = 4
learning_rate = 0.1
training_prop = 0.8
max_iters = 10000

### This function fits a neural network with one hidden layer using 
# gradient descent

FitAnn <- function(y, x, nodes, learning_rate = 0.1,
                   training_prop = 0.8, max_iters = 10000){
  
  ### Declare activation function ----
  s <- function(z) 1 / (1 + exp(-z))
  
  ### Format data (factors to model.matrix etc) ----
  # for now, assuming Y is a factor vector and X is a numeric matrix
  ynames <- levels(y)
  
  Y <- data.frame(y = y)
  
  Y <- model.matrix(~y, Y)
  
  colnames(Y) <- ynames
  
  # model.matrix always makes the first column all "1", fix this
  Y[ , 1 ] <- as.numeric(rowSums(Y[ , -1 ]) == 0)
  
  # format X
  X <- cbind(1, as.matrix(x))
  
  ### Separate training and validation ----
  training_rows <- sample(1:nrow(X), size = round(training_prop * nrow(X)))
  test_rows <- setdiff(1:nrow(X), training_rows)
  
  X_train <- X[ training_rows, ]
  Y_train <- Y[ training_rows, ]
  
  X_test <- X[ test_rows , ]
  Y_test <- Y[ test_rows , ]
  
  ### begin propogation -----
  
  #initialize the weights 
  weights <- vector(mode = "list", length = 2)
  
  # initialize with draws from a normal distribution
  # this keeps them small and centered around zero, which I believe is less
  # biased than initalizing them to all positive numbers
  weights[[ 1 ]] <- 
    do.call(cbind,
            lapply(1:(nodes), function(n) rnorm(n = ncol(X) , mean = 0, sd = 0.05)))
  
  
  weights[[ 2 ]] <- 
    do.call(cbind,
            lapply(1:ncol(Y), function(n){
              rnorm(n = nodes + 1, mean = 0, sd = 0.05)
            }))
  
  
  # initialize network 
  net <- list(X_train,
              c(),
              c())
  
  test_net <- list(X_test,
                   c(),
                   c())
  
  # initialize error
  cost <- function(y, yhat){
    0.5 * sum( colSums((y - yhat) ^ 2))
  }
  
  cost_history <- double(max_iters)
  
  # this just initializes a value so the below while loop will work
  cost_curr <- 100
  
  
  # declare some variables before we loop
  iter <- 1
  delta <- vector(mode = "list", length = 2)
  
  delta_history <- vector(mode = "list", length = max_iters)
  
  weight_history <- delta_history
  
  while (iter < max_iters & cost_curr > 0.001) {
    
    # get the network
    net[[ 2 ]] <- cbind(1, s(net[[ 1 ]] %*% weights[[ 1 ]]))
    
    net[[ 3 ]] <- s(net[[ 2 ]] %*% weights[[ 2 ]])
    
    ##################################################
    # get the error and weight update rule
    # this is so horrifically wrong

    error <- (net[[ 3 ]] - Y_train) 
    
    delta[[ 2 ]] <- error * net[[ 3 ]] * (1 - net[[ 3 ]]) # initial delta
    
    delta[[ 1 ]] <- (delta[[ 2 ]] %*% t(weights[[ 2 ]] ))* 
      net[[ 2 ]] * (1 - net[[ 2 ]]) # initial delta
    
    delta[[ 2 ]] <- t(delta[[ 2 ]]) %*% net[[ 2 ]] # final delta
    
    delta[[ 1 ]] <- t(delta[[ 1 ]][ , 2:ncol(delta[[ 1 ]])]) %*% net[[ 1 ]] # final delta
    
    
    # update weights
    weights <- mapply(function(w, d) w - learning_rate * t(d),
                      w = weights, d = delta,
                      SIMPLIFY = FALSE)
    
    # update the test network
    test_net[[ 2 ]] <- cbind(1, s(test_net[[ 1 ]] %*% weights[[ 1 ]]))
    
    test_net[[ 3 ]] <- s(test_net[[ 2 ]] %*% weights[[ 2 ]])
    
    # record the current value of the cost function
    cost_history[ iter ] <- cost(y = Y_test, yhat = test_net[[ 3 ]])
    
    delta_history[[ iter ]] <- delta
    
    weight_history[[ iter ]] <- weights
    
    iter <- iter + 1
    
    # print(weights)
    
  }  
  fitted <- s(X %*% weights[[ 1 ]])
  
  fitted <- s(cbind(1, fitted) %*% weights[[ 2 ]])
  
  result <- list(weights = weights,
                 cost_history = cost_history[ seq_len(iter - 1) ],
                 fitted_values = fitted)
  
  class(result) <- "CSI_873_ANN"
  
  return(result)
}

result <- FitAnn(y = y, x = x, nodes = 2,
                 learning_rate = 0.1, 
                 training_prop = 0.8,
                 max_iters = 10000)

plot(result$cost_history, type = "l")

