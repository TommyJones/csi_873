rm(list = ls())

data(iris)
x <- iris[ , 1:4 ]
y <- iris$Species # as.numeric(iris$Species == "setosa")
nodes = 4
learning_rate = 0.1
training_prop = 0.8
epochs = 50
momentum = 0.1
### This function fits a neural network with one hidden layer using 
# stochastic gradient descent

FitAnnSgd <- function(y, x, nodes, learning_rate = 0.1, momentum = 0.1,
                   training_prop = 0.8, epochs = 100){
  
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
  
  cost_history <- double(epochs)
  
  # this just initializes a value so the below while loop will work
  cost_curr <- 100
  
  
  # declare some variables before we loop
  iter <- 1
  delta <- vector(mode = "list", length = 2)
  
  delta_last <- lapply(weights, function(w) t(w) * 0) 
  
  while (iter < epochs) {
    
    # Run through training instances completely
    for (j in seq_len(nrow(X_train))) {
      
      # get the network
      net[[ 1 ]] <- matrix(X_train[ j, ], nrow = 1)
      
      net[[ 2 ]] <- matrix(c(1, s(net[[ 1 ]] %*% weights[[ 1 ]])), nrow = 1)
      
      net[[ 3 ]] <- matrix(s(net[[ 2 ]] %*% weights[[ 2 ]]), nrow = 1)
      
      ##################################################
      # get the error and weight update rule
      # this is so horrifically wrong
      
      error <- (net[[ 3 ]] - Y_train[ j , ]) 
      
      delta[[ 2 ]] <- error * net[[ 3 ]] * (1 - net[[ 3 ]]) # initial delta
      
      delta[[ 1 ]] <- (delta[[ 2 ]] %*% t(weights[[ 2 ]] ))* 
        net[[ 2 ]] * (1 - net[[ 2 ]]) # initial delta
      
      delta[[ 2 ]] <- t(delta[[ 2 ]]) %*% net[[ 2 ]] # final delta
      
      delta[[ 1 ]] <- t(matrix(delta[[ 1 ]][ , 2:ncol(delta[[ 1 ]]) ], nrow = 1)) %*% 
        net[[ 1 ]] # final delta
      
      # update weights
      weights <- mapply(function(w, d, d_last){ 
        w - learning_rate * t(d) - momentum * t(d_last)
      },w = weights, d = delta, d_last = delta_last,
      SIMPLIFY = FALSE)
      
    }
    
    # record delta as delta_last for momentum
    delta_last <- delta
    
    # update the test network for this epoch
    test_net[[ 2 ]] <- cbind(1, s(test_net[[ 1 ]] %*% weights[[ 1 ]]))
    
    test_net[[ 3 ]] <- s(test_net[[ 2 ]] %*% weights[[ 2 ]])
    
    # record the current value of the cost function
    cost_history[ iter ] <- cost(y = Y_test, yhat = test_net[[ 3 ]])
    
    iter <- iter + 1
    
    # print(weights)
    
  }  
  fitted <- s(X %*% weights[[ 1 ]])
  
  fitted <- s(cbind(1, fitted) %*% weights[[ 2 ]])
  
  colnames(fitted) <- levels(y)
  
  rownames(weights[[ 1 ]]) <- c("Intercept", colnames(x))
  
  result <- list(weights = weights,
                 cost_history = cost_history[ seq_len(iter - 1) ],
                 fitted_values = fitted)
  
  class(result) <- "CSI_873_ANN"
  
  return(result)
}

result <- FitAnnSgd(y = y, x = x, nodes = 2,
                 learning_rate = 0.1,
                 momentum = 0,
                 training_prop = 0.8,
                 epochs = 1000)

plot(result$cost_history, type = "l")

table(y , apply(result$fitted_values, 1, function(z) names(z)[ which.max(z) ]))


predict.CSI_873_ANN <- function(object, newdata) {
  
  if (class(object) != "CSI_873_ANN") {
    stop("object must be of class CSI_873_ANN")
  }
  
  if (! class(newdata) %in% c("data.frame", "matrix")) {
    stop("newdata must be of class data.frame or matrix")
  }
  
  # Declare activation function 
  s <- function(z) 1 / (1 + exp(-z))
  
  
  # make sure columns of newdata line up to rows of weights
  col_names <- rownames(object$weights[[ 1 ]])[ 2:nrow(object$weights[[ 1 ]]) ]
  
  net <- list(cbind(1, as.matrix(newdata[ , col_names])),
              c(),
              c())
  
  net[[ 2 ]] <- cbind(1, s(net[[ 1 ]] %*% object$weights[[ 1 ]]))
  
  net[[ 3 ]] <- s(net[[ 2 ]] %*% object$weights[[ 2 ]])
  
  result <- net[[ 3 ]]
  
  colnames(result) <- colnames(object$fitted_values)
  
  result
}

pred <- predict(result, iris)

table(y, apply(pred, 1, function(x) names(x)[ which.max(x) ]))
