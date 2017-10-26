
rm(list = ls())

FitLogGd <- function(y, x, learning_rate = 0.1, momentum = 0, max_iters = 10000,
                     training_prop = 0.8){
  
  # give x an intercept term
  X <- cbind(1, as.matrix(x))
  
  colnames(X) <- c("intercept", colnames(x))
  
  # split data into test and training
  test_rows <- sample(seq_len(nrow(x)), size = round((1 - training_prop)* nrow(x)))
  
  X_test <- X[ test_rows , ]
  y_test <- y[ test_rows ]
  
  X <- X[ -test_rows , ]
  y <- y[ -test_rows ]
  
  # initialize the weights
  weights <- matrix(rnorm(n = ncol(X), mean = 0, sd = 0.01), ncol = 1)
  
  # sigmoid activation function
  s <- function(y) 1 / (1 + exp(-y))
  
  # initialize network
  net <- s(X %*% weights)
  
  # squared error cost function
  cost <- function(net, y) {
    0.5 * sum( (net - y)^2 )
  }
  
  # keep history
  cost_history <- double(max_iters)
  weights_history <- matrix(0, nrow = max_iters, ncol = ncol(X))
  colnames(weights_history) <- colnames(X)
  weights_history[ 1 , ] <- t(weights)
  weight_ch_history <- double(max_iters)

  # gradient descent
  # stoping criteria is that the total absolute change in weights is < 0.0001 
  # or the max number of iterations
  weight_ch <- 100
  iter <- 1
  delta <- 0
  
  while ( weight_ch > 0.0001 & iter <= max_iters){
    
    # get the new network
    net <- s(X %*% weights)
    
    # get the error and weight update rule
    error <- (net - y)
  
    delta_last <- delta
    
    delta <- t(X) %*% error / length(y)
    
    weights <- weights - learning_rate * delta 
    
    # record the current value of the cost function
    cost_history[ iter ] <- cost(s(X_test %*% weights), y_test)
    
    # record the history of the weights
    weights_history[ iter + 1 , ] <- t(weights)
    weight_ch <- abs(sum(weights_history[ iter + 1,  ] - 
                           weights_history[iter , ]))
    weight_ch_history[ iter + 1 ] <- weight_ch
    iter <- iter + 1
  }

  return(list(weights = weights, cost_history = cost_history[ seq_len(iter) ], 
         weights_history = as.data.frame(weights_history[ seq_len(iter) , ]),
         weight_ch_history = weight_ch_history[ seq_len(iter) ]))

}


data(iris)
x <- iris[ , 1:4 ]
y <- as.numeric(iris$Species == "setosa")
# x <- runif(1000, -5, 5)
# y <- x + rnorm(1000) + 3

result <- FitLogGd(y = y, x = x, learning_rate = 0.1, momentum = 0.05, max_iters = 15000)


result$weights

plot(result$cost_history, type = "l")

par(new = TRUE)
plot(result$weights_history[[ 1 ]], type = "l",
     yaxt = "n", xaxt = "n", ylab = "", xlab = "",
     col = "cyan", ylim = range(do.call(c, result$weights_history)))

cols <- c("cyan", "red", "green", "blue", "magenta")

for(j in seq_len(ncol(result$weights_history) - 1))
  lines(result$weights_history[[ j + 1 ]], col = cols[ j + 1 ])

legend("bottomright", legend = colnames(result$weights_history),
       col = cols, lty = 1)