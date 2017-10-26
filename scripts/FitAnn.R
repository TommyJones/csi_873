



FitAnn <- function(Y, X, hidden_layers, nodes, learning_rate = 0.1, momentum = NULL) {
  
  ### Check consistency of inputs ----
  
  ### Declare activation function ----
  s <- function(y) 1 / (1 + exp(-y))
  ds <- function(y) s(y) * (1 - s(y))
  
  ### Format data (factors to model.matrix etc) ----
  # for now, assuming Y is a factor vector and X is a numeric matrix
  ynames <- levels(Y)
  
  Y <- data.frame(y = Y)
  
  Y <- model.matrix(~y, Y)
  
  colnames(Y) <- ynames
  
  # model.matrix always makes the first column all "1", fix this
  Y[ , 1 ] <- as.numeric(rowSums(Y[ , -1 ]) == 0)
  
  
  # each hidden layer has M nodes and M - 1 outputs
  
  X <- as.matrix(X)
  
  ### initialize the weights and network ----
  weights <- vector(mode = "list", length = hidden_layers + 1)
  
  # initialize with draws from a normal distribution
  weights[[ 1 ]] <- 
    do.call(cbind,
            lapply(1:(nodes), function(n) rnorm(n = ncol(X) + 1, mean = 0, sd = 0.05)))
  
  # the below loop only gets run if you have more than one hidden layer
  for (j in seq_along(weights)[ -c(1, length(weights)) ]) {
    weights[[ j ]] <- 
      do.call(cbind,
              lapply(1:(nodes), function(n) rnorm(n = nodes + 1, mean = 0, sd = 0.05)))
  }
  
  weights[[ length(weights) ]] <- 
    do.call(cbind,
            lapply(1:ncol(Y), function(n){
              rnorm(n = nodes + 1, mean = 0, sd = 0.05)
            }))
  
  # initialize the network
  net <- vector(mode = "list", length = hidden_layers + 2)
  
  net[[ 1 ]] <- cbind(const = rep(1, nrow(X)), X)
  
  for (j in seq_along(net)[ -1 ]) {
    
    net[[ j ]] <- cbind(1, s(net[[ j - 1 ]] %*% weights[[ j - 1 ]]))
    
  }
  
  # drop constant on output layer
  net[[ length(net) ]] <- net[[ length(net) ]][ , -1 ] 
  
  ### Initialize weight updates --- 
  
  # delta_y
  d_y <- net[[ length(net) ]] * (1 - net[[ length(net) ]]) * Y
  
  
  # (rowSums( (Y - net[[ length(net) ]]) * 
  #             net[[ l + 1 ]] * (1 - net[[ l + 1 ]]) * net[[ l ]] *
  #             cumprod(net[[ l:(length(net) - 1) ]])))
  
}

