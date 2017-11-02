rm(list = ls())

FitKnn <- function(y, x, k, weighted = FALSE){
  
  # format y as a n X c binary matrix
  Y <- data.frame(y = y)
  
  Y <- model.matrix(~y, Y)
  
  colnames(Y) <- levels(y)
  
  Y[ , 1 ] <- as.numeric(rowSums(Y[ , -1 ]) == 0) # fix weird formatting issue
  
  ### Because all calculation is actually done in the prediction ----
  # Call the predict method to fill in the "fitted" slot in the result
  # This also avoids having the same code in the predict method as the 
  # fit function
  
  result <- list(fitted = c(), 
                 training_points = x,
                 training_classes = Y,
                 k = k,
                 weighted = weighted)
  
  class(result) <- "kNNClassifier"
  
  result$fitted <- predict(object = result, newdata = x)
  
  result # return this value
}

predict.kNNClassifier <- function(object, newdata, votes = TRUE) {
  
  if (class(object) != "kNNClassifier")
    stop("object must be of class kNNClassifier")
  
  # get distance from all points in newdata to points in training data
  d <- suppressWarnings({
    pdist::pdist(X = newdata[ , colnames(object$training_points) ], 
                 Y = object$training_points)
  })
  
  d <- as.matrix(d)
  
  # get variables out of our object
  k <- object$k
  Y <- object$training_classes
  weighted <- object$weighted
  
  ### two cases: k is infinite (use all points) or use only a subset ----
  
  if (is.infinite(k)) {
    
    if(weighted){
      
      weights <- 1 / (d ^ 2 + 1)
      
    } else {
      
      weights <- matrix(1, nrow = nrow(d), ncol = ncol(d))
      
      rownames(weights) <- rownames(d) 
      colnames(weights) <- colnames(d)
      
    }
    
    predictions <- weights %*% Y
    
    predictions <- predictions / rowSums(predictions)
    
  } else {
    # make our predictions
    predictions <- apply(d, 1, function(z){
      
      # get a list of candidates by returning indicies of points within k-NN
      i <- which(z <= sort(z)[ k ])
      
      # get the class values of those points
      yhat <- Y[ i , ]
      
      # vote, either weighted or unweighted
      if ( weighted ) {
        w <- 1 / (z[ i ] ^ 2 + 1)
        
        yhat <- w %*% yhat
        
        yhat <- yhat / sum(yhat)
        
      } else {
        
        yhat <- colSums(yhat) / sum(yhat)
        
      }
      
      yhat # return this value
    })
    
    predictions <- t(predictions)
  }
  
  if (! votes) {
    predictions <- apply(predictions, 1, function(x) names(x)[ which.max(x) ][ 1 ])
  }
  
  predictions # return this value
}



data(iris)

model <- FitKnn(y = iris$Species, x = iris[ , 1:4],
                k = 5, weighted = TRUE)

pred <- predict(model, iris[ sample(seq_len(nrow(iris)), 100), 1:4 ], votes = TRUE)

model2 <- FitKnn(y = iris$Species, x = iris[ , 1:4],
                k = Inf, weighted = TRUE)

pred2 <- predict(model2, iris[ sample(seq_len(nrow(iris)), 100), 1:4 ], votes = TRUE)

model3 <- FitKnn(y = iris$Species, x = iris[ , 1:4],
                 k = 5, weighted = FALSE)

pred3 <- predict(model3, iris[ sample(seq_len(nrow(iris)), 100), 1:4 ], votes = TRUE)

model4 <- FitKnn(y = iris$Species, x = iris[ , 1:4],
                 k = Inf, weighted = FALSE)

pred4 <- predict(model4, iris[ sample(seq_len(nrow(iris)), 100), 1:4 ], votes = TRUE)



