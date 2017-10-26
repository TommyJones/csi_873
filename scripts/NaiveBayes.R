FitNaiveBayes <- function(Y, X){
  
  ### check consistency of inputs -----
  # is Y a character or factor?
  # is each column of X a character or factor?
  # do either x or y have any missing values?
  
  # remove any x features that have only one level
  num_levels <- sapply(X, function(x) length(levels(x)))
  
  X <- X[ , num_levels > 1 ]
  
  ### get probabilities of outcomes of y ------
  py <- lapply(levels(Y), function(y){
    sum(Y == y) / length(Y)
  })
  
  names(py) <- levels(Y)
  
  ### get probabilities of outcmes of x, given outcomes of y ----
  
  # subset the data on each level of y...
  px <- by(data = X, INDICES = Y, function(x){
    
    # for each column of x...
    result <- lapply(x, function(V){
      
      # for each level of x, get its probability conditional on Y = y
      out <- sapply(levels(V), function(v){
        sum(V == v) / length(V)
      })
      
      names(out) <- levels(V)
      
      out # return this value
    })
    
    result # return this value
  })
  
  model <- list(py = py, px = px)
  
  class(model) <- "NaiveBayesClassifier"
  
  model # return this value
}

predict.NaiveBayesClassifier <- function(object, newdata, prob = FALSE) {
  
  ### check consistency of inputs ------
  if(class(object) != "NaiveBayesClassifier"){
    stop("object must be of class NaiveBayesClassifier")
  }
  
  if(class(newdata) != "data.frame") {
    stop("newdata must be of class data.frame")
  }
  
  
  xnames <- intersect(names(newdata),
                      names(object$px[[ 1 ]]))
  
  
  ### Iterate over model objects and data to get predictions ----
  result <- parallel::mclapply(object$px, function(ylev){
    xprobs <- mapply(function(feature, probs){
      result <- sapply(levels(feature), function(l){
        prob_out <- rep(0, length(feature))
        prob_out[ feature == l ] <- probs[ l ]
        prob_out
      })
      result <- rowSums(result)
    }, feature = newdata[ , xnames ], probs = ylev[ xnames ],
    SIMPLIFY = FALSE)
    
    xprobs <- do.call(cbind, xprobs)
    xprobs <- apply(xprobs, 1, function(row) Reduce("*", row))
  }, mc.cores = min(c(length(object$px), 4, parallel::detectCores()), na.rm = TRUE))
  
  result <- mapply(FUN = function(xprob, yprob){
    xprob * yprob
  }, xprob = result[ names(object$py) ], yprob = object$py,
  SIMPLIFY = FALSE)
  
  result <- do.call(cbind, result)
  
  result <- result / rowSums(result)
  
  ### return either probabilities of each class or predicted class ----
  if (prob) {
    return(result)
  } else {
    return(apply(result, 1, function(row){
      result <- names(row)[ which.max(row) ]
      if(length(result) == 0) # if for some reason we don't get a prediction...
        return(NA) # return a missing value
      
      result
    }))
  }
  
}