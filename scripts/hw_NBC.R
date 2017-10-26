
### prepare the workspace ------------------------------------------------------

# make sure my workspace is clean
rm(list = ls()) 

# make sure any necessary packages are installed
packages <- c("stringr")

lapply(packages, function(p){
  if( ! p %in% installed.packages()[ , 1 ])
    install.packages(p)
})

### load and clean the training data -------------------------------------------

# unzip the training and testing data
unzip("data_raw/data.zip", exdir = "data_raw")

# declare a "preparation" function

Prepare <- function(filepath){
  cat(filepath, "\n")
  
  # read the data in as text, row-by-row
  data <- scan(filepath, what = "character", sep = "\n")
  
  # reformat into a numeric matrix
  data <- stringr::str_split(string = data, pattern = " ")
  
  data <- do.call(rbind, lapply(data, as.numeric))
  
  # return the numeric matrix
  data
}

# iteratively load each file and prepare it
file_paths <- list.files(path = "data_raw", 
                         pattern = "^train[0-9]+\\.txt$", 
                         full.names = TRUE)

training_data <- parallel::mclapply(X = file_paths, 
                                    FUN = Prepare, 
                                    mc.cores = min(4, parallel::detectCores(), na.rm = TRUE))
# combine into a single matrix
training_data <- do.call(rbind, training_data)

# split off Y (outcome) and X (features) and further format for training
Prepare2 <- function(data) {
  
  y <- factor(data[ , 1 ])
  
  x <- data.frame(data[ , -1 ],
                  stringsAsFactors = FALSE)
  
  x <- as.data.frame(lapply(x, as.numeric))
  
  # this normalizes the rows, though we don't need that for naive bayes
  # I'm leaving it here because it was in the instruction sheet...
  x <- x / rowSums(x) 
  
  # make x binary to ease computation for naive bayes
  x <- apply(x, 2, function(x) as.numeric(x > 0))
  
  x <- as.data.frame(x)
  
  x <- as.data.frame(lapply(x, as.factor))
  
  # return a list with y and x
  list(y = y, x = x)
  
}

training_data <- Prepare2(data = training_data)


### declare functions for learning and predicting ------------------------------
FitNaiveBayes <- function(Y, X){
  
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
  # this is esentially a set of nested loops, with the outer loop running in 
  # parallel
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


### Train a Naive Bayes classifier ---------------------------------------------
model <- FitNaiveBayes(Y = training_data$y, X = training_data$x)

### load and clean the test data -----------------------------------------------
file_paths <- list.files("data_raw", pattern = "^test[0-9]+.txt$", 
                         full.names = TRUE)

test_data <- parallel::mclapply(X = file_paths,
                                FUN = Prepare,
                                mc.cores = min(4, parallel::detectCores(), na.rm = TRUE))

test_data <- do.call(rbind, test_data)

test_data <- Prepare2(data = test_data)

### get predictions on the test data set ---------------------------------------
test_predictions <- predict(object = model,
                            newdata = test_data$x,
                            prob = FALSE)

### calculate error rates and confidence intervals for each class --------------
err <- sum(test_predictions != as.character(test_data$y), na.rm = TRUE)

err <- err / length(test_predictions)

se <- 1.96 * sqrt(err * (1 - err) / length(test_predictions))

conf <- c(err - se, err + se)

