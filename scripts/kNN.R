### prepare the workspace ------------------------------------------------------

# make sure my workspace is clean
rm(list = ls()) 

# make sure any necessary packages are installed
packages <- c("stringr", "pdist", "Rcpp")

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
  
  # this normalizes the rows
  x <- x / rowSums(x) 
  
  x <- as.data.frame(x)
  
  # return a list with y and x
  list(y = y, x = x)
  
}

training_data <- Prepare2(data = training_data)

### declare functions for learning and predicting ------------------------------

### This function calculates distance between the rows of two matrices
# I wrote it in C++ and compile it here. C++ code is at the bottom of this report
Rcpp::sourceCpp("scripts/DistMat2Mat.cpp")

CalcDist <- function(x, y){
  # runs in parallel on lunux/unix systems with 4 cpus
  
  x <- as.matrix(x)
  y <- as.matrix(y)
  
  b_size <- round(nrow(x) / 4)
  
  batches <- seq(1, nrow(x), by = b_size)
  
  batches <- lapply(batches[ 1:4 ], function(b){
    x[ b:min(b + b_size - 1, nrow(x)) , ]
  })
  
  checksum <- sum(sapply(batches, nrow))
  
  if (checksum < nrow(x))
    batches[[ length(batches) ]] <- rbind(batches[[ length(batches) ]],
                                          x[ (checksum + 1):nrow(x) , ])
  
  result <- parallel::mclapply(batches, function(z){
    DistMat2Mat(A = z, B = y)
  }, mc.cores = 4)
  
  result <- do.call(rbind, result)
  
  result # return this 
}

### This function trains a KNN classifier
FitKnn <- function(y, x){
  
  # format y as a n X c binary matrix
  Y <- data.frame(y = y)
  
  Y <- model.matrix(~y, Y)
  
  colnames(Y) <- levels(y)
  
  Y[ , 1 ] <- as.numeric(rowSums(Y[ , -1 ]) == 0) # fix weird formatting issue
  
  # remove columns from x where there is less than 1 % of observations with nonzero values
  x <- x[ , colSums(x > 0) > nrow(x) / 100 ]
  
  ### Because all calculation is actually done in the prediction ----
  # just assemble the training points and class in a list
  
  result <- list(training_points = x,
                 training_classes = Y)
  
  class(result) <- "kNNClassifier"
  
  result # return this value
}

# this declares a predict method for the object returned from the above function
predict.kNNClassifier <- function(object, newdata, K, 
                                  training = FALSE) {
  
  if (class(object) != "kNNClassifier")
    stop("object must be of class kNNClassifier")
  
  # get distance from all points in newdata to points in training data
  if (training) { # if this is the special case of in-sample training of the model
    
    d <- dist(newdata)
    
  } else {
    
    d <- CalcDist(x = newdata[ , colnames(object$training_points) ],
                  y = object$training_points)
    
  }
  
  
  # get variables out of our object
  Y <- object$training_classes
  
  # do the calculations for all k specified, weighted and unweighted
  result <- lapply(K, function(k){
    out <- c(weighted = TRUE, unweighted = FALSE)
    out <- lapply(out, function(weighted){
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
        
        predictions  # return this value
        
      } else {
        # make our predictions
        predictions <- apply(d, 1, function(z){
          
          # get a list of candidates by returning indicies of points within k-NN
          i <- which(z <= sort(z)[ k ])
          
          # get the class values of those points
          yhat <- Y[ i , ]
          
          if (! is.matrix(yhat)) 
            yhat <- matrix(yhat, ncol = ncol(Y))
          
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
        
        colnames(predictions) <- colnames(Y)
        
        predictions # return this value
        
      }
    })
    
    out # return this value
  })
  
  names(result) <- as.character(K)
  
  result # return this value
}

### load and clean the test data -----------------------------------------------
file_paths <- list.files("data_raw", pattern = "^test[0-9]+.txt$", 
                         full.names = TRUE)

test_data <- parallel::mclapply(X = file_paths,
                                FUN = Prepare,
                                mc.cores = min(4, parallel::detectCores(), na.rm = TRUE))

test_data <- do.call(rbind, test_data)

test_data <- Prepare2(data = test_data)

### Iterate over $k = \{1, 2, ..., 7, \infty\}$ ----
# train and test

# Take a sample of 20,000 instances for training the networks
training_sample <- sample(seq_along(training_data$y), 20000)

model <- FitKnn(y = training_data$y[ training_sample ], 
                x = training_data$x[ training_sample , ])

pred <- predict(object = model, newdata = test_data$x,
                K = c(1:7, Inf))

### Calculate error rates ----

# go from probabilities to hard assignments
assignments <- parallel::mclapply(pred, function(p){
  out <- lapply(p, function(x){
    apply(x, 1, function(y) names(y)[ which.max(y) ][ 1 ])
  })
}, mc.cores = 4)

# calculate error rates
errors <- lapply(assignments, function(a){
  sapply(a, function(x){
    sum(x != test_data$y, na.rm = TRUE) / length(x)
  })
})

errors <- do.call(rbind, errors)

save(errors, assignments, pred, model, file = "kNN_out.RData")