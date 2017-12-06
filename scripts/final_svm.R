################################################################################
# This script runs the final exam stuff
################################################################################


### 1. Workplace setup ---------------------------------------------------------


rm(list = ls())

### 2. Load and pre-format data (can subset for specificts later) --------------

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


### 3. Decalre functions for training and predicting SVM -----------------------
FitSvm <- function(y, x, C = 200) {
  
  ### format inputs ----
  
  # make y in {-1, 1}
  y <- ifelse(test = y > 0, yes = 1, no = -1)
  
  # ensure x is a matrix, not a data.frame
  x <- as.matrix(x)
  
  # get some constants
  p <- ncol(x) # number of predictors/features
  
  N <- nrow(x) # number of instances
  
  ### declare radial basis function ----
  rbf <- function(x, gamma) {
    exp(-gamma * as.matrix(dist(x))^2)
  }
  
  ### prepare matrices for quadratic optimization ----
  
  H <- y %*% t(y) * rbf(x, 0.05)
  
  c_vec <- rep(1, N)
  
  l <- rep(1, N)
  
  u <- rep(C, N)
  
  A <- y
  
  b <- 0
  
  r <- 0
  
  ### Get the solution using kernlab's quadratic solver ----
  sol <- kernlab::ipop(c = c_vec,
                       H = H,
                       A = A,
                       b = b,
                       l = l,
                       u = u,
                       r = r)
  
  ### Get the beta coefficients from the solution to the dual ----
  x_inv <- MASS::ginv(x)
  
  beta <- colSums(dual(sol) * y * rbf(x, 0.05)) %*% t(x_inv)
  
  yhat <- x %*% t(beta)
  
  beta0 <- mean(y - yhat, na.rm = TRUE)
  
  ### Use logistic regression to convert fitted values to probabilities ----
  # Note: this also means I don't have to solve for the intercep as 
  # this will be de-biased in the regression
  d <- data.frame(y = y > 0, 
                  yhat = yhat,
                  stringsAsFactors = FALSE)
  
  fit <- glm(y ~ yhat, data = d, family = binomial("logit"))
  
  ### Prepare result for export ----
  
  result <- list(b = beta0,
                 w = as.vector(beta),
                 fitted_values = fit$fitted.values,
                 logit_model = fit)
  
  class(result) <- "SVM"
  
  result
}

predict.SVM <- function(object, newdata, prob = FALSE){
  
  ### Check/format inputs ----
  # make sure object is of the right class
  if(class(object) != "SVM")
    stop("object must be of class SVM")
  
  # make sure newdata is a matrix
  newdata <- as.matrix(newdata)
  
  ### Get predictions ----
  # biased
  yhat_biased <- object$b + newdata %*% object$w
  
  # unbiased
  if (prob) {
    d <- data.frame(yhat = yhat_biased)
    
    yhat <- predict(object$logit_model, d, "response")
  } else {
    yhat <- sign(yhat_biased)
  }
  
  return(yhat)
}

### 4. Classify 3's vs 6's -----------------------------------------------------

# pull out all 3's and 6's
all_36 <- data.frame(y = training_data$y[ training_data$y %in% c("3", "6") ],
                 training_data$x[ training_data$y %in% c("3", "6") , ],
                 stringsAsFactors = FALSE)

# pull 500 records for training, stratify by 3 or 6
samp <- by(all_36, INDICES = all_36$y,
               function(x){
                 sample(rownames(x), 250)
               })

samp <- do.call(c, samp)

train_36 <- all_36[ samp , ]

test_36 <- all_36[ setdiff(rownames(all_36), samp) , ]

# train an svm model
svm_36 <- FitSvm(y = train_36$y == "3", 
                 x = train_36[ , -1 ],
                 C = 200)

# get predictions
pred_36 <- predict(object = svm_36, newdata = test_36[ , -1 ],
                   prob = TRUE)

# calculate error
error_36 <- table(test_36$y == "3", pred_36 >= 0.5)

error_36 <- 1 - sum(diag(error_36)) / sum(error_36)

### 5. Reduce original number of pixels by 50%, 75%, 90%, 95% ------------------
perc <- c(0.5, 0.25, 0.1, 0.05)
names(perc) <- perc

error_low_pix <- parallel::mclapply(perc, function(p){
  cols <- sample(2:ncol(train_36),
                 round(p * ncol(train_36[ , -1 ])))
  
  fit <- FitSvm(y = train_36$y == "3",
                x = train_36[ , cols ],
                C = 200)
  
  pred <- predict(fit, test_36[ , cols ], prob = TRUE)
  
  err <- table(pred >= 0.5 , test_36$y == "3")
  
  err <- 1 - sum(diag(err)) / sum(err)
}, mc.cores = 4)

### 6. run SVD to reduce dimensions by 50%, 75%, 90%, 95% ----------------------

# Declare a function to get SVD
# An LSA model is just an SVD of a document term matrix, so this still works
CalcSvd <- function(x, k){
  out <- textmineR::FitLsaModel(dtm = as.matrix(x), k = k)
  
  out <- list(alpha = out$theta,
              gamma = MASS::ginv(diag(out$sv) %*% out$phi))
}

# Get a prediction function to include rescaling test data based on SVD
Predict <- function(x, gamma, svm){
  
  x_new <- as.matrix(x) %*% gamma
  
  predict(svm, x_new, prob = TRUE)
  
}

# get svd and svm for each percent
perc <- c(0.5, 0.25, 0.1, 0.05)
names(perc) <- perc


error_svd_36 <- parallel::mclapply(perc, function(p){
  svd <- CalcSvd(x = train_36[ , -1 ], k = round(p * ncol(train_36[ , -1 ])))
  
  fit <- FitSvm(y = train_36$y == "3", 
                x = svd$alpha)
  
  pred <- Predict(x = test_36[ , -1 ],
                  gamma = svd$gamma,
                  svm = fit)
  
  err <- table(pred >= 0.5, test_36$y == "3")
  
  err <- 1 - sum(diag(err)) / sum(err)
  
  err
}, mc.cores = 4)

### 7. Reduce number of observations by 50%, 75%, 90%, 95% ---------------------

perc <- c(0.5, 0.25, 0.1, 0.05)
names(perc) <- perc


error_lower_obs <- parallel::mclapply(perc, function(x){
  train <- train_36[ sample(1:nrow(train_36), round(nrow(train_36) * x)) , ]
  
  fit <- FitSvm(y = train$y == "3",
                x = train[ , -1 ],
                C = 200)
  
  pred <- predict(fit, newdata = test_36[ , -1 ])
  
  err <- table(test_36$y == "3", pred >= 0.5)
  
  err <- 1 - sum(diag(err)) / sum(err)
}, mc.cores = 4)

# remove temporary tables to save memory
rm(all_36, test_36, train_36)
gc()

### 8. Classify evens vs. odds -------------------------------------------------

# code everything as even or odd
even_odd <- data.frame(y = training_data$y %in% c(0,2,4,6,8),
                       training_data$x,
                       stringsAsFactors = FALSE)

# pull 1000 records for training, stratify by even or odd
samp <- by(even_odd, INDICES = training_data$y,
           function(x){
             sample(rownames(x), 100)
           })

samp <- do.call(c, samp)

train_even_odd <- even_odd[ samp , ]

test_even_odd <- even_odd[ setdiff(rownames(even_odd), samp) , ]

# train an svm model
svm_even_odd <- FitSvm(y = train_even_odd$y, 
                 x = train_even_odd[ , -1 ],
                 C = 200)

# get predictions
pred_even_odd <- predict(object = svm_even_odd, newdata = test_even_odd[ , -1 ],
                   prob = TRUE)

# calculate error
error_even_odd <- table(test_even_odd$y, pred_even_odd >= 0.5)

error_even_odd <- 1 - sum(diag(error_even_odd)) / sum(error_even_odd)


### 9. Build 10 SVMs to detect all digits --------------------------------------

# big ol' loop - get training data for each digit, fit a model, return model
digits <- as.character(0:9)
names(digits) <- digits

models <- parallel::mclapply(digits, function(x){
  d <- data.frame(y = training_data$y == x, 
                  training_data$x)
  
  samp <- by(d, INDICES = d$y, function(x){
    sample(rownames(x), 250)
  })
  
  samp <- do.call(c, samp)
  
  training <- d[ samp , ]
  
  fit <- FitSvm(y = training$y,
                x = training[ , -1 ],
                C = 200)
  
  fit
}, mc.cores = 4)

# load test data to ensure we don't recycle any observations
file_paths <- list.files("data_raw", pattern = "^test[0-9]+.txt$", 
                         full.names = TRUE)

test_data <- parallel::mclapply(X = file_paths,
                                FUN = Prepare,
                                mc.cores = min(4, parallel::detectCores(), na.rm = TRUE))

test_data <- do.call(rbind, test_data)

test_data <- Prepare2(data = test_data)

# get predictions
predictions <- parallel::mclapply(models, function(model){
  p <- predict(model, test_data$x, prob = TRUE)
}, mc.cores = 4)

predictions <- do.call(cbind, predictions)

assignments <- apply(predictions, 1, function(x){
  names(x)[ which.max(x) ][ 1 ]
})

error_all <- table(assignments, test_data$y)

error_all <- 1 - sum(diag(error_all)) / sum(error_all)

### 10. Compare results to NB, ANN, and KNN -------------------------------------

# first, let's save our results
save(list = grep("error_", ls(), value = TRUE),
     file = "svm_errors.RData")

# get 95% confidence interval for total SVM error
conf <- c(error_all - 1.96 * sqrt(error_all * (1 - error_all) / 20000),
          error_all + 1.96 * sqrt(error_all * (1 - error_all) / 20000))

# hand-coding results from my previous experiments
comp <- data.frame(model = c("NB", "ANN", "kNN", "SVM"),
                   error = c(.159, .141, .036, error_all),
                   conf_low = c(.152, .134, .034, conf[ 1 ]),
                   conf_high = c(.166, .148, .039, conf[ 2 ]),
                   stringsAsFactors = FALSE)


