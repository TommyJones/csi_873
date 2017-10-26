
rm(list = ls())

### Load and prepare the data --------------------------------------------------
d <- scan("data_raw/em/Jones.txt", what = "", sep = "\n")
d <- unlist(strsplit(d, " "))
d <- as.numeric(d)

### Run EM Algorithm -----------------------------------------------------------

# initialize random cluster centers by taking means of the largest/smallest
# halves of the data

mu_1 <- mean(sort(d, decreasing = TRUE)[ 1:(length(d) / 2) ])
mu_2 <- mean(sort(d)[ 1:length(d) / 2 ])

# initialize some variables used for looping

mu_diff <- 100

cost_history <- c()

step <- 0

# loop until convergence

while (mu_diff > 0.0001) {
  
  step <- step + 1
  
  # e step
  e_1 <- dnorm(d, mean = mu_1, sd = 1) / 
    (dnorm(d, mean = mu_1, sd = 1) + dnorm(d, mean = mu_2, sd = 1))
  
  e_2 <- dnorm(d, mean = mu_2, sd = 1) / 
    (dnorm(d, mean = mu_1, sd = 1) + dnorm(d, mean = mu_2, sd = 1))
  
  # update mu_last for comparison to assess convergence
  mu_last <- c(mu_1, mu_2)
  
  # m step
  mu_1 <- as.numeric((t(d) %*% e_1) / sum(e_1))
  
  mu_2 <- as.numeric((t(d) %*% e_2) / sum(e_2))
  
  # update mu_diff as sum of absolute differences in means between iterations
  mu_diff <- sum(abs(c(mu_1, mu_2) - mu_last))
  
  cost_history <- c(cost_history, mu_diff)
  
}

# Once converged, store result as a single object
result <- list(centers = c(mu_1 = mu_1, mu_2 = mu_2), # found means
               assignments = data.frame(d = d, # probabilistic assignments
                                        e_1 = e_1,
                                        e_2 = e_2,
                                        stringsAsFactors = FALSE),
               cost_history = cost_history) # convergence by iterations

# Save result
save(d, result, file = "hwk_em.RData")

# Histogram of clustering result with normal overlays
hist(d, breaks = 100, col = rgb(0,0,0,0.4), freq = FALSE,
     main = "EM Clustering Result",
     xlab = "Range of Data")
abline(v = result$centers, lwd = 2, col = c("blue", "red"))
lines(seq(-5,5,by = 0.05), 
      dnorm(seq(-5,5,by = 0.05), mean = result$centers[ 1 ], sd = 1), 
      col = "blue", lty = 2, lwd = 2)
lines(seq(-5,5,by = 0.05), 
      dnorm(seq(-5,5,by = 0.05), mean = result$centers[ 2 ], sd = 1), 
      col = "red", lty = 2, lwd = 2)
legend("topright", 
       legend = c(expression(paste(mu[ 1 ], "= 0.399")),
                  expression(paste(mu[ 2 ], "= -0.647"))),
       col = c("blue", "red"), lwd = 2)

# Plot of cost history for each iteration
plot(result$cost_history, type = "l", lwd = 3,
     col = "#66c2a5",
     main = "Sum of Absolute Differences \nin Means Between Iterations", 
     xlab = "Number of Iterations",
     ylab = "")

