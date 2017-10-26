rm(list = ls())

library(RWeka)

### 3.2
d <- data.frame(y = c("+", "+", "-", "+", "-", "-"), 
                a1 = c(T, T, T, F, F, F), 
                a2 = c(T, T, F, F, T, T))

round(InfoGainAttributeEval(y ~ ., d), 4)

### 3.4
es <- data.frame(sky = c("sunny", "sunny", "rainy", "sunny"),
                 airtemp = c("warm", "warm", "cold", "warm"),
                 humidity = c("normal", "high", "high", "high"),
                 wind = c("strong", "strong", "strong", "strong"),
                 water = c("warm", "warm", "warm", "cool"),
                 forecast = c("same" , "same", "change", "change"),
                 enjoysport = c("yes", "yes", "no", "yes"))

round(InfoGainAttributeEval(enjoysport ~ ., es), 4)

round(InfoGainAttributeEval(enjoysport ~ ., es[ es$sky == "sunny" , ]), 4)



es2 <- data.frame(sky = c("sunny", "sunny", "rainy", "sunny", "sunny"),
                  airtemp = c("warm", "warm", "cold", "warm", "warm"),
                  humidity = c("normal", "high", "high", "high", "normal"),
                  wind = c("strong", "strong", "strong", "strong", "weak"),
                  water = c("warm", "warm", "warm", "cool", "warm"),
                  forecast = c("same" , "same", "change", "change", "same"),
                  enjoysport = c("yes", "yes", "no", "yes", "no"))

round(InfoGainAttributeEval(enjoysport ~ ., es), 4)

round(InfoGainAttributeEval(enjoysport ~ ., es[ es$sky == "sunny" , ]), 4)

round(InfoGainAttributeEval(enjoysport ~ ., es[ es$sky == "sunny" & es$wind == "strong", ]), 4)


