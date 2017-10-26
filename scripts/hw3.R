


### Declare functions for my update rules --------------------------------------
S <- function(x) 1 / (1 + exp(-1 * x))

chat <- function(wc0 , wc1, wc2, a, b)
  S(wc0 + wc1 * a + wc2 * b)

dhat <- function(wd0, wd1, chat)
  S(wd0 + wd1 * chat)

d_outer <- function(y, yhat, S1)
  S1 * (1 - S1) * (y - yhat)

d_inner <- function(S2, d_outer, w_outer)
  S2 * (1 - S2) * w_outer * d_outer

delta_w <- function(nu, delta, x, alpha, dw)
  nu * delta * x + alpha * dw

### Declare my data ------------------------------------------------------------
data <- data.frame(a = c(1,0), b = c(0,1), d = c(1, 0))

### Iterate --------------------------------------------------------------------

# initialize alpha and nu
alpha <- 0.9
nu <- 0.3

# initialize weights
wd0 <- 0.1
wd1 <- 0.1
wc0 <- 0.1
wc1 <- 0.1
wc2 <- 0.1

dwd0 <- 0
dwd1 <- 0
dwc0 <- 0
dwc1 <- 0
dwc2 <- 0

# step 1 - forward pass
ch <- chat(wc0 = wc0,
           wc1 = wc1,
           wc2 = wc2,
           a = data$a[ 1 ],
           b = data$b[ 1 ])

dh <- dhat(wd0 = wd0,
           wd1 = wd1,
           chat = ch)

do <- d_outer(y = data$d[ 1 ], 
              yhat = dh,
              S1 = dh)

# wd0
dwd0 <- delta_w(nu = nu, alpha = alpha,
               x = 1, # because it's a constant
               delta = do,
               dw = dwd0)

wd0 <- wd0 + dwd0 

# wd1
dwd1 <- delta_w(nu = nu, alpha = alpha,
                x = ch,
                delta = do,
                dw = dwd0)

wd1 <- wd1 + dwd1

# wc0
di <- d_inner(S2 = ch,
              d_outer = do,
              w_outer = wd1)

dwc0 <- delta_w(nu = nu, alpha = alpha,
                x = 1, # because it's a constant,
                delta = di,
                dw = dwc0)

wc0 <- wc0 + dwc0

# wc1
dwc1 <- delta_w(nu = nu, alpha = alpha,
                x = data$a[ 1 ], 
                delta = di,
                dw = dwc1)

wc1 <- wc1 + dwc1

# wc2
dwc2 <- delta_w(nu = nu, alpha = alpha,
                x = data$b[ 1 ], # because it's a constant,
                delta = di,
                dw = dwc2)

wc2 <- wc2 + dwc2
