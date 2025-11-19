# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# AUXILIARY FUNCTIONS 
# - functions to be used in the main functions
# - but that not need documentation 
# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
Vec = function(X){
  vecx = matrix(X, ncol = 1)
  return(vecx)
}

# ---------------------------------------------------------------------------------
myrecode = function(x, old, new){
  C = length(old)
  z = x
  for(c in 1:C){
    z = ifelse(x == old[c], new[c], z)
  }
  return(z)
}



#----------------------------------------------------------------------------------
expected.p = function(y, theta, m){
  # computes expected value of p (E(p))
  # y: ordinal response variable
  # theta: current ``(bi)-linear predictor''
  # m: current thresholds
  tau = c(-Inf, m)
  # y.adjusted: drop missing levels and reorder accordingly
  y.adj = as.numeric(factor(rank(y)))
  ymax = max(y.adj)
  y = y.adj + 1
  ymin1 = y.adj
  # compute expected value of p
  p = ifelse(y.adj == 1, exp(2*tau[y] - 2*theta) / (2 * ((exp(tau[y]- theta) + 1)^2))/FF(tau[y] - theta),
             ifelse(y.adj == ymax,(2 * exp(tau[ymin1] - theta) + 1)/(2 * ((exp(tau[ymin1]- theta) + 1)^2))/(1 -  FF(tau[ymin1] - theta)),
                    ((2 * exp(tau[ymin1] - theta) + 1)/(2 * ((exp(tau[ymin1] - theta) + 1)^2)) - (2 * exp(tau[y] - theta) + 1)/
                       (2 * ((exp(tau[y] - theta) + 1)^2)))/(FF(tau[y] - theta) - FF(tau[ymin1] - theta))
             ))
  return(p)
}

# ---------------------------------------------------------------------------------
FF = function(x){1/(1 + exp(-x))}


# Categorize function for having 5 levels

categorize = function(x, ncat){
  if(ncat == 2){
    breaks = quantile(x, probs = runif(1, 0.3, 0.7))
    xx = ifelse(x < breaks[1], 1, 2)
  }
  else if(ncat == 3){
    breaks = quantile(x, probs = c(runif(1, 0.1, 0.4), runif(1, 0.6, 0.9)))
    xx = ifelse(x < breaks[1], 1, 
                ifelse(x < breaks[2], 2, 3))
  }
  else if(ncat == 4){
    breaks = quantile(x, probs = c(runif(1, 0.1, 0.3), runif(1, 0.4, 0.6), runif(1, 0.7, 0.9)))
    xx = ifelse(x < breaks[1], 1, 
                ifelse(x < breaks[2], 2, 
                       ifelse(x < breaks[3], 3, 4)))
  }
  else if(ncat == 5) {
    breaks = quantile(x, probs = c(runif(1, 0.1, 0.3), runif(1, 0.4, 0.5), 
                                   runif(1, 0.6, 0.7), runif(1, 0.8, 0.9)))
    xx = ifelse(x < breaks[1], 1,
                ifelse(x < breaks[2], 2,
                       ifelse(x < breaks[3], 3,
                              ifelse(x < breaks[4], 4, 5))))
  }
  return(xx)
}
#----------------------------------------------------------------------------



categorize = function(x, ncat){
  mybreaks = quantile(x, probs = seq(0, 1, length = (ncat+1)))
  # xx gives the ordered scores, 1, 2, 3
  xx = cut(x, breaks = mybreaks, labels = FALSE, include.lowest = T)
  # breaks = breaks[-1]
  # xx = rep(NA, length(x))
  # for(c in 2:length(breaks)){
  #   if((breaks[c-1] <= x) & (x < breaks[c])){xx = c-1}
  # }
  # xxx gives the averages of x per category of xx
  xxx = xx
  for(c in 1:ncat) xxx[xx == c] = mean(x[xx == c])
  output = list(xo = xx, xa = xxx)
  return(output)
}


#---

# This function iterates through each column of the model's coefficient matrix and
# collects the names of predictors whose absolute coefficients exceed a set threshold, 
# grouping them by dimension:

significantP <- function(gmr4.final.model, threshold = 0.001){
  
  coefficients <- gmr4.final.model$B
  if (is.null(rownames(coefficients))){
    rownames(coefficients) <- paste0("X", 1:nrow(coefficients))
  }
  
  interesting_predictors <- list()
  
  
  for (dim in seq_len(ncol(coefficients))){
    dim_coefs <- coefficients[, dim]
    idx_interesting <- which(abs(dim_coefs) > threshold)
    interesting_predictors[[paste0("Dim", dim)]] <- rownames(coefficients)[idx_interesting]
  }
  
  return(interesting_predictors)
}

