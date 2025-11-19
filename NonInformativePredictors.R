# Generating Uninformative Predictors/Noise

generateOrthogonalNoise <- function(Y, nPredictors, sd = 0.15) {
  N <- nrow(Y)
  noise <- matrix(rnorm(N * nPredictors, mean = 0, sd = sd), ncol = nPredictors)
  df <- as.data.frame(Y)
  
  for (j in 1:nPredictors) {
    fit <- lm(noise[, j] ~ ., data = df)
    noise[, j] <- residuals(fit)
    noise[, j] <- scale(noise[, j], center = TRUE, scale = FALSE)
    noise[, j] <- noise[, j] * (sd / sd(noise[, j]))
  }
  
  return(noise)
}


generateOrthogonalOrdinalData <- function(Y, nPredictors, sd = 0.15) {
  N <- nrow(Y)
  ordinalMatrix <- matrix(nrow = N, ncol = nPredictors)
  df <- as.data.frame(Y)
  
  for (j in 1:nPredictors) {
    ndata <- rnorm(N, mean = 0, sd = sd)
    fit <- lm(ndata ~ ., data = df)
    ndata_resid <- residuals(fit)
    ndata_resid <- scale(ndata_resid, center = TRUE, scale = FALSE)
    ndata_resid <- ndata_resid * (sd / sd(ndata_resid))
    
    thresholds <- quantile(ndata_resid, c(0.25, 0.5, 0.75))
    ordinalMatrix[, j] <- as.numeric(cut(ndata_resid, breaks = c(-Inf, thresholds, Inf), labels = FALSE))
  }
  
  return(ordinalMatrix)
}



generate_noise <- function(Y, n_num, n_ord, sd = 0.15) {
  X_num <- generateOrthogonalNoise(Y, n_num, sd)
  X_ord <- generateOrthogonalOrdinalData(Y, n_ord, sd)
  X_noise <- cbind(X_num, X_ord)
  return(X_noise)
}


