
evaluateB <- function(B, nPred, nNoise, Xscale, threshold = 0.01) {
  is_selected <- apply(B, 1, function(row) any(abs(row) > threshold))
  true_idx <- 1:nPred 
  noise_idx <- (nPred + 1):(nPred + nNoise) 
  
  TP_global <- sum(is_selected[true_idx]) 
  FP_global <- sum(is_selected[noise_idx])   
  FN_global <- nPred - TP_global 
  total_selected_global <- TP_global + FP_global 
  
  FDR_global <- if (total_selected_global > 0) FP_global / total_selected_global else 0 
  TDR_global <- TP_global / nPred
  
  
  types <- c("N", "O", "C")
  FDR_by_type <- setNames(numeric(length(types)), paste0("FDR_", types))
  
  for (t in types) {
    idx_type <- which(Xscale == t)
    
    idx_true <- idx_type[idx_type <= nPred]
    idx_noise <- idx_type[idx_type > nPred]
    
    TP_t <- sum(is_selected[idx_true])
    FP_t <- sum(is_selected[idx_noise])
    total_selected_t <- TP_t + FP_t
    
    FDR_by_type[paste0("FDR_", t)] <- 
      if (!is.na(total_selected_t) && total_selected_t > 0) FP_t / total_selected_t else NA
  }
  
  return(c(
    list(
      FDR = FDR_global,
      TDR = TDR_global,
      TP = TP_global,
      FP = FP_global,
      FN = FN_global,
      NumSelected = sum(is_selected)
    ),
    as.list(FDR_by_type)
  ))
}




run_parallel_singlecomb <- function(
    sample_size = 250,
    noise_level = 50,
    response_size = 6,
    replications = 100,
    S = 2,
    K = 10,
    cutoff = 0.01
) {
  
  # Libraries
  library(parallel)
  library(doSNOW)
  library(foreach)
  library(MASS)
  library(nnet)
  library(splines2)
  library(monotone)
  library(haven)
  library(dplyr)
  
  # Forcing the recovering of class.ind
  if (!exists("class.ind")) {
    class.ind <- get("class.ind", envir = asNamespace("nnet"))
  }
  
  # Function for creating qmix based on the responses
  create_qmix <- function(R) {
    base <- floor(R / 3)
    rem <- R %% 3
    q.mix <- rep(base, 3)
    if (rem > 0) q.mix[1:rem] <- q.mix[1:rem] + 1
    return(q.mix)
  }
  
  # Updated find_lambda function
  find_lambda <- function(summary_df) {
    min_ape <- min(summary_df$APE, na.rm = TRUE)
    se_min <- summary_df$SE[which.min(summary_df$APE)]
    
    lambda_min <- summary_df$lambda[which.min(summary_df$APE)]
    lambda_1se <- max(summary_df$lambda[summary_df$APE <= min_ape + se_min])
    lambda_2se <- max(summary_df$lambda[summary_df$APE <= min_ape + 2 * se_min])
    lambda_3se <- max(summary_df$lambda[summary_df$APE <= min_ape + 3 * se_min])
    
    return(list(
      lambda_min = lambda_min,
      lambda_1se = lambda_1se,
      lambda_2se = lambda_2se,
      lambda_3se = lambda_3se
    ))
  }
  
  q.mix <- create_qmix(response_size)
  total_tasks <- replications
  pb <- txtProgressBar(min = 0, max = total_tasks, style = 3)
  opts <- list(progress = function(n) setTxtProgressBar(pb, n))
  
  # Parallel clusters
  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores, type = "SOCK")
  registerDoSNOW(cl)
  on.exit(stopCluster(cl))
  
  results <- foreach(sim = 1:replications, .combine = rbind,
                     .export = c("rrr.sim3b", "categorize", "generate_noise", "generateOrthogonalNoise", "generateOrthogonalOrdinalData",
                                 "xval.start", "summary.function",
                                "gmr4", "gmr4.start", "predict.gmr4", "class.ind", "Vec", "myrecode",
                                 "expected.p", "FF", "evaluateB"),
                     .packages = c("MASS", "nnet", "splines2", "monotone", "haven", "dplyr"),
                     .options.snow = opts) %dopar% {
                       
                       set.seed(100 + sim)
                       
                       # Simulating the data
                       sim_data <- rrr.sim3b(n = sample_size, p = 10, q.mix = q.mix, nrank = S,
                                             intercept = rep(0, sum(q.mix)), mis.prop = 0)
                       X_true <- sim_data$X
                       Y <- sim_data$Y
                       
                       # Splitting Y
                       idx_n <- 1:q.mix[1]
                       idx_b <- (max(idx_n) + 1):(max(idx_n) + q.mix[2])
                       idx_o <- (max(idx_b) + 1):(sum(q.mix))
                       Yn <- if (length(idx_n) > 0) Y[, idx_n, drop = FALSE] else NULL
                       Yb <- if (length(idx_b) > 0) Y[, idx_b, drop = FALSE] else NULL
                       Yo <- if (length(idx_o) > 0) Y[, idx_o, drop = FALSE] else NULL
                       
                       # Noise
                       X_noise <- generate_noise(Y, n_num = noise_level / 2, n_ord = noise_level / 2, sd = 0.15)
                       X <- cbind(X_true, X_noise)
                       Xscale <- c(rep("N", 5), rep("C", 2), rep("O", 3), rep("N", noise_level / 2), rep("O", noise_level / 2))
                       
                       # Cross-validation
                       cv_out <- xval.start(Yn, Yb, Yo, X = X, Xscale = Xscale, S = S, K = K, repeats = 1,
                                            lambda.lasso.seq = 0, 
                                            lambda.ridge.seq = 0.000001,
                                            lambda.glasso.seq = seq(0, 2, by = 1))
                       
                       ape_summary <- summary.function(cv_out, penalty = "glasso", K = K)
                       lambda_vals <- find_lambda(ape_summary)
                       
                       # Fit for each lambda
                       out_1se <- gmr4(Yn, Yb, Yo, X = X, Xscale = Xscale, S = S,
                                       lambda.lasso = 0, lambda.ridge = 0.000001, lambda.glasso = lambda_vals$lambda_1se)
                       out_2se <- gmr4(Yn, Yb, Yo, X = X, Xscale = Xscale, S = S,
                                       lambda.lasso = 0, lambda.ridge = 0.000001, lambda.glasso = lambda_vals$lambda_2se)
                       out_3se <- gmr4(Yn, Yb, Yo, X = X, Xscale = Xscale, S = S,
                                       lambda.lasso = 0, lambda.ridge = 0.000001, lambda.glasso = lambda_vals$lambda_3se)
                       
                       # Evaluation
                       eval_1se <- evaluateB(round(out_1se$B, 2), nPred = 10, nNoise = noise_level, Xscale = Xscale, threshold = cutoff)
                       eval_2se <- evaluateB(round(out_2se$B, 2), nPred = 10, nNoise = noise_level, Xscale = Xscale, threshold = cutoff)
                       eval_3se <- evaluateB(round(out_3se$B, 2), nPred = 10, nNoise = noise_level, Xscale = Xscale, threshold = cutoff)
                       
                       # Return results
                       data.frame(
                         Simulation = sim,
                         SampleSize = sample_size,
                         NoiseLevel = noise_level,
                         ResponseSize = response_size,
                         Qmix = paste(q.mix, collapse = "-"),
                         APE = ape_summary$APE[which.min(as.numeric(ape_summary$APE))],
                         LambdaMin = lambda_vals$lambda_min,
                         Lambda1SE = lambda_vals$lambda_1se,
                         Lambda2SE = lambda_vals$lambda_2se,
                         Lambda3SE = lambda_vals$lambda_3se,
                         
                         TP_1SE = eval_1se$TP,
                         FP_1SE = eval_1se$FP,
                         FN_1SE = eval_1se$FN,
                         TDR_1SE = eval_1se$TDR,
                         FDR_1SE = eval_1se$FDR,
                         NumSelected_1SE = eval_1se$NumSelected,
                         
                         TP_2SE = eval_2se$TP,
                         FP_2SE = eval_2se$FP,
                         FN_2SE = eval_2se$FN,
                         TDR_2SE = eval_2se$TDR,
                         FDR_2SE = eval_2se$FDR,
                         NumSelected_2SE = eval_2se$NumSelected,
                         
                         TP_3SE = eval_3se$TP,
                         FP_3SE = eval_3se$FP,
                         FN_3SE = eval_3se$FN,
                         TDR_3SE = eval_3se$TDR,
                         FDR_3SE = eval_3se$FDR,
                         NumSelected_3SE = eval_3se$NumSelected
                       )
                     }
  
  stopCluster(cl)
  close(pb)  
  
  return(results)
}



# Sim analysis:

result_1 <- run_parallel_singlecomb(sample_size = 250, noise_level = 10, response_size = 6, replications = 100, cutoff = 0.01) 
result_2 <- run_parallel_singlecomb(sample_size = 250, noise_level = 10, response_size = 12, replications = 100, cutoff = 0.01) 
result_3 <- run_parallel_singlecomb(sample_size = 250, noise_level = 50, response_size = 6, replications = 100, cutoff = 0.01) 
result_4 <- run_parallel_singlecomb(sample_size = 250, noise_level = 50, response_size = 12, replications = 100, cutoff = 0.01) 
result_5 <- run_parallel_singlecomb(sample_size = 250, noise_level = 200, response_size = 6, replications = 100, cutoff = 0.01) 
result_6 <- run_parallel_singlecomb(sample_size = 250, noise_level = 200, response_size = 12, replications = 100, cutoff = 0.01)

result_7 <- run_parallel_singlecomb(sample_size = 500, noise_level = 10, response_size = 6, replications = 100, cutoff = 0.01) 
result_8 <- run_parallel_singlecomb(sample_size = 500, noise_level = 10, response_size = 12, replications = 100, cutoff = 0.01)
result_9 <- run_parallel_singlecomb(sample_size = 500, noise_level = 50, response_size = 6, replications = 100, cutoff = 0.01) 
result_10 <- run_parallel_singlecomb(sample_size = 500, noise_level = 50, response_size = 12, replications = 100, cutoff = 0.01)
result_11 <- run_parallel_singlecomb(sample_size = 500, noise_level = 200, response_size = 6, replications = 100, cutoff = 0.01) 
result_12 <- run_parallel_singlecomb(sample_size = 500, noise_level = 200, response_size = 12, replications = 100, cutoff = 0.01)

result_13 <- run_parallel_singlecomb(sample_size = 1000, noise_level = 10, response_size = 6, replications = 100, cutoff = 0.01) 
result_14 <- run_parallel_singlecomb(sample_size = 1000, noise_level = 10, response_size = 12, replications = 100, cutoff = 0.01)
result_15 <- run_parallel_singlecomb(sample_size = 1000, noise_level = 50, response_size = 6, replications = 100, cutoff = 0.01) -
result_16 <- run_parallel_singlecomb(sample_size = 1000, noise_level = 50, response_size = 12, replications = 100, cutoff = 0.01)
result_17 <- run_parallel_singlecomb(sample_size = 1000, noise_level = 200, response_size = 6, replications = 100, cutoff = 0.01) 
result_18 <- run_parallel_singlecomb(sample_size = 1000, noise_level = 200, response_size = 12, replications = 100, cutoff = 0.01)



