
xval.start = function(Yn, Yb, Yo, X, Xscale, S = 2, K = 5, repeats = 1, lambda.lasso.seq = 0, lambda.ridge.seq = 0, lambda.glasso.seq = 0){
  # getting some constants from the data
  # browser()
  N = nrow(X)
  P = ncol(X)
  if(!is.null(Yn)) if(nrow(Yn) != N) stop("Number of rows of X and Yn should be equal")
  if(!is.null(Yb)) if(nrow(Yb) != N) stop("Number of rows of X and Yb should be equal")
  if(!is.null(Yo)) if(nrow(Yo) != N) stop("Number of rows of X and Yo should be equal")
  ones.n = rep(1, N)
  
  Rn = Rb = Ro = idd = 0
  Nseq = Bseq = Oseq = NULL
  if(!is.null(Yn)){
    Rn = ncol(Yn)
    Nseq = 1:Rn
    idd = Rn
  }
  
  if(!is.null(Yb)){
    Rb = ncol(Yb)
    Bseq = (idd + 1):(idd + Rb)
    idd = idd + Rb
    Qb = 2 * as.matrix(Yb) - 1 # assumes Yb is binary 0, 1
  }
  
  if(!is.null(Yo)){
    Ro = ncol(Yo)
    Oseq = (idd + 1):(idd + Ro)
    idd = idd + Ro
    xi = matrix(0, N, Ro) # needed for computing expected value
  }
  
  R = Rn + Rb + Ro
  
  Y = cbind(Yn, Yb, Yo)
  Ylist = vector(mode = "list", length = ncol(Y))
  for (r in 1:R) {
    if (Rn > 0 && r %in% Nseq) {
      Ylist[[r]] = Y[ , r]
    }
    if (Rb > 0 && r %in% Bseq) {
      Ylist[[r]] = Y[, r]
    }
    if (Ro > 0 && r %in% Oseq) {
      Ylist[[r]] = class.ind(Y[ , r])
    }
  }
  
  # preparation for cross-validation
  penaltygrid = expand.grid(lambda.lasso.seq, lambda.ridge.seq, lambda.glasso.seq)
  folds <- cut(seq(1, N), breaks = K, labels = FALSE)
  
  pe.df = data.frame(matrix(nrow = (nrow(penaltygrid) * repeats * K), ncol = (5 + R + 1)))
  colnames(pe.df) = c("lasso", "ridge", "glasso", "repeat", "fold", paste0("PE", 1:R), "PEtotal")
  
  teller = 0
  
  for(rep in 1:repeats){
    folds = sample(folds)
    for(k in 1:K){
      for(i in 1:nrow(penaltygrid)){
        set.seed((1234 + i))
        teller = teller + 1
        cat("This is analysis", teller, "from a total of", nrow(pe.df), "\n")
        idx = which(folds == k, arr.ind=TRUE)
        idp = which(apply(X[-idx,] , 2, var) != 0)  
        
        
        if(i == 1) {
          # browser()
          out = gmr4(Yn = Yn[-idx, ], Yb = Yb[-idx, ], Yo = Yo[-idx, ], X = X[-idx, idp ], S = S, Xscale = Xscale, 
                     lambda.lasso = penaltygrid[i, 1], lambda.ridge = penaltygrid[i, 2], lambda.glasso = penaltygrid[i, 3], trace = FALSE)
        } else {
          
          start <- list(
            B = out$B,
            V = out$V,
            m = out$m,
            mm = out$mm,
            G = out$G, # add
            originalcategories = out$originalCategories, # add
            quantifications = out$quantifications
          )
          
          # train
          out = gmr4.start(Yn = Yn[-idx, ], Yb = Yb[-idx, ], Yo = Yo[-idx, ], X = X[-idx, idp ], S = S, Xscale = Xscale,
                           lambda.lasso = penaltygrid[i, 1], lambda.ridge = penaltygrid[i, 2], lambda.glasso = penaltygrid[i, 3], start = start)
          
        }
        # predict
        Yhat =  predict.gmr4(out, newX = X[idx, idp ])
        
        # compute deviance of the predictions
        prederror = rep(NA, R)
        for (r in 1:R) {
          if (Rn > 0 && r %in% Nseq) {
            YY = Ylist[[r]] # vector
            YY = YY[idx] # select test cases
            prederror[r] = (sum((YY - Yhat[[r]])^2)/(2 * out$sigma2) + N/2 * log(sqrt(2 * pi * out$sigma2))) / length(YY)
            
            
          }
          if (Rb > 0 && r %in% Bseq) {
            YY = Ylist[[r]] # vector
            YY = YY[idx] # select test cases
            PI = Yhat[[r]]
            prederror[r] = -(sum(log(PI[which(YY == 1)])) + sum(log(1 - PI[which(YY == 0)])))/length(YY)
            
          }
          if (Ro > 0 && r %in% Oseq) {
            PI = Yhat[[r]]
            YY = Ylist[[r]] # matrix
            YY = YY[idx, ] #select test cases
            prederror[r] = -mean(log(PI[which(YY == 1)]))
          }
        } #response
        
        pe.df[teller, ] = c(penaltygrid[i, 1], penaltygrid[i, 2], penaltygrid[i, 3], rep, k, prederror, sum(prederror))
      } # folds
    } # repeats
  } # penalty grid
  
  return(pe.df)
}

