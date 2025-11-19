# If you want to use starting values:


gmr4.start = function(Yn = NULL, Yb = NULL, Yo = NULL, X, S = 2, Xscale, lambda.lasso = 0, lambda.ridge = 0, lambda.glasso = 0, start){
  
  # simplified gmr4 for validation
  # ----------------------------------------------------
  eps = 1e-12
  sigma2 = 4
  Xori = X
  
  N = nrow(X)
  P = ncol(X)
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
  
  # ----------------------------------------------------
  # initalization optimal scaling for X
  originalCategories = quantifications = DG = D = G = list()
  
  for (p in 1:P){
    if (Xscale[p] == "C" && length(unique(X[, p])) == 2) {
      Xscale[p] <- "N"
    }
  }
  for (p in 1:P){
    if(Xscale[p] != "N"){
      originalCategories[[p]] = sort(unique(X[ , p])) # added
      G[[p]] = class.ind(X[, p])
      if(ncol(G[[p]]) == length(start$quantifications[[p]])){
        quantifications[[p]] = start$quantifications[[p]]
      }
      else{
        quantifications[[p]] = sort(unique(X[ , p]))
      }
      D[[p]] = t(G[[p]]) %*% G[[p]]
      DG[[p]] = solve(D[[p]], t(G[[p]]))
    }
  }
  


  PHI = X
  
  a = scale(X[, which(Xscale == "N")])
  mx = attr(a, "scaled:center")
  sdx = attr(a, "scaled:scale")
  
  PHI[, which(Xscale == "N")] = a # standardize numeric predictors
  for(p in 1:P){
    if(Xscale[p] != "N") PHI[ , p] = G[[p]] %*% quantifications[[p]]
  }
  
  # starting values
  B = start$B
  V = start$V
  m = start$m
  mm = start$mm
  
  theta = (PHI %*% B) %*% t(V) # structural part
  
  mm = rep(0, R) # same as elements of m[[]] for numeric/binary with zeros for ordinal
  m = list() # intercepts for each response variable (where for ordinal there are multiple thresholds)
  
  
  dev = rep(NA, R)
  if(!(is.null(Yn))) mm[Nseq] = colMeans(Yn - PHI %*% B %*% t(V[Nseq, , drop = FALSE]))
  if(!(is.null(Yb))) mm[Bseq] = colMeans(Yb) # Zb - PHI %*% B %*% t(V[Bseq, ])
  if(!is.null(Yo)){
    for(r in 1:Ro){
      polr.out = polr(as.factor(Yo[ , r]) ~ 1 + offset(theta[, Oseq[r]]))
      m[[Oseq[r]]] = polr.out$zeta
      dev[Oseq[r]] = polr.out$deviance
    }
  }
  
  eta = outer(ones.n, mm) + theta # "linear predictor"
  
  # ----------------------------------------------------
  pen1 = lambda.lasso * sum(abs(B))
  pen2 = lambda.ridge * sum(B^2)
  pen3 = lambda.glasso * sqrt(S) * sum(sqrt(rowSums(B^2)))
  penalty = pen1 + pen2 + pen3
  
  dev.n = dev.b = dev.o = 0 #initialisation - stays 0 when either is not available. 
  if(!is.null(Yn)){
    RES = Yn - eta[ , Nseq]
    sigma2 = sd(RES)^2
    dev.n = sum(RES^2)/(2 * sigma2) + N*Rn/2 * log(sqrt(2 * pi * sigma2)) 
  } # numeric part
  if(!is.null(Yb)) dev.b =  -sum(log(plogis(Qb * theta[, Bseq]))) # binary part
  if(!is.null(Yo)) dev.o = sum(dev[Oseq])/2  # ordinal part
  
  loss.old = dev.n + dev.b + dev.o + penalty
  
  # ----------------------------------------------------
  # Working responses matrix Z
  Zn = Zb = Zo = NULL
  if(!is.null(Yn)) Zn = Yn
  
  for (iter in 1:65536){
    
    # ----------------------------------------------------
    # update majorization constant
    # ----------------------------------------------------
    kappa = max(1/sigma2, 0.25) 
    ikappa = 1/kappa
    
    # ----------------------------------------------------
    # update working responses
    # ----------------------------------------------------
    # continuous
    if(!is.null(Yn)) Zn = eta[ , Nseq] - ikappa * (-RES/sigma2)
    
    # ------------------------------
    # binary
    if(!is.null(Yb)) Zb = as.matrix(eta[, Bseq] + ikappa * Qb * (1 - plogis(Qb * eta[, Bseq])))
    
    # ------------------------------
    #ordinal
    if(!is.null(Yo)){
      for(r in 1:Ro){
        Ep = expected.p(Yo[ , r], theta[ , Oseq[r] ], m[[ Oseq[r] ]])
        xi[ , r] = 1 - 2 * Ep
      }
      Zo = as.matrix(theta[, Oseq] - ikappa * xi)
    } 
    # ------------------------------
    Z = cbind(Zn, Zb, Zo)
    
    # ----------------------------------------------------
    
    # update transformations
    # ----------------------------------------------------
    BV = B %*% t(V)
    for(p in 1:P){
      if(Xscale[p] != "N"){
        U = Z - PHI[ , -p, drop = FALSE] %*% BV[-p, ]
        
        # Calculate the nominal (unrestricted) solution to the system
        quantifications[[p]] = DG[[p]] %*% U %*% t(BV[p, , drop = FALSE]) %*% solve(crossprod(BV[p, ]))
        
        # restrictions
        if(Xscale[p] == "O"){ #if predictors are ordinal
          lp = length(quantifications[[p]])
          wp = as.numeric(diag(D[[p]]))
          # monotone increasing
          qq1 = monotone(quantifications[[p]], w = wp)
          l1 = sum(wp * (quantifications[[p]] - qq1)^2)
          # monotone decreasing
          qq2 = monotone(quantifications[[p]][lp:1], w = wp[lp:1])[lp:1]
          l2 = sum(wp * (quantifications[[p]] - qq2)^2)
          # best fitting wins
          if(l1 < l2){
            quantifications[[p]] = qq1
          }
          else{
            quantifications[[p]] = qq2
          }
        }
        
        # Standardize the quantifications
        quantifications[[p]] = matrix(scale(rep(quantifications[[p]], times = diag(D[[p]])))[cumsum(c(1, diag(D[[p]])))[1:(nrow(D[[p]]))]], ncol = 1)
        PHI[, p] = G[[p]] %*% quantifications[[p]]
      }
    }
    
    A = kronecker(diag(S), t(PHI) %*% PHI)
    
    # ----------------------------------------------------
    # update B
    # ----------------------------------------------------
    
    bold = Vec(B)
    absb = abs(bold) # necessary for lasso
    sqrtb2 = rep( sqrt(rowSums(B^2)), S) 
    D1 = diag(drop((lambda.lasso /2) / absb))
    D2 = lambda.ridge * diag(P*S)
    D3 = diag(drop((lambda.glasso /2) / sqrtb2))
    Dtot = D1 + D2 + D3
    # update 
    if(any(Dtot > (1/eps))){
      idx = which(Dtot > (1/eps))
      Dtot[idx] = 1/eps # upper bound on elements of D
      warning = TRUE
    }
    Zc = (Z - outer(ones.n, mm))
    vz = Vec(Zc)
    b = kappa * solve(kappa * A + 2*Dtot, t(kronecker(V, PHI)) %*% vz)
    if(any(Dtot > (1/eps))) b[idx] = bold[idx]
    B = matrix(b, ncol = S)
    
    # ----------------------------------------------------
    # update V
    # ----------------------------------------------------
    udv = svd(t(B) %*% t(PHI) %*% (Zc))
    V = t(udv$u[, 1:S] %*% t(udv$v[, 1:S]))    
    
    theta = (PHI %*% B) %*% t(V)
    
    # ----------------------------------------------------
    # update m
    if(!(is.null(Yn))) mm[Nseq] = colMeans(Zn - theta[ , Nseq])
    if(!(is.null(Yb))) mm[Bseq] = colMeans(Zb - theta[ , Bseq])
    if(!is.null(Yo)){
      for(r in 1:Ro){
        rr = Oseq[r]
        polr.out = polr(as.factor(Yo[ , r]) ~ 1 + offset(theta[, rr]))
        m[[rr]] = polr.out$zeta
        dev[rr] = polr.out$deviance
      }
    }
    
    eta = outer(ones.n, mm) + theta # PHI %*% B %*% t(V)
    
    # ----------------------------------------------------
    # compute loss and determine convergence
    # ----------------------------------------------------
    
    pen1 = lambda.lasso * sum(abs(B))
    pen2 = lambda.ridge * sum(B^2)
    pen3 = lambda.glasso * sqrt(S) * sum(sqrt(rowSums(B^2)))
    penalty = pen1 + pen2 + pen3
    
    if(!is.null(Yn)){
      RES = Yn - (outer(ones.n, mm[Nseq]) + PHI %*% B %*% t(V[Nseq, , drop = FALSE]))
      sigma2 = sd(RES)^2
      # sigma2 = sum(diag(t(RES) %*% RES))/(N*Rn)
      dev.n = sum(RES^2)/(2 * sigma2) + N*Rn/2 * log(sqrt(2 * pi * sigma2))  # numeric part
    } 
    if(!is.null(Yb)) dev.b =  -sum(log(plogis(Qb * eta[, Bseq]))) # binary part
    if(!is.null(Yo)) dev.o = sum(dev[Oseq])/2  # ordinal part
    
    loss.new = dev.n + dev.b + dev.o + penalty
    
    # if (trace) {cat(iter, loss.old, loss.new, (2*(loss.old - loss.new)/(loss.old + loss.new)), "\n")}
    # if (loss.new > loss.old) stop("Divergence - should not occur!")
    if ( (2*(loss.old - loss.new)/(loss.old + loss.new)) < 1e-6 ) break
    if (iter == 65536) warning("Maximum number of iterations reached - not converged (yet)")
    loss.old = loss.new
  }
  
  
  
  # make output object
  output = list(
    Xoriginal = Xori,
    Xscale = Xscale,
    mx = mx,
    sdx = sdx,
    Y = Y,
    Yscale = list(Nseq = Nseq, Bseq = Bseq, Oseq = Oseq),
    Yn = Yn,
    Yb = Yb,
    Yo = Yo,
    Zn = Zn,
    Zb = Zb,
    Zo = Zo,
    PHI = PHI,
    sigma2 = sigma2,
    m = m,
    mm = mm,
    B = B,
    V = V,
    originalcategories = originalCategories,
    quantifications = quantifications,
    penalty = penalty,
    D1 = D1,
    D2 = D2,
    D3 = D3,
    Dtot = Dtot,
    iter = iter,
    loss = loss.new
  )
  # ----------------------------------------------------
  return(output)
}



