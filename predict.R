predict.gmr4 <- function(object, newX) {
  Rn <- Rb <- Ro <- idd <- 0
  
  if (!is.null(object$Yn)) {
    Rn <- ncol(object$Yn)
    Nseq <- 1:Rn
    idd <- Rn
  }  
  
  if (!is.null(object$Yb)) {
    Rb <- ncol(object$Yb)
    Bseq <- (idd + 1):(idd + Rb)
    idd = idd + Rb
  }  
  
  if (!is.null(object$Yo)) {
    Ro <- ncol(object$Yo)
    Oseq <- (idd + 1):(idd + Ro)
    idd = idd + Ro
  }  
  
  
  newPHI = newX
  P = ncol(newX)
  
  
  columnsToScale <- which(object$Xscale == "N")
  
  newPHI[,columnsToScale] = scale(newX[, columnsToScale, drop = FALSE], 
                                  center = object$mx, scale = object$sdx)
  
  for (p in 1: P) {
    if (Xscale[p] != "N") {
      newPHI[, p]  = myrecode(newX[, p], object$originalcategories[[p]], object$quantifications[[p]])
    }
  }
  
  structural_theta <- newPHI %*% (object$B %*% t(object$V))
  R = ncol(structural_theta)
  Yhat <- vector(mode = "list", length = R)
  
  for (r in 1:R) {
    if (Rn > 0 && r %in% Nseq) {
      theta <- outer(rep(1, nrow(newX)), object$mm[Nseq]) + structural_theta[, Nseq]
      Yhat[[r]] = theta[,r]
    }
    
    if (Rb > 0 && r %in% Bseq) {
      rb_index <- which(Bseq == r)  
      theta <- outer(rep(1, nrow(newX)), object$mm[Bseq]) + structural_theta[, Bseq]
      Yhat[[r]] = plogis(theta[, rb_index])  
    }
    
    if (Ro > 0 && r %in% Oseq) {
      zeta = c(-Inf, object$m[[r]], Inf)
      Cr = length(zeta)
      P = plogis(outer(rep(1,nrow(newX)), zeta) - outer(structural_theta[, r], rep(1, Cr)))
      Yhat[[r]] = t(apply(P, 1, diff)) 
    }
  }
  
  return(Yhat) 
}




#---


predict.gmr4 <- function(object, newX) {
  Rn <- Rb <- Ro <- idd <- 0
  
  if (!is.null(object$Yn)) {
    Rn <- ncol(object$Yn)
    Nseq <- 1:Rn
    idd <- Rn
  }  
  
  if (!is.null(object$Yb)) {
    Rb <- ncol(object$Yb)
    Bseq <- (idd + 1):(idd + Rb)
    idd = idd + Rb
  }  
  
  if (!is.null(object$Yo)) {
    Ro <- ncol(object$Yo)
    Oseq <- (idd + 1):(idd + Ro)
    idd = idd + Ro
  }  
  
  newPHI <- newX
  P <- ncol(newX)
  
  
  numericCols <- which(object$Xscale == "N")
  if (length(numericCols) > 0) {
    newPHI[, numericCols] <- scale(newX[, numericCols, drop = FALSE], 
                                   center = object$mx, scale = object$sdx)
  }
  
  
  for (p in 1:P) {
    if (object$Xscale[p] != "N") {
      if (!is.null(object$originalcategories[[p]]) && !is.null(object$quantifications[[p]])) {
        newPHI[, p] <- myrecode(newX[, p], 
                                object$originalcategories[[p]], 
                                object$quantifications[[p]])
      } else {
        stop(paste("Quantification missing for predictor", p))
      }
    }
  }
  
 
  structural_theta <- newPHI %*% (object$B %*% t(object$V))
  R <- ncol(structural_theta)
  Yhat <- vector(mode = "list", length = R)
  
  for (r in 1:R) {
    if (Rn > 0 && r %in% Nseq) {
      theta <- outer(rep(1, nrow(newX)), object$mm[Nseq]) + structural_theta[, Nseq]
      Yhat[[r]] <- theta[, r]
    }
    
    if (Rb > 0 && r %in% Bseq) {
      rb_index <- which(Bseq == r)  
      theta <- outer(rep(1, nrow(newX)), object$mm[Bseq]) + structural_theta[, Bseq]
      Yhat[[r]] <- plogis(theta[, rb_index])  
    }
    
    if (Ro > 0 && r %in% Oseq) {
      zeta <- c(-Inf, object$m[[r]], Inf)
      Cr <- length(zeta)
      P <- plogis(outer(rep(1, nrow(newX)), zeta) - outer(structural_theta[, r], rep(1, Cr)))
      Yhat[[r]] <- t(apply(P, 1, diff)) 
    }
  }
  
  return(Yhat)
}


