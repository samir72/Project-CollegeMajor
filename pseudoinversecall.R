pseudoinversecall <- function(df,mod.mat)
  {
#  browser()
  M = as.matrix(mod.mat)
  head(M)
  MTM = t(M) %*% M
  head(MTM)
  dim(MTM)
  #compute the SVD of model matrix M (195 * 60)
  # Examine the singular values and in the process check for "Rank Deficiency".
  mSVD <- svd(MTM)
  #Validate whether singular vectors are orthogonal.
  uOrth <- t(mSVD$u) %*% mSVD$u
  vOrth <- mSVD$v %*% t(mSVD$v)
  uOrthSingLeftVector <- mSVD$u
  vOrthSingRightVector <- mSVD$v
  diagSDV <- mSVD$d
  diagSDV
  #By looking at singlular values we can now deduce that this matrix is rank deficient with 1 value below zero.
  # Let us now compute the pseudo inverse of MTM matrix.
  #cat('Compute and print the inverse singular value matrix')
  d.trim = rep(0, 18)
  d.trim[1:17] =1/ mSVD$d[1:17]
  mD = diag(d.trim)
  #cat('Compute and print the pseudo inverse')
  mInv = mSVD$v %*% mD %*% t(mSVD$u)
  #cat('Compute and print the dimensions of the matrix MInvM')
  MInvM = mInv %*% t(M)
  dim(MInvM)
  
  # Compute the vector of model coefficients by multiplying MInvM with normalized median pay.
  b <- MInvM %*% df$normalized_gradmedian_round
  coeffcount <- nrow(b)
  
  # Now we can evaluate the model using this vector of model coefficient.
  df$score = M %*% b + mean(df$normalized_gradmedian_round)
  df$resids = df$score - df$normalized_gradmedian_round
  df$coeffcount = coeffcount
  return(df)
}