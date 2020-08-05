#install.packages('tidyverse')
#install.packages('RcppCNPy')
library(dplyr)
library(RcppCNPy)

N = 5000
k_l = c(5, 10, 20)

adMatrix = matrix(0L, nrow = N, ncol = N)

indexfilter = function(matrix, row) {
  colSum = c()
  if (row + 1 > N) return
  else if (row + 1 == N) colSum = sum(matrix[,N])
  else colSum = colSums(matrix[,(row + 1):N])
  hv = which(ifelse(colSum < k, 1, 0) %in% 1)
  potcon = hv + (row - 1)
  return(potcon)
}

for (k in k_l) {
  cat('calculating and saving matrix for k = ', k, '\n')
  for (i in 1:N) {
    cat('\rworking on line ', i)
    potCon = indexfilter(adMatrix, i)
    addCon = k - sum(adMatrix[i,])
    
    if(addCon > 0 & length(potCon) > 0) {
      newCon = min(addCon, length(potCon))
      indices = sample(potCon, size = newCon, replace = FALSE)
      adMatrix[i, indices + 1] = 1
      adMatrix[indices + 1, i] = 1
      
    }
  }
  cat(adMatrix, '\n')
  adMatrix = ifelse(adMatrix == 1, TRUE, FALSE)
  filename = paste(c('adMatrix_k', k, '.npy'), collapse = '')
  npySave(filename, adMatrix)
  cat('Saved ', filename, '\n')
}
