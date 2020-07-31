#install.packages("tidyverse")
library(dplyr)

N = 5
k = 2

adMatrix = matrix(0L, nrow = N, ncol = N)

indexfilter_2 = function(matrix, row) {
  if (N <= row) colSum = sum(matrix[,N])
  else colSum = colSums(matrix[,row:N])
  hv = ifelse(colSum < k, 1, 0)
  potcon = hv + (row)
  return(potcon)
}

for (i in 1:N) {
  potCon = indexfilter_2(adMatrix, i)
  addCon = k - sum(adMatrix[i,])
  
  if(addCon > 0 & length(potCon) > 0) {
    newCon = min(addCon, length(potCon))
    print(potCon)
    print(newCon)
    indices = sample(potCon, size = newCon, replace = FALSE)
    adMatrix[i, indices] = 1
    adMatrix[indices, i] = 1
    
  }
}

(adMatrix)
