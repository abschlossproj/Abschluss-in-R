library(dplyr)
library(RcppCNPy)

matrixFiles = list.files('.', pattern = 'adMatrix_*')

for (matrix in matrixFiles) {
 load = npyLoad(matrix)
 cat('\nname: ', matrix, '\nSum of Columns: ', colSums(load), '\nSum of Diagonal: ', sum(diag(load)))
}


    