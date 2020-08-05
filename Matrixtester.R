library(dplyr)

(matrixFiles = list.files('.', pattern = '*rds'))

for (matrix in matrixFiles) {
 load = readRDS(matrix)
 cat('\nname: ', matrix, '\nSum of Columns: ', colSums(load), '\nSum of Diagonal: ', sum(diag(load)))
}


    