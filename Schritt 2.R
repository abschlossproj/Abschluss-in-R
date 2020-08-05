#install.packages('tidyverse')
library(tidyverse)

t = 110
N = 5000
mr = 0.05
x = 5
pCD = c(8/10, 15/100, 5/100)

tmin = 10
tmax = 16

output_directory = 'documentation_tables'

diseaseCourse = function() {
  hv = which(rmultinom(1, prob = pCD, size = 1) == 1)
  return(switch(hv, 'L', 'M', 'S'))
}

dcVector = function(n) {
  ret = 1:n
  for (i in ret) {
    ret[i] = diseaseCourse()
  }
  return(ret)
}

getCourseCount = function(data, cond) {
  return(nrow(filter(data, course == cond)))
}

scenario = function(k, m, p) {
  adMatrix = readRDS(paste(c('adMatrix_k', k, '.rds'), collapse = ''))
  data = tibble(ID = 1:N,
                status = 'H', 
                day = integer(N), 
                duration = integer(N), 
                isolation = FALSE,
                course = 'tbd')
  
  index = sample(N, 5, replace = TRUE)
  
  data[index,]['status'] = 'D'
  data[index,]['duration'] = as.integer(runif(5, tmin, tmax))
  data[index,]['course'] = dcVector(m)
  
  documentation = tibble(day = 1:t,
                         H = integer(t),
                         D = integer(t),
                         R = integer(t),
                         T = integer(t),
                         Iso = integer(t),
  
                         L = integer(t),
                         M = integer(t),
                         S = integer(t),
  ) %>% add_row(H = N - m,
                D = m, 
                L = getCourseCount(data, 'L'), 
                M = getCourseCount(data, 'M'), 
                S = getCourseCount(data, 'S'), 
                .before = 1)
  documentation[is.na(documentation)] = 0
  print(documentation)
}

scenario(5,5,5)


