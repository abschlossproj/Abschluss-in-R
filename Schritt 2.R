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

rdVector = function(n, fun) {
  ret = 1:n
  for (i in ret) {
    ret[i] = fun()
  }
  return(ret)
}

scenario = function(k, m, p) {
  p = 1/p
  adMatrix = readRDS(paste(c('adMatrix_k', k, '.rds'), collapse = ''))
  data = tibble(ID = 1:N,
                status = 'H', 
                day = integer(N), 
                duration = integer(N), 
                isolation = FALSE,
                course = '')
  
  index = sample(N, 5, replace = TRUE)
  
  data[index,]['status'] = 'D'
  data[index,]['duration'] = as.integer(runif(5, tmin, tmax))
  data[index,]['course'] = rdVector(m, diseaseCourse)
  
  documentation = tibble(day = 1:t,
                         H = integer(t),
                         D = integer(t),
                         R = integer(t),
                         T = integer(t),
                         Iso = integer(t),
  
                         L = integer(t),
                         M = integer(t),
                         S = integer(t),
  )
  
  for (currDay in 1:t) {
    data = mutate(data, 
                  day = ifelse(status == 'D', day + 1, day),
                  isolation = ifelse(day == x, TRUE, isolation))
    
    crit = filter(data, day == duration & status == 'D')[['ID']]
    data = mutate(data, 
                    status = ifelse(is.element(ID, crit), ifelse(rbinom(1, 1, mr), 'T', 'R'), status),
                    isolation = ifelse(ID %in% crit, FALSE, isolation),
                    course = ifelse(ID %in% crit, '', course))
    
    ill = filter(data, isolation == FALSE & status == 'D')['ID']
    newInf = c()
    
    for (i in ill) {
      contacts = which(adMatrix[1,] == 1)
      for (j in contacts) {
        if (rbinom(1, 1, p) & filter(data, ID == j)['status'] == 'H') {
          newInf = c(newInf, j)
        }
      }
    }
    
    data = mutate(data, 
                  status = ifelse(ID %in% newInf, 'D', status),
                  duration = ifelse(ID %in% newInf, sample(tmin:tmax, 1), duration),
                  course = ifelse(ID %in% newInf, diseaseCourse(), course))
    
    stati = table(data$status)
    courses = table(data$course)
    
    documentation = mutate(documentation,
                           H = ifelse(day == currDay, stati['H'], H),
                           D = ifelse(day == currDay, stati['D'], D),
                           R = ifelse(day == currDay, stati['R'], R),
                           T = ifelse(day == currDay, stati['T'], T),
                           
                           Iso = ifelse(day == currDay, sum(data$isolation), Iso),
                           
                           L = ifelse(day == currDay, courses['L'], L),
                           M = ifelse(day == currDay, courses['M'], M),
                           S = ifelse(day == currDay, courses['S'], S))
    
  }
  
  documentation = documentation %>% add_row(H = N - m,
                                            D = m, 
                                            L = nrow(filter(data, course == 'L')), 
                                            M = nrow(filter(data, course == 'M')), 
                                            S = nrow(filter(data, course == 'S')), 
                                            .before = 1)
  documentation[is.na(documentation)] = 0
  
  
  print(data)
  print(documentation)
}

scenario(5,5,5)


