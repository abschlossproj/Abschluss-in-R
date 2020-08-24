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
  cat('calculating and saving csv for k =', k, ', m =', m, ', p =', p, '\n')
  p = 1/p
  adMatrix = readRDS(paste(c('adMatrix_k', k, '.rds'), collapse = ''))
  data = tibble(ID = 1:N,
                status = 'H', 
                days = integer(N), 
                duration = integer(N), 
                isolation = FALSE,
                course = '')
  
  index = sample(N, m, replace = TRUE)
  
  data[index,]['status'] = 'D'
  data[index,]['duration'] = as.integer(runif(m, tmin, tmax))
  data[index,]['course'] = rdVector(m, diseaseCourse)
  
  documentation = tibble(days = 1:t,
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
    cat('\rprogress: ', (100 * currDay)/t, '%\t')
    data = mutate(data, 
                  days = ifelse(status == 'D', days + 1, days),
                  isolation = ifelse(days == x, TRUE, isolation))
    
    crit = filter(data, days == duration & status == 'D')[['ID']]
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
                           H = ifelse(days == currDay, stati['H'], H),
                           D = ifelse(days == currDay, stati['D'], D),
                           R = ifelse(days == currDay, stati['R'], R),
                           T = ifelse(days == currDay, stati['T'], T),
                           
                           Iso = ifelse(days == currDay, sum(data$isolation), Iso),
                           
                           L = ifelse(days == currDay, courses['L'], L),
                           M = ifelse(days == currDay, courses['M'], M),
                           S = ifelse(days == currDay, courses['S'], S))
    
  }
  
  documentation = documentation %>% add_row(H = N - m,
                                            D = m, 
                                            L = nrow(filter(data, course == 'L')), 
                                            M = nrow(filter(data, course == 'M')), 
                                            S = nrow(filter(data, course == 'S')), 
                                            .before = 1)
  documentation[is.na(documentation)] = 0
  
  p = 1/p
  filename = paste(c(
    output_directory, 
    '/documentation_simul_k', 
    ifelse(k >= 10, k, paste(c('0', k), collapse = '')), 
    '_m', 
    ifelse(m >= 10, m, paste(c('0', m), collapse = '')), 
    '_p', 
    ifelse(p >= 10, p, paste(c('0', p), collapse = '')), 
    '.csv'), collapse = '')
  
  write_csv(documentation, filename)
  cat('Saved ', filename, '\n')
}


ks = c(5, 10, 20)
ms = c(1, 5, 10)
ps = c(10, 25, 50)

i = 0
for (k in ks) {
  for (m in ms) {
    for (p in ps) {
      scenario(k, m, p)
      
      i = i + 1
      cat('Finished files:', i, '/', length(ks) * length(ms) * length(ps), '\n')
    }
  }
}


