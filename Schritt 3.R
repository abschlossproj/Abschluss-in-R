library(ggplot2)
library(tidyverse)

WorkingDirectory = readline(prompt="Enter the working directory or press [Enter] if your on matthias's machine: ")

paste('Working directory:', WorkingDirectory)

graphics = file.path(WorkingDirectory,"Rgraphics/")

#log10 = function(x) {
#  if (x > 10) {
#    return(x)
#  } else if (x > 0) {
#    return(11)
#  } else {
#    return(10) 
#  }
#}

files = list.files(file.path(WorkingDirectory, 'documentation_tables'))

docs = data.frame()
for (x in files) {
  df = read_delim(file.path(WorkingDirectory,'documentation_tables' , x), delim=',') 
  df = df %>% mutate(
                    k = as.integer(str_sub(x, 22, 23)),
                    m = as.integer(str_sub(x, 26, 27)),
                    p = as.integer(str_sub(x, 30, 31))
                    )
  docs = bind_rows(docs, df)  
}

t = 105

docm05 = docs[docs['m']==5,]
file = files[25] #arbitrarely chosen scenario with a nice development

doc = read_delim(file.path(WorkingDirectory,'documentation_tables', file), delim=',')



docm05_cp = docm05
docm05_cp = mutate(docm05_cp, 
                   k = toString('k'), 
                   p = toString('p'))

doc_interest = docm05_cp[docm05_cp['days'] < 10]

# tägliche Neuinfektionen, vs Veränderung Infizierter vs Tote
D_chng = c() 
D_nw = c() 
R_nw = c() 
T_nw = c()

for (i in 2:length(doc)) {
  
  D_chng = c(D_chng, doc[i,]['D'] - doc[i-1,]['D'])
  D_nw = c(D_chng, doc[i-1,]['H'] - doc[i,]['H'])
  R_nw = c(D_chng, doc[i,]['R'] - doc[i-1,]['R'])
  T_nw = c(D_chng, doc[i,]['T'] - doc[i-1,]['T'])
}

doc_change = tibble(H = doc$H,
                    D = doc$D,
                    G = doc$H + doc$R,
                    D_Change = D_chng,
                    D_New = D_nw,
                    R_New = R_nw,
                    days = doc$days,
                    T = doc$T,
                    T_new = T_nw * -1,
                    T_new_pos = T_nw,
                    T_new_log = sapply(T_nw, log10))

#cumsums -> makes overlapping possible, doc_c['T'] (not doc_c.T) actually is always 5000
doc_c = doc.copy()
doc_c['D'] = doc_c['D']+doc_c['H']
doc_c['R'] = doc_c['R']+doc_c['D']
doc_c['T'] = doc_c['T']+doc_c['R']

# logaithmierte D und T
doc_logT = doc['T'].apply(lambda x: math.log(log10(x), 10)) #sehr langsam :/
doc_logD = doc['D'].apply(lambda x: math.log(log10(x), 10)) # -> np.log(x) (geht auch mit df/ser?!)
doc_logIso = doc.Iso.apply(lambda x: math.log(log10(x),10))
doc_logDT = pd.DataFrame((doc['days'], doc_logT, doc_logD, doc_logIso)).T


#################
# 2 Plotting    #
################


# units for saving
w=15; h=10; u='cm'; dpi=300

# Mimicking the ZIB Plots on Instagramm
ggsave(filename = paste0(graphics , 'ZIB-like.png'), width=w, height=w, units=u, dpi=dpi, plot=
         ggplot(doc_change, aes(x= days, y=D_new)) +
          geom_col(fill='white', width=.8) + geom_smooth(color='yellow', size=1.5)+
          labs(title='XYZ Virus in Scenario ABC:\n Tägliche Neuinfektionen', y='')+
          theme_classic()+
          theme( rect=element_rect(color='#0072B2', size=0, fill='#0072B2'),
                axis.text = element_text(colour = 'white'), axis.title = element_text(colour = 'white'), 
                axis.line = element_line(color='white'), plot.title = element_text(colour = 'white'))+
          xlim(20,55) + ylim(0,180))#value in R as c()?


g = ggplot(docm05, aes(x='days'))

# facet über p, k, m fix 5
ggsave(filename = paste0(graphics , 'pkm_facet.png'), width=w, height=h, units=u, dpi=dpi, plot=
         g + geom_line(aes(y=T))  + labs(title='Herding immunity works, \nHygiene and Distancing is better', y='virus fatalities')+
          facet_grid(k~p, labeller='label_both'))
# interesting: for (20,50) there's less deads in total than vor (10,25)


# line-graphs, die die totalen Infizierten über verschiedenen p,k vergleichen
ggsave(filename = paste0(graphics , 'line_pk.png'), width=w, height=1.5*h, units=u, dpi=dpi, plot=
         g + geom_line(aes(y=D, color=k, fill=k)) + facet_wrap(p~k), labeller='label_both')+
          labs(y='infected persons', title='Only the strictest measures flatten the curve') + scale_color_gradient(guide=False) + theme_void())

g_int = ggplot(doc_interest, aes(x=days,y=D))

ggsave(filename = paste0(graphics , 'line_pk-log.png'), width=w, height=h, units=u, dpi=dpi, plot=
         g_int + geom_line(aes(colour=k,linetype=p)) + scale_y_log10()+
          labs(y='Infected persons', title='Cases for some scenarios from \'day zero\'') + theme_linedraw())



# Zeit vs Anzahl -> transparent überlappendes Säulendiagramm
g_col = ggplot(doc, aes(x= days)) +
   geom_line(aes(y=H),color='#203910',position='stack',alpha=0.4) +
   geom_line(aes(y=D),color='purple',position='stack',alpha=0.4) +
   geom_line(aes(y=R),color='#4101a2',position='stack',alpha=0.4) +
   geom_line(aes(y=T),color='#b21281',position='stack',alpha=0.4) 

ggsave(filename = paste0(graphics , 'time_line-col-log.png'), width=w, height=h, units=u, dpi=dpi, plot=
         g_col + scale_y_log10() + theme_bw() )

# "gestacktes" Säulendiagramm, sodass gut die Verhältnisse sichtbar sind
ggsave(filename = paste0(graphics , 'time_line-colstacked.png'), width=w, height=h, units=u, dpi=dpi, plot=
         ggplot(doc_c, aes(x=days)) +
          geom_col(aes(y=T),fill='red',position='stack') +
          geom_col(aes(y=R),fill='green',position='stack') +
          geom_col(aes(y=D),fill='yellow',position='stack') +
          geom_col(aes(y=H),fill='blue',position='stack') +
          labs(y='', title='Ratio of populace that is infected, recovered or dead') + theme_bw() + xlim(0,80)+
          annotate(geom='text', x=39, y=3000,label='Infected')+
          annotate(geom='text', x=55, y=4000,label='Recovered')+
          annotate(geom='text', x=14, y=1900,label='Healthy')+
          annotate(geom='text', x=75, y=4850,label='Dead'))

ggsave(filename = paste0(graphics , 'time_line_linesimple.png'), width=w, height=h, units=u, dpi=dpi, plot=            
         ggplot(doc_logDT, aes(x=days))+
          geom_line(aes(y=D),color='#0072B2') +
          geom_line(aes(y=Iso),color='#56B4E9') +
          geom_line(aes(y=T),color='#D55E00') +
          labs(title='Isolating helps keeping fatalities low', y='cases and deaths, log10'))

ggsave(filename = paste0(graphics , 'changes.png'), width=w, height=h, units=u, dpi=dpi, plot=
         ggplot(doc_change, aes(x=days))  +
          geom_col(aes(y=D_new), position='dodge',fill= 'orange', width=0.8) +
          geom_col(aes(y=R_new), position='dodge',fill='blue', width=0.8,alpha=.6) +
          geom_col(aes(y=T_new_pos),fill='red', width=1)+
          labs(title='Daily new infected, recovered and deceased', x='', y='daily new cases') +
          xlim(0,80)) 


print('----------------------------------------')
#print('Start at:', starttime)
#print('Fin.  at:', datetime.datetime.now())
print(paste('Graphics are saved in folder', graphics))
print("Done")
