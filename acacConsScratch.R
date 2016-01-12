library(ggplot2)

acacCons=finalTransectData %>% filter(Species.Name=='Acacia constricta')

for(thisPlot in 1:24){
  print(ggplot(filter(acacCons, Plot==thisPlot), aes(x=Year, y=cover,ymax=0.25))+geom_line())#+ggtitle(paste('Plot',thisPlot))
}

lm1=lm(cover~Year+factor(Plot), data=acacCons)
