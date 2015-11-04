library(tidyr)
csvFolder='~/projects/portalShrubs/shrubData/excelExports/'

#Each year of transect data is slightly different, so I will just handle
#each by itself.

#################################
#year 1989. 
#Read in a reshape data
year89=read.csv(paste(csvFolder,'89in.csv',sep='')) %>%
      gather(Plot, Species, -Transect, -Point)

#Extract plot number from old column data, eg '89P16'
year89$Plot=substring(year89$Plot, 5)
#Set year
year89$Year=1989


###############################
#year 1992