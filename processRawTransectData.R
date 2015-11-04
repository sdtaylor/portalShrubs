library(tidyr)
csvFolder='~/projects/portalShrubs/shrubData/excelExports/'

#Each year of transect data is slightly different, so I will just handle
#each seperatly.

#################################
#year 1989. 
#Read in and reshape data
year89=read.csv(paste(csvFolder,'89in.csv',sep='')) %>%
      gather(Plot, SpeciesID, -Transect, -Point)

#Extract plot number from old column data, eg '89P16'
year89$Plot=substring(year89$Plot, 5)
#Set year
year89$Year=1989

#Assign species name
speciesList89=read.csv(paste(csvFolder, '1989_species_list.csv',sep=''))
year89=merge(year89, speciesList89, by.x='SpeciesID',by.y='Number')

###############################
#year 1992
#read in and reshape data
year92=read.csv(paste(csvFolder,'92in.csv',sep='')) %>%
  gather(Plot, SpeciesID, -Transect, -Point)

#Extract plot number from old column data, eg '89P16'
year92$Plot=substring(year92$Plot, 5)
#Set year
year92$Year=1992

#Assign species name
speciesList92=read.csv(paste(csvFolder, '1992_species_list.csv',sep=''))
year92=merge(year92, speciesList92, by.x='SpeciesID',by.y='Number')

##############################
#Year 1995
#read in and reshape data
year95=read.csv(paste(csvFolder,'95in.csv',sep=''))
  gather(Plot, SpeciesID, -Transect, -Position)

#Extract plot number from old column data, eg '89P16'
year95$Plot=substring(year95$Plot, 5)
#Set year
year95$Year=1995