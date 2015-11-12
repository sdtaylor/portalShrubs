library(tidyr)
library(dplyr)
library(stringr)
csvFolder='~/projects/portalShrubs/shrubData/excelExports/'


#Final df of Year, Plot, Group, Cover
finalTransectData=data.frame()

#Once each year is in the format data.frame(Transect, Point, Plot, Year, Group)
#pass it to this to get percent cover. 
processFormattedData=function(df){
  transectTotals=df %>%
    group_by(Year, Plot) %>%
    summarize(totalPoints=n())
  
  df=df %>%
    group_by(Year, Plot, Group) %>%
    summarize(n=n())
  
  df=merge(df, transectTotals, by=c('Year','Plot'))
  df=df %>% mutate(cover = n/totalPoints)
    
  return(select(df, -n, -totalPoints))
}

#Each year of transect data is slightly different, so  for the most part I just process each 
#seperately 
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
speciesList89=read.csv(paste(csvFolder, '1989_species_list_ShawnsEdits.csv',sep=''))
year89=merge(year89, speciesList89, by.x='SpeciesID',by.y='Number') %>%
  select(Transect, Point, Plot, Year, Group)

year89=processFormattedData(year89)


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
speciesList92=read.csv(paste(csvFolder, '1992_species_list_ShawnsEdits.csv',sep=''))
year92=merge(year92, speciesList92, by.x='SpeciesID',by.y='Number') %>%
  select(Transect, Point, Plot, Year, Group)

year92=processFormattedData(year92)

##############################
#Year 1995
#read in and reshape data
year95=read.csv(paste(csvFolder,'95in.csv',sep=''))
#Need to force some of the columns into character to work with gather()
year95[,colnames(year95)[-2]]= lapply(year95[,colnames(year95)[-2]], as.character)

#Reshape
year95=gather(year95, Plot, SpeciesID, -Transect, -Position)

#Extract plot number from old column data, eg '89P16'
year95$Plot=substring(year95$Plot, 5)
#Set year
year95$Year=1995

#speciesID has multiple hits per point in the form of "45,34,2". I use only the 1st one,
#assuming that is the top most one and what sattelites will see.
getFirstValue=function(x){
  return(unlist(strsplit(x, ','))[1])
}
year95$SpeciesID=lapply(year95$SpeciesID, getFirstValue)

#Assigne species names
speciesList95=read.csv(paste(csvFolder, '1995_species_list_ShawnsEdits.csv',sep=''))
year95=merge(year95, speciesList95, by.x='SpeciesID',by.y='Code.Number')

