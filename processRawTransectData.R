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
  
  #Change Species.Name to Group here to get total cover of functional groups
  #Change to Species.Name to get total cover of species for diagnostic
  df=df %>%
    group_by(Year, Plot, Species.Name) %>%
    summarize(n=n())
  
  df=merge(df, transectTotals, by=c('Year','Plot'))
  df=df %>% mutate(cover = n/totalPoints)
    
  return(select(df, -n, -totalPoints))
}

#Each year of transect data is slightly different, so  for the most part I just process each 
#seperately 
#################################
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
year89=merge(year89, speciesList89, by.x='SpeciesID',by.y='Code.Number') %>%
  select(Transect, Point, Plot, Year, Group, Species.Name)

year89=processFormattedData(year89)

finalTransectData=year89
###############################
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
year92=merge(year92, speciesList92, by.x='SpeciesID',by.y='Code.Number') %>%
  select(Transect, Point, Plot, Year, Group, Species.Name)

year92=processFormattedData(year92)

finalTransectData=rbind(finalTransectData, year92)
##############################
##############################
#Year 1995
#read in and reshape data
year95=read.csv(paste(csvFolder,'95in.csv',sep=''))
#Need to force some of the columns into character to work with gather()
year95[,colnames(year95)[-2]]= lapply(year95[,colnames(year95)[-2]], as.character)

speciesList95=read.csv(paste(csvFolder, '1995_species_list_ShawnsEdits.csv',sep=''))


#Reshape
year95=gather(year95, Plot, SpeciesID, -Transect, -Position)

#Extract plot number from old column data, eg '89P16'
year95$Plot=substring(year95$Plot, 5)
#Set year
year95$Year=1995

#speciesID has multiple hits per point in the form of "45,34,2". If thats the case, then I
#pull out the shrub value if one exists. Otherwise I just get the 1st value. 
getFirstValue=function(x){
  #Split up string by commas
  values=unlist(strsplit(x, ','))
  #If there weren't multiple values just return the single value
  if(length(values)==1){
    return(values[1])
  }
  #Merge just these values to find the shrub.
  values=merge(values, speciesList95, all.x=TRUE, all.y=FALSE, by.x='x', by.y='Code.Number')
  
  #Get list of shrubs, return the 1st one. If there are none, return the original 1st hit
  shrubs=values %>% filter(Group=='Shrub') %>% top_n(1)
  if(nrow(shrubs)>0){
    return(shrubs$x)
  } else {
    return(values[1])
  }
}
year95$SpeciesID=as.character(sapply(year95$SpeciesID, getFirstValue))

#Assigne species names
year95=left_join(year95, speciesList95, by=c('SpeciesID' = 'Code.Number'))  %>%
  rename(Point = Position) %>%
  select(Transect, Point, Plot, Year, Group, Species.Name) %>%
  #Some issues still in this year, but I will filter them out for the time being. Specifcially plot 22 has many problems
  filter(Plot!=22)



year95=processFormattedData(year95)

finalTransectData=rbind(finalTransectData, year95)
#################################
#################################
#year 98
#Read in and reshape data
year98=read.csv(paste(csvFolder,'98in.csv',sep=''), na.strings=c(''))

#Need to force some of the columns into character to work with gather()
year98[,colnames(year98)[-2]]= lapply(year98[,colnames(year98)[-2]], as.character)

year98 = year98 %>% gather(Plot, SpeciesID, -Transect, -Position)

#Extract plot number from old column data, eg '89P16'
year98$Plot=substring(year98$Plot, 5)
#Set year
year98$Year=1998

#One of the datasheet entries is actually no entry. Those get read as NA's, so change
#that here to 'blank'. 
year98$SpeciesID[is.na(year98$SpeciesID)]='blank'

speciesList98=read.csv(paste(csvFolder, '1998_species_list_ShawnsEdits.csv',sep=''))

#speciesID has multiple hits per point in the form of "45,34,2". If thats the case, then I
#pull out the shrub value if one exists. Otherwise I just get the 1st value. 
getFirstValue=function(x){
  #Split up string by commas
  values=unlist(strsplit(x, ','))
  #If there weren't multiple values just return the single value
  if(length(values)==1){
    return(values[1])
  }
  #Merge just these values to find the shrub.
  values=merge(values, speciesList98, all.x=TRUE, all.y=FALSE, by.x='x', by.y='Code.Number')
  
  #Get list of shrubs, return the 1st one. If there are none, return the original 1st hit
  shrubs=values %>% filter(Group=='Shrub') %>% top_n(1)
  if(nrow(shrubs)>0){
    return(shrubs$x)
  } else {
    return(values[1])
  }
}
year98$SpeciesID=as.character(sapply(year98$SpeciesID, getFirstValue))

#Assigne species names
year98=left_join(year98, speciesList98, by=c('SpeciesID' = 'Code.Number'))  %>%
  rename(Point = Position) %>%
  select(Transect, Point, Plot, Year, Group, Species.Name) 


year98=processFormattedData(year98)

finalTransectData=rbind(finalTransectData, year98)
############################################
############################################
#  2001
year01=read.csv(paste(csvFolder,'01in.csv',sep=''), na.strings=c(''))

#Need to force some of the columns into character to work with gather()
year01[,colnames(year01)[-2]]= lapply(year01[,colnames(year01)[-2]], as.character)

year01 = year01 %>% gather(Plot, SpeciesID, -Transect, -Position)

#Extract plot number from old column data, eg '89P16'
year01$Plot=substring(year01$Plot, 5)
#Set year
year01$Year=2001

#Numerous blank/no entries this year. Assuming they are bare ground. Those get read as NA's, so change
#that here to 'blank'. 
year01$SpeciesID[is.na(year01$SpeciesID)]='blank'

speciesList01=read.csv(paste(csvFolder, '2001_species_list_ShawnsEdits.csv',sep=''))

#Assigne species names
year01=left_join(year01, speciesList01, by=c('SpeciesID' = 'Code.Number'))  %>%
  rename(Point = Position) %>%
  select(Transect, Point, Plot, Year, Group, Species.Name) 

year01=processFormattedData(year01)

finalTransectData=rbind(finalTransectData, year01)