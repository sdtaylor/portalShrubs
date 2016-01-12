#Extract pixel data from landsat archive using predefined points from the portal site
#designed to work over many years of imagery + several different sensors

library(raster)
library(rgdal)
library(doParallel)

#If running this on hipergator, use "Rscript weatherDependentGrowthModel.R hipergator" 
args=commandArgs(trailingOnly = TRUE)

#If the 1st argument is na (ie, no argument), then this script is being run inside rstudo
if(is.na(args[1])){
  print('Running locally (probably rstudio)')
  dataDir='~/data/portal/Landsat/'
  finalDataFile='~/projects/portalShrubs/landsatOutput.csv'
  tempParentDir='/tmp/' 
  numProcs=1
  
} else if(args[1]=='local') {
  print('Running locally (probably cli)')
  dataDir='~/data/portal/Landsat/'
  finalDataFile='~/projects/portalShrubs/landsatOutput.csv'
  tempParentDir='/tmp/' 
  numProcs=1

  #If not na, then check to see if the 1st arg means running in the ufl hpc.  
} else if(args[1]=='hipergator') {
  print('Running on hipergator')
  dataDir='/scratch/lfs/shawntaylor/portalLandsat/'
  finalDataFile='/scratch/lfs/shawntaylor/finalData.csv'
  tempParentDir='/tmp/' 
  
  numProcs=32
}

#The shapefile containg points/shapes for each of 24 plots
plotPoints=readOGR('./gisData', 'plotLandsatPoints')


fileList=list.files(dataDir, '*tar.gz')

imageSuffixes=c('cfmask','sr_band1','sr_band2','sr_band3','sr_band4','sr_band5','sr_band6','sr_band7')

processImage = function(imageFileName) {
  #create a temporary directory and untar the image into it
  tempDir=paste(tempParentDir, strsplit(imageFileName, '.', fixed=TRUE)[[1]][1],'/' , sep='')
  dir.create(tempDir)
  untar(paste(dataDir, imageFileName, sep=''), exdir=tempDir)
  
  #Get info for this image. prefix, year, dayofyear
  prefix=list.files(tempDir, '*band1.tif') #Get full file name from the band1 image tif
  prefix=substr(prefix, 1,21) #extract the prefix
  sensor=substr(prefix, 1,3) #sensor/sattelite
  year=substr(prefix, 10,13)
  doy=substr(prefix, 14,16)
  
  #Build matrix with initial data
  imageData=data.frame(Plot=plotPoints$Plot)
  imageData$sensor=sensor
  imageData$year=year
  imageData$doy=doy
  
  #for each image suffix (band1, band2, etc), extract and store data
  for(thisSuffix in imageSuffixes){
    #Put together the full name for this particular suffix
    tifFile=paste(tempDir,prefix,'_',thisSuffix,'.tif', sep='')
    #Some image sets are missing a tif file, so check to see if it 
    #exists before reading it. If it doens't fill in those values with -1.
    if(! file.exists(tifFile)){
      thisTifData=rep(-1, length(plotPoints$Plot))
    } else {
    #Read in raster .tif 
    thisTif=raster(tifFile)
    #extract cell values based off the plotPoints shapefile
    thisTifData=unlist(extract(thisTif, plotPoints))
    }
    #Merge with the full DF for this image and name the columne to the correct band.
    imageData=cbind(imageData, thisTifData)
    colnames(imageData)[ncol(imageData)] = thisSuffix
  }
  unlink(tempDir, recursive=TRUE, force=TRUE)
  return(imageData)
}

#Setup parallel processing
cl=makeCluster(numCores)
registerDoParallel(cl)

#here be parallel (foreach %dopar%) code to process all the images and write a single csv file.
finalData=foreach(fileName = fileList, .combine=rbind, .packages=c('raster','rgdal')) %dopar% {
  processImage(fileName)
}

stopCluster(cl)

write.csv(finalData, finalDataFile)
