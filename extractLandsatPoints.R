#Extract pixel data from landsat archive using predefined points from the portal site
#designed to work over many years of imagery + several different sensors

library(raster)
library(rgdal)
landsatBaseFolder='~/data/portal/'

lsFile='/home/shawn/foo/LT50340381994048XXX03_sr_ndvi.tif'
plotPoints=readOGR('/home/shawn/projects/portalShrubs/gisData', 'plotLandsatPoints')

rasterFile=raster(lsFile)

x=matrix(cbind(unlist(extract(rasterFile, pointsFile)), pointsFile$Plot), ncol=2)

#Change these two vars when running on hipergator
dataDir='~/data/portal/Landsat8/test/'
tempParentDir='/tmp/' 
fileList=list.files(dataDir)

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
    #The name for this file to be 
    tifFile=paste(tempDir,prefix,'_',thisSuffix,'.tif', sep='')
    
    thisTif=raster(tifFile)
    thisTifData=unlist(extract(thisTif, plotPoints))
    imageData=cbind(imageData, thisTifData)
    colnames(imageData)[ncol(imageData)] = thisSuffix
  }
}