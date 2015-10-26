library(raster)
library(rgdal)
landsatBaseFolder='~/data/portal/'

lsFile='/home/shawn/foo/LT50340381994048XXX03_sr_ndvi.tif'
pointsFile=readOGR('/home/shawn/projects/portalShrubs/gisData', 'plotLandsatPoints')

rasterFile=raster(lsFile)

x=data.frame(cbind(unlist(extract(rasterFile, pointsFile)), pointsFile$Plot))

#Change these two vars when running on hipergator
dataDir='~/data/portal/Landsat8/test/'
tempParentDir='/tmp/' 
fileList=list.files(dataDir)

imagePrefixes=c('cvmask','sr_band1','sr_band2','sr_band3','sr_band4','sr_band5','sr_band6','sr_band7')

processImage = function(imageFileName, toProcess) {
  #create a temporary directory and untar the image into it
  tempDir=paste(tempParentDir, strsplit(imageFileName, '.', fixed=TRUE)[[1]][1] , sep='')
  dir.create(tempDir)
  untar(paste(dataDir, imageFileName, sep=''), exdir=tempDir)
  
  #Get list of files to extract data from and process them. these will be all the tif files
  tifFiles=list.files(tempDir, '*tif')
  for(thisTif in tifFiles){
    #The name for this file to be 
  }
}