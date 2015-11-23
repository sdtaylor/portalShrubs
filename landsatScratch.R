library(dplyr)
library(tidyr)
dataFolder='~/data/portal/'

landsatRaw=read.csv('portalLandsatPixelData.csv') %>%
  filter(sensor=='LT5', cfmask==0, year!=1984, year!=2007) %>%
  mutate(ndvi=(sr_band4-sr_band3)/(sr_band4+sr_band3) ) 

#Put the day of year observation into seasonal bins
bins=c(0, 100, 200, 280, 366)
binNames=data.frame(doyBin=c('(0,100]', '(100,200]','(200,280]','(280,366]'), season=c('Spring','Summer','Fall','Winter'))
landsatRaw$doyBin=cut(landsatRaw$doy, bins)
landsatRaw=merge(landsatRaw, binNames, all.x=TRUE, all.y=FALSE)

landsatRaw = landsatRaw %>%
  select(-X, -sensor, -cfmask, -sr_band6, -doy, -doyBin) %>%
  group_by(season, Plot, year) %>%
  summarize_each(funs(mean))

landsatRaw = landsatRaw %>%
  gather(band, value, -Plot, -year, -season) %>%
  mutate(season_band=paste(season, band, sep='_')) %>%
  select(-band, -season) %>%
  spread(season_band, value)
  
###################################################
#carried over from processRawTranssectData.R
shrubs=bind_rows(year92, year89, year95) %>%
  filter(Group=='Shrub') %>%
  spread(Group, cover)

shrubs$Plot=as.integer(shrubs$Plot)

data=full_join(shrubs, landsatRaw, by=c('Plot','Year' = 'year')) %>%
  filter(Year==1989 | Year==1992 | Year==1995)

##################################################

rf=randomForest(Shrub~., data=select(data, -Plot, -Year))


lm1=lm(Shrub~., data=select(data, -Plot, -Year))
aic=step(lm1, direction='both')
summary(aic)

cvScores=data.frame()
for(i in 1:nrow(data)){
  #model=lm(log(Shrub)~., data=select(data, -Plot, -Year) %>% slice(-i))
  #model=step(model, direction='both')
  model=randomForest(log(Shrub)~., data=select(data, -Plot, -Year) %>% slice(-i), na.action=na.omit)
  y_pred=predict(model, newdata=slice(data, i))
  cvScores=rbind(cvScores, c(data$Shrub[i], exp(y_pred)))
}
colnames(cvScores)=c('Actual','Predicted')
with(cvScores, plot(Predicted~Actual))
abline(0,1)
##################################################
valuesCols=colnames(landsatRaw)[-1:-2]
data=landsatRaw[,valuesCols]
data=scale(data)

d=dist(data, method='euclidean')
fit = hclust(d, method='ward')
plot(fit)
group=cutree(fit, k=15)
