# crop-test-areas ---------------------------------------------------------

# load required libraries
library("rgdal")
library("raster")
library("maptools")

# set working directory accordingly
setwd("/home/bruno/Nextcloud/Promotion/010-Paper/002-IPS-SPREADS/002-validation/") 

# import required data for whole national park (PAS, stand polygons with spruce proportion,
# delay of first flightwave, infestation pattern as of 2015, host capacity and source capacity)
PAS <- raster("~/Nextcloud/Promotion/014-GIS/PAS/PAS.asc")
bepoly <- raster("~/Nextcloud/Promotion/014-GIS/proportion/poly.asc")
firstwave16 <- raster("~/Nextcloud/Promotion/014-GIS/temperature-and-radiation/resulting-rasters/2016-firstwave.asc")
firstwave17 <- raster("~/Nextcloud/Promotion/014-GIS/temperature-and-radiation/resulting-rasters/2017-firstwave.asc")
inf <- raster("~/Nextcloud/Promotion/012-Daten/NLP-Sachsen/GESAMT/infestations.tif")
DGM10 <- raster("/home/bruno/Nextcloud/Promotion/012-Daten/NLP-Sachsen/base-raster.tif")
DGM10 <- disaggregate(DGM10,fact=2)

DGdiff <- raster("/home/bruno/Nextcloud/Promotion/012-Daten/NLP-Sachsen/GESAMT/daten_August/ndom/ndom5.tif")

bepoly <- crop(bepoly,DGdiff)
origin(bepoly) <- origin(DGdiff)
proj4string(bepoly) <- proj4string(DGdiff)

inf15 <- reclassify(inf, cbind(2015,2015,1),include.lowest=T) # set cells with infestation to correct value for IPS-SPREADS
inf15 <- reclassify(inf15,cbind(2016,2016,2),include.lowest=T)
inf15 <- reclassify(inf15,cbind(2017,2017,0),include.loswest=T)

inf16 <- reclassify(inf,cbind(2015,2015,3),include.lowest=T)
inf16 <- reclassify(inf16,cbind(2016,2016,1),include.lowest=T)
inf16 <- reclassify(inf16,cbind(2017,2017,2),include.lowest=T)

rm(inf)

hostcapacity <- raster("/home/bruno/Nextcloud/Promotion/014-GIS/capacity/hostcapacity.asc")
sourcecapacity <- raster("/home/bruno/Nextcloud/Promotion/014-GIS/capacity/sourcecapacity.asc")

# create list with all data
.dd <- lapply(ls(), get)

# correctly name data inside the list
names(.dd) <- objects()

# remove all objects short of data list
rm(list=ls())

# retrive data list
dd <- .dd

# delete hidden data list
rm(.dd)

# region A
aa <- lapply(dd,function(x){crop(x,extent(c(439104-250,439104+250,5646327-250,5646327+250)))})

# region B
bb <- lapply(dd,function(x){crop(x,extent(c(451123-250,451123+250,5638697-250,5638697+250)))})

# region C
cc <- lapply(dd,function(x){crop(x,extent(c(449967-250,449967+250,5639254-250,5639254+250)))})

# region D
ddd <- lapply(dd,function(x){crop(x,extent(c(454365-250,454365+250,5639907-250,5639907+250)))})

# correct origin an minimum and maximum cells of bepoly
origin(aa$bepoly) <- origin(aa$DGdiff)
origin(bb$bepoly) <- origin(bb$DGdiff)
origin(cc$bepoly) <- origin(cc$DGdiff)
origin(ddd$bepoly) <- origin(ddd$DGdiff)

xmin(aa$bepoly) <- xmin(aa$DGdiff)
xmax(aa$bepoly) <- xmax(aa$DGdiff)
ymin(aa$bepoly) <- ymin(aa$DGdiff)
ymax(aa$bepoly) <- ymax(aa$DGdiff)

xmin(bb$bepoly) <- xmin(bb$DGdiff)
xmax(bb$bepoly) <- xmax(bb$DGdiff)
ymin(bb$bepoly) <- ymin(bb$DGdiff)
ymax(bb$bepoly) <- ymax(bb$DGdiff)

xmin(cc$bepoly) <- xmin(cc$DGdiff)
xmax(cc$bepoly) <- xmax(cc$DGdiff)
ymin(cc$bepoly) <- ymin(cc$DGdiff)
ymax(cc$bepoly) <- ymax(cc$DGdiff)

xmin(ddd$bepoly) <- xmin(ddd$DGdiff)
xmax(ddd$bepoly) <- xmax(ddd$DGdiff)
ymin(ddd$bepoly) <- ymin(ddd$DGdiff)
ymax(ddd$bepoly) <- ymax(ddd$DGdiff)

####
# EXPORT
####
# A
attach(aa)
setwd("/home/bruno/Nextcloud/Promotion/010-Paper/002-IPS-SPREADS/002-validation/input/A/")
writeRaster(inf15,"inf.asc",prj=T,overwrite=T)
writeRaster(bepoly,"poly.asc",prj=T,overwrite=T)
writeRaster(firstwave16,"firstwave.asc",prj=T,overwrite=T)
writeRaster(hostcapacity,"hostcapacity.asc",prj=T,overwrite=T)
writeRaster(PAS,"PAS.asc",prj=T,overwrite=T)
writeRaster(sourcecapacity,"sourcecapacity.asc",prj=T,overwrite=T)
writeRaster(DGdiff,"tree-height.asc",prj=T,overwrite=T)
detach(aa)

# B
attach(bb)
setwd("/home/bruno/Nextcloud/Promotion/010-Paper/002-IPS-SPREADS/002-validation/input/B/")
writeRaster(inf15,"inf.asc",prj=T,overwrite=T)
writeRaster(bepoly,"poly.asc",prj=T,overwrite=T)
writeRaster(firstwave16,"firstwave.asc",prj=T,overwrite=T)
writeRaster(hostcapacity,"hostcapacity.asc",prj=T,overwrite=T)
writeRaster(PAS,"PAS.asc",prj=T,overwrite=T)
writeRaster(sourcecapacity,"sourcecapacity.asc",prj=T,overwrite=T)
writeRaster(DGdiff,"tree-height.asc",prj=T,overwrite=T)
detach(bb)

# C
attach(cc)
setwd("/home/bruno/Nextcloud/Promotion/010-Paper/002-IPS-SPREADS/002-validation/input/C/")
writeRaster(inf15,"inf.asc",prj=T,overwrite=T)
writeRaster(bepoly,"poly.asc",prj=T,overwrite=T)
writeRaster(firstwave16,"firstwave.asc",prj=T,overwrite=T)
writeRaster(hostcapacity,"hostcapacity.asc",prj=T,overwrite=T)
writeRaster(PAS,"PAS.asc",prj=T,overwrite=T)
writeRaster(sourcecapacity,"sourcecapacity.asc",prj=T,overwrite=T)
writeRaster(DGdiff,"tree-height.asc",prj=T,overwrite=T)
detach(cc)

# D
attach(ddd)
setwd("/home/bruno/Nextcloud/Promotion/010-Paper/002-IPS-SPREADS/002-validation/input/D/")
writeRaster(inf15,"inf.asc",prj=T,overwrite=T)
writeRaster(bepoly,"poly.asc",prj=T,overwrite=T)
writeRaster(firstwave16,"firstwave.asc",prj=T,overwrite=T)
writeRaster(hostcapacity,"hostcapacity.asc",prj=T,overwrite=T)
writeRaster(PAS,"PAS.asc",prj=T,overwrite=T)
writeRaster(sourcecapacity,"sourcecapacity.asc",prj=T,overwrite=T)
writeRaster(DGdiff,"tree-height.asc",prj=T,overwrite=T)
detach(ddd)

rm(list=ls())


# plot localization of research areas inside nlp --------------------------

# set working directory accordingly
sewd("/home/bruno/Nextcloud/Promotion/010-Paper/002-IPS-SPREADS/002-validation/")

library("raster")
library("maptools")
library("rgdal")
library("rosm")
library("prettymapr")
library("rgeos")

# import nlp shape file
nlpsize <- readOGR("/home/bruno/Nextcloud/Promotion/012-Daten/NLP-Sachsen/GESAMT/Zonierung_2017.shp")
nlpsize <- gUnaryUnion(nlpsize,id=nlpsize$Flaeche,checkValidity = F)
nlpsize <- spTransform(nlpsize,CRSargs(CRS("+init=epsg:3857")))

# get extents of research areas
aa <- c(439104-250,439104+250,5646327-250,5646327+250)
bb <- c(451123-250,451123+250,5638697-250,5638697+250)
cc <- c(449967-250,449967+250,5639254-250,5639254+250)
ddd <- c(454365-250,454365+250,5639907-250,5639907+250)

pdf("figures/VAL-location.pdf",width = 8,height = 4)
prettymap({
  osm.plot(nlpsize,project=T,forcedownload = T,res=800,stoponlargerequest = F,zoomin=-2)
  plot(nlpsize,add=T,border="green4")
  osm.lines(c(aa[1],aa[2]),c(aa[3],aa[3]),epsg = 25833,toepsg = 3857,cex=1)
  osm.lines(c(aa[1],aa[1]),c(aa[3],aa[4]),epsg = 25833,toepsg = 3857,cex=1)
  osm.lines(c(aa[1],aa[2]),c(aa[4],aa[4]),epsg = 25833,toepsg = 3857,cex=1)
  osm.lines(c(aa[2],aa[2]),c(aa[3],aa[4]),epsg = 25833,toepsg = 3857,cex=1)
  osm.text(c(aa[1]+250),c(aa[3]+250),labels="A",epsg=25833,toepsg=3857)
  osm.lines(c(bb[1],bb[2]),c(bb[3],bb[3]),epsg = 25833,toepsg = 3857,cex=1)
  osm.lines(c(bb[1],bb[1]),c(bb[3],bb[4]),epsg = 25833,toepsg = 3857,cex=1)
  osm.lines(c(bb[1],bb[2]),c(bb[4],bb[4]),epsg = 25833,toepsg = 3857,cex=1)
  osm.lines(c(bb[2],bb[2]),c(bb[3],bb[4]),epsg = 25833,toepsg = 3857,cex=1)
  osm.text(c(bb[1]+250),c(bb[3]+250),labels="B",epsg=25833,toepsg=3857)
  osm.lines(c(cc[1],cc[2]),c(cc[3],cc[3]),epsg = 25833,toepsg = 3857,cex=1)
  osm.lines(c(cc[1],cc[1]),c(cc[3],cc[4]),epsg = 25833,toepsg = 3857,cex=1)
  osm.lines(c(cc[1],cc[2]),c(cc[4],cc[4]),epsg = 25833,toepsg = 3857,cex=1)
  osm.lines(c(cc[2],cc[2]),c(cc[3],cc[4]),epsg = 25833,toepsg = 3857,cex=1)
  osm.text(c(cc[1]+250),c(cc[3]+250),labels="C",epsg=25833,toepsg=3857)
  osm.lines(c(ddd[1],ddd[2]),c(ddd[3],ddd[3]),epsg = 25833,toepsg = 3857,cex=1)
  osm.lines(c(ddd[1],ddd[1]),c(ddd[3],ddd[4]),epsg = 25833,toepsg = 3857,cex=1)
  osm.lines(c(ddd[1],ddd[2]),c(ddd[4],ddd[4]),epsg = 25833,toepsg = 3857,cex=1)
  osm.lines(c(ddd[2],ddd[2]),c(ddd[3],ddd[4]),epsg = 25833,toepsg = 3857,cex=1)
  osm.text(c(ddd[1]+250),c(ddd[3]+250),labels="D",epsg=25833,toepsg=3857)
  }, drawbox = T, drawarrow = T
)
dev.off()

rm(list=ls())  

# combine data parts ------------------------------------------------------

# set working directory accordingly
setwd("/home/bruno/Nextcloud/Promotion/010-Paper/002-IPS-SPREADS/002-validation/output/")

# get names of all available files
temp <- list.files(pattern=".csv")

# load all files into one list
datalist = lapply(temp, function(x){read.csv(x,skip=6)}) 

# remove file extension
temp <- substr(temp,1,nchar(temp)-4)

# give the data its corresponding names
names(datalist) <- temp

rm(temp) # delete names

# order each data frame according to the run ID
for (i in 1:length(datalist)) {
  datalist[[i]] <- datalist[[i]][order(datalist[[i]]$X.run.number.),]
}

# create empty result data frame
df <- data.frame(NULL)

# rbind all data into result data frame
for (i in 1:length(datalist)) {df <- rbind(df,datalist[[i]])}

# calculate calibration error
df$e <- round((abs(df$nright - df$nshould) / df$nright + abs(df$nright - df$nhost) / df$nright) / 2 * 100,digits=2)

# delete obsolete columns
df <- df[,-c(1,3)]

# export data
write.csv(df,"../results.csv",row.names = F)

# delete everything
rm(list=ls())

# best -----------------------------------------------------------

setwd("/home/bruno/Nextcloud/Promotion/010-Paper/002-IPS-SPREADS/002-validation/")

df <- read.csv("results.csv")

dl <- split(df,f=df$path.to.input)

df <- data.frame()

for (i in 1:length(dl)) {df <- rbind(df,dl[[i]][which.min(dl[[i]]$e),])}

df

write.table(df,"best.csv",row.names = F, sep=" & ",eol=" \\\\\ \n",dec=",")

rm(list=ls())

# parameter influences --------------------------------------------------------

setwd("/home/bruno/Nextcloud/Promotion/010-Paper/002-IPS-SPREADS/002-validation/")

d <- read.csv("results.csv")

levels(d$path.to.input) <- c("Area A","Area B","Area C","Area D")

d$generations <- as.factor(d$generations)

d <- d[d$e<750,]

library("ggplot2")

# generation
ggplot(d,aes(as.factor(generations),e)) +
  geom_violin() +
  stat_summary(fun="mean",col="red",pch=4,size=0.3) +
  facet_grid(~d$path.to.input) +
  theme_bw() +
  theme(axis.text.x = element_text(colour="black",size=8),
        axis.text.y = element_text(colour="black"),
        legend.position = "top") +
  labs(x="beetle generations [n]",y="error [%]")

ggsave("figures/VAL-generations.pdf",width=7,height=2.5)

# windspeed
ggplot(d,aes(windspeed,e,col=generations)) +
  geom_jitter(size=0.5) +
  geom_smooth(size=0.5,span=0.75) +
  facet_grid(~d$path.to.input) +
  theme_bw() +
  theme(axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"),
        legend.position = "top") +
  labs(x="windspeed [m/s]",y="error [%]")

ggsave("figures/VAL-speed.pdf",width=7,height=3)

# winddirection
ggplot(d,aes(winddirection,e,col=generations)) +
  geom_jitter(size=0.5) +
  geom_smooth(size=0.5,span=0.75) +
  facet_grid(~d$path.to.input) +
  theme_bw() +
  theme(axis.text.x = element_text(colour="black",size=8),
        axis.text.y = element_text(colour="black"),
        legend.position = "top") +
  labs(x="winddirection [°]",y="error [%]")

ggsave("figures/VAL-direction.pdf",width=7,height=3)

# energy
ggplot(d,aes(meanenergy,e,col=generations)) +
  geom_point(size=0.5) +
  geom_smooth(size=0.5,span=0.75) +
  facet_grid(~d$path.to.input) +
  theme_bw() +
  theme(axis.text.x = element_text(colour="black",size=8),
        axis.text.y = element_text(colour="black"),
        legend.position = "top") +
  labs(x="beetle energy",y="error [%]")

ggsave("figures/VAL-energy.pdf",width=7,height=3)

# swarming
ggplot(d,aes(swarming,e,col=generations)) +
  geom_point(size=0.5) +
  geom_smooth(size=0.5,span=0.75) +
  facet_grid(~d$path.to.input) +
  theme_bw() +
  theme(axis.text.x = element_text(colour="black",size=8),
        axis.text.y = element_text(colour="black"),
        legend.position = "top") +
  labs(x="swarming [d]",y="error [%]")

ggsave("figures/VAL-swarming.pdf",width=7,height=3)

####
# ANOVA of mixed effects linear model
####

# load library
library("nlme")

# define model
lm0 <- lme(e~1+meanenergy+swarming+winddirection+generations+windspeed,d,random=~1|path.to.input)

# calculate anova
anova(lm0)

dev.off()
rm(list=ls())
