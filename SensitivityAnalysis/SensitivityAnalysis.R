# create-test-areas -------------------------------------------------------

# load required libraries
library("rgdal")
library("raster")
library("maptools")
library("rosm")
library("prettymapr")

# set working directory accordingly
setwd("/home/bruno/Nextcloud/Promotion/017-Modelltests/SA/") 

# import required data for whole national park (PAS, stand polygones with spruce proportion,
# delay of first flightwave, infestation pattern as of 2015, host capacity and source capacity)
PAS <- raster("~/Nextcloud/Promotion/014-GIS/PAS/PAS.asc")
bepoly <- raster("~/Nextcloud/Promotion/014-GIS/proportion/poly.asc")
bepoly[is.na(bepoly[])] <- 0 
firstwave <- raster("~/Nextcloud/Promotion/014-GIS/temperature-and-radiation/resulting-rasters/2016-firstwave.asc")
inf <- raster("~/Nextcloud/Promotion/012-Daten/NLP-Sachsen/GESAMT/infestations2015.tif")
inf <- reclassify(inf, cbind(0,+Inf,1),right=FALSE) # set cells with infestation to correct value for IPS-SPREADS
hostcapacity <- raster("~/Nextcloud/Promotion/014-GIS/capacity/hostcapacity.asc")
hostcapacity[is.na(hostcapacity[])] <- 0
sourcecapacity <- raster("~/Nextcloud/Promotion/014-GIS/capacity/sourcecapacity.asc")
sourcecapacity[is.na(sourcecapacity[])] <- 0
DGM10 <- raster("~/Nextcloud/Promotion/012-Daten/NLP-Sachsen/base-raster.tif")
DGdiff <- raster("/home/bruno/Nextcloud/Promotion/012-Daten/NLP-Sachsen/GESAMT/daten_August/ndom/ndom5.tif")
nlpsize <- readOGR("/home/bruno/Nextcloud/Promotion/012-Daten/NLP-Sachsen/GESAMT/Zonierung_2017.shp")
testd <- rgeos::gUnaryUnion(nlpsize,id=nlpsize$Flaeche,checkValidity = F)
rm(nlpsize)

# set parameters of bepoly right
bepoly <- crop(bepoly,DGM10)
origin(bepoly) <- origin(DGM10)
proj4string(bepoly) <- proj4string(DGM10)

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

# get left upper lower corner for the two test regions (A & B) through random sampling

# region A
set.seed(161615)
a <- sampleRandom(dd$DGM10,1,xy=T)

# region B
set.seed(8916)
b <- sampleRandom(dd$DGM10,1,xy=T)

# region C
set.seed(66156875)
cc <- sampleRandom(dd$DGM10,1,xy=T)

# region D
set.seed(10287)
d <- sampleRandom(dd$DGM10,1,xy=T)

# set extent for test regions
aa <- extent(a[1],a[1]+1500,a[2],a[2]+1500)
bb <- extent(b[1],b[1]+1500,b[2],b[2]+1500)
ccc <- extent(cc[1],cc[1]+1500,cc[2],cc[2]+1500)
ddd <- extent(d[1],d[1]+1500,d[2],d[2]+1500)

dda <- lapply(dd,function(x){crop(x,aa)})
ddb <- lapply(dd,function(x){crop(x,bb)})
ddc <- lapply(dd,function(x){crop(x,ccc)})
dddd <- lapply(dd,function(x){crop(x,ddd)})

# correct somewhat false size of bepoly
xmin(dda$bepoly) <- xmin(dda$DGM10)
xmax(dda$bepoly) <- xmax(dda$DGM10)
ymin(dda$bepoly) <- ymin(dda$DGM10)
ymax(dda$bepoly) <- ymax(dda$DGM10)

xmin(ddb$bepoly) <- xmin(ddb$DGM10)
xmax(ddb$bepoly) <- xmax(ddb$DGM10)
ymin(ddb$bepoly) <- ymin(ddb$DGM10)
ymax(ddb$bepoly) <- ymax(ddb$DGM10)

xmin(ddc$bepoly) <- xmin(ddc$DGM10)
xmax(ddc$bepoly) <- xmax(ddc$DGM10)
ymin(ddc$bepoly) <- ymin(ddc$DGM10)
ymax(ddc$bepoly) <- ymax(ddc$DGM10)

xmin(dddd$bepoly) <- xmin(dddd$DGM10)
xmax(dddd$bepoly) <- xmax(dddd$DGM10)
ymin(dddd$bepoly) <- ymin(dddd$DGM10)
ymax(dddd$bepoly) <- ymax(dddd$DGM10)

dd$testd <- spTransform(dd$testd,CRSargs(CRS("+init=epsg:3857")))

# plot result
pdf("/home/bruno/Nextcloud/Promotion/017-Modelltests/SA/SA-Lage.pdf",width=8,height=4)
prettymap({
  osm.plot(dd$testd,project=T,forcedownload = T,res=800,stoponlargerequest = F,zoomin=-2)
  plot(dd$testd,add=T,border="green4")
  osm.lines(c(aa[1],aa[2]),c(aa[3],aa[3]),epsg = 25833,toepsg = 3857,col="red",cex=1)
  osm.lines(c(aa[1],aa[1]),c(aa[3],aa[4]),epsg = 25833,toepsg = 3857,col="red",cex=1)
  osm.lines(c(aa[1],aa[2]),c(aa[4],aa[4]),epsg = 25833,toepsg = 3857,col="red",cex=1)
  osm.lines(c(aa[2],aa[2]),c(aa[3],aa[4]),epsg = 25833,toepsg = 3857,col="red",cex=1)
  osm.text(c(aa[1]+750),c(aa[3]+750),labels = "A",epsg = 25833, toepsg = 3857, col="red")
  osm.lines(c(bb[1],bb[2]),c(bb[3],bb[3]),epsg = 25833,toepsg = 3857,col="blue",cex=1)
  osm.lines(c(bb[1],bb[1]),c(bb[3],bb[4]),epsg = 25833,toepsg = 3857,col="blue",cex=1)
  osm.lines(c(bb[1],bb[2]),c(bb[4],bb[4]),epsg = 25833,toepsg = 3857,col="blue",cex=1)
  osm.lines(c(bb[2],bb[2]),c(bb[3],bb[4]),epsg = 25833,toepsg = 3857,col="blue",cex=1)
  osm.text(c(bb[1]+750),c(bb[3]+750),labels = "B",epsg = 25833, toepsg = 3857, col="blue")
  osm.lines(c(ccc[1],ccc[2]),c(ccc[3],ccc[3]),epsg = 25833,toepsg = 3857,col="purple",cex=1)
  osm.lines(c(ccc[1],ccc[1]),c(ccc[3],ccc[4]),epsg = 25833,toepsg = 3857,col="purple",cex=1)
  osm.lines(c(ccc[1],ccc[2]),c(ccc[4],ccc[4]),epsg = 25833,toepsg = 3857,col="purple",cex=1)
  osm.lines(c(ccc[2],ccc[2]),c(ccc[3],ccc[4]),epsg = 25833,toepsg = 3857,col="purple",cex=1)
  osm.text(c(ccc[1]+750),c(ccc[3]+750),labels = "C",epsg = 25833, toepsg = 3857, col="purple")
  osm.lines(c(ddd[1],ddd[2]),c(ddd[3],ddd[3]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(ddd[1],ddd[1]),c(ddd[3],ddd[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(ddd[1],ddd[2]),c(ddd[4],ddd[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(ddd[2],ddd[2]),c(ddd[3],ddd[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.text(c(ddd[1]+750),c(ddd[3]+750),labels = "D",epsg = 25833, toepsg = 3857, col="black")
}, drawbox = T, drawarrow = T
)
dev.off()

# export / save cropped test region A
attach(dda)
writeRaster(DGdiff,"~/Nextcloud/Promotion/017-Modelltests/SA/data/A/tree-height.asc","ascii",overwrite=T,prj=T)
writeRaster(bepoly,"~/Nextcloud/Promotion/017-Modelltests/SA/data/A/poly.asc","ascii",overwrite=T,prj=T)
writeRaster(firstwave,"~/Nextcloud/Promotion/017-Modelltests/SA/data/A/firstwave.asc","ascii",overwrite=T,prj=T)
writeRaster(PAS,"~/Nextcloud/Promotion/017-Modelltests/SA/data/A/PAS.asc","ascii",overwrite=T,prj=T) 
writeRaster(inf,"~/Nextcloud/Promotion/017-Modelltests/SA/data/A/inf.asc","ascii",overwrite=T,prj=T)
writeRaster(hostcapacity,"~/Nextcloud/Promotion/017-Modelltests/SA/data/A/hostcapacity.asc","ascii",overwrite=T,prj=T)
writeRaster(sourcecapacity,"~/Nextcloud/Promotion/017-Modelltests/SA/data/A/sourcecapacity.asc","ascii",overwrite=T,prj=T)
detach(dda)

# export / save cropped test region B
attach(ddb)
writeRaster(DGdiff,"~/Nextcloud/Promotion/017-Modelltests/SA/data/B/tree-height.asc","ascii",overwrite=T,prj=T)
writeRaster(bepoly,"~/Nextcloud/Promotion/017-Modelltests/SA/data/B/poly.asc","ascii",overwrite=T,prj=T)
writeRaster(firstwave,"~/Nextcloud/Promotion/017-Modelltests/SA/data/B/firstwave.asc","ascii",overwrite=T,prj=T)
writeRaster(PAS,"~/Nextcloud/Promotion/017-Modelltests/SA/data/B/PAS.asc","ascii",overwrite=T,prj=T) 
writeRaster(inf,"~/Nextcloud/Promotion/017-Modelltests/SA/data/B/inf.asc","ascii",overwrite=T,prj=T)
writeRaster(hostcapacity,"~/Nextcloud/Promotion/017-Modelltests/SA/data/B/hostcapacity.asc","ascii",overwrite=T,prj=T)
writeRaster(sourcecapacity,"~/Nextcloud/Promotion/017-Modelltests/SA/data/B/sourcecapacity.asc","ascii",overwrite=T,prj=T)
detach(ddb)

# export / save cropped test region C
attach(ddc)
writeRaster(DGdiff,"~/Nextcloud/Promotion/017-Modelltests/SA/data/C/tree-height.asc","ascii",overwrite=T,prj=T)
writeRaster(bepoly,"~/Nextcloud/Promotion/017-Modelltests/SA/data/C/poly.asc","ascii",overwrite=T,prj=T)
writeRaster(firstwave,"~/Nextcloud/Promotion/017-Modelltests/SA/data/C/firstwave.asc","ascii",overwrite=T,prj=T)
writeRaster(PAS,"~/Nextcloud/Promotion/017-Modelltests/SA/data/C/PAS.asc","ascii",overwrite=T,prj=T) 
writeRaster(inf,"~/Nextcloud/Promotion/017-Modelltests/SA/data/C/inf.asc","ascii",overwrite=T,prj=T)
writeRaster(hostcapacity,"~/Nextcloud/Promotion/017-Modelltests/SA/data/C/hostcapacity.asc","ascii",overwrite=T,prj=T)
writeRaster(sourcecapacity,"~/Nextcloud/Promotion/017-Modelltests/SA/data/C/sourcecapacity.asc","ascii",overwrite=T,prj=T)
detach(ddc)

# export / save cropped test region C
attach(dddd)
writeRaster(DGdiff,"~/Nextcloud/Promotion/017-Modelltests/SA/data/D/tree-height.asc","ascii",overwrite=T,prj=T)
writeRaster(bepoly,"~/Nextcloud/Promotion/017-Modelltests/SA/data/D/poly.asc","ascii",overwrite=T,prj=T)
writeRaster(firstwave,"~/Nextcloud/Promotion/017-Modelltests/SA/data/D/firstwave.asc","ascii",overwrite=T,prj=T)
writeRaster(PAS,"~/Nextcloud/Promotion/017-Modelltests/SA/data/D/PAS.asc","ascii",overwrite=T,prj=T) 
writeRaster(inf,"~/Nextcloud/Promotion/017-Modelltests/SA/data/D/inf.asc","ascii",overwrite=T,prj=T)
writeRaster(hostcapacity,"~/Nextcloud/Promotion/017-Modelltests/SA/data/D/hostcapacity.asc","ascii",overwrite=T,prj=T)
writeRaster(sourcecapacity,"~/Nextcloud/Promotion/017-Modelltests/SA/data/D/sourcecapacity.asc","ascii",overwrite=T,prj=T)
detach(dddd)

rm(list=ls())
# combine data parts ------------------------------------------------------

# set working directory accordingly
setwd("/home/bruno/Nextcloud/Promotion/010-Paper/002-IPS-SPREADS/001-SA/output")

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

# delete obsolete columns
df <- df[,-c(1,2,4,5)]

# round values to 2 digits
df[,c(9:ncol(df))] <- round(df[,c(9:ncol(df))],digits=2)

# export data
write.csv(df,"../results.csv",row.names = F)

# delete everything
rm(list=ls())

# mixed effects linear model ----------------------------------------------

# set working directory accordingly
setwd("/home/bruno/Nextcloud/Promotion/010-Paper/002-IPS-SPREADS/001-SA")

# import data
dd <- read.csv("results.csv",stringsAsFactors = T)

library("nlme")

# rename columns for better readability
colnames(dd) <- c("plot","pd","ma","dr","sw","wd","ws","me","beetle_percentsucc1","mean_flightdist_inf1",
                  "mean_distance_host1","mean_flightdist_all1","mean_traveldist1","mean_airline1","nhost1")

####
# mean_flightdist_inf1
####
m1 <- lme(mean_flightdist_inf1 ~ 1+pd*dr*ma*sw*wd*ws*me,data=dd,random=~1|plot)

g <- anova(m1)
g$prop = round(g$`F-value` / sum(g$`F-value`) * 100,2)
g$parameter <- row.names(g)
row.names(g) <- NULL
g <- subset(g,parameter!="(Intercept)")
g <- g[with(g,order(-g$prop)),]
g <- subset(g,prop>=1)
g <- g[,c(6,5)]

oresult <- g

####
# mean_distance_host1
####
m1 <- lme(mean_distance_host1 ~ 1+pd*dr*ma*sw*wd*ws*me,data=dd,random=~1|plot)

g <- anova(m1)
g$prop = round(g$`F-value` / sum(g$`F-value`) * 100,2)
g$parameter <- row.names(g)
row.names(g) <- NULL
g <- subset(g,parameter!="(Intercept)")
g <- g[with(g,order(-g$prop)),]
g <- subset(g,prop>=1)
g <- g[,c(6,5)]

oresult$result <- c("mean_flightdist_inf1")
g$result <- c("mean_distance_host1")

oresult <- rbind(oresult,g)

####
# beetle_percentsucc1
####
m1 <- lme(beetle_percentsucc1 ~ 1+pd*dr*ma*sw*wd*ws*me,data=dd,random=~1|plot)

g <- anova(m1)
g$prop = round(g$`F-value` / sum(g$`F-value`) * 100,2)
g$parameter <- row.names(g)
row.names(g) <- NULL
g <- subset(g,parameter!="(Intercept)")
g <- g[with(g,order(-g$prop)),]
g <- subset(g,prop>=1)
g <- g[,c(6,5)]

g$result <- c("beetle_percentsucc1")
oresult <- rbind(oresult,g)

####
# nhost1
####
m1 <- lme(nhost1 ~ 1+pd*dr*ma*sw*wd*ws*me,data=dd,random=~1|plot)

g <- anova(m1)
g$prop = round(g$`F-value` / sum(g$`F-value`) * 100,2)
g$parameter <- row.names(g)
row.names(g) <- NULL
g <- subset(g,parameter!="(Intercept)")
g <- g[with(g,order(-g$prop)),]
g <- subset(g,prop>=1)
g <- g[,c(6,5)]

g$result <- c("nhost1")
oresult <- rbind(oresult,g)

# # plot data
library(ggplot2)
ggplot(oresult[oresult$prop>=5,], aes(parameter, result)) +
  geom_tile(aes(fill = prop),na.rm = T) +
  geom_text(aes(label = round(prop, 0)),na.rm=T) +
  scale_fill_gradient(low = "#98C5E3", high = "#3A7EAB",name="variance [%]") +
  labs(x="",y="") +
  theme_bw() +
  scale_x_discrete(labels=c("moveangle","m-angle:swarming:energy","energy","swarming","swarming:w-speed:energy")) +
  theme(axis.text.x = element_text(colour="black",angle = 45, hjust = 1),
        axis.text.y = element_text(colour="black"),
        legend.position = "right")
        #text = element_text(size=14))

ggsave("../000-manuscript/figures/SA-anova.pdf",width=6.5,height=4)

oresult <- oresult[,c(3,1,2)]

write.table(oresult[oresult$prop>=5,],"anova.txt",row.names = F, sep=" & ",eol=" \\\\\ \n",dec=".")

rm(list=ls())
dev.off()

