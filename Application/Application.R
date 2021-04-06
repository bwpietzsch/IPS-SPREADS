# create-simulation-areas -------------------------------------------------

# load required libraries
library("rgdal")
library("raster")
library("maptools")

# set working directory accordingly
setwd("/home/bruno/Nextcloud/Promotion/010-Paper/002-IPS-SPREADS/003-application/")

# import all data needed
PAS <- raster("~/Nextcloud/Promotion/014-GIS/PAS/PAS.asc")
firstwave16 <- raster("~/Nextcloud/Promotion/014-GIS/temperature-and-radiation/resulting-rasters/2016-firstwave.asc")
firstwave17 <- raster("~/Nextcloud/Promotion/014-GIS/temperature-and-radiation/resulting-rasters/2017-firstwave.asc")
firstwave18 <- raster("~/Nextcloud/Promotion/014-GIS/temperature-and-radiation/resulting-rasters/2018-firstwave.asc")
inf <- raster("~/Nextcloud/Promotion/012-Daten/NLP-Sachsen/GESAMT/infestations.tif")
rborder <- raster("/home/bruno/Nextcloud/Promotion/012-Daten/NLP-Sachsen/base-border-nlp.tif")
DGdiff <- raster("/home/bruno/Nextcloud/Promotion/012-Daten/NLP-Sachsen/GESAMT/daten_August/ndom/ndom5.tif")
hostcapacity <- raster("/home/bruno/Nextcloud/Promotion/014-GIS/capacity/hostcapacity.asc")
sourcecapacity <- raster("/home/bruno/Nextcloud/Promotion/014-GIS/capacity/sourcecapacity.asc")

bepoly <- raster("~/Nextcloud/Promotion/014-GIS/proportion/poly.asc")
bepoly <- crop(bepoly,PAS)

# set cells with infestation to correct value for IPS-SPREADS
# 1 = beetle source, 3 = removed trees due to infestations from the years before,
# 0 = no implication
inf15 <- inf
inf15[inf15 != 2015] <- 0
inf15[inf15 == 2015] <- 1
inf15[is.na(inf15)] <- 0

inf16 <- inf
inf16[inf16 == 2015] <- 3
inf16[inf16 == 2016] <- 1
inf16[inf16 == 2017] <- 0
inf16[is.na(inf16)] <- 0

inf17 <- inf
inf17[inf17 != 2017] <- 3
inf17[inf17 == 2017] <- 1
inf17[is.na(inf17)] <- 0

# set cells outside nlp to inf = 4
rborder[rborder == 0] <- 4
rborder[rborder == 1] <- 0

inf15 <- inf15 + rborder
inf16 <- inf16 + rborder
inf17 <- inf17 + rborder

rm(inf,rborder)

# create list with all data
.dd <- lapply(ls(), get)

# correctly name data inside the list
names(.dd) <- objects()

# remove all objects short of data list
rm(list=ls())

# retrieve data list
dl <- .dd

# delete hidden data list
rm(.dd)

# import coordinates of simulation area mids
dcoords <- read.csv("area-coords.csv")

# area A
da <- lapply(dl,function(x){crop(x,extent(c(dcoords[1,2]-500,dcoords[1,2]+500,dcoords[1,3]-500,dcoords[1,3]+500)))})

# area B
db <- lapply(dl,function(x){crop(x,extent(c(dcoords[2,2]-500,dcoords[2,2]+500,dcoords[2,3]-500,dcoords[2,3]+500)))})

# area C
dc <- lapply(dl,function(x){crop(x,extent(c(dcoords[3,2]-500,dcoords[3,2]+500,dcoords[3,3]-500,dcoords[3,3]+500)))})

# area D
dd <- lapply(dl,function(x){crop(x,extent(c(dcoords[4,2]-500,dcoords[4,2]+500,dcoords[4,3]-500,dcoords[4,3]+500)))})

# area E
de <- lapply(dl,function(x){crop(x,extent(c(dcoords[5,2]-500,dcoords[5,2]+500,dcoords[5,3]-500,dcoords[5,3]+500)))})

# # plot localization of areas inside national park
library("rosm")
library("prettymapr")
library("OpenStreetMap")

# create extents of research sites
ea <- c(dcoords[1,2]-500,dcoords[1,2]+500,dcoords[1,3]-500,dcoords[1,3]+500)
eb <- c(dcoords[2,2]-500,dcoords[2,2]+500,dcoords[2,3]-500,dcoords[2,3]+500)
ec <- c(dcoords[3,2]-500,dcoords[3,2]+500,dcoords[3,3]-500,dcoords[3,3]+500)
ed <- c(dcoords[4,2]-500,dcoords[4,2]+500,dcoords[4,3]-500,dcoords[4,3]+500)
ee <- c(dcoords[5,2]-500,dcoords[5,2]+500,dcoords[5,3]-500,dcoords[5,3]+500)

nlpsize <- readOGR("/home/bruno/Nextcloud/Promotion/012-Daten/NLP-Sachsen/GESAMT/Zonierung_2017.shp")
testd <- rgeos::gUnaryUnion(nlpsize,id=nlpsize$Flaeche,checkValidity = F)
testd <- spTransform(testd,CRSargs(CRS("+init=epsg:3857")))

# plot nlp and both sites
pdf("figures/APP-locations.pdf",width=8,height=4)
prettymap({
  osm.plot(dl$bepoly,project=T,forcedownload = F,res=800,stoponlargerequest = F,zoomin=-2)
  
  plot(testd,add=T,border="green4")
  
  osm.lines(c(ea[1],ea[2]),c(ea[3],ea[3]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(ea[1],ea[1]),c(ea[3],ea[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(ea[1],ea[2]),c(ea[4],ea[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(ea[2],ea[2]),c(ea[3],ea[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.text(c(ea[1]+500),c(ea[3]+500),labels="A",epsg = 25833,toepsg = 3857,col="black",cex=1.4)
  
  osm.lines(c(eb[1],eb[2]),c(eb[3],eb[3]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(eb[1],eb[1]),c(eb[3],eb[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(eb[1],eb[2]),c(eb[4],eb[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(eb[2],eb[2]),c(eb[3],eb[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.text(c(eb[1]+500),c(eb[3]+500),labels="B",epsg = 25833,toepsg = 3857,col="black",cex=1.4)
  
  osm.lines(c(ec[1],ec[2]),c(ec[3],ec[3]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(ec[1],ec[1]),c(ec[3],ec[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(ec[1],ec[2]),c(ec[4],ec[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(ec[2],ec[2]),c(ec[3],ec[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.text(c(ec[1]+500),c(ec[3]+500),labels="C",epsg = 25833,toepsg = 3857,col="black",cex=1.4)
  
  osm.lines(c(ed[1],ed[2]),c(ed[3],ed[3]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(ed[1],ed[1]),c(ed[3],ed[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(ed[1],ed[2]),c(ed[4],ed[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(ed[2],ed[2]),c(ed[3],ed[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.text(c(ed[1]+500),c(ed[3]+500),labels="D",epsg = 25833,toepsg = 3857,col="black",cex=1.4)
  
  osm.lines(c(ee[1],ee[2]),c(ee[3],ee[3]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(ee[1],ee[1]),c(ee[3],ee[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(ee[1],ee[2]),c(ee[4],ee[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.lines(c(ee[2],ee[2]),c(ee[3],ee[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
  osm.text(c(ee[1]+500),c(ee[3]+500),labels="E",epsg = 25833,toepsg = 3857,col="black",cex=1.4)
}, drawbox = T, drawarrow = T
)
dev.off()

# plot site A
# pdf("figures/APP-A-OSM.pdf",width=4,height=3)
# prettymap({
#   osm.plot(da$PAS,project=T,forcedownload = F,res=800,stoponlargerequest = F,zoomin=-2)
#   osm.lines(c(ea[1],ea[2]),c(ea[3],ea[3]),epsg = 25833,toepsg = 3857,col="black",cex=1)
#   osm.lines(c(ea[1],ea[1]),c(ea[3],ea[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
#   osm.lines(c(ea[1],ea[2]),c(ea[4],ea[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
#   osm.lines(c(ea[2],ea[2]),c(ea[3],ea[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
# }, drawbox = T, drawarrow = T
# )
# dev.off()

# # plot site B
# pdf("figures/APP-B-OSM.pdf",width=4,height=3)
# prettymap({
#   osm.plot(db$PAS,project=T,forcedownload = F,res=800,stoponlargerequest = F,zoomin=-2)
#   osm.lines(c(eb[1],eb[2]),c(eb[3],eb[3]),epsg = 25833,toepsg = 3857,col="black",cex=1)
#   osm.lines(c(eb[1],eb[1]),c(eb[3],eb[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
#   osm.lines(c(eb[1],eb[2]),c(eb[4],eb[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
#   osm.lines(c(eb[2],eb[2]),c(eb[3],eb[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
# }, drawbox = T, drawarrow = T
# )
# dev.off()
# 
# # plot site C
# pdf("figures/APP-C-OSM.pdf",width=4,height=3)
# prettymap({
#   osm.plot(dc$PAS,project=T,forcedownload = F,res=800,stoponlargerequest = F,zoomin=-2)
#   osm.lines(c(ec[1],ec[2]),c(ec[3],ec[3]),epsg = 25833,toepsg = 3857,col="black",cex=1)
#   osm.lines(c(ec[1],ec[1]),c(ec[3],ec[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
#   osm.lines(c(ec[1],ec[2]),c(ec[4],ec[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
#   osm.lines(c(ec[2],ec[2]),c(ec[3],ec[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
# }, drawbox = T, drawarrow = T
# )
# dev.off()
# 
# # plot site D
# pdf("figures/APP-D-OSM.pdf",width=4,height=3)
# prettymap({
#   osm.plot(dd$PAS,project=T,forcedownload = T,res=800,stoponlargerequest = F,zoomin=-2)
#   osm.lines(c(ed[1],ed[2]),c(ed[3],ed[3]),epsg = 25833,toepsg = 3857,col="black",cex=1)
#   osm.lines(c(ed[1],ed[1]),c(ed[3],ed[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
#   osm.lines(c(ed[1],ed[2]),c(ed[4],ed[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
#   osm.lines(c(ed[2],ed[2]),c(ed[3],ed[4]),epsg = 25833,toepsg = 3857,col="black",cex=1)
# }, drawbox = T, drawarrow = T
# )
# dev.off()

# remove obsolete data
rm(dcoords,dl,ea,eb,ec,ed,nlpsize,testd)

####
# EXPORT
####

# A
attach(da)
setwd("/home/bruno/Nextcloud/Promotion/010-Paper/002-IPS-SPREADS/003-application/input/A/")
writeRaster(inf15,"inf.asc",overwrite=T)
writeRaster(bepoly,"poly.asc",overwrite=T)
writeRaster(firstwave16,"firstwave.asc",overwrite=T)
writeRaster(hostcapacity,"hostcapacity.asc",overwrite=T)
writeRaster(PAS,"PAS.asc",overwrite=T)
writeRaster(sourcecapacity,"sourcecapacity.asc",overwrite=T)
writeRaster(DGdiff,"tree-height.asc",overwrite=T)
detach(da)

# B
attach(db)
setwd("../B/")
writeRaster(inf15,"inf.asc",overwrite=T)
writeRaster(bepoly,"poly.asc",overwrite=T)
writeRaster(firstwave18,"firstwave.asc",overwrite=T)
writeRaster(hostcapacity,"hostcapacity.asc",overwrite=T)
writeRaster(PAS,"PAS.asc",overwrite=T)
writeRaster(sourcecapacity,"sourcecapacity.asc",overwrite=T)
writeRaster(DGdiff,"tree-height.asc",overwrite=T)
detach(db)

# C
attach(dc)
setwd("../C/")
writeRaster(inf17,"inf.asc",overwrite=T)
writeRaster(bepoly,"poly.asc",overwrite=T)
writeRaster(firstwave18,"firstwave.asc",overwrite=T)
writeRaster(hostcapacity,"hostcapacity.asc",overwrite=T)
writeRaster(PAS,"PAS.asc",overwrite=T)
writeRaster(sourcecapacity,"sourcecapacity.asc",overwrite=T)
writeRaster(DGdiff,"tree-height.asc",overwrite=T)
detach(dc)

# D
attach(dd)
setwd("../D/")
writeRaster(inf17,"inf.asc",overwrite=T)
writeRaster(bepoly,"poly.asc",overwrite=T)
writeRaster(firstwave18,"firstwave.asc",overwrite=T)
writeRaster(hostcapacity,"hostcapacity.asc",overwrite=T)
writeRaster(PAS,"PAS.asc",overwrite=T)
writeRaster(sourcecapacity,"sourcecapacity.asc",overwrite=T)
writeRaster(DGdiff,"tree-height.asc",overwrite=T)
detach(dd)

# E
attach(de)
setwd("../E/")
writeRaster(inf17,"inf.asc",overwrite=T)
writeRaster(bepoly,"poly.asc",overwrite=T)
writeRaster(firstwave18,"firstwave.asc",overwrite=T)
writeRaster(hostcapacity,"hostcapacity.asc",overwrite=T)
writeRaster(PAS,"PAS.asc",overwrite=T)
writeRaster(sourcecapacity,"sourcecapacity.asc",overwrite=T)
writeRaster(DGdiff,"tree-height.asc",overwrite=T)
detach(de)

rm(list=ls())
dev.off()

# combine data parts ------------------------------------------------------

# set working directory accordingly
setwd("/home/bruno/Nextcloud/Promotion/010-Paper/002-IPS-SPREADS/003-application/output/")

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

# remove unnecessary columns
df <- df[,-c(1,5)]

# calculate sum of damaged trees outside the nlp
df$ndam <- df$nadj + df$ncut2

# rename column 1
colnames(df)[1] <- "site"

# define levels of research sites
levels(df$site) <- c("site A","site B","site C","site D","site E")

# export result
write.csv(df,"../results.csv",row.names = F)

# delete everything
rm(list=ls())

# plot ndam ----------------------------------------------------------------

# set working directory accordingly
setwd("/home/bruno/Nextcloud/Promotion/010-Paper/002-IPS-SPREADS/003-application/")

# import results
df <- read.csv("results.csv")

# df$sanitation.fellingA <- as.factor(df$sanitation.fellingA)
# df$sanitation.fellingB <- as.factor(df$sanitation.fellingB)

# levels(df$sanitation.fellingA) <- c("0 % felling NLP","25 % felling NLP","50 % felling NLP","75 % felling NLP","100 % felling NLP")
# levels(df$sanitation.fellingB) <- c("0 % felling ADJ","25 % felling ADJ","50 % felling ADJ","75 % felling ADJ","100 % felling ADJ")

# df$sanitation.fellingB <- factor(df$sanitation.fellingB,
                                 # levels = c("100 % felling ADJ","75 % felling ADJ","50 % felling ADJ","25 % felling ADJ","0 % felling ADJ"))

# levels(df$site) <- c("A","B","C","D","E")

# load needed libraries
library("ggplot2")

# ggplot(df,aes(x=site,y=ndam)) +
#   geom_violin(trim=F) + 
#   stat_summary(fun='mean', geom='point', size=0.5, col="red") +
#   facet_grid(sanitation.fellingB~sanitation.fellingA) +
#   theme_bw() +
#   theme(axis.text.x = element_text(colour="black"),
#         axis.text.y = element_text(colour="black")) +
#   labs(x="research site",y="trees killed outside nlp [n]")
# 
# ggsave("figures/APP-boxplot.pdf",width=10,height=9)

df2 <- aggregate(ndam ~ sanitation.fellingA+sanitation.fellingB+site,df,mean)
df2 <- df2[df2$sanitation.fellingA==0 & df2$sanitation.fellingB==0,c(3,4)]

df2$nsource <- c(35,24,45,53,63)
df2$beetles <- c(457571,393372,508346,489970,861765)
df2$distance <- c(192.10,128.55,149.15,29.15,152.30)
df2$primADJ <- c(3.99,3.27,4.76,5.02,3.87)
df2$primNLP <- c(4.10,4.32,4.15,3.28,3.26)

df2 <- df2[,-1]

cor(df2)[1,2:6]

dl <- list()

for (i in 2:6) {
  dl[[i-1]] <- lm(df2[,1]~df2[,i])
}

names(dl) <- colnames(df2)[2:6]

lapply(dl,function(x){shapiro.test(resid(x))})

library("car")
lapply(dl,ncvTest)

lapply(dl,summary)

lapply(dl,anova)


library("reshape2")

df3 <- melt(df2[,-1])

df3$ndam <- rep(df2$ndam,5)

levels(df3$variable) <- c("beetle sources [n]","beetles [n]","distance to ADJ forest [m]","attractiveness of ADJ forest",
                          "attractiveness of NLP forest")

ggplot(df3,aes(x=value,y=ndam)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~variable,scales="free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black")) +
  labs(x="",y="trees killed outside nlp [n]")


ggsave("../000-manuscript/figures/APP-ndam.pdf",width=8,height=6)

rm(list=ls())
dev.off()

# plot effect --------------------------------------------------------------

# set working directory accordingly
setwd("/home/bruno/Nextcloud/Promotion/010-Paper/002-IPS-SPREADS/003-application/")

# import results
df <- read.csv("results.csv")

df$sanitation.fellingA <- as.factor(df$sanitation.fellingA)
df$sanitation.fellingB <- as.factor(df$sanitation.fellingB)

# calculate means of repetitions
ddf <- aggregate(ndam~site+sanitation.fellingA+sanitation.fellingB,df,mean)
dddf <- aggregate(ndam~sanitation.fellingA+sanitation.fellingB,df,mean)

dddf$site = "mean"

ddf <- rbind(ddf,dddf)

df <- split(ddf,f=list(ddf$site))

df2 <- data.frame()

for (i in 1:length(df)) {
  a <- max(df[[i]]$ndam)
  df[[i]]$ndam <- (1 - (df[[i]]$ndam / a)) * 100
  rm(a)
  df2 <- rbind(df2,df[[i]])
}

levels(df2$sanitation.fellingB) <- c("0","25","50","75","100")

library("ggplot2")

# plot effectiveness
ggplot(df2,aes(x=sanitation.fellingA,y=sanitation.fellingB)) +
  geom_tile(aes(fill = ndam),col="black",na.rm = T) +
  geom_text(aes(label = round(ndam, 0)),na.rm=T,colour="black") +
  scale_fill_gradient(low = "white", high = "#3A7EAB",name="damage \nreduction \n[%]") +
  facet_wrap(~site) +
  theme_bw() +
  theme(axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"),
        legend.position = "top") +
  labs(x="sanitation felling [%] NLP",y="sanitation felling [%] ADJ")

ggsave("../000-manuscript/figures/APP-effect.pdf",width=9,height=6)

dev.off()
rm(list=ls())
