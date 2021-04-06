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

oresult <- oresult[,c(3,1,2)]

write.table(oresult[oresult$prop>=5,],"anova.txt",row.names = F, sep=" & ",eol=" \\\\\ \n",dec=".")

rm(list=ls())
