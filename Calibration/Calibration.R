# combine data parts ------------------------------------------------------

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

df <- read.csv("results.csv")

dl <- split(df,f=df$path.to.input)

df <- data.frame()

for (i in 1:length(dl)) {df <- rbind(df,dl[[i]][which.min(dl[[i]]$e),])}

df

write.table(df,"best.csv",row.names = F, sep=" & ",eol=" \\\\\ \n",dec=",")

rm(list=ls())

# parameter influences --------------------------------------------------------

d <- read.csv("results.csv")

levels(d$path.to.input) <- c("Area A","Area B","Area C","Area D")

d$generations <- as.factor(d$generations)

d <- d[d$e<750,]

library("ggplot2")

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
