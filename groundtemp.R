# Set working directory, load libraries, load data
setwd("/home/zonination/Dropbox/R/Ground Temp")
library(ggplot2)
library(reshape2)
library(viridis)
temp <- read.csv("marktemp2013.csv") # Thanks to /u/larkar for the data https://redd.it/543isa

# Reshape the data into plottable data
temp      <- subset(temp,!is.na(Kolumn5))             # Remove NA's from end of data set
temp      <- temp[,1:19]                              # Remove last 3 columns from set
temp      <- melt(temp,id=1:9)                        # Convert all data into column format
temp$Tid  <- strptime(temp$Tid,"%a %b %d %H:%M:%S %Y")# Convert time into readable format
temp$variable <- as.numeric(substr(temp$variable,3,5))# Convert "J"s into numeric values
temp      <- subset(temp,!is.na(value))               # Remove NA's from value column

# Set heights of the data, since geom_tile() will not yield a full fill
temp$height<-NA
a<-c(0,unique(temp$variable))
for(n in 1:length(a)){b[n-1]<-a[n]-a[n-1]}
b<-c(0,b)

for(n in 1:nrow(temp)){
  temp$height[n]<-b[a==temp$variable[n]]
  print(paste(signif(n*100/nrow(temp),3),"% complete",sep=""))}
rm(n)

ggplot(temp)+
  geom_rect(aes(xmin=Tid,
                ymin=variable-height,
                ymax=variable,
                xmax=Tid+5*5*60,
                fill=value))+
  scale_fill_viridis("Temperature (C)")+
  scale_y_reverse()+
  ggtitle("Ground Temperature at 64.7N, 20.9E")+
  ylab("Depth (cm)")+
  xlab("Date")+
  theme_minimal()
