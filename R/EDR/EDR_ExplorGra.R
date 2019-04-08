library(readr)
library(dplyr)
library(maps)

pollution <- read_csv("avgpm25.csv",
    col_types = cols(latitude = col_number(),
      longitude = col_number(),
      pm25 = col_number(),
      region = col_factor(levels = c("east","west"))))

# Are there any counties in the U.S. that exceed the national standard for fine particle pollution?

head(pollution)
str(pollution)

# One Dimension

## Five Number Summary
fivenum(pollution$pm25)
summary(pollution$pm25)

## Boxplot
boxplot(pollution$pm25,col = 'blue')

map('county','california')
with(filter(pollution,pm25>15),points(longitude,latitude))

## Histogram
hist(pollution$pm25,col = 'green')
rug(pollution$pm25)
hist(pollution$pm25,col = 'green',breaks = 100)
rug(pollution$pm25)

## Overlaying features
boxplot(pollution$pm25,col = 'blue')
abline(h=12)

hist(pollution$pm25,col = 'green')
abline(v=12,lwd=2)
abline(v=median(pollution$pm25),col='magenta',lwd=4)

## Barplot
table(pollution$region) %>% barplot(col='wheat')

# Two Dimension

## Multi-Boxplot
boxplot(pm25~region,data = pollution,col='red')
## Multi-Hist
par(mfrow=c(2,1),mar=c(4,4,2,1))
hist(subset(pollution,region=='east')$pm25,col='green')
hist(subset(pollution,region=='west')$pm25,col='green')
## Scatterplots
with(pollution,plot(latitude,pm25,col=region))
abline(h=12,lwd=2,lty=2)
## Multi-Scatter
par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))

library(lattice)
xyplot(pm25~latitude|region,data=pollution)

library(ggplot2)
qplot(latitude,pm25,data=pollution,facets=.~region)

