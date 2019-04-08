# They three systems are the base plotting system, the lattice system, and the ggplot2 system.
library(datasets)

# Base
data(airquality)
with(airquality,{
  plot(Temp,Ozone)
  lines(loess.smooth(Temp,Ozone))
})

hist(airquality$Ozone)

airquality <- transform(airquality,Month=factor(Month))
boxplot(Ozone~Month,airquality,xlab='Month',ylab='Ozone(ppb)')

with(airquality,plot(Wind,Ozone))

data(cars)
with(cars,plot(speed,dist))
title('Speed vs. Stopping distance')
with(cars,plot(speed,dist,main = 'Speed vs. Stopping distance'))

## some important Base Graphics Parameters
# • pch: the plotting symbol (default is open circle)
# • lty: the line type (default is solid line), can be dashed, dotted, etc.
# • lwd: the line width, specified as an integer multiple
# • col: the plotting color, specified as a number, string, or hex code; the colors() function gives you a vector of colors by name
# • xlab: character string for the x-axis label
# • ylab: character string for the y-axis label
# The par() function is used to specify the global graphics parameters that affect all plots in an R session.
# • las: the orientation of the axis labels on the plot
# • bg: the background color
# • mar: the margin size
# • oma: the outer margin size (default is 0 for all sides)
# • mfrow: number of plots per row, column (plots are filled row-wise)
# • mfcol: number of plots per row, column (plots are filled column-wise)
## To see default parameters
par('lty')
## Base Plotting Function
# • lines: add lines to a plot, given a vector of x values and a corresponding vector of y values (or a 2-column matrix); this function just connects the dots
# • points: add points to a plot
# • text: add text labels to a plot using specified x, y coordinates
# • title: add annotations to x, y axis labels, title, subtitle, outer margin
# • mtext: add arbitrary text to the margins (inner or outer) of the plot
# • axis: adding axis ticks/labels

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City"))
with(subset(airquality,Month==5),points(Wind,Ozone,col='red'))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City",type = 'n'))
with(subset(airquality,Month==5),points(Wind,Ozone,col='red'))
with(subset(airquality,Month!=5),points(Wind,Ozone,col='blue'))
legend('topright',pch = 1,col = c('red','blue'),legend = c('May','Other Months'))

## Regression Line
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City",pch=20))
model<-lm(Ozone~Wind,airquality)
abline(model,lwd=2)

## Multi Plot
par(mfrow=c(1,2))
with(airquality,{
  plot(Wind,Ozone,main = 'Ozone and Wind')
  plot(Solar.R,Ozone,main = 'Ozone and Solar Radiation')
})

par(mfrow=c(1,3),mar=c(4,4,2,1),oma=c(0,0,2,0))
with(airquality,{
  plot(Wind,Ozone,main = 'Ozone and Wind')
  plot(Solar.R,Ozone,main = 'Ozone and Solar Radiation')
  plot(Temp,Ozone,main = 'Ozone and Temperature')
  mtext('Ozone and Weather in New Your City',outer = TRUE)
})

## Color
pal <- colorRamp(c('red','blue'))
pal(1)
pal(seq(0,1,len=10))

pal <- colorRampPalette(c('red','yellow'))
pal(2)
pal(10) #Return 10 colors in between red and yellow

rgb(0,0,234,maxColorValue = 255)

library(RColorBrewer)
display.brewer.all()

cols<-brewer.pal(3,'BuGn')
pal<-colorRampPalette(cols)
image(volcano,col = pal(20))

## SmoothScatter()
par(mfrow=c(1,2))
with(airquality,{
  smoothScatter(Wind,Ozone)
  plot(Wind,Ozone)
})
## Transparency
with(airquality,plot(Wind,Temp,pch=20,col=rgb(0,0,0,0.15)))

# Lattice
library(lattice)
state<-data.frame(state.x77,region=state.region)
xyplot(Life.Exp~Income|region,data=state,layout=c(4,1))

# ggplot2
library(ggplot2)
data(mpg)
qplot(displ,hwy,data=mpg)