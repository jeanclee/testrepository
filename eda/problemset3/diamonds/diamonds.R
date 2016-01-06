library(ggplot2)
data(diamonds)

install.packages('gridExtra')
library(gridExtra)

library(tidyr)
library(dplyr)

getwd()
setwd('Personal/datascience')

### diamonds
summary(diamonds)

?diamonds
str(diamonds)

### price histogram
qplot(x=price, data=diamonds, main="Diamond prices")
ggsave('priceHistogram.png')

summary(diamonds$price)

### diamond counts
length(subset(diamonds$price,diamonds$price<500))
length(subset(diamonds$price,diamonds$price<250))
length(subset(diamonds$price,diamonds$price>=15000))

### cheaper diamonds

qplot(x=price, data=diamonds) +
  scale_x_continuous(limits=c(0,2500))

qplot(x=price, data=diamonds, binwidth=25, color=I('black'), fill=I('#F79420'), main="Cheaper diamonds")  +  
  scale_x_continuous(limits=c(500,1000), breaks=seq(500,1000,50))

ggsave('cheaperdiamonds_priceHistogram.png')

## price by cut histograms
qplot(x=price, data=diamonds, main="Diamond price by cut") +
  facet_wrap(~cut, ncol=5)

ggsave('bycut_priceHistogram.png')

## price by cut
by(diamonds$price,diamonds$cut,max)
by(diamonds$price,diamonds$cut,min)
by(diamonds$price,diamonds$cut,median)

## scales and multiple histograms
qplot(x=price, data=diamonds, main="Diamond price by cut 2") +
  facet_wrap(~cut, ncol=5, scales="free_y")

ggsave('bycutrescaled_priceHistogram.png')

## price per carat by cut
qplot(x=price/carat, data=diamonds, main="Price per carat by cut") +
  facet_wrap(~cut, ncol=5, scales="free_y") +
  scale_x_log10()

ggsave('bycut_pricepercaratHistogram.png')

## price box plots  --> by cut, clarity, and color
bpcut<-qplot(x=cut, y=price,
      data = diamonds, 
      geom='boxplot',
      main='Price by cut')

bpclarity<-qplot(x=clarity, y=price,
      data = diamonds, 
      geom='boxplot',
      main='Price by clarity')

bpcolor<-qplot(x=color, y=price,
      data = diamonds, 
      geom='boxplot',
      main='Price by color')

pdf("priceboxplots.pdf")
grid.arrange(bpcut,bpclarity,bpcolor, ncol=1)
dev.off()

## interquartile range
by(diamonds$price,diamonds$color,summary)

## price per carat box plots by color
qplot(x=color, y=price/carat,
               data = diamonds, 
               geom='boxplot',
      main='Price per carat by color')

ggsave('bycolor_pricepercaratBoxplot.png')

## carat frequency polygon --> using different bin widths and limits
summary(diamonds$carat)

p1 <- qplot(x = carat, data = diamonds, geom='freqpoly')
p2 <- qplot(x = carat, data = diamonds, binwidth=.5, geom='freqpoly')  +   scale_x_continuous(limits = c(0, 3),  breaks = seq(0, 3, .5))
p3 <- qplot(x = carat, data = diamonds, binwidth=.1, geom='freqpoly', ) +   scale_x_continuous(limits = c(0, 3),  breaks = seq(0, 3, .1))
p4 <- qplot(x = carat, data = diamonds, binwidth=.01, geom='freqpoly') +   scale_x_continuous(limits = c(0, 1),  breaks = seq(0,1, .01))
p5 <- qplot(x = carat, data = diamonds, binwidth=.01, geom='freqpoly') +   scale_x_continuous(limits = c(1, 1.2),  breaks = seq(1, 1.2, .01))

pdf("caratfrequencypolygons.pdf")
grid.arrange(p1, p2, p3, p4, p5, ncol=1)
dev.off()

table(diamonds$carat,diamonds$carat>0)

