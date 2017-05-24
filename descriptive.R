library(ggplot2)
library(gcookbook)
library(plyr)
library(MASS)
library(reshape2)

#绘制简单直方图,bins参数代表数据被分成多少组
ggplot(faithful,aes(x=waiting))+geom_histogram(bins = 10,fill="white",colour="black")
#设定组距为5
ggplot(faithful,aes(x=waiting))+geom_histogram(binwidth = 5,fill="white",colour="black")
#将X分为15组
binsize<-diff(range(faithful$waiting))/15
ggplot(faithful,aes(x=waiting))+geom_histogram(binwidth = binsize,fill="white",colour="black")
#设定分组原点，通过boundary参数设定
h<-ggplot(faithful,aes(x=waiting))
h+geom_histogram(binwidth = 8,fill="white",colour="black",boundary=31)
h+geom_histogram(binwidth = 8,fill="white",colour="black",boundary=35)
