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
#基于分组数据绘制分组直方图

ggplot(birthwt,aes(x=bwt))+geom_histogram(fill="white",colour="black")+facet_grid(smoke~.)
birthwt
ggplot(birthwt,aes(x=bwt))+geom_histogram(fill="white",colour="black")+facet_grid(race~.)
#改变分组数据
birthwt1<-birthwt
birthwt1$smoke<-factor(birthwt1$smoke)
levels(birthwt1$smoke)
#重置标签
birthwt1$smoke<-revalue(birthwt1$smoke,c("0"="No Smoke","1"="Smoke"))
ggplot(birthwt1,aes(x=bwt))+geom_histogram(fill="white",colour="black")+facet_grid(smoke~.)
ggplot(birthwt,aes(x=bwt))+geom_histogram(fill="white",colour="black")+facet_grid(race~.)
#单独设置y轴标度
ggplot(birthwt,aes(x=bwt))+geom_histogram(fill="white",colour="black")+facet_grid(race~.,scales = "free")
#另一种分组方法，把分组变量映射给fill,没有position = "identity"，直方图会被堆积
ggplot(birthwt1,aes(x=bwt,fill=smoke))+geom_histogram(position = "identity",alpha=0.4)
#绘制密度曲线
ggplot(faithful,aes(x=waiting))+geom_density()
ggplot(faithful,aes(x=waiting))+geom_line(stat = "density")+expand_limits(y=0)
#快速绘制未在数据框内的数据图像
w<-faithful$waiting
ggplot(NULL,aes(x=w))+geom_density()
#设置密度曲线的带宽，使用adjust参数
ggplot(faithful,aes(x=waiting))+geom_line(stat = "density",adjust=0.25,colour="red")+geom_line(stat = "density")+geom_line(stat = "density",adjust=2,colour="blue")
#手动设定X轴的范围,使用xlim函数
ggplot(faithful,aes(x=waiting))+geom_density(fill="blue",alpha=0.2)+xlim(35,105)
#设置y=..density..使密度曲线与直方图的标度相对
ggplot(faithful,aes(x=waiting,y=..density..))+geom_histogram(fill="cornsilk",colour="grey60",size=0.2)+geom_density()+xlim(35,105)
#基于分组数据绘制分组密度曲线
str(birthwt1)
ggplot(birthwt1,aes(x=bwt,colour=smoke))+geom_density()
ggplot(birthwt1,aes(x=bwt,fill=smoke))+geom_density(alpha=0.3)
#可以使用分面
ggplot(birthwt1,aes(x=bwt))+geom_density(fill="blue",alpha=0.3)+facet_grid(smoke~.)
#加上直方图
ggplot(birthwt1,aes(x=bwt,y=..density..))+geom_histogram(binwidth = 200,fill="cornsilk",colour="grey60",size=0.2)+geom_density()+facet_grid(smoke~.)
#绘制频数多边形
ggplot(faithful,aes(x=waiting))+geom_freqpoly()
ggplot(faithful,aes(x=waiting))+geom_freqpoly(binwidth=4)
#绘制箱线图
ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_boxplot()
#修改箱线图的宽度
ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_boxplot(width=0.5)
#修改异常点的大小和点形
ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_boxplot(outlier.size = 1.5,outlier.shape = 21)
#单组数据箱线图
ggplot(birthwt,aes(x=1,y=bwt))+geom_boxplot()+scale_x_continuous(breaks = NULL)+theme(axis.title.x = element_blank())
#向箱线图添加槽口
ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_boxplot(notch = TRUE)
ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_boxplot(notch = FALSE)
#向箱线图添加均值
ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_boxplot()+stat_summary(fun.y = "mean",geom = "point",shape=23,size=3,fill="white")
#绘制小提琴图
p<-ggplot(heightweight,aes(x=sex,y=heightIn))
p+geom_violin()
#添加箱线图
p+geom_violin(fill="red",alpha=0.4)+geom_boxplot(width=0.1,fill="blue",alpha=0.3,outlier.colour = NA)+stat_summary(fun.y = median,geom = "point",fill="white",shape=21,size=2.5)
#设置保留尾部
p+geom_violin(fill="lightblue",alpha=0.4,trim=FALSE)
#校准小提琴图的面积
p+geom_violin(fill="lightblue",alpha=0.4,trim=FALSE,scale = "count")
#调整平滑程度
p+geom_violin(fill="lightblue",alpha=0.4,trim=FALSE,scale = "count",adjust=2)
#绘制Wilkinson点图
countries2009<-subset(countries,Year==2009&healthexp>2000)
p<-ggplot(countries2009,aes(x=infmortality))
p+geom_dotplot()
#移除无意义的Y轴
p+geom_dotplot(binwidth = 0.25)+geom_rug()+scale_y_continuous(breaks = NULL)+theme(axis.title.y = element_blank())
#固定间距的分组
p+geom_dotplot(binwidth = 0.25,method = "histodot")+geom_rug()+scale_y_continuous(breaks = NULL)+theme(axis.title.y = element_blank())
#中心堆叠
p+geom_dotplot(binwidth = 0.25,stackdir = "center")+scale_y_continuous(breaks = NULL)+theme(axis.title.y = element_blank())
p+geom_dotplot(binwidth = 0.25,stackdir = "centerwhole")+scale_y_continuous(breaks = NULL)+theme(axis.title.y = element_blank())
#基于分组数据绘制分组点图
e<-ggplot(heightweight,aes(x=sex,y=heightIn))
e+geom_dotplot(binaxis = "y",binwidth = 0.5,stackdir = "center")
#将点图叠在箱线图上
e+geom_boxplot(outlier.colour = NA,width=0.4)+geom_dotplot(binaxis = "y",binwidth = 0.5,stackdir = "center",fill=NA)
#将点图放置于箱线图旁边,离散型变量绘制点图，箱线图必须指定分组
e+geom_boxplot(aes(x=as.numeric(sex)+0.3,group=sex),width=.25)+
  geom_dotplot(aes(x=as.numeric(sex)-0.3,group=sex),binaxis = "y",binwidth = .5,stackdir = "center")+
  scale_x_continuous(breaks = 1:nlevels(heightweight$sex),labels = levels(heightweight$sex))
#绘制二维数据的密度图
p<-ggplot(faithful,aes(x=eruptions,y=waiting))
p+geom_point()+stat_density2d()
#将密度估计映射给填充色
p+stat_density2d(aes(fill=..density..),geom="raster",contour = FALSE)
#将密度映射给透明度 ,raster,tile代表栅格与瓦片
p+geom_point()+stat_density2d(aes(alpha=..density..),geom="tile",contour = FALSE)
#设置带宽
p+stat_density2d(aes(fill=..density..),geom="raster",contour = FALSE,h=c(.5,5))

























