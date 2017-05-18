library(ggplot2)
library(gcookbook)
library(plyr)

#绘制散点图
ggplot(heightweight,aes(x=ageYear,y=heightIn))+geom_point()
#绘制空心圆
ggplot(heightweight,aes(x=ageYear,y=heightIn))+geom_point(shape=21)
#使用点形和颜色属性对数据进行分组
ggplot(heightweight,aes(x=ageYear,y=heightIn,colour=sex))+geom_point()
ggplot(heightweight,aes(x=ageYear,y=heightIn,shape=sex))+geom_point()
ggplot(heightweight,aes(x=ageYear,y=heightIn,shape=sex,colour=sex))+geom_point()
#scale_shape_manual()使用其他点形，scale_color_brewer()使用其他调色板
ggplot(heightweight,aes(x=ageYear,y=heightIn,shape=sex,colour=sex))+geom_point()+scale_shape_manual(values = c(1,2))+scale_color_brewer(palette = "Set1")
#使用具有颜色和填充色的点形及对应于空值和填充色的点
hw<-heightweight
str(hw)
hw$weightGroup<-cut(hw$weightLb,breaks = c(-Inf,100,Inf),labels = c("<100",">=100"))
ggplot(hw,aes(x=ageYear,y=heightIn,shape=sex,fill=weightGroup))+geom_point(size=2.5)+scale_shape_manual(values = c(21,24))+scale_fill_manual(values = c(NA,"black"),guide=guide_legend(override.aes =list(21) ))
#将连续型变量映射到点的颜色和大小上 
heightweight[,c("sex","ageYear","heightIn","weightLb")]
ggplot(heightweight,aes(x=ageYear,y=heightIn,colour=weightLb))+geom_point()
ggplot(heightweight,aes(x=ageYear,y=heightIn,size=weightLb))+geom_point()
#边框与颜色
ggplot(heightweight,aes(x=ageYear,y=heightIn,fill=weightLb))+geom_point()+geom_point(shape=21,size=2.5)+scale_fill_gradient(low="black",high = "white")
#以离散色代替色阶
ggplot(heightweight,aes(x=ageYear,y=heightIn,fill=weightLb))+geom_point()+geom_point(shape=21,size=2.5)+scale_fill_gradient(low="black",high = "white",breaks=seq(70,170,by=20),guide = guide_legend())
#
ggplot(heightweight,aes(x=ageYear,y=heightIn,size=weightLb,colour=sex))+geom_point(alpha=0.5)+
  scale_size_area()+  #使数据点面积正比于变量值
  scale_color_brewer(palette = "Set1")
#处理图形重叠
sp<-ggplot(diamonds,aes(x=carat,y=price))
sp+geom_point()
sp+geom_point(alpha=0.2)
#对数据进行分箱
sp+stat_bin2d()
sp+stat_bin2d(bins = 50)+scale_fill_gradient(low="lightblue",high="red",limits=c(0,6000))
#添加数据扰动
sp1<-ggplot(ChickWeight,aes(x=Time,y=weight))
sp1+geom_point()
#position_jitter()函数添加数据扰动
sp1+geom_point(position = "jitter")
sp1+geom_point(position =position_jitter(width = 0.5,height = 0))




