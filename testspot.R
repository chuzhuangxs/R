library(ggplot2)
library(gcookbook)
library(plyr)
library(MASS)

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
#绘制箱线图group参数对变量进行分组
sp1+geom_boxplot()
sp1+geom_boxplot(aes(group=Time))
#添加回归模型拟合线
sp2<-ggplot(heightweight,aes(x=ageYear,y=heightIn))
sp2+geom_point()+stat_smooth(method = lm)
#设定置信域为99%，默认为95%
sp2+geom_point()+stat_smooth(method = lm,level = 0.99)
#取消置信域,设定颜色
sp2+geom_point()+stat_smooth(method = lm,se=FALSE,colour="red")
#不指定拟合直线类型
sp2+geom_point()+stat_smooth()
#使用Logistic回归
b<-biopsy
#添加分类变量classn，如果class=benign则classn的值为0，反之为1
b$classn[b$class=="benign"]<-0
b$classn[b$class=="malignant"]<-1
b
ggplot(b,aes(x=V1,y=classn))+geom_point(position = position_jitter(width = 0.3,height = 0.06),alpha=0.6,shape=21,size=1.5)+stat_smooth(method = glm)
#数据分组后，有分组变量
sps<-ggplot(heightweight,aes(x=ageYear,y=heightIn,colour=sex))+geom_point()+scale_colour_brewer(palette = "Set1")
sps+geom_smooth()
#将拟合线外推，只有直线可以外推,并且将fullrange参数设定为true
sps+geom_smooth(method = lm,fullrange=TRUE)
#向图形中添加自己建拟合模型线
#自建拟合模型
model<-lm(heightIn~ageYear+I(ageYear^2),heightweight)
heightweight$ageYear
xmin<-min(heightweight$ageYear)
xmin
xmax<-max(heightweight$ageYear)
xmax
#在最大值和最小值之间进行插值
predicted<-data.frame(ageYear=seq(xmin,xmax,length.out = 100))
predicted
#计算变量heightIn的预测值
predicted$heightIn<-predict(model,predicted)
predicted
#先画出点图，在画出算好的拟合模型线
sp<-ggplot(heightweight,aes(x=ageYear,y=heightIn))+geom_point(colour="grey40")
sp
sp+geom_line(data = predicted,size=1)
#编写predictvals函数
predictvals<-function(model,xvar,yvar,xrange=NULL,samples=100,...){
  #xrange为x轴的范围，samples为x轴上包含的样本数
  if(is.null(xrange)){#验证xrange是否有输入
    if(any(class(model)%in%c("lm","glm")))
      xrange<-range(model$model[[xvar]])
    else if(any(class(model)%in%"loess"))
      xrange<-range(model$x)
  }
  newdata<-data.frame(x=seq(xrange(1)))
}













