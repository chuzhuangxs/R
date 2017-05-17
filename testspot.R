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
