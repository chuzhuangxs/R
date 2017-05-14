library(ggplot2)
library(gcookbook)
library(plyr)

#绘制折线图
ggplot(BOD,aes(x=Time,y=demand))+geom_line(colour="blue")+theme_bw()+theme(panel.grid.major.x = element_blank(),
                                                              panel.grid.major.y = element_blank()
                                                              )
#将Time转换为因子型变量，参数group可以确保ggplot知道这些数据点属于同一个分组
BOD1<-BOD
BOD1$Time<-factor(BOD1$Time)
ggplot(BOD1,aes(x=Time,y=demand,group=1))+geom_line(colour="blue")+theme_bw()+theme(panel.grid.major.x = element_blank(),
                                                                                    panel.grid.major.y = element_blank()
)
#设定y轴范围
ggplot(BOD1,aes(x=Time,y=demand,group=1))+geom_line(colour="blue")+ylim(0,max(BOD1$demand))+theme_bw()+theme(panel.grid.major.x = element_blank(),
                                                                                    panel.grid.major.y = element_blank()
)
#向折线图添加数据标签
ggplot(BOD,aes(x=Time,y=demand))+geom_line(colour="blue")+geom_point(colour="red")

ggplot(worldpop,aes(x=Year,y=Population))+geom_line(colour="blue")+geom_point(colour="red")
#将Y轴取对数
ggplot(worldpop,aes(x=Year,y=Population))+geom_line(colour="blue")+geom_point(colour="red")+scale_y_log10()
#绘制多重折线图,ddply函数分组求平均值
str(ToothGrowth)
ToothGrowth
tg<-ddply(ToothGrowth,c("supp","dose"),summarise,length=mean(len))
tg
#将supp分组变量映射给颜色
ggplot(tg,aes(x=dose,y=length,colour=supp))+geom_line()
#将supp分组变量映射给折线类型
ggplot(tg,aes(x=dose,y=length,linetype=supp,colour=supp))+geom_line()
#将x轴变为因子对象,group指定分组,group未指定时ggplot会以colour或者linetype映射的变量来分组
ggplot(tg,aes(x=factor(dose),y=length,linetype=supp,colour=supp,group=supp))+geom_line()




