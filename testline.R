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
ggplot(tg,aes(x=dose,y=length,shape=supp))+geom_line()+geom_point(size=4)
#移动数据点的位置，position参数
ggplot(tg,aes(x=dose,y=length,shape=supp))+geom_line(position = position_dodge(0.2))+geom_point(position = position_dodge(0.2),size=4)
#修改线条样式
ggplot(BOD,aes(x=Time,y=demand))+geom_line(linetype="dashed",size=1,colour="blue")
#使用调色板
ggplot(tg,aes(x=dose,y=length,colour=supp))+geom_line()+scale_color_brewer(palette = "Set1")
ggplot(tg,aes(x=dose,y=length,colour=supp))+geom_line()+scale_color_brewer(palette = "Set1")+geom_point(shape=22)
#使用fill填充色
ggplot(tg,aes(x=dose,y=length,colour=supp))+geom_line()+scale_color_brewer(palette = "Set1")+geom_point(shape=22,size=4,fill="blue")
#使用另一个调色板,这个调色板对点调色
ggplot(tg,aes(x=dose,y=length,fill=supp))+geom_line()+geom_point(shape=22,size=4)+scale_fill_manual(values = c("red","black"))
#绘制面积图
sunspotyear<-data.frame(
  Year=as.numeric(time(sunspot.year)),
  Sunspots=as.numeric(sunspot.year)
  )
ggplot(sunspotyear,aes(x=Year,y=Sunspots))+geom_area()
ggplot(sunspotyear,aes(x=Year,y=Sunspots))+geom_area(colour="black",fill="blue",alpha=0.2)
#直接添加边框线，系统会在起点和终点位置分别绘制一套垂直线
ggplot(sunspotyear,aes(x=Year,y=Sunspots))+geom_area(fill="blue",alpha=0.2)+geom_line()
#绘制堆积面积图
ggplot(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup))+geom_area()
#翻转图例堆积顺序
ggplot(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup))+geom_area(colour="black",size=2,alpha=0.4)+scale_fill_brewer(palette = "Blues",breaks=rev(levels(uspopage$AgeGroup)))
#反转面积图的堆积顺序
ggplot(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup,order=desc(AgeGroup)))+geom_area(colour="black",size=2,alpha=0.4)+scale_fill_brewer(palette = "Blues")
#不绘制多边形
ggplot(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup,order=desc(AgeGroup)))+geom_area(colour=NA,alpha=0.4)+scale_fill_brewer(palette = "Blues")+geom_line(position = "stack",size=0.2)
#绘制百分比堆积条形图
uspopage_prob<-ddply(uspopage,"Year",transform,Percent=Thousands/sum(Thousands)*100)
ggplot(uspopage_prob,aes(x=Year,y=Percent,fill=AgeGroup))+geom_area(colour=NA,alpha=0.4)+scale_fill_brewer(palette = "Blues")+geom_line(position = "stack",size=0.2)
#添加置信域
clim<-subset(climate,Source=="Berkeley",select = c("Year","Anomaly10y","Unc10y"))
clim
ggplot(clim,aes(x=Year,y=Anomaly10y))+geom_ribbon(aes(ymin=Anomaly10y-Unc10y,ymax=Anomaly10y+Unc10y),alpha=0.2)+geom_line()




