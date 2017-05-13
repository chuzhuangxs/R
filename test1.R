library(ggplot2)
library(gcookbook)
library(plyr)

#条形图的学习
BOD
#str函数用来展示数据结构
str(BOD)
#画出条形图，注意连续型变量与因子型变量的区别
ggplot(BOD,aes(x=Time,y=demand))+geom_bar(stat = "identity")
ggplot(BOD,aes(x=factor(Time),y=demand))+geom_bar(stat = "identity")
#fill属性改变条形图颜色，colour属性改变框的颜色
ggplot(BOD,aes(x=factor(Time),y=demand))+geom_bar(stat = "identity",fill="lightblue",colour="black")

cabbage_exp
#绘制簇状条形图，position参数，设定条形图错开排列
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(position = "dodge",stat = "identity")
diamonds
#频数条形图
ggplot(diamonds,aes(x=cut))+geom_bar(fill="lightblue",colour="red")
#连续变量的情况
ggplot(diamonds,aes(x=carat))+geom_bar(fill="lightblue",colour="red")

uspopchange
#选出人口增长最快的10个州
upc<-subset(uspopchange,rank(Change)>40)
upc
#绘制条形图,fill参数指定条形图颜色，参数值应该是分类变量
ggplot(upc,aes(x=Abb,y=Change,fill=Region))+geom_bar(stat = "identity")
#reorder函数设定条形图根据高度Change排列，scale_fill_manual函数设定颜色，Xlab设定x轴标签
ggplot(upc,aes(x=reorder(Abb,Change),y=Change,fill=Region))+geom_bar(stat = "identity",colour="black")+scale_fill_manual(values = c("blue","red"))+xlab("State")
#正负条形图的分别着色
climate
csub<-subset(climate,Source=="Berkeley"&Year>=1900)
#设置正负值标识变量
csub$pos<-csub$Anomaly10y>=0
csub
#绘图，position参数设为identity可以避免系统对负值绘制堆积条形而发出的警告信息
ggplot(csub,aes(x=Year,y=Anomaly10y,fill=pos))+geom_bar(stat = "identity",position = "identity")
#微调颜色
ggplot(csub,aes(x=Year,y=Anomaly10y,fill=pos))+geom_bar(stat = "identity",position = "identity",colour="black",size=0.25)+scale_fill_manual(values = c("#CCEEFF","#FFDDDD"),guide=FALSE)
#设定条形宽度与条形间距
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat = "identity",width = 0.5)#设定条形宽度
#position=doge即默认position=position_dodge(0.9),此参数调节间距
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(stat="identity",width = 0.5,position = position_dodge(0.7))
#绘制堆积条形图
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(stat = "identity")
#guides函数调整堆积条形的顺序与图例的顺序
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(stat = "identity")+guides(fill=guide_legend(reverse = TRUE))
#调整条形的堆叠顺序
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar,order=desc(Cultivar)))+geom_bar(stat = "identity")
#绘制百分比堆叠条形图,ddply函数中Date用于分组,执行transform函数
ce<-ddply(cabbage_exp,"Date",transform,percent_weight=Weight/sum(Weight)*100)
ce
ggplot(ce,aes(x=Date,y=percent_weight,fill=Cultivar))+geom_bar(stat = "identity")
#添加数据标签
#在条形图顶端下方,geom_text函数添加数据标签vjust参数设置在图形顶端的上方还是下方
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+geom_bar(stat = "identity")+geom_text(aes(label=Weight),vjust=1.5,colour="white")
#在条形图顶端上方
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+geom_bar(stat = "identity")+geom_text(aes(label=Weight),vjust=-0.2)


