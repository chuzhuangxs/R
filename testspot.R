library(ggplot2)
library(gcookbook)
library(plyr)
library(MASS)
library(reshape2)

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
  newdata<-data.frame(x=seq(xrange[1],xrange[2],length.out = samples))
  names(newdata)<-xvar
  newdata[[yvar]]<-predict(model,newdata=newdata,...)
  newdata
}
#使用上面编写的函数测试
modlinear<-lm(heightIn~ageYear,heightweight)
lm_predict<-predictvals(modlinear,"ageYear","heightIn")
sp+geom_line(data = lm_predict,colour="red",size=8)
#添加多个模型的拟合线dlply函数中sex是分类变量
make_model<-function(data){
  lm(heightIn~ageYear,data)
}
models<-dlply(heightweight,"sex",.fun = make_model)
models
#获取预测值,dlply与ldply函数的作用都是切分数据
predvals<-ldply(models,.fun = predictvals,xvar = "ageYear",yvar="heightIn")
ggplot(heightweight,aes(x=ageYear,y=heightIn,colour=sex))+geom_point()+geom_line(data=predvals)
#使两组预测值的x轴的范围相同,添加xrange参数
predvals<-ldply(models,.fun = predictvals,xvar="ageYear",yvar="heightIn",xrange=range(heightweight$ageYear))
ggplot(heightweight,aes(x=ageYear,y=heightIn,colour=sex))+geom_point()+geom_line(data=predvals)
#向散点图添加系数模型
model<-lm(heightIn~ageYear,heightweight)
summary(model)
pred<-predictvals(model,"ageYear","heightIn")
sp<-ggplot(heightweight,aes(x=ageYear,y=heightIn))+geom_point()+geom_line(data=pred)
#添加系数
sp+annotate("text",label="r^2=0.42",parse=TRUE,x=16.5,y=52)
#使用expression函数检测字符串（数学公式）是否可以作为系数输出
expression(r^2==0.42)
expression(r^2=0.42)
#利用函数解析并返回一个公式
eqn<-as.character(as.expression(
  substitute(italic(y)==a+b*italic(x)*","~~italic(r)^2~"="~r2,
             list(a=format(coef(model)[1],digits = 3),
                  b=format(coef(model)[2],digits = 3),
                  r2=format(summary(model)$r.squared,digits = 2)
                  )
             )
))
eqn
parse(text=eqn)
#添加到图形上,x=Inf,y=-Inf,hjust=1.1,vjust=-.5,这些参数调整公式的位置
sp+annotate("text",label=eqn,parse=TRUE)
sp+annotate("text",label=eqn,parse=TRUE,x=Inf,y=-Inf,hjust=1.1,vjust=-.5)
#向散点图添加边际地毯
ggplot(faithful,aes(x=eruptions,y=waiting))+geom_point()+geom_rug()
#添加扰动并设定size减少数据重叠
ggplot(faithful,aes(x=eruptions,y=waiting))+geom_point()+geom_rug(position = "jitter",size=0.2)
#向散点图添加标签
subset(countries,Year==2009&healthexp>2000)
sp<-ggplot(subset(countries,Year==2009&healthexp>2000),aes(x=healthexp,y=infmortality))+geom_point()
sp
sp+annotate("text",x=4350,y=5.4,label="Canada")+annotate("text",x=7400,y=6.8,label="USA")
#设置自动添加标签
sp+geom_text(aes(label=Name),size=4)
#做一些调整
sp+geom_text(aes(label=Name),size=4,vjust=-1)
#设置左对齐或者右对齐，最好不要改变hjust的值,去改变x的值比较好
sp+geom_text(aes(label=Name),size=4,hjust=0)
sp+geom_text(aes(x=healthexp+100,label=Name),size=4,hjust=0)
#自动给一部分有标签的点添加标签
cdat<-subset(countries,Year==2009 & healthexp>2000)
cdat
cdat$Name1<-cdat$Name
cdat$Name1
#%in% 运算符返回布尔值
idx<-cdat$Name1 %in% c("Canada","Ireland","United Kingdom","United States","New Zealand","Iceland","Japan","Luxembourg","Netherlands","Switzerland")
idx
cdat$Name1[!idx]<-NA
cdat
ggplot(cdat,aes(x=healthexp,y=infmortality))+geom_point()+geom_text(aes(x=healthexp+100,label=Name1),size=4,hjust=0)+xlim(2000,10000)
#绘制气泡图
cdat<-subset(countries,Year==2009&Name %in% c("Canada","Ireland","United Kingdom","United States","New Zealand","Iceland","Japan","Luxembourg","Netherlands","Switzerland"))
cdat
#只将GDP映射给size，GDP会被映射成点的半径
p<-ggplot(cdat,aes(x=healthexp,y=infmortality,size=GDP))+geom_point(shape=21,colour="black",fill="cornsilk")
p
#将GDP映射为面积
p+scale_size_area(max_size = 15)
#x轴与y轴皆为分类变量时，气泡图可以用来表示网格点的值
HairEyeColor
#对男性组与女性组求和
hec<-HairEyeColor[,,"Male"]+HairEyeColor[,,"Female"]
hec
#将矩阵转换为长格式
hec<-melt(hec,value.name = "count")
hec
ggplot(hec,aes(x=Eye,y=Hair))+geom_point(aes(size=count),shape=21,colour="black",fill="cornsilk")+
  scale_size_area(max_size = 20,guide=FALSE)+geom_text(aes(y=as.numeric(Hair)-sqrt(count)/22,label=count),vjust=1,colour="grey60",size=4)

ggplot(hec,aes(x=Eye,y=Hair))+geom_point(aes(size=count),shape=21,colour="black",fill="cornsilk")+
  scale_size_area(max_size = 20,guide=FALSE)+geom_text(aes(label=count),vjust=1,colour="grey60",size=4)
#绘制散点图矩阵
c2009<-subset(countries,Year==2009,select = c(Name,GDP,laborrate,healthexp,infmortality))
c2009
pairs(c2009[,2:5])
#自定义面板函数
panel.cor<-function(x,y,digits=2,prefix="",cex.cor,...){
  usr<-par("usr")
  on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r<-abs(cor(x,y,use = "complete.obs"))
  txt<-format(c(r,0.123456789),digits = digits)[1]
  txt<-paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex.cor<-0.8/strwidth(txt)
  text(0.5,0.5,txt,cex=cex.cor*(1+r)/2)
}
#展示直方图函数
panel.hist<-function(x,...){
  usr<-par("usr")
  on.exit(par(usr))
  par(usr=c(usr[1:2],0,1.5))
  h<-hist(x,plot = FALSE)
  breaks<-h$breaks
  nB<-length(breaks)
  y<-h$counts
  y<-y/max(y)
  rect(breaks[-nB],0,breaks[-1],y,col = "white",...)
}
pairs(c2009[,2:5],upper.panel = panel.cor,diag.panel = panel.hist,lower.panel = panel.smooth)






























