#加载tm包
library(XML)
library(SnowballC)
library(tm.plugin.webmining)
library(tm)

reut21578<-system.file("texts","crude",package = "tm") #设置默认文件路径
#Corpus函数生成语料库文件，DirSource参数指定文件路径，readerControl指定读取文件
reuters<-VCorpus(DirSource(reut21578),readerControl = list(reader=readReut21578XMLasPlain))
reuters
#reuters<-Corpus(VectorSource(reuters))另一种方式建立语料库
#用tm_map命令对语料库文件进行预处理，将其转为纯文本并去除多余空格，转换小写，去除常用词汇、合并异形同意词汇
#网上的代码大多写as.PlainTextDocument,这是错误的写法
#reuters<-tm_map(reuters,PlainTextDocument) #转换为纯文本 #这里不用转换为纯文本
reuters<-tm_map (reuters,stripWhitespace)#去除多余空白
reuters<-tm_map(reuters,content_transformer(tolower))#转换小写 新版本的TM包使你不能操作简单字符值，要用content_transformer函数包裹

reuters<-tm_map(reuters,removeWords,stopwords("english"))#去掉停止词
tm_map(reuters,stemDocument)#需要SnowballC包(并行计算)支持
#reuters<-Corpus(VectorSource(reuters))
dtm<-DocumentTermMatrix(reuters)
dtm
#inspect函数查看部分矩阵内容
inspect(dtm[1:5,740:743])
#使用筛选器筛选出ID=237的文档
idx<-meta(reuters,"id")=='237'
reuters[idx]
#找出词频在5次以上的词
findFreqTerms(dtm,5)
#找出与opec相关系数在0.8以上的词
findAssocs(dtm,"opec",0.8)
#去掉词频少的词
inspect(removeSparseTerms(dtm,0.4))
#创建单词--文档矩阵，以限定词典的方式
inspect(DocumentTermMatrix(reuters,list(dictionary=c("prices","crude","oil"))))
