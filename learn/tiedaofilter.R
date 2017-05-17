library(xlsx)
library(tm)
library(SnowballC)
library(Rwordseg)
library(wordcloud)
library(RColorBrewer)
#读取数据
data<-read.xlsx("E:/R/R_graphics_cookbook/learn/tiedao1.xlsx", stringsAsFactors=FALSE,header=TRUE,encoding = "UTF-8",sheetIndex = 1)
#生成语料库
tiedao<-VCorpus(VectorSource(data$comment))
reuters<-tm_map (tiedao,stripWhitespace)
reuters<-tm_map(reuters,removeWords,stopwords("english"))
tm_map(reuters,stemDocument)
dtm<-DocumentTermMatrix(reuters)

#开启人名识别
segment.options(isNameRecognition=TRUE)
getOption("isNameRecognition")
data1<-segmentCN(reuters)

str(data1)
tiedao
