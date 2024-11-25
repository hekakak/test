# 第3章 多多元数据直观表示 -------------------------
## 数据整理 
library(openxlsx)
d3.1=read.xlsx('mvstats5.xlsx','d3.1',rowNames=T);d3.1
## ——   3.2 条图 ----
barplot(apply(d3.1,1,mean))#按行做均值条形图
barplot(apply(d3.1,1,mean),las=3)#按行做均值条形图
barplot(apply(d3.1,2,mean))#按列做均值图条形图
barplot(apply(d3.1,2,mean),col=1:8) #按列做彩色均值图条形图

barplot(apply(d3.1[,2:8],2,mean))#去掉第一列后的数据按列做均值条形图
barplot(apply(d3.1,2,median),col=1:8)  #按列取色

boxplot(d3.1)#按列做箱线图
boxplot(d3.1,horizontal=T,las=1)#箱线图中图形按水平放置

stars(d3.1)                #具有图例的360度星相图
stars(d3.1,key.loc=c(17,7))                #具有图例的360度星相图
stars(d3.1,full=F,key.loc=c(17,7))                #具有图例的180度星相图
stars(d3.1,draw.segments=T,key.loc=c(17,7))#具有图例的360度彩色圆形星相图
stars(d3.1,full=F,draw.segments=T,key.loc=c(17,7))#具有图例的180度彩色圆形星相图

library(aplpack) #加载aplpack包
faces(d3.1)
faces(d3.1[,2:8],ncol.plot=7)#去掉第一个变量按每行7个做脸谱图
faces(d3.1[c(1,9,19,28,29,30),])#选择第1,9,19,28,29,30个观测的多元数据做脸谱图
library("TeachingDemos") #install.packages("TeachingDemos")
faces2(d3.1,ncols=7) #TeachingDemos::faces(d3.1)

source("msaR.R")#加自定义函数
msa.andrews(d3.1)#绘制调和曲线图
msa.andrews(d3.1[c(1,9,19,28,29,30),])

library("fmsb")
rddat=d3.1[c(1,9,19,28,29,30),]
maxmin=rbind(apply(rddat,2,max),apply(rddat,2,min))
rddat=rbind(maxmin,rddat)
radarchart(rddat, axistype=2, pcol=topo.colors(6), plty=1, pdensity=seq(5,40,by=5), pangle=seq(0,150,by=30), pfcol=topo.colors(6))




#install.packages("andrews")
library(andrews) 
andrews(d3.1,clr=5,ymax=6)
#选择第1,9,19,28,29,30个观测的多元数据做调和曲线图
andrews(d3.1[c(1,9,19,28,29,30),],clr=5,ymax=6) 
