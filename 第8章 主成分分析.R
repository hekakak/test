# 第8章 主成分分析 ----------------------------------
x1=c(147,171,175,159,155,152,158,154,164,168,166,159,164,177)
x2=c(32,57,64,41,38,35,44,41,54,57,49,47,46,63)
plot(x1,x2,xlim=c(145,180),ylim=c(25,75))
lines(c(146,178),c(30,66));text(180,68,"y1")
lines(c(161,168),c(60,38));text(161,63,"y2")      
library(shape)
lines(getellipse(24,3,mid=c(162,48),angle=48),lty=3)

cov(x1,x2)  #协方差
cor(x1,x2)  #相关系数

X=cbind(x1,x2);X
S=cov(X);S   #协差阵
R=cor(X);R   #相关系数阵

#根据方差协方差阵分解 
pc_s=princomp(X);pc_s
names(pc_s)  
summary(pc_s)
pc_s$sdev^2  #主成分方差
pc_s$loadings  #主成分系数
pc_s$scores    #主成分得分
round(cor(pc_s$scores),2)  #样本主成分不相关

plot(pc_s$scores,asp=1);abline(h=0,v=0,lty=3)
#重叠信息图
biplot(pc_s$scores,pc$loadings);abline(h=0,v=0,lty=3)

#根据相关系数阵分解
pc_c=princomp(X,cor=T);pc_c
summary(pc_c)
pc_c$sdev^2  #主成分方差
pc_c$loadings  #主成分系数
pc_c$scores    #主成分得分
round(cor(pc_c$scores),2)  #样本主成分不相关
biplot(pc_c$scores,pc$loadings);abline(h=0,v=0,lty=3)

###直接求特征和特征向量
eigen(cov(X)*13/14)
eigen(cor(X))

#自编主成分分析系数
msa.pca<-function(X,cor=FALSE,m=2,scores=TRUE,ranks=TRUE,sign=TRUE,plot=TRUE){  
  if(m<1) return
  PC=princomp(X,cor=cor)
  Vi=PC$sdev^2
  Vari=data.frame('Variance'=Vi[1:m],'Proportion'=(Vi/sum(Vi))[1:m],
                  'Cumulative'=(cumsum(Vi)/sum(Vi))[1:m])
  cat("\n")
  Loadi=as.matrix(PC$loadings[,1:m])
  Compi=as.matrix(PC$scores[,1:m])
  if(sign) 
    for (i in 1:m)  
      if(sum(Loadi[,i])<0){
        Loadi[,i] = -Loadi[,i] 
        Compi[,i] = -Compi[,i] 
      }
  pca<-NULL
  pca$vars=Vari
  if(m<=1) pca$loadings = data.frame(Comp1=Loadi) 
  else pca$loadings = Loadi;
  if(scores & !ranks) pca$scores=round(Compi,4)
  if(scores & plot){
    plot(Compi);abline(h=0,v=0,lty=3)
    text(Compi,row.names(X)) 
    #	par(mar=c(4,4,2,3))
    #	biplot(Compi,Loadi); abline(h=0,v=0,lty=3)
    #	par(mar=c(4,4,1,1))
  }
  if(scores & ranks){
    pca$scores=round(Compi,4)
    Wi=Vi[1:m];Wi
    Comp=Compi%*%Wi/sum(Wi)
    Rank=rank(-Comp)
    pca$ranks=data.frame(Comp=round(Comp,4),Rank=Rank)
  }
  pca
}
#例子8.2
d3.1=read.xlsx('mvstats5.xlsx','d3.1',rowNames=T);d3.1
pca=msa.pca(d3.1);pca            #基于协差阵分解
pca_c=msa.pca(d3.1,cor=T);pca_c  #基于相关系数阵分解

par(mfrow=c(1,2),mar=c(3,4,1,2))
biplot(pca$scores,pca$loadings);abline(h=0,v=0,lty=3)
biplot(pca_c$scores,pca_c$loadings);abline(h=0,v=0,lty=3)
par(mfrow=c(1,1))

cbind(pca$ranks,pca_c$ranks)

###缺失值填充
X <- data.matrix(scale(USArrests))
dim(X)
pcob <- prcomp(X)
summary(pcob)

#设置20个缺失值
nomit <-20
set.seed(15)
ina <- sample(seq(50), nomit)
inb <- sample(1:4, nomit, replace = TRUE)
Xna <- X
index.na <- cbind(ina, inb)
Xna[index.na] <- NA
#利用主成分拟合函数，M为主成分个数
fit.pc <- function(X, M = 1){
  op=prcomp(X)
  up=op$rotation[,1:M]
  X%*%up%*%t(up)
}
#采用均值为初始填充
Xhat <- Xna
xbar <- colMeans(Xna, na.rm = TRUE)
Xhat[index.na] <- xbar[inb]
#设定迭代收敛条件
thresh <- 1e-7
rel_err <- 1
iter <- 0
ismiss <- is.na(Xna)
mssold <- mean((scale(Xna, xbar, FALSE)[!ismiss])^2)
mss0 <- mean(Xna[!ismiss]^2)
#迭代填充值
while(rel_err > thresh) {
  iter <- iter + 1               # Step 2(a)
  Xapp <- fit.pc(Xhat, M = 1)   # Step 2(b)
  Xhat[ismiss] <- Xapp[ismiss]   # Step 2(c)
  mss <- mean(((Xna - Xapp)[!ismiss])^2)
  rel_err <- (mssold - mss) / mss0
  mssold <- mss
  cat("Iter:", iter, "MSS:", mss,
      "Rel. Err:", rel_err, "\n")
}
#填充效果
cor(Xapp[ismiss], X[ismiss])
plot(Xapp[ismiss], X[ismiss])
abline(a=0,b=0.5)

###图片压缩---
#install.packages("jpeg")
library(jpeg)
dog <- readJPEG('dog.jpg')
nrow(dog)
ncol(dog)
dog.pca1<- prcomp(dog[,,1],center=F)
dog.pca2<- prcomp(dog[,,2],center=F)
dog.pca3<- prcomp(dog[,,3],center=F)
#提取前20个主成分
k=200
dog.compressed=array(0,dim=c(1156,942,3))
dog.compressed[,,1]<- dog.pca1$x[,1:k] %*% t(dog.pca1$rotation[,1:k])
dog.compressed[,,2]<- dog.pca2$x[,1:k] %*% t(dog.pca2$rotation[,1:k])
dog.compressed[,,3]<- dog.pca3$x[,1:k] %*% t(dog.pca3$rotation[,1:k])

writeJPEG(dog.compressed, paste('dog_compressed.jpg'))
file.info('dog_compressed.jpg')$size/file.info('dog.jpg')$size*100

###转化为灰度压缩---
library(wavelets)
library(jpeg)
library(magick)  # 加载magick包

#图片路径
path = 'dog.jpg'
img <- image_read(path)  # 读入图片
#查看图像属性
result = image_info(img)
image_info(img)

#获取图片矩阵行数H 列数W
H = result$height
W = result$width
# 转为灰度图
gray <- image_convert(img, colorspace='gray')
gray#显示灰度图像

# 转为rgb图
rgb <- image_convert(img, colorspace='rgb')
rgb#显示彩色图像

# 获取图片灰度值矩阵X
X <- as.integer(image_data(gray))[, , 1]
#归一化
X <- X/255
#保存灰度图
writeJPEG(X, 'X.jpg')


#对原始图像奇异值分解
svd_result1 = svd(X)
u1 = svd_result1$u
v1 = svd_result1$v
d1 = svd_result1$d
d1 = diag(d1)
#截断奇异值k
k = 25
svd_X = u1[,1:k]%*%d1[1:k,1:k]%*%t(v1[,1:k])
svd_X[svd_X<0] = 0
svd_X[svd_X>1] = 1
#保存原始图像的压缩图像
writeJPEG(svd_X, 'svd_X.jpg')

dog.pca_g<- prcomp(X,center=F)
k=25
dog.compressed_g<- dog.pca_g$x[,1:k] %*% t(dog.pca_g$rotation[,1:k])
writeJPEG(dog.compressed_g, 'pca_X.jpg')

###去除图片中的黑点---
#稀疏噪音
#生成高斯噪声
sigma = 0.6
D <- matrix(0,H,W)
#噪声点数目
num = 1000
temp1 = sample(1:H,num,T)
temp2 = sample(1:W,num,T)
for(i in seq(1,num)){
  D[temp1[i],temp2[i]] = rnorm(1,0,0.3)
}

#合成含噪图像矩阵Y
Y <- X + D
Y[Y<0] = 0
Y[Y>1] = 1
#保存含噪图像
writeJPEG(Y, 'yy.jpg')

dog.pca_g<- prcomp(Y,center=F)
k=100
dog.compressed_g<- dog.pca_g$x[,1:k] %*% t(dog.pca_g$rotation[,1:k])
writeJPEG(dog.compressed_g, 'pca_cat.jpg')

