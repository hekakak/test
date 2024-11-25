# 第7章 聚类分析 -----------------------
x1=c(2.5,3.0,6.0,6.6,7.2,4.0,4.7,4.5,5.5)
x2=c(2.1,2.5,2.5,1.5,3.0,6.4,5.6,7.6,6.9)
plot(x1,x2,xlim=c(1,8),ylim=c(1,8)); text(x1,x2,labels=c(1:9),adj=-0.5) 
X=cbind(x1,x2); #形成数据矩阵
options(digits=3)
dist(X) #默认为euclidean距离

dist(X,diag=TRUE)  #添加主对角线距离
dist(X,upper=TRUE) #添加上三角距离 
dist(X,diag=TRUE,upper=TRUE)
#通过method参数选择不同的点与点距离
dist(X,method="manhattan")     #manhattan距离
dist(X,method="minkowski",p=1) #manhattan距离
dist(X,method="minkowski",p=2) #euclidean距离

D=dist(X);D
min(D)
which.max(D)

hc<-hclust(D);hc #默认最长距离法
cbind(hc$merge,hc$height) #分类过程
plot(hc,main="method:complete",hang=-1) #聚类图
rect.hclust(hc,3) #加3分类框
cutree(hc,1:3) #显示分类结果

hc<-hclust(D,"single");hc #最短距离法
names(hc)
cbind(hc$merge,hc$height) #分类过程
plot(hc) #聚类图

hc<-hclust(dist(X),"ward.D") #ward距离法 
cbind(hc$merge,hc$height) #分类过程
plot(hc) #聚类图        

#d3.1=read.xlsx('mvstats5.xlsx','d3.1',rowNames=T);d3.1
plot(d3.1,gap=0) #矩阵散点图
D=dist(d3.1);D
plot(hclust(D,'single')) #最短距离法
plot(hclust(D,'complete'),hang=-1) #最长距离法
plot(hclust(D,'median'),hang=-1) #中间距离法 
plot(hclust(D,'average'),hang=-1) #类平均法        
plot(hclust(D,'centroid'),hang=-1) #重心法        
plot(hclust(D,'ward.D'),hang=-1) #ward.D法
plot(hclust(D,'ward.D2'),hang=-1) #ward.D2法

H=hclust(D,'ward.D2');H
plot(H); rect.hclust(H,2); cutree(H,2)
plot(H); rect.hclust(H,3); cutree(H,3)
plot(H); rect.hclust(H,4); cutree(H,4)

set.seed(123)
#产生均值0,标准差为0.3的100x10的正态随机数矩阵
x1=matrix(rnorm(1000,0,0.3),ncol=10)
#产生均值1,标准差为0.3的100x10的正态随机数矩阵
x2=matrix(rnorm(1000,1,0.3),ncol=10)
#形成200x10的正态随机数矩阵
X=rbind(x1,x2);
summary(X) 

H=hclust(dist(X),'ward.D2')
plot(H); rect.hclust(H,2);
cutree(H,2)

km=kmeans(X,2)     #kmeans聚类
km$cluster         #分类结果
plot(X,pch=km$cluster)

set.seed(123)
x1=matrix(rnorm(10000,0,0.25),ncol=10)
x2=matrix(rnorm(10000,1,0.25),ncol=10) 
X=rbind(x1,x2)
km=kmeans(X,2) #kmeans聚类
kc=km$cluster;kc
plot(X,pch=kc,col=kc)


###自编函数-----
#自编欧氏距离矩阵计算函数，对角元素设为正无穷
myDist=function(X){
  n=nrow(X)
  D=diag(n);diag(D)=Inf #先生成对角元素为正无穷的对角矩阵
  for(i in 2:n){
    for(j in 1:(i-1)){
      D[i,j]=D[j,i]=(sum((X[i,]-X[j,])^2))^0.5 #循环计算各点欧氏距离并赋值到矩阵的对称位置，可改为其他定义的距离
    }
  }
  D
}
#计算例7-1距离矩阵
x1=c(5,7,3,6,6)
x2=c(7,1,2,5,6)
X=cbind(x1,x2)
(D=myDist(X))
#与dist(X,diag=TRUE,upper=T)除了对角线外全部一致

#自编系统聚类函数
#需要手动修改递推公式参数
myhc=function(D){
  n=nrow(D)
  nrs=rep(1,n) 
  rownames(D)=colnames(D)=-(1:n) 
  merge=matrix(0,n-1,2) 
  height=numeric(n-1) 
  for(k in 1:(n-2)){  
    r=which(D==min(D),arr.ind = T)[1,1]
    s=which(D==min(D),arr.ind = T)[1,2]
    merge[k,]=as.numeric(rownames(D)[c(r,s)]) 
    height[k]=min(D) 
    Drs=min(D) 
    Drsq=matrix(D[-c(r,s),c(r,s)],ncol=2) 
    Dpq=numeric(n-k-1) 
    ar=0.5;as=0.5;be=0;ga=0.5  
    #如果为最短距离法将ga=-0.5,类平均法则权数修改为
    #ar=nrs[r]/(nrs[r]+nrs[s]);as=nrs[s]/(nrs[r]+nrs[s]);be=0;ga=0
    for(i in 1:(n-k-1)){ 
      Dpq[i]=(ar*Drsq[i,1]+as*Drsq[i,2]+be*Drs+ga*abs(Drsq[i,1]-Drsq[i,2]))
    }
    D1=as.matrix(D[-c(r,s),-c(r,s)]);colnames(D1)=row.names(D1)=rownames(D)[-c(r,s)] 
    D=rbind(c(Inf,Dpq),cbind(Dpq,D1)) 
    rownames(D)[1]=colnames(D)[1]=k   
    nrs=c(nrs[r]+nrs[s],nrs[-c(r,s)])  
  }
  merge[n-1,]=as.numeric(rownames(D)) 
  height[n-1]=min(D)
  cbind(merge,height=height) 
}
#对例7-1系统聚类
myhc(myDist(X))

#例7-1三种系统聚类结果与系统函数一致
hc<-hclust(dist(X),"single")#最短距离法 
cbind(hc$merge,hc$height)
hc<-hclust(dist(X),"complete")#最长距离法 
cbind(hc$merge,hc$height)
hc<-hclust(dist(X),"average")#ward距离法 
cbind(hc$merge,hc$height)


#自编快速聚类函数
mykm=function(X,k,e=0.001){ #e停止迭代的临界值
  n=nrow(X)
  p=ncol(X)
  D=matrix(0,n,k) 
  id=rep(1:k,each=ceiling(n/k))[1:n]  
  #id=rep(1:k,time=ceiling(n/k))[1:n]
  dm=1  
  ite=-1
  m=as.matrix(aggregate(as.data.frame(X),by=list(id),mean)[,-1]) 
  while(dm>e){ 
    for(i in 1:k){
      D[,i]=apply(apply(X,1,function(x) x-m[i,])^2,2,sum) 
    }
    id1=apply(D,1,which.min) 
    m1=as.matrix(aggregate(as.data.frame(X),by=list(id1),mean)[,-1])  
    dm=sum((m-m1)^2)^0.5 
    id=id1 
    ite=ite+1 
    m=m1
  }
  list(iteration=ite,mean=m,class=id)
}

mykm(X,k=2,e=0.001)
kmeans(X,2)

###非凸集数据判别比较
#set.seed(2022)
n1=n2=200
r=runif(n1,0,1)
theta=runif(n1,0,2*pi)
dat1=cbind(x=r*sin(theta),y=r*cos(theta))
r=runif(n2,1.5,2.5)
theta=runif(n2,0,2*pi)
dat2=cbind(x=r*sin(theta),y=r*cos(theta))
dat=data.frame(rbind(dat1,dat2))
dat$g=rep(c(0,1),c(n1,n2))
plot(dat$x,dat$y,col=dat$g+1)

#系统聚类
D=dist(dat)
plot(hclust(D,'single')) #最短距离法
plot(hclust(D,'complete'),hang=-1) #最长距离法
plot(hclust(D,'median'),hang=-1) #中间距离法 
plot(hclust(D,'average'),hang=-1) #类平均法        
plot(hclust(D,'centroid'),hang=-1) #重心法        
plot(hclust(D,'ward.D'),hang=-1) #ward.D法
plot(hclust(D,'ward.D2'),hang=-1) #ward.D2法

H=hclust(D,'single')
plot(H) 
k=5;rect.hclust(H,k); 
plot(dat$x,dat$y,cex=1.5,col=cutree(H,k))

#K-mean聚类
k=2
km=kmeans(dat,k) #kmeans聚类
plot(dat$x,dat$y,cex=1.5,col=km$cluster)

#DBSCAN聚类
library(fpc)
r=0.5
dbs=dbscan(dat,eps=r,scale=T)
plot(dat$x,dat$y,cex=1.5,col=dbs$cluster)

