###例题6-1 Fisher判别----------------------------
d6.1=read.xlsx('mvstats5.xlsx','d6.1');d6.1
attach(d6.1)#绑定数据
plot(x1,x2); text(x1,x2,G,adj=-0.5) #标识点所属类别G
library(MASS)
(ld=lda(G~x1+x2,data=d6.1))#线性判别模型
Z=predict(ld)#根据线性判别模型预测所属类别
newG=Z$class#预测的所属类别结果
cbind(G,Z$x,newG)#显示结果
(tab=table(G,newG))#混淆矩阵        
sum(diag(prop.table(tab)))#判对率   
a1=ld$scaling[1,]; a2=ld$scaling[2,]
#y=a1x1+a2x2 ===> x2=y0/a2-a1/a2x1  
a = 1/a2*(a1*mean(x1)+a2*mean(x2))
b =-a1/a2
abline(a,b)  ##画出判别线
text(x1,x2,newG,adj=-0.5)  ##标记出图上点的原始分类结果
plot(ld,type='both') #投影分布

###例题6-3 距离判别----------------------------
d6.2=read.xlsx('mvstats5.xlsx','d6.2');d6.2
attach(d6.2)#绑定数据
par(mar=c(4,4,2,1),cex=0.75) 
plot(Q,C);text(Q,C,G1,adj=-0.5)
plot(Q,P);text(Q,P,G1,adj=-0.5)
plot(C,P);text(C,P,G1,adj=-0.5)
#马氏距离函数
discrim.dist=function (X, G, newX = NULL, var.equal = FALSE) 
{
  disp = newX
  G = as.factor(G)
  if (is.null(newX) == TRUE) 
    newX <- X
  if (is.vector(newX) == TRUE) 
    newX <- t(as.matrix(newX))
  else if (is.matrix(newX) != TRUE) 
    newX <- as.matrix(newX)
  if (is.matrix(X) != TRUE) 
    X <- as.matrix(X)
  nx <- nrow(newX)
  newG <- matrix(rep(0, nx), nrow = 1, dimnames = list("newG", 
                                                       1:nx))
  g <- length(levels(G))
  mu <- matrix(0, nrow = g, ncol = ncol(X))
  for (i in 1:g) mu[i, ] <- colMeans(X[G == i, ])
  D <- matrix(0, nrow = g, ncol = nx)
  if (var.equal == TRUE || var.equal == T) {
    for (i in 1:g) D[i, ] <- mahalanobis(newX, mu[i, ], var(X))
  }
  else {
    for (i in 1:g) D[i, ] <- mahalanobis(newX, mu[i, ], var(X[G == 
                                                                i, ]))
  }
  for (j in 1:nx) {
    dmin <- Inf
    for (i in 1:g) if (D[i, j] < dmin) {
      dmin <- D[i, j]
      newG[j] <- i
    }
  }
  if (is.null(disp) == FALSE) 
    list(Dist = D, newG = newG)
  else print(data.frame(G = G, D = t(D), newG = t(newG)))
}
###二次判别
discrim.dist(cbind(Q,C,P),as.factor(G1))#马氏距离判别（默认）
discrim.dist(cbind(Q,C,P),as.factor(G1),c(8.0,7.5,65))#判断一个观测所属类别
kk=discrim.dist(cbind(Q,C,P),as.factor(G1))[,2:3]
#qda函数
qd=qda(G1~Q+C+P);qd  
cbind(G1,newG=predict(qd)$class)
predict(qd,data.frame(Q=8,C=7.5,P=65))
###线性判别
discrim.dist(cbind(Q,C,P),as.factor(G1),var.equal = T)#马氏距离判别（默认）
discrim.dist(cbind(Q,C,P),as.factor(G1),var.equal = T,c(8.0,7.5,65))
#lda函数
ld=lda(G1~Q+C+P);ld
W=predict(ld)
cbind(G1,Wx=W$x,newG=W$class)
predict(ld,data.frame(Q=8,C=7.5,P=65)) #判定


###例题6-3------
d6.3=read.xlsx('mvstats5.xlsx','d6.3');d6.3
attach(d6.3)#绑定数据
plot(Q,C);text(Q,C,G3,adj=-0.5,cex=0.75)
plot(Q,P);text(Q,P,G3,adj=-0.5,cex=0.75)        
plot(C,P);text(C,P,G3,adj=-0.5,cex=0.75)        

ld=lda(G3~Q+C+P); ld
Z=predict(ld)
newG=Z$class
cbind(G3,round(Z$x,3),newG)
(tab=table(G3,newG))
diag(prop.table(tab,1))
sum(diag(prop.table(tab)))
plot(Z$x)
text(Z$x[,1],Z$x[,2],G3,adj=-0.8,cex=0.75)
predict(ld,data.frame(Q=8,C=7.5,P=65)) #判定

qd=qda(G3~Q+C+P); qd
Z=predict(qd)
newG=Z$class
cbind(G3,newG)
(tab=table(G3,newG))
sum(diag(prop.table(tab)))
predict(qd,data.frame(Q=8,C=7.5,P=65)) #判定

(ld1=lda(G3~Q+C+P,prior=c(1,1,1)/3))#先验概率相等的Bayes判别模型
(ld2=lda(G3~Q+C+P,prior=c(5,8,7)/20))#先验概率不相等的Bayes判别模型        
Z1=predict(ld1)#预测所属类别        
cbind(G3,round(Z1$x,3),newG=Z1$class)#显示结果 
Z2=predict(ld2)#预测所属类别
cbind(G3,round(Z2$x,3),newG=Z2$class)#显示结果
table(G3,Z1$class)#混淆矩阵
table(G3,Z2$class)#混淆矩阵
round(Z1$post,3) #ld1模型后验概率
round(Z2$post,3) #ld2模型后验概率
predict(ld1,data.frame(Q=8,C=7.5,P=65))  # ld1模型的判定
predict(ld2,data.frame(Q=8,C=7.5,P=65))  # ld2模型的判定


###非凸集数据判别比较
set.seed(2022);n1=n2=100
r=runif(n1,0,1)
theta=runif(n1,0,2*pi)
dat1=cbind(x=r*sin(theta),y=r*cos(theta))
r=runif(n2,1,3)
theta=runif(n2,0,2*pi)
dat2=cbind(x=r*sin(theta),y=r*cos(theta))
dat=data.frame(rbind(dat1,dat2))
dat$g=rep(c(0,1),c(n1,n2))
plot(dat$x,dat$y,col=dat$g+1)

lg=glm(g~x*y+I(x^2)+I(y^2),family = "binomial",data=dat)
summary(lg)
pre_lg=predict(lg,type="response")>0.5
table(pre_lg,dat$g)
plot(dat$x,dat$y,col=pre_lg+1)

ld=lda(g~x+y,data=dat)
pre_ld=as.numeric(predict(ld)$class)
table(pre_ld,dat$g)
plot(dat$x,dat$y,col=pre_ld+1)

qd=qda(g~x+y,data=dat)
pre_qd=as.numeric(predict(qd)$class)
table(pre_qd,dat$g)
plot(dat$x,dat$y,col=pre_qd+1)

