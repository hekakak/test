library(openxlsx)
###相关系数与检验 ------------------------
x1=c(171,175,159,155,152,158,154,164,168,166,159,164)#身高
x2=c(57,64,41,38,35,44,41,51,57,49,47,46)#体重
par(mar=c(5,4,2,1))
plot(x1,x2)#做散点图
lxy<-function(x,y) sum(x*y)-sum(x)*sum(y)/length(x)  #定义离均差乘积和函数
lxy(x1,x1)#x1的离均差平方和
lxy(x2,x2)#x2的离均差平方和
lxy(x1,x2)#x1，x2的离均差乘积和
(r=lxy(x1,x2)/sqrt(lxy(x1,x1)*lxy(x2,x2)))#显示用离均差乘积和计算的相关系数
cor(x1,x2)#计算相关系数

n=length(x1)#向量的长度
tr=r/sqrt((1-r^2)/(n-2));tr#相关系数假设检验t统计量
pv=pt(abs(tr),n-1,lower.tail = F)*2;pv
cor.test(x1,x2) #相关系数假设检验

####一元回归方程与检验 ------------------------
###手动计算
x=x1       #自变量,数据来自例2.2
y=x2       #因变量,数据来自例2.2
b=lxy(x,y)/lxy(x,x)    #线性回归方程斜率
a=mean(y)-b*mean(x)    #线性回归方程截距
c(a=a,b=b)             #显示线性回归方程估计值
plot(x,y)              #做散点图
lines(x,a+b*x)         #添加估计方程线

SST=lxy(y,y)     #因变量的离均差平方和
SSR=b*lxy(x,y)   #回归平方和
SSE=SST-SSR      #误差平方和
MSR=SSR/1        #回归均方
MSE=SSE/(n-2)    #误差均方
F= MSR/MSE       #F统计量
c(SST=SST,SSR=SSR,SSE=SSE,MSR=MSR,MSE=MSE,F=F)    #显示结果
sy.x=sqrt(MSE)           #估计标准差
sb=sy.x/sqrt(lxy(x,x))   #离均差平方和
t=b/sb                   #t统计量
ta=qt(1-0.05/2,n-2)      #t分位数
c(sy.x=sy.x,sb=sb,t=t,ta=ta)                      #显示结果

###自动计算
fm=lm(y~x)#一元线性回归模型
fm
summary(lm(y~x))
plot(y~x)#做散点图
abline(fm)#添加回归线
anova(fm)#模型方差分析
summary(fm)#回归系数t检验

d4.3=read.xlsx('mvstats5.xlsx','d4.3',rowNames=T);d4.3
fm=lm(y~x,data=d4.3)#一元线性回归模型
fm
summary(lm(y~x))
plot(y~x,data=d4.3)#做散点图
abline(fm)#添加回归线
anova(fm)#模型方差分析
summary(fm)#回归系数t检验


#多元回归方程与检验 ------------------------
d4.4=read.xlsx('mvstats5.xlsx','d4.4',rowNames=T);d4.4
plot(d4.4,gap=0)

###回归系数
X=model.matrix(y~x1+x2+x3+x4,data=d4.4)
Y=d4.4[,1]
B=solve(t(X)%*%X)%*%t(X)%*%Y;t(B)

fm=lm(y~x1+x2+x3+x4,data=d4.4);fm

#拟合值
h_matrix=X%*%solve(t(X)%*%X)%*%t(X) #帽子矩阵、投影矩阵
Yh=h_matrix%*%Y
t(Yh)

fitted(fm)

###残差
P=(diag(nrow(d4.4))-h_matrix)       #消灭矩阵
e=P%*%Y
t(e)

resid(fm)

###参数检验
n=31;k=4
sy=(sum(e^2)/(n-k-1))^0.5  #估计标准误
(Sb=solve(t(X)%*%X)*sy^2)
diag(Sb)^0.5  #系数估计标准差
t=B/diag(Sb)^0.5  #t值
pv=pt(-abs(t),n-k-1)*2  #p值
cbind(t,pv)

summary(fm)#多元线性回归系数t检验和F检验

###标准回归系数
coef.sd<-function(fm){  
  b=fm$coef;b
  si=apply(fm$model,2,sd);si
  bs=b[-1]*si[-1]/si[1]
  bs
}
coef.sd(fm)

#复相关系数
summary(fm)$fstat
cor(d4.4)               #多元数据相关系数矩阵
pairs(d4.4)             #多元数据散点图
msa.cor.test(d4.4)      #多元数据相关系数检验
(R2=summary(fm)$r.sq)   #显示多元线性回归模型决定系数
(R=sqrt(R2))            #显示多元数据复相关系数

###变量选择--------------
###最优子集法
library(leaps)            #加载leaps包
varsel=regsubsets(y~x1+x2+x3+x4,data=d4.4)                                #多元数据线性回归变量选择模型
result=summary(varsel)                                                    #变量选择方法结果           
data.frame(result$outmat,RSS=result$rss,R2=result$rsq)                    #RSS和决定系数准则结果展示 
data.frame(result$outmat,adjR2=result$adjr2,Cp=result$cp,BIC=result$bic)  #调整决定系数,Cp和BIC准则结果展示

summary(lm(y~x2+x4,data=d4.4)) 

###逐步回归
#向前引入法变量选择结果
fm0=lm(y~1,data=d4.4)                                  #初始模型只包含截距
fm.step=step(fm0,scope=formula( y~x1+x2+x3+x4),direction="forward")

#逐步筛选法变量选择结果
fm.step=step(fm0,scope=formula( y~x1+x2+x3+x4),direction="both") 

#向后剔除法变量选择结果
fm=lm(y~x1+x2+x3+x4,data=d4.4)                         #初始模型保护全部自变量
fm.step=step(fm,direction="backward")                 

###lasso
library(glmnet)                    
cv.out=cv.glmnet (X, Y,alpha=1)   #交叉检验确定$\lambda$值
plot(cv.out)
bestlam<-cv.out$lambda.min
las=glmnet (X,Y,alpha=1,lambda=bestlam)
predict(las,type="coefficients") 

