## —— 5.2.2  Logistic模型 -----------------------
d5.1=read.xlsx('mvstats5.xlsx','d5.1');head(d5.1)
logit.glm<-glm(y~x1+x2+x3,family=binomial,data=d5.1)#Logistic回归模型
summary(logit.glm)#Logistic回归模型结果


#手编极大似然估计
Y=d5.1[,1];X=as.matrix(cbind(x0=1,d5.1[,2:4]))
maxIterNum <- 20000;  #最大迭代次数
W <- rep(0, ncol(X))  #系数估计向量，初始为0向量
sigmoid <- function(z) { 1 / (1 + exp(-z))}
for (i in 1:maxIterNum){
  P=as.vector(sigmoid(X %*% W))
  grad <- t(X) %*% (P-Y);
  grad2<- t(X) %*% diag(P*(1-P))%*%X
  if (sqrt(as.numeric(t(grad) %*% grad)) < 1e-8){
    print(sprintf('iter times=%d', i));
    break;
  }
  W <- W - solve(grad2)%*%grad  #Newton-Raphson迭代
}
print(W)
logit.glm$coefficients

#系估计标准差
(diag(solve(grad2)))^0.5
#AIC值
-2*(t(Y)%*%X %*% W-sum(log(1+exp(X %*% W))))+8
#伪R方
1-logit.glm$deviance/logit.glm$null.deviance
#概率预测值
logit.glm$fitted.values
#注意不能用predict函数
#y预测值
(yh=as.numeric(logit.glm$fitted.values>0.5))
#混淆矩阵
(hx=table(yh,Y))
#预测正确比例
sum(diag(hx))/sum(hx)
#PEA
xbat<-(apply(X,2,mean))
dlogis(sum(xbat*W))*W
#AME
mean(dlogis(X %*% W))*W


logit.step<-step(logit.glm,direction="both")#逐步筛选法变量选择
summary(logit.step)#逐步筛选法变量选择结果
pre1<-predict(logit.step,data.frame(x1=1))#预测视力正常司机Logistic回归结果
p1<-exp(pre1)/(1+exp(pre1))#预测视力正常司机发生事故概率
pre2<-predict(logit.step,data.frame(x1=0))#预测视力有问题的司机Logistic回归结果
p2<-exp(pre2)/(1+exp(pre2))#预测视力有问题的司机发生事故概率
c(p1,p2)#结果显示


###糖尿病案例------
diabetes<-read.csv("diabetes.csv",header=T)
diabetes$Diabetes=factor(diabetes$Diabetes)  #因变量y必须为因子

###划分训练集和测试集----
id.train<- 1:450  #文件中注明前450个观察为训练集
id.test <- 451:724 
data.train<- diabetes[id.train, ]
data.test <- diabetes[id.test, ]

prop.table(table(data.train[,2])) #训练集糖尿病率36.44%
prop.table(table(data.test[,2]))  #测试集糖尿病率31.02%，两者大致相等

###Logistic----
#全变量
glm.fits=glm(Diabetes~.,data=diabetes,subset=id.train,family=binomial);glm.fits
summary(glm.fits)
glm.probs=predict(glm.fits,data.test,type="response")
y.pred <- rep("neg", 274)
y.pred[glm.probs>.5]<-"pos"
y.pred <- factor(y.pred,levels=c("pos","neg"))
y <- factor(data.test$Diabetes,levels=c("pos","neg"))
t2=table(y,y.pred);colnames(t2)=c("pos_pred","neg_pred")
t2
Accuracy=mean(y.pred==y)
Precision=t2[1,1]/sum(t2[1,])
Recall=t2[1,1]/sum(t2[,1])
F1=2*Precision*Recall/(Precision+Recall)
op2=c(Accuracy=Accuracy,Precision=Precision,Recall=Recall,F1=F1);op2
#对训练集预测正确率为80.66%，其中阴性预测正确率为88.89%，阳性预测正确率为62.35%
#Logistic回归的预测能力略好于朴素贝叶斯，但对阴性预测能力较好，对阳性预测能力较差

#逐步回归
step.glm=step(glm.fits)
summary(step.glm)
glm.probs=predict(step.glm,data.test,type="response")
y.pred <- rep("neg", 274)
y.pred[glm.probs>.5]<-"pos"
y.pred <- factor(y.pred,levels=c("pos","neg"))
y <- factor(data.test$Diabetes,levels=c("pos","neg"))
t2=table(y,y.pred);colnames(t2)=c("pos_pred","neg_pred")
t2
Accuracy=mean(y.pred==y)
Precision=t2[1,1]/sum(t2[1,])
Recall=t2[1,1]/sum(t2[,1])
F1=2*Precision*Recall/(Precision+Recall)
op2=c(Accuracy=Accuracy,Precision=Precision,Recall=Recall,F1=F1);op2
#对训练集预测正确率为80.66%，其中阴性预测正确率为88.89%，阳性预测正确率为62.35%
#Logistic回归的预测能力略好于朴素贝叶斯，但对阴性预测能力较好，对阳性预测能力较差

#均值边际效应 PEA
X=as.matrix(cbind(1,data.train[,-c(1,2,5)]))
xbat<-(apply(X,2,mean))
W=coef(step.glm)
dlogis(sum(xbat*W))*W
#平均边际效应 AME
mean(dlogis(X %*% W))*W


## —— 5.2.3 对数线性模型 -----------------------
d5.2=read.xlsx('mvstats5.xlsx','d5.2');head(d5.2)
log.glm<-glm(y~x1+x2,family=poisson(link=log),data=d5.2)#多元对数线性模型
summary(log.glm)#多元对数线性模型结果

Y=d5.2[,1];X=as.matrix(cbind(x0=1,d5.2[,2:3]))
maxIterNum <- 20000;
W <- rep(1, ncol(X))
m = nrow(X)
for (i in 1:maxIterNum){
  la=as.vector(exp(X %*% W))
  grad <- t(X) %*% (la-Y);
  grad2<- t(X) %*% diag(la)%*%X
  if (sqrt(as.numeric(t(grad) %*% grad)) < 1e-8){
    print(sprintf('iter times=%d', i));
    break;
  }
  W <- W - solve(grad2)%*%grad
}
print(W)
log.glm$coefficients
#系估计标准差
(diag(solve(grad2)))^0.5
#AIC值
s=0
for(i in 1:length(Y)){
  s=s+sum(log(1:Y[i]))
}
-2*(t(Y)%*%X %*% W-sum(exp(X %*% W))-s)+6

op=glm(y~factor(x1)+factor(x2),family=poisson(link=log),data=d5.2)#多元对数线性模型
summary(op)

## —— 5.3.1 完全随机设计模型 ----------------------
d5.3=read.xlsx('mvstats5.xlsx','d5.3');head(d5.3)
anova(lm(Y~factor(A),data=d5.3))
summary(lm(Y~factor(A),data=d5.3))
#更换基期水平
summary(lm(Y~factor(A,levels=c(2,1,3)),data=d5.3))
## —— 5.3.2 随机区组设计模型 ---------------------
d5.4=read.xlsx('mvstats5.xlsx','d5.4');head(d5.4)
anova(lm(Y~factor(A)+factor(B),data=d5.4))
summary(lm(Y~factor(A)+factor(B),data=d5.4))
## —— 5.3.3 析因设计模型 ------------------------
d5.5=read.xlsx('mvstats5.xlsx','d5.5');head(d5.5)
anova(lm(Y~factor(A)*factor(B),data=d5.5))
summary(lm(Y~factor(A)*factor(B),data=d5.5))
## —— 5.3.5 正交设计模型 ------------------------
d5.6=read.xlsx('mvstats5.xlsx','d5.6');d5.6
anova(lm(Y~factor(A)*factor(B),data=d5.6))