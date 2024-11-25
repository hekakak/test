d9.1=read.xlsx('mvstats5.xlsx','d9.1',rowNames=T); d9.1
cor(d9.1)

#极大似然法因子分析
factanal(d9.1,3,rotation='none')  
#直接输入协差阵进行因子分析
#factanal(covmat=cov(d9.1),factors=3,rotation='none')
#提取初始因子载荷
Fa1=factanal(d9.1,3,rotation='none')
round(Fa1$loadings[1:6,],4)  
A=Fa1$loadings[1:6,]
#各变量共同度
apply(A^2,1,sum)
#各因子的方差贡献率与累计方差贡献率
apply(A^2,2,sum)/6
cumsum(apply(A^2,2,sum))/6
#特殊因子方差
Fa1$uniquenesses
#残差矩阵
cor(d9.1)
round(t(A)%*%diag(Fa1$uniquenesses^-1)%*%A,4)
round(cor(d9.1)-A%*%t(A)-diag(Fa1$uniquenesses),4)

#主成分法因子分析
#source("msaR.R")
msa.fa(d9.1,3,rotation='none')$loadings
#特征向量、特征根根求法
tz=eigen(cor(d9.1))
(tz$vectors%*%diag(tz$values^0.5))[,1:3]
#提取因子模型分解的各部分信息
Fp1=msa.fa(d9.1,3,rotation='none')
(Ap=Fp1$loadings)
Fp1$vars  #主因子法方差贡献
Fp1$common  #主因子法共同度
1-Fp1$common  #特殊因子方差
#残差矩阵
round(cor(d9.1)-Ap%*%t(Ap)-diag(1-Fp1$common),4)

#因子旋转
Fp2=msa.fa(d9.1,3,rotation='varimax'); 
Fp2$vars
cbind(Fp1$loadings,Fp2$loadings)

Fa2=factanal(d9.1,3,rotation='varimax') 
Fa2$loadings
cbind(Fa1$loadings,Fa2$loadings)


#因子得分，默认方法为加权最小二乘法
Fp1$scores  #旋转前因子得分
Fp2$scores  #旋转后因子得分
cbind(Fp1$scores,Fp2$scores)

#综合得分
cbind(Fp1$ranks,Fp2$ranks)

##极大似然法综合排名
Fa1=factanal(d9.1,3,rot="varimax",scores="Bartlett")#使用回归估计法的极大似然法因子分析
Fa1$scores
W=apply(Fa1$loadings^2,2,sum)
ss=Fa1$scores%*%W/sum(W)  #综合得分
rr=rank(ss)     #综合排名
cbind(score=ss,rank=15-rr)

biplot(Fa1$scores,Fa1$loadings) #前2个因子信息重叠图 


####稳健因子分析模拟例子------
##假设存在两个公共因子
s=diag(6)
s[1,2]=s[2,1]=s[1,3]=s[3,1]=s[2,3]=s[3,2]=0.95
s[4,5]=s[5,4]=s[4,6]=s[6,4]=s[5,6]=s[6,5]=0.90
s    #总体相关系数矩阵

set.seed(221207)
dat_nomal=data.frame(MASS::mvrnorm(100, mu=rep(0,6),Sigma=s))
colnames(dat_nomal)=paste0("X",1:6)
cor(dat_nomal)    #样本相关系数矩阵

factanal(dat_nomal,2,rotation='none') 

factanal(dat_nomal,2,rotation="varimax") 

fac=factanal(dat_nomal,2,rotation="varimax",scores="Bartlett") 
F1=fac$scores
W1=apply(fac$loadings^2,2,sum)

###加入5个异常值
dat_outter=rbind(dat_nomal,data.frame(matrix(c(5,-5),5,6)))
tail(dat_outter)
factanal(dat_outter,2,rotation='none') 
factanal(dat_outter,2,rotation="varimax") 

fac_out=factanal(dat_outter,2,rotation="varimax",scores="Bartlett") 
F2=fac_out$scores
W2=apply(fac_out$loadings^2,2,sum)

###采用稳健协差矩阵估计
cor_robu=MASS::cov.rob(dat_outter,cor=T,method="mcd")

factanal(covmat=cor_robu,factors=2,rotation='none')
factanal(covmat=cor_robu,factors=2,rotation="varimax")

fac_robu=factanal(covmat=cor_robu,factors=2,rotation="varimax")
A=unclass(fac_robu$loadings)
O=diag(fac_robu$uniquenesses)
F3=as.matrix(dat_outter)%*%t(solve(t(A)%*%solve(O)%*%A)%*%t(A)%*%solve(O))
W3=apply(fac_robu$loadings^2,2,sum)

##比较不同情况的效果
round(cbind(F1,F2[1:100,],F3[1:100,]),4)
round(cbind(W1,W2,W3),4)

FF1=F1%*%(W1/sum(W1))
FF2=F2[1:100,]%*%(W2/sum(W2))
FF3=F3[1:100,]%*%(W3/sum(W3))

round(cbind(FF1,FF2,FF3),4)
cbind(rank(FF1),rank(FF2),rank(FF3))

