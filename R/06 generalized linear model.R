# logistic model
## family用以指定被解释变量所服从的概率分布
### logistic regression 为 binominal
### 还可以取值gaussian（正态）,poisson（泊松）
### 如果无法确定分布，则使用准极大似然估计:quasi,quasibinomial,quaipoisson
## link用以指定对应分布的连接函数
### logistic = logit; gaussian = identity（此时，广义线性模型既为一般线性模型）;poisson=log
fit <- glm(formula, family = binomial(link='logit'))
anova(fit,test='Chisq') #根据卡方分布进行回归方程的显著性检验
summary(fit) #会附上wald检验结果
exp(coef(fit)) #直接给出优势比的比值 哈哈哈

## 回归诊断
### GLM的图形诊断
glm.diag.plots(Fit)
### 基于混淆矩阵的结果
#### 预测准确率越高，认为模型就越好
ResultProb<-predict(fit,dataset,type="response")
YorN<-ifelse(ResultProb>0.5,1,0)
(ConfuseMatrix<-table(dataset$y,ResultProb))
prop.table(ConfuseMatrix,1)*100
### McFadden R Square
anova(fit)
McR2<-1-anova(fit)[3,4]/anova(fit)[1,4]
### 过散布诊断
#### 如果出现过散布的情况，则应使用准极大似然法
summary(fit)$dispersion #浏览默认散布系数，link='logit'时默认为1
fit$deviance/fit$df.residual #计算散步值
#### 或者用qcc包
library("qcc")
Count.01<-tapply(y,INDEX=y,FUN=length)  #计算y的0、1的观测个数
qcc.overdispersion.test(c(0,1),type="binomial",size=Count.01)

# 泊松回归
## 当被解释变量是单位时间内事件发生的次数，即被解释变量是计数变量时
## 研究单位时间内事件发生次数y的均值
## 对数-水平模型，回归系数的涵义即：当其他解释变量保持不变时，x1变动一个单位，将引起y的对数平均变动β个单位
fit <- glm(formula, family = poisson(link='log'))
anova(fit,test='Chisq')
summary(fit)
exp(coef(fit))
## 过散布的诊断同上
fit$deviance/fit$df.residual
qcc.overdispersion.test(y,type='poisson')

# 广义线性模型的交叉验证
library("boot")
sum(fit$residuals^2)/fit$df.residual   #模型的预测误差
set.seed(12345)
# K代表交叉验证的次数
cv.glm(data=dataset,glmfit=fit,K=8)$delta   #交叉验证给出的模型的预测误差估计
