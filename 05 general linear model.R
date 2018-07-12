# OLS
## 变量与交互
## y~A+B 即：A,B两者的影响
## y~A+B+A:B 即：加上A,B交互的影响
## y~A*B*C 即：A,B,C三者独立、二阶交互、三阶交互的影响
## y~(A+B+C)^2 即：A,B,C三者+二阶交互作用;^3则涵盖到三阶
## y~. 即：剩余所有变量的独立作用
## y~A*B-A:B 即：剔除指定的解释变量
## y~.-1 即：去掉截距项
## y~I(A+B)+c 即：用以包含数学表达式
## 各项都可以用函数名，例如log(y)~A+B
fit <- lm(formula)
summary(fit)
confint(fit) #显示回归系数默认的95%置信区间
fitY <- predict(fit, dataset, type = 'response')
## 预测与实际值可视化比对
plot(A,y,pch=1,xlab="A",ylab="y")
points(A,fitY,pch=10,col=2)
legend("topright",c("实际值","拟合值"),pch=c(1,10),col=c(1,2))

## 回归诊断
### 绘制残差图
### Q-Q:用于检验因变量的正太分布性，若服从正太分布，则散点应分布在一条直线上
### Residuals vs Fitted: 等方差时，残差的反差不应该随拟合值的增大而呈现明显趋势
### Resuals vs Leverage: 独立性检验
### Scale-Location: 解释变量的充分性，应当不存在趋势性；异常观测点
# (1)正态性（主要使用QQ图） 当预测变量值固定时，因变量成正态分布，则残差值也应该是一个均值为0的正态分布。正态Q-Q图（Normal Q-Q，右上）是在正态分布对应的值下，标准化残差的概率图。若满足正态假设，那么图上的点应该落在呈45度角的直线上；若不是如此，那么就违反了正态性的假设。
# (2)独立性 你无法从这些图中分辨出因变量值是否相互独立，只能从收集的数据中来验证。上面的例子中，没有任何先验的理由去相信一位女性的体重会影响另外一位女性的体重。假若你发现数据是从一个家庭抽样得来的，那么可能必须要调整模型独立性的假设。
# (3)线性（使用左上角的图，该曲线尽量拟合所有点） 若因变量与自变量线性相关，那么残差值与预测（拟合）值就没有任何系统关联。换句话说，除了白噪声，模型应该包含数据中所有的系统方差。在“残差图与拟合图”Residuals vs Fitted，左上）中可以清楚的看到一个曲线关系，这暗示着你可能需要对回归模型加上一个二次项。
# (4)同方差性（左下角，点随机分布在曲线的周围） 若满足不变方差假设，那么在位置尺度图（Scale-Location Graph，左下）中，水平线周围的点应该随机分布。该图似乎满足此假设。最后一幅“残差与杠图”（Residuals vs Leverage，右下）提供了你可能关注的单个观测点的信息。从图形可以鉴别出离群点、高杠杆值点和强影响点
par(mfrow=c(2,2))
plot(fit)

### 高斯-马尔科夫假定诊断
### 在回归诊断中'car'包用到最多
library("car")
#### 自变量的正态分布
#### qqplot函数提供了更为精确的正态假设检验方法
qqPlot(fit)
#### 误差项的独立性假定（针对时间序列）
durbinWatsonTest(fit)    
#### 误差项不满足正态性
summary(powerTransform(y))   #BOX-COX，非正态性处理
#### 误差项等方差性假定
spreadLevelPlot(fit)   #若不满足，则会出现非水平线，并给出对y进行BOX-COX变换时的lambda建议值
ncvTest(fit)  #生成一个计分检验,原假设为等方差
#### 线性关系
crPlot(fit, one.page = TRUE, ask = FALSE)
### 线性模型的综合检验
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)

## 回归诊断数据异常点
### 定义绘制杠杆值的用户自定义函数
LeveragePlot <- function(fit) {
  Np <- length(coefficients(fit)) - 1
  N <- length(fitted(fit))
  plot(hatvalues(fit),
       main = "观测点的杠杆值序列图",
       ylab = "杠杆值",
       xlab = "观测编号")
  abline(2 * (Np + 1) / N, 0, col = "red", lty = 2)
  abline(3 * (Np + 1) / N, 0, col = "red", lty = 2)
  identify(1:N, hatvalues(fit), names(hatvalues(fit)))
}
LeveragePlot(fit)
### 离群点（基于学生化残差）
outlierTest(fit)
### 剔除离群点再建模，再检测
fit<-lm(y~A+B,data=dataset[-200,]) #剔除200号离群点
outlierTest(Fit)
### 强影响点（库克距离）
cooks.distance(fit)
#### 强影响点-库克距离可视化
par(mfrow=c(2,1))
plot(cooks.distance(fit),main="Cook's distance",cex=0.5)      #获得Cook距离
Np<-length(coefficients(fit))-1
N<-length(fitted(fit))
CutLevel<-4/(N-Np-1)
plot(Fit,which=4)
abline(CutLevel,0,lty=2,col="red")
#### 表现强影响点对斜率产生的影响
library(car)
avPlot(fit, ask=FALSE,onepage=TRUE,id.method='identify')
#### 异常观测点的综合展示
influencePlot(fit,id.method="identify",main="异常观测点的可视化")

### 诊断多重共线性-方差膨胀因子
library(car)
vif(fit)

## 回归建模的策略
### summary会直接给出R方
### 利用anova可以比较两个模型是否具有显著性差异
### AIC & BIC
#### 考虑复杂度和拟合优度，尽可能地小
AIC(fit1,fit2) 
BIC(fit1,fit2)
### 解释变量的筛选
#### 确定以拟合优度最高或是AIC(BIC)最小的策略之后，还需要考虑解释变量的进出顺序与方式
#### forward，backward，stepwise
step(fit,direction = 'both')
#### 或是用MASS包，AIC信息准则
library(MASS)
stepAIC(fit,direction = 'both')
### 全子集回归
library("leaps")
leapsFit<-regsubsets(y~A+B+C+D+E+F,nbest=2)
summary(leapsFit)
coef(leapsFit, c(模型编号))
#### 全子集回归的可视化评价
plot(leapsFit,scale="bic")
plot(leapsFit,scale="adjr2")
##### 或者
library("car")
subsets(leapsFit,statistic="cp",main="全子集回归模型评价图")
abline(1,1,lty=2,col="red")

## 回归方程的置换检验
### 如果该结果与满足假设前提的lm分析结果有明显差异，则需要慎重并重新审视数据，数据可能不满足假设或存在异常点
library("lmPerm")
set.seed(12345)
Fit<-lmp(y~A+B+C)

## N折交叉验证
library("bootstrap")
### k代表交叉折数
### R2>R2CV则代表模型比较乐观
MyNcross <- function(fit, k) {
  X <- as.matrix(fit$model[, 2:ncol(fit$model)]) #取得解释变量矩阵
  Y <- fit$model[, 1] # 取得被解释变量向量
  NcrossR <-
    crossval(
      x = X,
      y = Y,
      theta.fit = lsfit(X, Y),
      theta.predict = cbind(1, X) %*% lsfit(X, Y)$coef, # lsfit功能等同lm
      ngroup = k
    )
}
set.seed(12345)
Result <- MyNcross(fit, 10)
R2 <- cor(Fit$model[, 1], Fit$fitted.values) ^ 2 # 计算基于全体观测模型的判定系数
R2CV <- cor(Fit$model[, 1], Result$cv.fit) ^ 2 # 计算交叉验证的判定系数

##带虚拟变量的线性回归
### 这个与协方差分析是相同的。
### 事实上，aov()与lm()的结果是完全相同的。
Fit<-lm(y~A+B+factor)