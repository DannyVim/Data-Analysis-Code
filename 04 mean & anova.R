# 均值检验
t.test(
  var ~ factor, # 独立样本
  var1,var2, #配对样本
  var, #单样本
  paired = FALSE, # 是否配对
  var.equal = FALSE, # 方差未知且不相等
  mu = 0, # 检验值
  alternative = 'two.sided' # 'less\greater'检验方向
)
## levene's方差齐性检验
### 若无显著性差异，则方差齐性
library(car)
leveneTest(var, factor, center = mean) # mean\median

# 功效分析
## exactly one of n, d, power, and sig.level must be NULL
library(pwr)
pwr.t.test(
  d =  效应量,
  n = N,
  sig.level = 0.05,
  power = 统计功效,
  type = 'two.sample',
  alternative = 'two.sided'
)
## 样本量不同时
pwr.t2n.test(
  d = 效应量,
  n1 = N,
  n2 = N,
  sig.level = 0.05,
  power = 统计功效,
  type = 'two.sample',
  alternative = 'two.sided'
)

## 相关系数也可以检验
### 用法与涵义同上
pwr.r.test(
  r = coefficients,
  n = N,
  sig.level = 0.05,
  power = 0.8,
  alternative = 'two.sided'
)
## 列联表卡方、方差等都可以

# 检验分布差异的非参数检验
## 两独立样本
### Wilcoxon rank sum test
### 若p-value显著，则两者分布具有显著差异
wilcox.test(var1~factor)
### Kolmogorov-Smirnov test
ks.test(var1,var2)
## 配对样本
wilcox.test(var1,var2,paired = TRUE)

# 样本均值差的置换检验 Fisher-Pitman Permutation Test
## 检验两个总体均值是否存在显著差异，但不知道置信区间
library(coin)
oneway_test(var1 ~ factor, distribution = 'exact') #'exact,asymptotic,approximate(B=1000)'
## 置换检验也用于相关系数、总体分布
spearman_test(var1~var2,distribution='asymptotic')
chisq_test(factor1~factor2,distribution=approximate(10000))
wilcoxsign_test(var1~var2,distribution='exact')

# 样本均值差的自举法检验
DiffMean <- function(dataset, indices) {
  resample <- dataset[indices, ]
  diff <-
    tapply(resample[, 1], INDEX = as.factor(resample[, 2]), FUN = mean)
  return(diff[1] - diff[2])
}
library(boot)
bootObject <- boot(data = dataset,
                   statistics = DiffMean,
                   R = 200)
plot(bootObject)
boot.ci(bootObject, conf = 0.95, type = 'all')






# 方差分析
# 单因素方差分析
aov(var1~factor)
## 如果要包含方差分析表，则需要summary或anova函数，效果一致
anova(aovObject)
## 均值变化折线图（带置信区间）
plotmeans(
  var ~ factor,
  data = dataset,
  p = 0.95,
  use.t = TRUE,
  xlab = "var",
  ylab = "factor",
  main = "不同factor的var总体均值变化折线图(95%置信区间)"
)

## 方差分析前提
### 正态性检验
#### Q-Q图
qqnorm(var)
qqline(var,distribution = qnorm)
##### 或者
library("lattice")
qqmath(~var|factor)
#### Kolmogorov-Smirnov test
ks.test(var,'pnorm')
#### 方差齐性性检验
library("car")
leveneTest(var,factor, center=mean)
## 多重比较检验
## 基础分析只能判断控制变量是否对观测变量产生显著影响
## 进一步需要知道的是：
### 控制变量不同水平的影响程度如何
### 哪个水平的作用比较突出;哪个水平的作用不显著等
## LSD检验：观察参数估计的结果
OneWay<-aov(var~factor)
OneWay$coefficients
## Tukey HSD检验：因子之间两两比较
TukeyHSD(OneWay,ordered=TRUE,conf.level=0.95)

## 功效分析
pwr.anova.test(
  k = 控制变量水平数,
  f = 效应量,
  n = N,
  sig.level = 0.05,
  power = 0.8
)
## 当方差分析的前提无法满足时，换用置换检验
## 单因素方差分析的置换检验
library('lmPerm')
aovp(var~factor,perm = 'Exact')

# 单因素协方差分析
aov(var~var1+factor)
# 单因素协方差分析的前提检验
coplot(var~var1|factor)
# 多重检验
library("effects")
effect("factor",aovObject)
plot(effect("factor",aovObject))
# 置换检验同上

# 多因素方差分析
aov(formula)
## 变量与交互
## y~A+B 即：A,B两者的影响
## y~A+B+A:B 即：加上A,B交互的影响
## y~A*B*C 即：A,B,C三者独立、二阶交互、三阶交互的影响
## y~(A+B+C)^2 即：A,B,C三者+二阶交互作用
## y~. 即：剩余所有变量的独立作用
## 变量的顺序是敏感的，有三种类型：
### 序贯性（次序在前的最重要）
### 分层型（同阶的地位平等）
### 边际型（各变量地位平等，不做调整）
## R 默认为序贯型；SPSS等默认为边际型
## 可视化交互效应
interaction.plot(
  factor1,
  factor2,
  var,
  type = "b",
  main = "var1和var2对var的交互效应",
  xlab = "var1",
  ylab = "var"
)
## 置换检验同上