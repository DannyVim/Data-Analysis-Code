#基本描述统计
summary(var, na.rm = TRUE) #剔除NA值
##计算多个变量的某个描述统计值
sapply(X, FUN = functions, na.rm = TRUE)
##可以自定义函数来完成多个变量的多个描述统计值
##分组描述
###用subset来筛选子集
###用tapply函数
tapply(var,
       INDEX = VAR,
       FUN = functions,
       na.rm = TRUE)
#psych包
install.packages("psych")
library("psych")
describe(var)

#频数分布表
table(var) #频数
prop.table(table(var)) #比值
#相关系数
cor(var, var, use = , method =)
#use = "all.obs", "complete.obs", "pairwise.complete.obs","everything"
#method = “pearson”, “kendall”, “spearman”
##相关系数矩阵(表达式外面加括号，可以直接显示结果)
(CorMatrix <- cor(data[, c(5, 7, 8)], use = "everything", method = "pearson"))
###cov函数可以算协方差
##相关系数的检验
cor.test(var1, var2, alternative = "two.side", method = "pearson")
##偏相关系数
##corpcor包
library("corpcor")
cor2pcor(CorMatrix)
cov2pcor(CovMatrix)

#列联表
table(矩阵或数据框列号)
xtabs( ~ var1 + var2, data = name)
prop.table(xtabs( ~ var1 + var2)) #比值
##汇总
margin.table(xtabs( ~ var1 + var2), 1) #行汇总
margin.table(xtabs( ~ var1 + var2), 2) #列汇总
addmargins(xtabs( ~ var1 + var2)) #行列边际合并到列联表中
addmargins(prop.table(xtabs( ~ var1 + var2)), 2) #行比值边际合并到列联表中
#gmodels包
library("gmodels")
CrossTable(var1, var2)
#卡方检验
chisq.test(列联表对象, correct = TRUE)
##correct表示是否进行yates修正
##或者利用gmodels包
CrossTable(列联表对象, chisq = TRUE)
##计算基于卡方的相关系数
library("vcd")
assocstats(列联表对象)