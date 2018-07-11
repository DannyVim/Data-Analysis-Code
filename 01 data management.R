#打开数据编辑器
fix(var)

#合并
#多个数据文件合并，by通常是指两个数据当中一致的变量（例如ID）
merge(var1, var2, by = 'ch')
#如果两个数据存在两个矩阵中，则用cbind
##不过不指定关键字，因而两份数据行数据要一致
cbind()
##通常，也可以列合并
rbind()

#排序
order(var, na.last = TRUE, decreasing = FALSE)
#可以输入多个var，用正负号表示升降序
order(+var,-var, ...)
#order生成的位置向量保存下来
ord <- order(...)
#也可以将排序结果保存下来
ord_var <- data[ord, ]

#缺失数据
##缺失
is.na()
##不合理值, not a number
is.nan()
###其返回的都是逻辑型向量
m <- is.na(data$var)
data[m, ] #现实var值为na的样本
##矩阵或数据框中的数据
m <- complete.cases(...)
data[!m, ] #!m表示取反，因为m中完整的为TRUE
##生成缺失数据报告
library(mice)
md.pattern(data)
##缺失数据的处理
###删除法（listwise）或是成对删除（pairwise）

#重编码
library(car)
Recode()
##手动

#数据筛选
##条件筛选
subset(data, condition)
##随机抽样
###抽样结果复现则需要设定随机种子
set.seed(1000)
###prob可以设定var向量中各元素被抽中的概率
sample(var,
       size = ,
       prob = c(),
       replace = FALSE)