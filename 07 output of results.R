library(stargazer)

# 读入数据的时候改成data.frame
dataset <- data.frame(dataset)
# 展示数据集的描述性分析
stargazer(dataset, header = F)
# 展示attitude数据集的前四行
stargazer(dataset[1:4, ], summary = FALSE, rownames = FALSE)

# 展示回归模型结果
stargazer(
  model_1,
  model_2,
  probit_model,
  type = 'html',
  title = "Results",
  align = TRUE
)
## type默认为latex格式，除了HTML，还可以选择ascii格式(type=text)
## 对模型结果输出做部分调整
stargazer(
  model_1,
  model_2,
  probit_model,
  type = 'html',
  title = "Regression Results",
  align = TRUE,
  dep.var.labels = c('M1', 'M2'),
  # 更改因变量名
  covariate.labels = c('A', 'b', 'c', 'd'),
  # 更改自变量名
  omit.stat = c("LL", "ser", "f"),
  # 删除极大似然统计量、残差标准差、F统计量
  no.space = TRUE,
  #删除表中的空行
  ci = TRUE,
  ci.level = 0.95,
  #置信区间
  single.row = TRUE
  #参数使估计量与置信区间并排展示
)

stargazer(
  model_1,
  model_2,
  probit_model,
  title = "Results",
  align = TRUE,
  order = c("learning", "privileges"),
  # 控制自变量展示的顺序，将两个变量放在表的前两行
  keep.stat = "n" # 要展示的统计量
)


# 展示矩阵
## 可以用来展示向量、矩阵或者数据框的内容
correlation.matrix <- cor(attitude[, c("A", "B", "C")])
stargazer(correlation.matrix, title = "Correlation Matrix")

# 还可以添加自定义变量
library(sandwich)
## 计算异方差-稳健标准误
cov <- vcovHC(linear.1, type = "HC")
robust.se <- sqrt(diag(cov))
stargazer(
  linear.1,
  linear.1,
  se = list(NULL, robust.se),
  column.labels = c("default", "robust")
)
