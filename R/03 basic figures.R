# 绘图部分涉及到大量参数，在此不详述
# 此部分内容为 R 基础包内的内容，不包括ggplot、lattice等
#
# pch  符号种类
# cex  符号大小
# bg   符号填充色
# lty  线条线型
# lwd  线条宽度
# col  颜色
# xlab 横坐标
# ylab 纵坐标
# main 主标题

#图形布局
par(mfrow = c(2, 2), mar = c(n1, n2, n3, n4))
## mfrow表示逐行，mfcol表示逐列；mar设置下、左、上、右边界的宽度
## layout更加自由地摆放图形
## matrix函数的值，0表示不放，1代表第一幅（此处横跨1、2列）
MyLayout <- matrix(c(1, 1, 0, 2),
                   nrow = 2,
                   ncol = 2,
                   byrow = TRUE)
## widths\heights表示图形高度比、宽度比
## respect表示坐标刻度单位是否统一
DrawLayout <-
  layout(
    MyLayout,
    widths = c(1, 1),
    heights = c(1, 2),
    respect = TRUE
  )
layout.show(DrawLayout)

# 图形参数
## par统一指定了绘图的参数值
par()
## 否则，则使用各绘图函数的参数值调整
#图例说明
legend()

# 茎叶图
stem(var)
# 箱线图
## axes 是否带有坐标轴；ylim 纵坐标范围；horizontal 横竖放置
boxplot(var,
        horizontal = ,
        axes = TRUE,
        ylim = c(0, 100))
## 分组
### var2为分组，交叉分组则用*连接；names则为各组的标识信息
boxplot(var1 ~ var2[ * var3], data = , names = c())
# 直方图
## true频数，false频率
hist(var, freq = TRUE)
## 在已有图形上添加曲线
lines(x = 横坐标向量, y = 纵坐标向量)
# MeanTmp=mean(Forest$temp,rm.na=TRUE)
# SdTmp=sd(Forest$temp)
# d=seq(from=min(Forest$temp),to=max(Forest$temp),by=0.1)
# hist(Forest$temp,xlab="温度",ylab="频率",main="森林地区温度直方图",cex.lab=0.7,freq=FALSE,ylim=c(0,0.08))
# lines(x=d,y=dnorm(d,MeanTmp,SdTmp),lty=2,col=2)
# 柱形图
## names.arg 指定条形的类别标签，字符串型向量
## heights指的是高度，输入的var都会按照高度进行计算；可以输入矩阵
barplot(heights , horiz = , names.arg = c())
# 饼图
pie(var, labels =  切片向量, clockwise = TRUE)
# 马赛克图
mosaicplot()
## 或是用vcd包
library(vcd)
mosaic(formula, shade = TRUE, legend = TRUE)

# 散点图
plot(var1, var2)
plot(var1 ~ var2)
## 一元线性拟合
fit <- lm(var1 ~ var2)
abline(fit$coefficients)
## 局部加权散点平滑法拟合回归
M.Loess <- loess(var1 ~ var2)
Ord <- order(data$var2)  ##按x轴取值排序后再绘图
lines(
  data$var2[Ord],
  M.Loess$fitted[Ord],
  lwd = 1,
  lty = 1,
  col = 2
)
## 高密度散点图
plot(jitter(var1, factor = 1) ~ jitter(var2, factor = 1))
smoothScatter(var1 ~ var2)
## 三维散点图
library(scatterplot3d)
## 气泡图
symbols(
  var1,
  var2,
  circles = var3,
  inches = 0.1,
  fg = 'white',
  bg = 'lightblue'
)
## 矩阵散点图
pairs(formula)
### 如果需要回归曲线，则使用car包scatterplotMatrix
scatterplotMatrix(formula)
## 相关系数图
### 使用corrgram包
library(corrgram)
corrgram(
  矩阵,
  lower.panel = panel.pts,
  upper.panel = panel.ellipse,
  diag.panel = panel.minmax
)
## 分组散点图
coplot(var1 ~ var2 | groupvar)

# lattice 绘图
# func(formula|groupvar,options)
library(lattice)
bwplot()
histogram()
densityplot()
dotplot()
barchart()
xyplot()
cloud()