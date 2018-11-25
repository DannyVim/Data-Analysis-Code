
Copyright@计量经济圈(ID: econometrics666)
____________________________________________________________________________________

webuse nlswork  //使用系统自带数据库
xtset idcode year, delta(1)  //设置面板
xtdescribe   //描述一下这个面板数据情况

gen age2= age^2
gen ttl_exp2=ttl_exp^2
gen tenure2=tenure^2

global xlist "grade age age2 ttl_exp ttl_exp2 tenure tenure2 not_smsa south race"
sum ln_w $xlist  //统计描述相关变量

————————————————————————————————————————————————————————————————————————————————————
**DID方法-----------------------------------
gen time = (year >= 77) & !missing(year)  //政策执行时间为1977年
gen treated = (idcode >2000)&!missing(idcode) //政策执行地方为idcode大于2000的地方
gen did = time*treated  //这就是需要估计的DID，也就所交叉项

reg ln_w did time treated $xlist //这就是一个OLS回归，也可以用diff命令
xtreg ln_w did time treated $xlist i.year, fe //也可以这去做，会省略掉一个虚拟变量

——————————————————————————————————————————————————————————————————————————————————————————————————————————————
**PSM-DID方法-------------------------------

** PSM的部分
set seed 0001	//定义种子
gen tmp = runiform() //生成随机数
sort tmp //把数据库随机整理
psmatch2 treated $xlist, out(ln_w) logit ate neighbor(1) common caliper(.05) ties //通过近邻匹配，这里可以要outcome，也可以不要它
pstest $xlist, both graph  //检验协变量在处理组与控制组之间是否平衡
gen common=_support
drop if common == 0  //去掉不满足共同区域假定的观测值
psgraph

** DID的部分，根据上面匹配好的数据
reg ln_w did time treated $xlist 
xtreg ln_w did time treated $xlist i.year, fe

**PSM-DID部分结束--------------------------------------
————————————————————————————————————————————————————————————————————————————————————————————————————————————
**DID方法需要满足的五个条件检验------------------------

**1.共同趋势假设检验

tab year, gen(yrdum) //产生year dummy，即每一年一个dummy变量
     forval v=1/7{
gen treated`v'=yrdum`v'*treated
}                     //这个相当于产生了政策实行前的那些年份与处理虚拟变量的交互项
xtreg ln_w did treated*  i.year ,fe  //这个没有加控制变量
xtreg ln_w did treated* $xlist i.year ,fe //如果did依然显著，且treated*这些政策施行前年份交互项并不显著，那就好
xtreg ln_w did treated* $xlist i.year if union!=1 ,fe //我们认为工会会影响这个处理组和控制组的共同趋势，因此我们看看union=0的情形

**2.政策干预时间的随机性
gen time1 = (year >= 75) & !missing(year)  //政策执行时间提前到1975年
capture drop treated1
gen treated1= (idcode >2000)&!missing(idcode) //政策执行地方为idcode大于2000的地方
gen did1 = time1*treated1  //这就是需要估计的DID，也就所交叉项

gen time2 = (year >= 76) & !missing(year)  //政策执行时间提前到1976年
capture drop treated2
gen treated2= (idcode >2000)&!missing(idcode) //政策执行地方为idcode大于2000的地方
gen did2 = time2*treated2  //这就是需要估计的DID，也就所交叉项

xtreg ln_w did1 $xlist i.year,fe 
xtreg ln_w did2 $xlist i.year,fe //看看这两式子里did1和did2显著不,显著为好

**3.控制组将不受到政策的影响
gen time3 = (year >= 77) & !missing(year) 
capture drop treated3 
gen treated3= (idcode<1600 & idcode>1000)&!missing(idcode) //我们考虑一个并没有受政策影响地方假设其受到政策影响
gen did3 = time3*treated3  
xtreg ln_w did3 $xlist i.year,fe //最好的情况是did3不显著，证明控制组不受政策影响


**4.政策实施的唯一性，至少证明这个政策才是主要影响因素
gen time4 = (year >= 77) & !missing(year)  
capture drop treated4
gen treated4= (idcode<3000 & idcode>2300)&!missing(idcode) //我们寻找某些受到其他政策影响的地方
gen did4 = time4*treated4  
xtreg ln_w did4 $xlist i.year,fe //did4可能依然显著，但是系数变小，证明还受到其他政策影响

**5.控制组和政策影响组的分组是随机的

xi:xtivreg2 ln_w (did=hours tenure) $xlist i.year,fe first //用工具变量来替代政策变量，解决因为分组非随机导致的内生性问题

——————————————————————————————————————————————————————————————————————————————————————————————————
**附加的，一般而言，我们需要看看这个政策的动态影响-------------
     forval v=8/15{
gen treated`v'=yrdum`v'*treated  
}               //注意，这里yrdum8就相当于year=78
	
reg  ln_w treated*  

xtreg  ln_w treated*, fe

xtreg ln_w treated* i.year,fe 

xtreg ln_w treated* $xlist i.year,fe  //一般而言上面这些式子里的treated*应该至少部分显著

————————————————————————————————————————————————————————————————————————————————————————————————————————

**RDD断点回归在这里的应用-------------------

tab treated,missing
keep if treated==1
cmogram ln_w year,cut(77) scatter lineat(77) qfitci //绘制断点回归图形

**最优带宽
set more off
rdbwselect ln_w year if 60<=year&year<=85,c(77) kernel(uni) //自己下载安装
rdob ln_w year, c(77)                      //这个命令是imbens新开发的，这里有rdob的程序

reg ln_w time $xlist if 60<year&year<80  //直接对time这个虚拟变量做了ols








	





