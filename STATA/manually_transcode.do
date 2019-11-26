*对数据标签进行转码
local data_lbl: data label
local data_lbl = ustrfrom("`data_lbl'", "gb18030", 1)
label data "`data_lbl'"

*对变量名、变量标签、字符型变量取值转码
foreach v of varlist _all {
	* 对字符型变量取值进行转码
	local type: type `v'   //将变量的类型放入宏type中
	if strpos("`type'", "str") {
		replace `v' = ustrfrom(`v', "gb18030", 1)   //如果变量是字符型变量，使用ustrfrom()函数进行转码
	}

	* 对变量标签进行转码
	local lbl: var label `v'   //将变量的标签放入宏lbl中
	local lbl = ustrfrom("`lbl'", "gb18030", 1)   //使用ustrfrom()函数对`lbl'转码
	label var `v' `"`lbl'"'   //将转码后的字符串作为变量的标签

	* 对变量名进行转码
	local newname = ustrfrom(`"`v'"', "gb18030", 1)   //使用ustrfrom()函数将变量名字符串进行转码
	qui rename `v' `newname'   //将转码后的字符串重命名为变量名
}

label save using mylabel.do,replace //将数据中定义值标签的程序存入到了一个叫mylabel的do文件中

label list

