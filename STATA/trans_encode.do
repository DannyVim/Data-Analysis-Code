unicode analyze [file]

unicode enc list

* Unicode all .dta files in CWD and files in sub-directories
unicode encoding set gb18030
unicode translate *.dta

* Unicode all files (.do, .ado, .dta, .hlp, etc.) in CWD and files in sub-directories
unicode encoding set gb18030
unicode translate *

*Restore backups of translated files
unicode restore filespec [, replace ]

*Delete backups of translated files
unicode erasebackups, badidea
