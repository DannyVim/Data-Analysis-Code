unicode analyze filespec

unicode enc list

* Unicode all .dta files in CWD and files in sub-directories
unicode encoding set gb18030
unicode translate *.dta, transutf8
unicode retranslate *.dta, transutf8

* Unicode all files (.do, .ado, .dta, .hlp, etc.) in CWD and files in sub-directories
unicode encoding set gb18030
unicode translate *, transutf8

* Restore backups of translated files
unicode restore filespec [, replace ]

* Delete backups of translated files
unicode erasebackups, badidea


/*
filespec is a single filename or a file specification containing
 * and ? specifying one or more files, such as

                             *.dta
                             *.do
                             *.*
                             *
                             myfile.* 
                             year??data.dta
*/
