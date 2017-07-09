
@set myvar=%date%
@set mytime=%time%
@set myday=%myvar:~0,2%
@set mymonth=%myvar:~3,2%
@set myyear=%myvar:~6,4%
@set myhour=%mytime:~0,2%
@set mymin=%mytime:~3,2%
@if %myhour% lss 10 (set myhour=0%mytime:~1,1%)

@rem собирается строка времени
@set fit_compile_time=%myyear%-%mymonth%-%myday% %myhour%:%mymin%

type data.tmp|repl "@@fit_compile_time@@" "%fit_compile_time%" > data.inc

@rem сжатие папки
@rem очистка переменных
@set comment=
@set myvar=
@set mytime=
@set myday=
@set mymonth=
@set myyear=
@set myhour=
@set mymin=
@set fit_compile_time=
