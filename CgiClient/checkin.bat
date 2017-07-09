@rem �������� ����������� � ������
@set /p comment=type comment (no spaces!): 

@set myvar=%date%
@set mytime=%time%
@set myday=%myvar:~0,2%
@set mymonth=%myvar:~3,2%
@set myyear=%myvar:~8,2%
@set myhour=%mytime:~0,2%
@set mymin=%mytime:~3,2%
@set backuproot=working
@if %myhour% lss 10 (set myhour=0%mytime:~1,1%)

@rem ���������� ��� �����
@set myfolder=%myyear%.%mymonth%.%myday%.%myhour%.%mymin%
@if "%comment%" neq "" (set myfolder=%myfolder%_%comment%)

@rem ��������� �����
@echo folder creating: %backuproot%\%myfolder%
@mkdir %backuproot%\%myfolder%
@mkdir %backuproot%\%myfolder%\doc

@rem ���������� �����
@echo files copying

@rem �����, ���. ����� ����������
@copy *.pas %backuproot%\%myfolder%
@copy *.lrs %backuproot%\%myfolder%
@copy *.lfm %backuproot%\%myfolder%
@copy *.lpi %backuproot%\%myfolder%
@copy *.lpr %backuproot%\%myfolder%
@copy *.htm %backuproot%\%myfolder%
@copy *.css %backuproot%\%myfolder%
@copy *.exe %backuproot%\%myfolder%
@copy *.bat %backuproot%\%myfolder%
@copy doc\log.txt %backuproot%\%myfolder%\doc
@copy doc\notes.txt %backuproot%\%myfolder%\doc
@copy doc\solutions.txt %backuproot%\%myfolder%\doc

@rem ������ �����
@echo backup compressing
@cd %backuproot%
@"7z.exe" a -r %myfolder%.7z %myfolder%

@rem �������� ���������� ������
@if %errorlevel% gtr 1 goto failure

@rem �������� ����� � ������ ������ ������
@rmdir /S /Q %myfolder%
@:failure
@cd..

@rem ������� ����������
@set comment=
@set myvar=
@set mytime=
@set myday=
@set mymonth=
@set myyear=
@set myhour=
@set mymin=
@set backuproot=
@set myfolder=
