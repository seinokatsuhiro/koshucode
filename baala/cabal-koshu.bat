@echo off
rem
rem  Build and install the koshu command using cabal sandbox.
rem

set CAB_BIN=%CD%\cabal\sandbox\bin
set CAB_KOSHU=%CAB_BIN%\koshu-cmd.bat
set CAB_SAMPLE=%CAB_BIN%\SAMPLE.k

:MAIN
	set /p res="Please type [i] to install koshu command or [d] to delete sandbox: "

	if "%res%" == "i" call :CALCULATOR install
	if "%res%" == "d" call :CALCULATOR delete

	echo.
	pause

goto :EOF
:CD
	cd %1

	echo.
	echo ======================================== %1
	echo.
	echo Entering %CD%
	echo.

goto :EOF
:CALCULATOR
	setlocal
	call :CD calculator

	if "%1" == "install" (
		cabal sandbox init --sandbox=..\cabal\sandbox

		cabal sandbox add-source ..\base
		cabal sandbox add-source ..\core
		cabal sandbox add-source ..\writer
		cabal sandbox add-source ..\operator
		cabal sandbox add-source ..\content

		cabal install --only-dependencies
		cabal install
		if exist %CAB_BIN% call :KOSHU
	)
	if "%1" == "delete" (
		cabal clean
		cabal sandbox delete
	)

goto :EOF
:KOSHU
	call :SAMPLE    > %CAB_SAMPLE%
	call :KOSHU_CMD > %CAB_KOSHU%
	explorer  /select,%CAB_KOSHU%

goto :EOF
:KOSHU_CMD
	echo @echo off

	echo rem
	echo rem  Invoke 'cmd.exe' with a path for 'koshu.exe'
	echo rem
	echo.

	echo set PATH=%%CD%%;%%PATH%%
	echo koshu -V
	echo cmd

goto :EOF
:SAMPLE
	echo ** -*- koshu -*-
	echo **
	echo **  USAGE
	echo **    koshu SAMPLE.k
	echo **
	echo.

	echo **  Input
	echo.

	echo ^|-- P  /a 1  /b 40
	echo ^|-- P  /a 1  /b 50
	echo ^|-- P  /a 2  /b 60
	echo.

	echo **  Output
	echo.

	echo ^|== Q : p ^| a 100
	echo ^|== R : p ^| a 200 ^| keep /a = 1
	echo.

	echo **  Relmap
	echo.

	echo p : source P /a /b
	echo a : add /c ( /a + /b + @'1 )

