SET ORIGINAL_DIR=%CD%
REM find out where this batch file is located
cd /d %0\..
SET OZFLYMAKEPATH=%CD%
REM go back to original dir
cd /d %ORIGINAL_DIR%

ozengine "%OZFLYMAKEPATH%/IsFunctor.ozf" "%1"
IF ERRORLEVEL 1 GOTO NONFUNCTOR

ozc -c "%1" 2>&1 | ozengine "%OZFLYMAKEPATH%/PostprocessErrors.ozf"
REM delete .ozf file created by ozc
DEL "%1f"
REM 
GOTO END

:NONFUNCTOR
ozengine "%OZFLYMAKEPATH%/OPISyntaxChecker.ozf" "%1" 2>&1 | ozengine "%OZFLYMAKEPATH%/PostprocessErrors.ozf"
GOTO END

:END
IF "%2" == "t" ozengine "%OZFLYMAKEPATH%/CheckArity.ozf" --source "%1" 2>&1 | ozengine "%OZFLYMAKEPATH%/PostprocessErrors.ozf"



