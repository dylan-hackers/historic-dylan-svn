@echo off

REM     LOW TECH = GOOD

REM Add "-verbose" to the following command to help in debugging.
REM ant -verbose -emacs -buildfile build.xml -DTOMCAT_HOME=%TOMCAT_HOME% %1 %2 %3 %4 %5 %6 %7 %8 %9

if "%1%" == "tags" goto TAGS

echo Building btrack application
set JAVACOMPILE=jikes +E -g

set btrack=%TOMCAT_HOME%\webapps\btrack
set classes=%btrack%\WEB-INF\classes

%JAVACOMPILE% -d %classes% -classpath %classpath%;%classes% java\*.java

copy web.xml %btrack%\WEB-INF\web.xml
copy btrack.tld %btrack%\btrack.tld
copy jsp\*.html %btrack%

goto DONE


:TAGS
echo Making tags file
etags java\*.java
etags -a *.tld
etags -a jsp\*.html
etags -a web.xml

goto DONE

:DONE
echo Done
