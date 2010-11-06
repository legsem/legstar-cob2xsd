@echo off
rem ---------------------------------------------------------------------------
rem Translate COBOL structures to XML schema
rem ----------------------------------------
rem type run -h to get help on available options
rem ---------------------------------------------------------------------------

set COB2XSD_CMD_LINE_ARGS=

:setupArgs
if %1a==a goto doneStart
set COB2XSD_CMD_LINE_ARGS=%COB2XSD_CMD_LINE_ARGS% %1
shift
goto setupArgs

:doneStart

rem Use the following to set your own JVM arguments
set JVM_ARGS=

rem Update the log4j configuration to set debug mode
set JVM_ARGS=%JVM_ARGS% -Dlog4j.configuration=file:conf/log4j.properties

java %JVM_ARGS% -jar legstar-cob2xsd-${project.version}-exe.jar %COB2XSD_CMD_LINE_ARGS%
