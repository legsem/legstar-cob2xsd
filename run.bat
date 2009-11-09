@echo off
rem ---------------------------------------------------------------------------
rem Translate COBOL structures to XML schema
rem ---------------------------------------------------------------------------
rem INPUT can be a folder or a file, relative or absolute, containing COBOL code
set INPUT=cobol
rem OUTPUT is a folder, relative or absolute to contain generated XSD files
set OUTPUT=xsd
rem Uncomment to add padding bytes to COBOL structures to accommodate PL/I structures mapping optimization
rem set ADDPAD=-addPad
rem Uncomment to add initial padding bytes to COBOL structures to accommodate PL/I hang bytes
rem set ADDHANG=-addHang

rem Use the following to set your own JVM arguments
set JVM_ARGS=

rem Uncomment to run the translator in debug mode
rem set JVM_ARGS=%JVM_ARGS% -Dlog4j.configuration=log4j.debug.properties


java -jar legstar-pli2cob-${project.version}-exe.jar -i%INPUT% -o%OUTPUT% %ADDPAD% %ADDHANG%
