#! /bin/sh
##   ---------------------------------------------------------------------------
##   Translate PL/I structures to COBOL
##   ---------------------------------------------------------------------------
##   INPUT can be a folder or a file, relative or absolute, containing PL/I code
INPUT=pl1
##   OUTPUT is a folder, relative or absolute to contain generated COBOL files
OUTPUT=cobol
##   Uncomment to add padding bytes to COBOL structures to accommodate PL/I structures mapping optimization
##   ADDPAD=-addPad
##   Uncomment to add initial padding bytes to COBOL structures to accommodate PL/I hang bytes
##   ADDHANG=-addHang

##   Use the following to set your own JVM arguments
JVM_ARGS=

##   Uncomment to run the translator in debug mode
##   JVM_ARGS="$JVM_ARGS -Dlog4j.configuration=log4j.debug.properties"


java $JVM_ARGS -jar legstar-pli2cob-${project.version}-exe.jar -i$INPUT -o$OUTPUT $ADDPAD $ADDHANG
