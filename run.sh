#! /bin/sh
##   ---------------------------------------------------------------------------
##   Translate COBOL structures to XML schema
##   ----------------------------------------
##   type run -h to get help on available options
##   ---------------------------------------------------------------------------
##   INPUT can be a folder or a file, relative or absolute, containing COBOL code
INPUT=cobol
##   OUTPUT is a folder, relative or absolute to contain generated XSD files
OUTPUT=schema

##   Use the following to set your own JVM arguments
JVM_ARGS=

##   Uncomment to run the translator in debug mode
##   JVM_ARGS="$JVM_ARGS -Dlog4j.configuration=log4j.debug.properties"


java $JVM_ARGS -jar legstar-pli2cob-${project.version}-exe.jar -i$INPUT -o$OUTPUT "$@"
