#! /bin/sh
##   ---------------------------------------------------------------------------
##   Translate COBOL structures to XML schema
##   ----------------------------------------
##   type run -h to get help on available options
##   ---------------------------------------------------------------------------

##   Use the following to set your own JVM arguments
JVM_ARGS=

##   Update the log4j configuration to set debug mode
JVM_ARGS="$JVM_ARGS -Dlog4j.configuration=file:conf/log4j.properties"

java $JVM_ARGS -jar legstar-cob2xsd-${project.version}-exe.jar "$@"
