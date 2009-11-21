#! /bin/sh
##   ---------------------------------------------------------------------------
##   Translate COBOL structures to XML schema
##   ----------------------------------------
##   type run -h to get help on available options
##   ---------------------------------------------------------------------------

##   Use the following to set your own JVM arguments
JVM_ARGS=

##   Uncomment to run the translator in debug mode
##   JVM_ARGS="$JVM_ARGS -Dlog4j.configuration=log4j.debug.properties"


java $JVM_ARGS -jar legstar-cob2xsd-${project.version}-exe.jar "$@"
