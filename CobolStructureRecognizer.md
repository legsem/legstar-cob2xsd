# COBOL structure Recognizer implementation #

Parsing is achieved using the [ANLR](http://www.antlr.org/) lexer/parser.

This is a summary of the COBOL structure recognizer process implementation in legstar-cob2xsd:

![http://legstar-cob2xsd.googlecode.com/svn/wiki/images/cobol-parsing.png](http://legstar-cob2xsd.googlecode.com/svn/wiki/images/cobol-parsing.png)

The only unusual step here is the CobolStructureKeywordsLexer which is an auxiliary lexer invoked by the main CobolStructureLexer to further recognize COBOL keywords. This significantly help reduce the complexity of the Lexer generated code.

The ANTLR grammars are [here](http://code.google.com/p/legstar-cob2xsd/source/browse/#svn/trunk/src/main/antlr3/com/legstar/cobol). The Abstract Syntax Tree produced by the parser is further described in CobolStructureAbstractSyntaxTree.

The COBOL model is a set of java classes in package [com.legstar.cobol.model](http://www.legsem.com/legstar/legstar-cob2xsd/apidocs/com/legstar/cobol/model/package-summary.html).

The ANTLR generated classes are used as abstract classes by legstar-cosb2xsd. The actual implementations (with Impl suffixes) are very lightweight, they just add Apache commons logging and error formatting. The idea that grammars should be self containing and testable using [AntlrWorks](http://www.antlr.org/works/index.html).

The generation of the ANTLR code is automated thanks to the [antlr3-maven-plugin](http://antlr.org/antlr3-maven-plugin/index.html).