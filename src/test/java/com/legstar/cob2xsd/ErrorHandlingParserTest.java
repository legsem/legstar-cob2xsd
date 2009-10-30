package com.legstar.cob2xsd;

import java.io.IOException;
import java.io.StringReader;
import java.util.List;

import org.antlr.runtime.CommonToken;
import org.antlr.runtime.CommonTokenStream;

import com.legstar.antlr.ANTLRNoCaseReaderStream;

/**
 * Test cases specific to error handling.
 *
 */
public class ErrorHandlingParserTest extends AbstractCob2XsdTester {
    
    /**
     * When a lexer encounters a character it does not recognize, a
     * warning should be emitted.
     */
    public void testUnrecognizedCharacters() {
        parseAndCheck(
                "  SPACE {{ à @ 01 @ . VALUE " + LS
                , "[@0,0:5='      ',<WHITESPACE>,channel=99,1:0]");
    }

    public void testUnbalancedDelimiter() {
        parseAndCheck(
                "'[[t" + LS
                , "[@0,0:5='      ',<WHITESPACE>,channel=99,1:0]");
    }

    public void testValueWithoutALiteral() {
        parseAndCheck(
                "VALUE."
                , "[@0,0:5='      ',<WHITESPACE>,channel=99,1:0]");
    }
    
    public void testN() throws IOException {
        CobolStructureKeywordsLexer lex = new CobolStructureKeywordsLexer(
                new ANTLRNoCaseReaderStream(
                        new StringReader("IS")));
        CommonTokenStream tokens = new CommonTokenStream(lex);
        if (lex.getNumberOfSyntaxErrors() > 0) {
            System.out.println(lex.getNumberOfSyntaxErrors() + " lex errors");
        }
        List < ? > tokenl = tokens.getTokens();
        if (tokenl.size() > 0) {
            CommonToken token = (CommonToken) tokens.getTokens().get(0);
            assertEquals("RENAMES", token.getText());
            assertEquals("RENAMES_KEYWORD", CobolStructureParser.tokenNames[token.getType()]);
        } else {
            fail("not identified");
        }
        
    }
}
