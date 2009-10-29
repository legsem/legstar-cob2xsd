package com.legstar.antlr;

import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.Token;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.DOTTreeGenerator;
import org.antlr.runtime.tree.Tree;
import org.antlr.stringtemplate.StringTemplate;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import junit.framework.TestCase;

/**
 * Generic test code for ANTLR based lexers parsers and tree walkers.
 *
 */
public abstract class AbstractAntlrTester extends TestCase {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** Line separator (OS specific).*/
    public static final String LS = System.getProperty("line.separator");
    
    /**
     * Cleanup source an compare to expected.
     * @param source original source
     * @param expected expected result
     */
    public void cleanAndCheck(final String source, final String expected) {
        assertEquals(expected, clean(source));
        
    }

    /**
     * Apply a lexer to a source and check that the token stream produced
     * is as expected.
     * @param source the source code
     * @param expected the expected token stream
     */
    public void lexAndCheck(final String source, final String expected) {
        CommonTokenStream ts = lex(source);
        StringBuilder sb = new StringBuilder();
        for (Object token : ts.getTokens()) {
            sb.append(toString((Token) token));
        }
        assertEquals(expected, sb.toString());
    }

    /**
     * A generic test helper that takes a source fragment and checks the result.
     * @param source the source fragment
     * @param expected the expected sub graph
     */
    public void parseAndCheck(final String source, final String expected) {
        CommonTree ast = parse(source);
        if (_log.isDebugEnabled()) {
            _log.debug(getGraph(ast).toString());
        }
        assertEquals(expected, (ast == null) ? "" : ast.toStringTree());
    }

    /**
     * @param token a lexer token
     * @return same as Token.toString but with token type label rather than int
     */
    private String toString(final Token token) {
        return token.toString().replace("<" + token.getType() + ">",
                "<" + getTokenNames()[token.getType()] + ">");
    }

    /**
     * Produce a dot source for an abstract syntax tree.
     * @param ast the abstract syntax tree
     * @return a dot source
     */
    private String getGraph(final Tree ast) {
        DOTTreeGenerator gen = new DOTTreeGenerator();
        StringTemplate st = gen.toDOT(ast);
        return st.toString();
    }

    /**
     * Perform initial source cleanup to keep ANLR grammar simple.
     * @param source original source code
     * @return cleaned up source code
     */
    public abstract String clean(final String source);

    /**
     * Apply the lexer to produce a token stream from source.
     * @param source the source code
     * @return an antlr token stream
     */
    public abstract CommonTokenStream lex(final String source);
    
    /**
     * Apply Lexer + Parser to produce an abstract syntax tree from source. 
     * @param source the source code
     * @return an antlr abstract syntax tree
     */
    public abstract CommonTree parse(final String source);

        /**
     * @return the parser token names (nicer looking than integer types)
     */
    public abstract String[] getTokenNames();
}
