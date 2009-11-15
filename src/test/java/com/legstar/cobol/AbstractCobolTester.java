package com.legstar.cobol;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;
import org.antlr.runtime.tree.TreeNodeStream;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.antlr.ANTLRNoCaseReaderStream;
import com.legstar.antlr.AbstractAntlrTester;
import com.legstar.antlr.CleanerException;
import com.legstar.antlr.RecognizerErrorHandler;
import com.legstar.antlr.RecognizerException;
import com.legstar.cobol.CobolStructureParser.cobdata_return;
import com.legstar.cobol.model.CobolDataItem;

/**
 * Generic test code for ANTLR based lexers parsers and tree walkers.
 *
 */
public abstract class AbstractCobolTester extends AbstractAntlrTester {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** Handles error messages.*/
    private RecognizerErrorHandler _errorHandler = new RecognizerErrorHandler();

    /**
     * {@inheritDoc}
     * @throws CleanerException 
     */
    public String clean(final String source) throws CleanerException {
        CobolSourceCleaner cleaner = new CobolSourceCleaner();
        return cleaner.execute(source);
    }

    /**
     * {@inheritDoc}
     */
    public CommonTokenStream lex(final String source) throws RecognizerException {
        try {
            CobolStructureLexer lex = new CobolStructureLexerImpl(
                    new ANTLRNoCaseReaderStream(
                            new StringReader(
                                    clean(source))),
                                    getErrorHandler());
            CommonTokenStream tokens = new CommonTokenStream(lex);
            if (lex.getNumberOfSyntaxErrors() > 0) {
                _log.warn(lex.getNumberOfSyntaxErrors() + " lex errors");
            }
            assertTrue(tokens != null);
            return tokens;
        } catch (IOException e) {
            throw new RecognizerException(e);
        }
    }
    
    /**
     * {@inheritDoc}
     */
    public CommonTree parse(final String source) throws RecognizerException {
        try {
            CommonTokenStream tokens = lex(source);
            CobolStructureParser parser = new CobolStructureParserImpl(
                    tokens, getErrorHandler());
            cobdata_return parserResult = parser.cobdata();
            if (parser.getNumberOfSyntaxErrors() > 0) {
                _log.warn(parser.getNumberOfSyntaxErrors() + " parse errors");
            }
            assertTrue(parserResult != null);
            return (CommonTree) parserResult.getTree();
        } catch (RecognitionException e) {
            throw new RecognizerException(e);
        }
    }

    /**
     * Starting from a COBOL source fragment translates to XML Schema.
     * @param source COBOL source fragment.
     * @return an XML Schema
     * @throws RecognizerException if emit fails
     */
    public String emit(final String source)  throws RecognizerException {
        try {
            CommonTree ast = parse(source);
            if (_log.isDebugEnabled()) {
                _log.debug(ast.toStringTree());
            }
            TreeNodeStream nodes = new CommonTreeNodeStream(ast);
            CobolStructureEmitter emitter = new CobolStructureEmitterImpl(
                    nodes, getErrorHandler());
            List < CobolDataItem > dataEntries = new ArrayList < CobolDataItem >();
            emitter.cobdata(dataEntries);
            return dataEntries.toString();
        } catch (RecognitionException e) {
            throw new RecognizerException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    public String[] getTokenNames() {
        return CobolStructureParser.tokenNames;
    }

    /**
     * @return the error messages handler
     */
    public RecognizerErrorHandler getErrorHandler() {
        return _errorHandler;
    }
}
