package com.legstar.cob2xsd;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.ANTLRReaderStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;
import org.antlr.runtime.tree.TreeNodeStream;
import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaCollection;
import org.apache.ws.commons.schema.XmlSchemaForm;

import com.legstar.cobol.CobolSourceCleaner;
import com.legstar.cobol.CobolStructureEmitter;
import com.legstar.cobol.CobolStructureLexer;
import com.legstar.cobol.CobolStructureLexerImpl;
import com.legstar.cobol.CobolStructureParser;
import com.legstar.cobol.CobolStructureParserImpl;
import com.legstar.cobol.CobolStructureParser.cobdata_return;
import com.legstar.cobol.model.CobolDataItem;

/**
 * Implements a COBOL Structure to XSD translator.
 * This is the API made available to programmatically invoke the COBOL to XSD translator.
 * <p/>
 * There are 5 steps involved:
 * <ul>
 * <li>Cleaning the source from non COBOL Structure characters</li>
 * <li>Lexing the source to extract meaningful keywords</li>
 * <li>Parsing keywords to extract meaningful COBOL statements</li>
 * <li>Emitting a COBOL model (a set of java classes) from the Abstract Syntax Tree</li>
 * <li>Emitting XSD from the COBOL model</li>
 * </ul>
 * 
 * 
 */
public class CobolStructureToXsd {

    /** Execution parameters for the COBOL to XSD utility. */
    private Cob2XsdContext _context;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Default constructor.
     */
    public CobolStructureToXsd() {
        this(new Cob2XsdContext());
    }

    /**
     * @param context execution parameters for the COBOL to XSD utility
     */
    public CobolStructureToXsd(final Cob2XsdContext context) {
        _context = context;
    }

    /**
     * Execute the translation from COBOL to XML Schema.
     * @param cobolSource the COBOL source code
     * @return the XML Schema
     * @throws CobolStructureLexingException if COBOL structure is unreadable
     * @throws CobolStructureParsingException if source contains unsupported statements
     */
    public XmlSchema translate(
            final String cobolSource) throws CobolStructureLexingException,
            CobolStructureParsingException {
        return emitXsd(emitModel(parse(lexify(clean(cobolSource)))));
    }

    /**
     * Execute the translation from COBOL to COBOL.
     * @param cobolSourceFile the COBOL source code
     * @param targetDir folder where XSD result is to be written
     * @return the COBOL source code
     * @throws CobolStructureLexingException if COBOL structure is unreadable
     * @throws CobolStructureParsingException if source contains unsupported statements
     * @throws IOException if file read or write operation fails
     */
    public File translate(
            final File cobolSourceFile,
            final File targetDir) throws CobolStructureLexingException,
            CobolStructureParsingException, IOException {
        _log.info("Translating COBOL file: " + cobolSourceFile);
        XmlSchema xsd = translate(
                FileUtils.readFileToString(cobolSourceFile));
        String xsdFileName = cobolSourceFile.getName() + ".xsd";
        File xsdFile = new File(targetDir, xsdFileName);
        FileWriter writer = new FileWriter(xsdFile);
        xsd.write(writer);
        _log.info("Created XSD file: " + xsdFile);
        return xsdFile;
    }

    /**
     * Remove any non COBOL Structure characters from the source.
     * @param cobolSource the raw source
     * @return a cleaned up source
     */
    public String clean(final String cobolSource) {
        if (_log.isDebugEnabled()) {
            debug("Cleaning COBOL source code:", cobolSource);
        }
        CobolSourceCleaner cleaner = new CobolSourceCleaner();
        return cleaner.execute(cobolSource);
    }

    /**
     * Apply the lexer to produce a token stream from source.
     * @param source the source code
     * @return an antlr token stream
     * @throws CobolStructureLexingException if COBOL structure is unreadable
     */
    public CommonTokenStream lexify(final String source) throws CobolStructureLexingException {
        if (_log.isDebugEnabled()) {
            _log.debug("Lexing COBOL source code");
        }
        String errorMessage = "Lexing failed.";
        try {
            CobolStructureLexer lex = new CobolStructureLexerImpl(
                    new ANTLRReaderStream(new StringReader(source)));
            CommonTokenStream tokens = new CommonTokenStream(lex);
            if (lex.getNumberOfSyntaxErrors() != 0 || tokens == null) {
                _log.error(errorMessage);
                throw (new CobolStructureLexingException(errorMessage));
            }
            return tokens;
        } catch (IOException e) {
            _log.error(errorMessage, e);
            throw (new CobolStructureLexingException(e));
        }
    }

    /**
     * Apply Parser to produce an abstract syntax tree from a token stream. 
     * @param tokens the stream token produced by lexer
     * @return an antlr abstract syntax tree
     * @throws CobolStructureParsingException if source contains unsupported statements
     */
    public CommonTree parse(final CommonTokenStream tokens) throws CobolStructureParsingException {
        if (_log.isDebugEnabled()) {
            debug("Parsing tokens:", tokens.toString());
        }
        String errorMessage = "Parsing token stream failed.";
        try {
            CobolStructureParser parser = new CobolStructureParserImpl(tokens);
            cobdata_return parserResult = parser.cobdata();
            if (parser.getNumberOfSyntaxErrors() != 0 || parserResult == null) {
                _log.error(errorMessage);
                throw (new CobolStructureParsingException(errorMessage));
            }
            return (CommonTree) parserResult.getTree();
        } catch (RecognitionException e) {
            _log.error(errorMessage, e);
            throw (new CobolStructureParsingException(e));
        }
    }

    /**
     * Generates a model from an Abstract Syntax Tree. 
     * @param ast the abstract syntax tree produced by parser
     * @return a list of COBOL data items
     * @throws CobolStructureParsingException if tree cannot be walked
     */
    public List < CobolDataItem > emitModel(final CommonTree ast) throws CobolStructureParsingException {
        List < CobolDataItem > cobolDataItems = new ArrayList < CobolDataItem >();
        if (_log.isDebugEnabled()) {
            debug("Emitting Model from: ", ((ast == null) ? "null" : ast.toStringTree()));
        }
        if (ast == null) {
            return cobolDataItems;
        }
        String errorMessage = "Parsing nodes stream failed.";
        try {
            TreeNodeStream nodes = new CommonTreeNodeStream(ast);
            CobolStructureEmitter emitter = new CobolStructureEmitter(nodes);
            emitter.cobdata(cobolDataItems);
            return cobolDataItems;
        } catch (RecognitionException e) {
            _log.error(errorMessage, e);
            throw (new CobolStructureParsingException(e));
        }
    }

    /**
     * Generate an XML Schema using a model of COBOL data items.
     * @param cobolDataItems a list of COBOL data items
     * @return the XML schema
     */
    public XmlSchema emitXsd(final List < CobolDataItem > cobolDataItems) {
        if (_log.isDebugEnabled()) {
            debug("Emitting XSD from: ", cobolDataItems.toString());
        }
        XmlSchema xsd = getNewXmlSchema();
        List < String > uniqueXsdTypeNames = new ArrayList < String >();
        XsdEmitter emitter = new XsdEmitter(xsd, getContext());
        for (CobolDataItem cobolDataItem : cobolDataItems) {
            XsdDataItem xsdDataItem = new XsdDataItem(
                    cobolDataItem, getContext(), null, uniqueXsdTypeNames);
            xsd.getItems().add(emitter.createXmlSchemaType(xsdDataItem));
        }
        return xsd;
    }

    /**
     * @return a new empty XML schema using the context 
     */
    protected XmlSchema getNewXmlSchema() {
        XmlSchema xsd = new XmlSchema(
                getContext().getTargetNamespace(), new XmlSchemaCollection());
        xsd.setElementFormDefault(new XmlSchemaForm(XmlSchemaForm.QUALIFIED));
        return xsd;
    }

    /**
     * Produce long text in a delimited debug zone.
     * @param title the debug text title
     * @param text the text itself
     */
    private void debug(final String title, final String text) {
        _log.debug(title);
        _log.debug(text);
        _log.debug("----------------------------------------------------------------");
    }

    /**
     * @return the execution parameters for the COBOL to XSD utility
     */
    public Cob2XsdContext getContext() {
        return _context;
    }
}
