package com.legstar.cob2xsd;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

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

import com.legstar.antlr.CleanerException;
import com.legstar.antlr.RecognizerErrorHandler;
import com.legstar.antlr.RecognizerException;
import com.legstar.cobol.CobolSourceCleaner;
import com.legstar.cobol.CobolStructureEmitter;
import com.legstar.cobol.CobolStructureEmitterImpl;
import com.legstar.cobol.CobolStructureLexer;
import com.legstar.cobol.CobolStructureLexerImpl;
import com.legstar.cobol.CobolStructureParser;
import com.legstar.cobol.CobolStructureParserImpl;
import com.legstar.cobol.CobolStructureParser.cobdata_return;
import com.legstar.cobol.model.CobolDataItem;

/**
 * Implements the COBOL Structure to XML Schema translator.
 * This is the API made available to invoke the COBOL to XML Schema translator
 *  from your own java code.
 * <p/>
 * There are 6 steps involved:
 * <ul>
 * <li>Cleaning the source from non COBOL Structure characters</li>
 * <li>Lexing the source to extract meaningful keywords</li>
 * <li>Parsing keywords to extract meaningful COBOL statements</li>
 * <li>Emitting a COBOL model (a set of java classes) from the Abstract Syntax Tree</li>
 * <li>Emitting XML Schema from the COBOL model</li>
 * <li>Writing the XML Schema, optionally applying a customization XSLT</li>
 * </ul>
 * <p/>
 * All options are bundled in {@link Cob2XsdContext} instance that is received at
 *  construction time.
 * <p/>
 * To invoke the translator, you normally call one of {@link #translate(String)},
 *  {@link #translate(File, File)} or {@link #translate(File, String, File)} methods.
 * <p/>
 * Any error encountered and recovered from is available in {@link #getErrorHistory()}.
 * 
 */
public class CobolStructureToXsd {

    /** Execution parameters for the COBOL to XSD utility. */
    private Cob2XsdContext _context;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** Handles error messages.*/
    private RecognizerErrorHandler _errorHandler = new RecognizerErrorHandler();

    /**
     * Default constructor.
     */
    public CobolStructureToXsd() {
        this(new Cob2XsdContext());
    }

    /**
     * @param context execution parameters for the COBOL to XML Schema utility
     */
    public CobolStructureToXsd(final Cob2XsdContext context) {
        _context = context;
    }

    /**
     * Execute the translation from COBOL to XML Schema.
     * @param cobolSource the COBOL source code
     * @return the XML Schema
     * @throws RecognizerException if COBOL recognition fails 
     * @throws XsdGenerationException if XML schema generation process fails
     */
    public String translate(
            final String cobolSource) throws RecognizerException, XsdGenerationException {
        if (_log.isDebugEnabled()) {
            debug("Translating with options:", getContext().toString());
        }
        return xsdToString(emitXsd(emitModel(parse(lex(clean(cobolSource))))));
    }

    /**
     * Execute the translation from COBOL to XML Schema.
     * @param cobolSourceFile the COBOL source code
     * @param targetDir folder where XML schema result is to be written
     * @return the XML Schema
     * @throws RecognizerException if COBOL recognition fails 
     * @throws XsdGenerationException if XML schema generation process fails
     */
    public File translate(
            final File cobolSourceFile,
            final File targetDir) throws RecognizerException, XsdGenerationException {
        return translate(cobolSourceFile, null, targetDir);
    }

    /**
     * Execute the translation from COBOL to XML Schema.
     * @param cobolSourceFile the COBOL source code (platform encoding by default)
     * @param cobolSourceFileEncoding the character set used to encode the COBOL source file
     * @param targetDir folder where XML schema result is to be written
     * @return the XML Schema
     * @throws RecognizerException if COBOL recognition fails 
     * @throws XsdGenerationException if XML schema generation process fails
     */
    public File translate(
            final File cobolSourceFile,
            final String cobolSourceFileEncoding,
            final File targetDir) throws RecognizerException, XsdGenerationException {
        try {
            if (_log.isDebugEnabled()) {
                _log.debug("Translating COBOL file: " + cobolSourceFile);
            }
            String xsdString = translate(
                    FileUtils.readFileToString(cobolSourceFile, cobolSourceFileEncoding));
            String xsdFileName = cobolSourceFile.getName() + ".xsd";
            File xsdFile = new File(targetDir, xsdFileName);
            FileUtils.writeStringToFile(xsdFile, xsdString, getContext().getXsdEncoding());
            if (_log.isDebugEnabled()) {
                _log.debug("Created XML schema file: " + xsdFile);
            }
            return xsdFile;
        } catch (IOException e) {
            throw (new XsdGenerationException(e));
        }
    }

    /**
     * Remove any non COBOL Structure characters from the source.
     * @param cobolSource the raw source
     * @return a cleaned up source
     * @throws CleanerException if source cannot be read

     */
    public String clean(final String cobolSource) throws CleanerException {
        if (_log.isDebugEnabled()) {
            debug("1. Cleaning COBOL source code:", cobolSource);
        }
        CobolSourceCleaner cleaner = new CobolSourceCleaner();
        return cleaner.execute(cobolSource);
    }

    /**
     * Apply the lexer to produce a token stream from source.
     * @param cleanedCobolSource the source code (clean outside columns 7 to 72)
     * @return an antlr token stream
     * @throws RecognizerException if lexer failed to tokenize COBOL source
     */
    public CommonTokenStream lex(
            final String cleanedCobolSource) throws RecognizerException {
        if (_log.isDebugEnabled()) {
            debug("2. Lexing COBOL source code:", cleanedCobolSource);
        }
        try {
            CobolStructureLexer lex = new CobolStructureLexerImpl(
                    new ANTLRReaderStream(
                            new StringReader(cleanedCobolSource)), getErrorHandler());
            CommonTokenStream tokens = new CommonTokenStream(lex);
            if (lex.getNumberOfSyntaxErrors() != 0 || tokens == null) {
                throw (new RecognizerException(
                        "Lexing failed. " + lex.getNumberOfSyntaxErrors() + " syntax errors."
                        + " Last error was " + getErrorHistory().get(getErrorHistory().size() - 1)));
            }
            return tokens;
        } catch (IOException e) {
            throw (new RecognizerException(e));
        }
    }

    /**
     * Apply Parser to produce an abstract syntax tree from a token stream. 
     * @param tokens the stream token produced by lexer
     * @return an antlr abstract syntax tree
     * @throws RecognizerException if source contains unsupported statements
     */
    public CommonTree parse(final CommonTokenStream tokens) throws RecognizerException {
        if (_log.isDebugEnabled()) {
            debug("3. Parsing tokens:", tokens.toString());
        }
        try {
            CobolStructureParser parser = new CobolStructureParserImpl(
                    tokens, getErrorHandler());
            cobdata_return parserResult = parser.cobdata();
            if (parser.getNumberOfSyntaxErrors() != 0 || parserResult == null) {
                throw (new RecognizerException(
                        "Parsing failed. " + parser.getNumberOfSyntaxErrors() + " syntax errors."
                        + " Last error was " + getErrorHistory().get(getErrorHistory().size() - 1)));
            }
            return (CommonTree) parserResult.getTree();
        } catch (RecognitionException e) {
            throw (new RecognizerException(e));
        }
    }

    /**
     * Generates a model from an Abstract Syntax Tree. 
     * @param ast the abstract syntax tree produced by parser
     * @return a list of COBOL data items
     * @throws RecognizerException if tree cannot be walked
     */
    public List < CobolDataItem > emitModel(final CommonTree ast) throws RecognizerException {
        List < CobolDataItem > cobolDataItems = new ArrayList < CobolDataItem >();
        if (_log.isDebugEnabled()) {
            debug("4. Emitting Model from AST: ", ((ast == null) ? "null" : ast.toStringTree()));
        }
        if (ast == null) {
            return cobolDataItems;
        }
        try {
            TreeNodeStream nodes = new CommonTreeNodeStream(ast);
            CobolStructureEmitter emitter = new CobolStructureEmitterImpl(
                    nodes, getErrorHandler());
            emitter.cobdata(cobolDataItems);
            return cobolDataItems;
        } catch (RecognitionException e) {
            throw new RecognizerException(e);
        }
    }

    /**
     * Generate an XML Schema using a model of COBOL data items.
     * The model is a list of root level items. From these, we only
     * process group items (structures) with children. 
     * @param cobolDataItems a list of COBOL data items
     * @return the XML schema
     */
    public XmlSchema emitXsd(
            final List < CobolDataItem > cobolDataItems) {
        if (_log.isDebugEnabled()) {
            debug("5. Emitting XML Schema from model: ", cobolDataItems.toString());
        }
        XmlSchema xsd = createXmlSchema(getContext().getXsdEncoding());
        List < String > nonUniqueCobolNames = getNonUniqueCobolNames(cobolDataItems);
        XsdEmitter emitter = new XsdEmitter(xsd, getContext());
        for (CobolDataItem cobolDataItem : cobolDataItems) {
            if (cobolDataItem.getChildren().size() > 0) {
                XsdDataItem xsdDataItem = new XsdDataItem(
                        cobolDataItem, getContext(), null, nonUniqueCobolNames);
                emitter.createXmlSchemaType(xsdDataItem);
            }
        }
        return xsd;
    }

    /**
     * Serialize the XML Schema to a string.
     * <p/>
     * If we are provided with an XSLT customization file then we
     * transform the XMLSchema.
     * @param xsd the XML Schema before customization
     * @return a string serialization of the customized XML Schema
     * @throws XsdGenerationException if customization fails
     */
    public String xsdToString(final XmlSchema xsd) throws XsdGenerationException {

        if (_log.isDebugEnabled()) {
            StringWriter writer = new StringWriter();
            xsd.write(writer);
            debug("6. Writing XML Schema: ", writer.toString());
        }

        String errorMessage = "Customizing XML Schema failed.";
        try {
            TransformerFactory tFactory = TransformerFactory.newInstance();
            try {
                tFactory.setAttribute("indent-number", "4");
            } catch (IllegalArgumentException e) {
                _log.warn("Unable to set indent-number on transfomer factory", e);
            }
            StringWriter writer = new StringWriter();
            Source source = new DOMSource(xsd.getAllSchemas()[0]);
            Result result = new StreamResult(writer);
            Transformer transformer;
            if (getContext().getCustomXslt() == null) {
                transformer = tFactory.newTransformer();
            } else {
                Source xslSource = new StreamSource(getContext().getCustomXslt());
                transformer = tFactory.newTransformer(xslSource);
            }
            transformer.setOutputProperty(OutputKeys.ENCODING,
                    getContext().getXsdEncoding());
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");
            transformer.setOutputProperty(OutputKeys.STANDALONE, "no");

            transformer.transform(source, result);
            writer.flush();

            return writer.toString();
        } catch (TransformerConfigurationException e) {
            _log.error(errorMessage, e);
            throw new XsdGenerationException(e);
        } catch (TransformerFactoryConfigurationError e) {
            _log.error(errorMessage, e);
            throw new XsdGenerationException(e);
        } catch (TransformerException e) {
            _log.error(errorMessage, e);
            throw new XsdGenerationException(e);
        }
    }

    /**
     * Create a list of COBOL names which are not unique.
     * This is useful when we build complex type names from the COBOL names.
     * Complex type names need to be unique within a target namespace.
     * @param cobolDataItems a list of root data items
     * @return a list of COBOL names which are not unique
     */
    protected List < String > getNonUniqueCobolNames(final List < CobolDataItem > cobolDataItems) {
        List < String > cobolNames = new ArrayList < String >();
        List < String > nonUniqueCobolNames = new ArrayList < String >();
        for (CobolDataItem cobolDataItem : cobolDataItems) {
            getNonUniqueCobolNames(cobolDataItem, cobolNames, nonUniqueCobolNames);
        }
        return nonUniqueCobolNames;
    }

    /**
     * If data item COBOL name is already used, we add it to the non unique list.
     * This recurse to the item children.
     * <p/>
     * We don't add COBOL FILLERs as they are always considered non unique.
     * 
     * @param cobolDataItem a COBOL data item
     * @param cobolNames the list of all COBOL names used so far
     * @param nonUniqueCobolNames the list of non unique COBOL names
     */
    protected void getNonUniqueCobolNames(
            final CobolDataItem cobolDataItem,
            final List < String > cobolNames,
            final List < String > nonUniqueCobolNames) {

        String cobolName = cobolDataItem.getCobolName();
        if (!cobolName.equalsIgnoreCase("FILLER")) {
            if (cobolNames.contains(cobolName)) {
                if (!nonUniqueCobolNames.contains(cobolName)) {
                    nonUniqueCobolNames.add(cobolName);
                }
            } else {
                cobolNames.add(cobolName);
            }
        }
        for (CobolDataItem child : cobolDataItem.getChildren()) {
            getNonUniqueCobolNames(child, cobolNames, nonUniqueCobolNames);
        }
    }

    /**
     * Create an empty XML Schema.
     * @param encoding the character set used to encode this XML Schema
     * @return a new empty XML schema using the context 
     */
    protected XmlSchema createXmlSchema(final String encoding) {
        XmlSchema xsd = new XmlSchema(
                getContext().getTargetNamespace(), new XmlSchemaCollection());
        xsd.setElementFormDefault(new XmlSchemaForm(XmlSchemaForm.QUALIFIED));
        xsd.setAttributeFormDefault(null);
        xsd.setInputEncoding(encoding);
        return xsd;
    }

    /**
     * Produce long text in a delimited debug zone.
     * @param title the debug text title
     * @param text the text itself
     */
    private void debug(final String title, final String text) {
        _log.debug(title);
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < title.length(); i++) {
            sb.append("-");
        }
        _log.debug(sb.toString());
        try {
            BufferedReader reader = new BufferedReader(new StringReader(text));
            String line;
            while ((line = reader.readLine()) != null) {
                _log.debug(line);
            }
        } catch (IOException e) {
            _log.error(e);
        }
        _log.debug("----------------------------------------------------------------");
    }

    /**
     * @return the error messages handler
     */
    private RecognizerErrorHandler getErrorHandler() {
        return _errorHandler;
    }

    /**
     * The execution parameters for the COBOL to XML Schema utility.
     * @return the execution parameters for the COBOL to XML Schema utility
     */
    public Cob2XsdContext getContext() {
        return _context;
    }

    /**
     * list of errors encountered while translating.
     * <p/>
     * Most of these errors are warnings which were recovered from but still
     * denote something that user should know about.
     * @return the list of errors encountered while translating
     */
    public List < String > getErrorHistory() {
        return getErrorHandler().getErrorMessages();
    }
}
