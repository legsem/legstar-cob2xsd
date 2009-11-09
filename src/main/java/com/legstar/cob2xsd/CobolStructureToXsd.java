package com.legstar.cob2xsd;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
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
 * There are 6 steps involved:
 * <ul>
 * <li>Cleaning the source from non COBOL Structure characters</li>
 * <li>Lexing the source to extract meaningful keywords</li>
 * <li>Parsing keywords to extract meaningful COBOL statements</li>
 * <li>Emitting a COBOL model (a set of java classes) from the Abstract Syntax Tree</li>
 * <li>Emitting XSD from the COBOL model</li>
 * <li>Optionally applying a customization XSLT to the XSD</li>
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
     * @throws CobolStructureToXsdException if XML schema generation process fails
     */
    public String translate(
            final String cobolSource) throws CobolStructureToXsdException {
        return customize(emitXsd(emitModel(parse(lexify(clean(cobolSource))))));
    }

    /**
     * Execute the translation from COBOL to COBOL.
     * @param cobolSourceFile the COBOL source code
     * @param targetDir folder where XSD result is to be written
     * @return the COBOL source code
     * @throws CobolStructureToXsdException if XML schema generation process fails
     */
    public File translate(
            final File cobolSourceFile,
            final File targetDir) throws CobolStructureToXsdException {
        try {
            _log.info("Translating COBOL file: " + cobolSourceFile);
            String xsdString = translate(
                    FileUtils.readFileToString(cobolSourceFile));
            String xsdFileName = cobolSourceFile.getName() + ".xsd";
            File xsdFile = new File(targetDir, xsdFileName);
            FileUtils.writeStringToFile(xsdFile, xsdString);
            _log.info("Created XSD file: " + xsdFile);
            return xsdFile;
        } catch (IOException e) {
            throw (new CobolStructureToXsdException(e));
        }
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
     * @throws CobolStructureToXsdException if COBOL structure is unreadable
     */
    public CommonTokenStream lexify(final String source) throws CobolStructureToXsdException {
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
                throw (new CobolStructureToXsdException(errorMessage));
            }
            return tokens;
        } catch (IOException e) {
            _log.error(errorMessage, e);
           throw (new CobolStructureToXsdException(e));
        }
    }

    /**
     * Apply Parser to produce an abstract syntax tree from a token stream. 
     * @param tokens the stream token produced by lexer
     * @return an antlr abstract syntax tree
     * @throws CobolStructureToXsdException if source contains unsupported statements
     */
    public CommonTree parse(final CommonTokenStream tokens) throws CobolStructureToXsdException {
        if (_log.isDebugEnabled()) {
            debug("Parsing tokens:", tokens.toString());
        }
        String errorMessage = "Parsing token stream failed.";
        try {
            CobolStructureParser parser = new CobolStructureParserImpl(tokens);
            cobdata_return parserResult = parser.cobdata();
            if (parser.getNumberOfSyntaxErrors() != 0 || parserResult == null) {
                _log.error(errorMessage);
                throw (new CobolStructureToXsdException(errorMessage));
            }
            return (CommonTree) parserResult.getTree();
        } catch (RecognitionException e) {
            _log.error(errorMessage, e);
            throw (new CobolStructureToXsdException(e));
        }
    }

    /**
     * Generates a model from an Abstract Syntax Tree. 
     * @param ast the abstract syntax tree produced by parser
     * @return a list of COBOL data items
     * @throws CobolStructureToXsdException if tree cannot be walked
     */
    public List < CobolDataItem > emitModel(final CommonTree ast) throws CobolStructureToXsdException {
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
            throw (new CobolStructureToXsdException(e));
        }
    }

    /**
     * Generate an XML Schema using a model of COBOL data items.
     * The model is a list of root level items. From these, we only
     * process group items (structures) with children. 
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
            if (cobolDataItem.getChildren().size() > 0) {
                XsdDataItem xsdDataItem = new XsdDataItem(
                        cobolDataItem, getContext(), null, uniqueXsdTypeNames);
                emitter.createXmlSchemaType(xsdDataItem);
            }
        }
        return xsd;
    }
    
    /**
     * If we are provided with an XSLT customization file then we
     * transform the XMLSchema.
     * @param xsd the XML Schema before customization
     * @return a string serialization of the customized XML Schema
     * @throws CobolStructureToXsdException if customization fails
     */
    public String customize(final XmlSchema xsd) throws CobolStructureToXsdException {
        if (_log.isDebugEnabled()) {
            StringWriter logWriter = new StringWriter();
            xsd.write(logWriter);
            debug("Customizing XML Schema:\n", logWriter.toString());
        }
        String errorMessage = "Customizing XML Schema failed.";
        try {
            StringWriter writer = new StringWriter();
            if (getContext().getCustomXslt() == null) {
                xsd.write(writer);
            } else {
                TransformerFactory tFactory = TransformerFactory.newInstance();
                Source xslSource = new StreamSource(getContext().getCustomXslt());
                Transformer transformer = tFactory.newTransformer(xslSource);
                StreamResult result = new StreamResult(writer);
                
                /* Attempt to pass the XmlSchema as a DOM directly to
                 * transformer fail. This is more expensive but works.*/
                File temp = File.createTempFile("genSchema", ".xsd");
                temp.deleteOnExit();
                FileWriter tempWriter = new FileWriter(temp);
                xsd.write(tempWriter);
                
                transformer.transform(
                        new StreamSource(temp),
                        result);
            }
            return writer.toString();
        } catch (TransformerConfigurationException e) {
            _log.error(errorMessage, e);
            throw new CobolStructureToXsdException(e);
        } catch (TransformerFactoryConfigurationError e) {
            _log.error(errorMessage, e);
            throw new CobolStructureToXsdException(e);
        } catch (TransformerException e) {
            _log.error(errorMessage, e);
            throw new CobolStructureToXsdException(e);
        } catch (IOException e) {
            _log.error(errorMessage, e);
            throw new CobolStructureToXsdException(e);
        }
    }

    /**
     * @return a new empty XML schema using the context 
     */
    protected XmlSchema getNewXmlSchema() {
        XmlSchema xsd = new XmlSchema(
                getContext().getTargetNamespace(), new XmlSchemaCollection());
        xsd.setElementFormDefault(new XmlSchemaForm(XmlSchemaForm.QUALIFIED));
        xsd.setAttributeFormDefault(null);
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
