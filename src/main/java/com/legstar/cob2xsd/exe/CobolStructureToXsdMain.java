package com.legstar.cob2xsd.exe;

import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Properties;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.PosixParser;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.antlr.RecognizerException;
import com.legstar.cob2xsd.Cob2XsdContext;
import com.legstar.cob2xsd.CobolStructureToXsd;
import com.legstar.cob2xsd.XsdGenerationException;

/**
 * COBOL structure to XML schema standalone executable.
 * <p/>
 * This is the main class for the executable jar. It takes options from the
 * command line and calls the {@link CobolStructureToXsd} API.
 * <p/>
 * Usage:
 * <code>
 * java -jar legstar-cob2xsd-x.y.z-exe.jar -i&lt;input file or folder&gt; -o&lt;output folder&gt;
 * </code>
 *
 */
public class CobolStructureToXsdMain {

    /** The version properties file name. */
    private static final String VERSION_FILE_NAME = "/version.properties";

    /** A file or folder containing COBOL code to translate to XSD. Defaults to cobol relative folder.*/
    private File _input = new File("cobol");

    /** Character set used to encode the input COBOL source files.*/
    private String _cobolSourceFileEncoding;

    /** A folder containing translated XML Schema. Defaults to schema relative folder. */
    private File _output = new File("schema");

    /** Set of translation options to use.    */
    private Cob2XsdContext _context = new Cob2XsdContext();;

    /** Line separator (OS specific).*/
    public static final String LS = System.getProperty("line.separator");

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * @param args translator options. Provides help if no arguments passed.
     */
    public static void main(final String[] args) {
        CobolStructureToXsdMain main = new CobolStructureToXsdMain();
        main.execute(args);
    }

    /**
     * Process command line options and run translator.
     * <p/>
     * If no options are passed, prints the help. Help is also printed
     * if the command line options are invalid.
     * @param args translator options
     */
    public void execute(final String[] args) {
        try {
            Options options = createOptions();
            if (collectOptions(options, args)) {
                execute(getInput(), getCobolSourceFileEncoding(), getOutput());
            }
        } catch (Exception e) {
            _log.error("Failed COBOL structure translation", e);
        }
    }
    
    /**
     * Take arguments received on the command line and setup corresponding
     * options.
     * <p/>
     * No arguments is valid. It means use the defaults.
     * 
     * @param options the expected options
     * @param args the actual arguments received on the command line
     * @return true if arguments were valid
     * @throws Exception if something goes wrong while parsing arguments
     */
    protected boolean collectOptions(
            final Options options,
            final String[] args) throws Exception {
        if (args != null && args.length > 0) {
            CommandLineParser parser = new PosixParser();
            CommandLine line = parser.parse(options, args);
            return processLine(line, options);
        }
        return true;
    }

    /**
     * @param options options available
     * @throws Exception if help cannot be produced
     */
    protected void produceHelp(final Options options) throws Exception {
        HelpFormatter formatter = new HelpFormatter();
        String version = getVersion();
        formatter.printHelp("java -jar legstar-cob2xsd-"
                + version.substring(0, version.indexOf(' '))
                + "-exe.jar followed by:", options);
    }

    /**
     * @return the command line options
     */
    protected Options createOptions() {
        Options options = new Options();

        Option version = new Option("v", "version", false, "print the version information and exit");
        options.addOption(version);

        Option help = new Option("h", "help", false, "print the options available");
        options.addOption(help);

        Option input = new Option("i", "input", true,
        "file or folder holding the COBOL code to translate. Name is relative or absolute");
        options.addOption(input);

        Option cobolSourceFileEncoding = new Option("c", "cobolSourceFileEncoding", true,
        "Character set used for COBOL source files encoding");
        options.addOption(cobolSourceFileEncoding);

        Option output = new Option("o", "output", true,
        "folder receiving the translated XML schema");
        options.addOption(output);

        /* -------------------------------------------------------------------
         * XML Schema related options
         * */

        Option xsdEncoding = new Option("e", "xsdEncoding", true,
        "character set used to encode the generated XML Schema");
        options.addOption(xsdEncoding);

        Option targetNamespace = new Option("t", "targetNamespace", true,
        "target namespace for generated XML schema");
        options.addOption(targetNamespace);

        Option elementNamesStartWithUppercase = new Option("u", "elementNamesStartWithUppercase", false,
        "whether XSD element names should start with an uppercase (compatible with LegStar 1.2)");
        options.addOption(elementNamesStartWithUppercase);

        Option mapConditionsToFacets = new Option("m", "mapConditionsToFacets", false,
                "whether COBOL conditions (level 88) should be mapped to facets."
                + " Facets restrict the content which might not be desirable");
        options.addOption(mapConditionsToFacets);

        Option nameConflictPrependParentName = new Option("n", "nameConflictPrependParentName", false,
                "whether parent complex type name should be prepended in case of name conflict"
                + " (otherwise, the COBOL source line will be appended)");
        options.addOption(nameConflictPrependParentName);

        Option customXsltFileName = new Option("x", "customXsltFileName", true,
        "optional XSLT transform stylesheet for XML schema customization");
        options.addOption(customXsltFileName);

        /* -------------------------------------------------------------------
         * LegStar annotations related options
         * */
        Option addLegStarAnnotations = new Option("a", "addLegStarAnnotations", false,
        "whether we should generate LegStar COBOL/JAXB annotations");
        options.addOption(addLegStarAnnotations);

        Option jaxbPackageName = new Option("p", "jaxbPackageName", true,
        "the package name for JAXB generated Java classes");
        options.addOption(jaxbPackageName);

        Option jaxbTypeClassesSuffix = new Option("s", "jaxbTypeClassesSuffix", true,
        "the JAXB type name prefix (generated JAXB class names will have this suffix)");
        options.addOption(jaxbTypeClassesSuffix);

        /* -------------------------------------------------------------------
         * COBOL compiler related options
         * */
        Option decimalPointIsComma = new Option("d", "decimalPointIsComma", false,
        "whether COBOL comma is the decimal point (DECIMAL-POINT IS COMMA clause in the SPECIAL-NAMES)");
        options.addOption(decimalPointIsComma);

        Option currencySymbol = new Option("w", "currencySymbol", true,
        "the COBOL currency symbol used (CURRENCY SIGN clause in the SPECIAL-NAMES)");
        options.addOption(currencySymbol);

        Option nSymbolDbcs = new Option("z", "nSymbolDbcs", false,
        "the COBOL NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if unspecified");
        options.addOption(nSymbolDbcs);

        Option quoteIsApost = new Option("q", "quoteIsApost", false,
        "the COBOL APOST compiler option. Assume QUOTE if unspecified");
        options.addOption(quoteIsApost);

        return options;
    }

    /**
     * Process the command line options selected.
     * @param line the parsed command line
     * @param options available
     * @return false if processing needs to stop, true if its ok to continue
     * @throws Exception if line cannot be processed
     */
    protected boolean processLine(
            final CommandLine line,
            final Options options) throws Exception {
        if (line.hasOption("version")) {
            System.out.println("version " + getVersion());
            return false;
        }
        if (line.hasOption("help")) {
            produceHelp(options);
            return false;
        }
        if (line.hasOption("input")) {
            setInput(line.getOptionValue("input").trim());
        }
        if (line.hasOption("cobolSourceFileEncoding")) {
            setCobolSourceFileEncoding(line.getOptionValue("cobolSourceFileEncoding").trim());
        }
        if (line.hasOption("output")) {
            setOutput(line.getOptionValue("output").trim());
        }

        /* -------------------------------------------------------------------
         * XML Schema related options
         * */
        if (line.hasOption("xsdEncoding")) {
            getContext().setXsdEncoding(line.getOptionValue("xsdEncoding").trim());
        }
        if (line.hasOption("targetNamespace")) {
            getContext().setTargetNamespace(line.getOptionValue("targetNamespace").trim());
        }
        if (line.hasOption("addLegStarAnnotations")) {
            getContext().setAddLegStarAnnotations(true);
        }
        if (line.hasOption("elementNamesStartWithUppercase")) {
            getContext().setElementNamesStartWithUppercase(true);
        }
        if (line.hasOption("mapConditionsToFacets")) {
            getContext().setMapConditionsToFacets(true);
        }
        if (line.hasOption("customXsltFileName")) {
            getContext().setCustomXsltFileName(
                    line.getOptionValue("customXsltFileName").trim());
        }

        /* -------------------------------------------------------------------
         * LegStar annotations related options
         * */
        if (line.hasOption("nameConflictPrependParentName")) {
            getContext().setNameConflictPrependParentName(true);
        }
        if (line.hasOption("jaxbPackageName")) {
            getContext().setJaxbPackageName(line.getOptionValue("jaxbPackageName").trim());
        }
        if (line.hasOption("jaxbTypeClassesSuffix")) {
            getContext().setJaxbTypeClassesSuffix(line.getOptionValue("jaxbTypeClassesSuffix").trim());
        }

        /* -------------------------------------------------------------------
         * COBOL compiler related options
         * */
        if (line.hasOption("decimalPointIsComma")) {
            getContext().setDecimalPointIsComma(true);
        }
        if (line.hasOption("currencySymbol")) {
            getContext().setCurrencySymbol(line.getOptionValue("currencySymbol").trim());
        }
        if (line.hasOption("nSymbolDbcs")) {
            getContext().setNSymbolDbcs(true);
        }
        if (line.hasOption("quoteIsApost")) {
            getContext().setQuoteIsQuote(false);
        }
        
        return true;
    }

    /**
     * Translate a single file or all files from an input folder.
     * Place results in the output folder.
     * @param input the input COBOL file or folder
     * @param cobolSourceFileEncoding the input file character set
     * @param targetDir the output folder where XML schema file must go
     * @throws XsdGenerationException if XML schema cannot be generated
     * @throws RecognizerException if COBOL parsing fails
     */
    protected void execute(
            final File input,
            final String cobolSourceFileEncoding,
            final File targetDir) throws RecognizerException, XsdGenerationException {

        _log.info("Started translation from COBOL to XML Schema");
        _log.info("Taking COBOL from      : " + input);
        _log.info("COBOL files encoding   : " + cobolSourceFileEncoding);
        _log.info("Output XML Schema to   : " + targetDir);
        _log.info("Options in effect      : " + getContext().toString());
        CobolStructureToXsd cob2xsd = new CobolStructureToXsd(getContext());
        if (_input != null && _output != null) {
            if (input.isFile()) {
                _log.info("Translation started for: " + input);
                File xmlSchemaFile = cob2xsd.translate(input, cobolSourceFileEncoding, targetDir);
                _log.info("Result XML Schema is   : " + xmlSchemaFile);
            } else {
                for (File cobolFile : input.listFiles()) {
                    if (cobolFile.isFile()) {
                        _log.info("Translation started for: " + cobolFile);
                        File xmlSchemaFile = cob2xsd.translate(cobolFile, cobolSourceFileEncoding, targetDir);
                        _log.info("Result XML Schema is   : " + xmlSchemaFile);
                    }
                }
            }
        } else {
            throw new IllegalArgumentException("No input or output was specified.");
        }
        _log.info("Finished translation");

    }

    /**
     * Pick up the version from the properties file.
     * @return the product version
     * @throws IOException if version cannot be identified
     */
    protected String getVersion() throws IOException {
        InputStreamReader stream = null;
        try {
            Properties version = new Properties();
            stream = new InputStreamReader(
                    CobolStructureToXsdMain.class.getResourceAsStream(
                            VERSION_FILE_NAME));
            version.load(stream);
            return  version.getProperty("version");
        } finally {
            if (stream != null) {
                stream.close();
            }
        }
    }

    /**
     * Check the input parameter and keep it only if it is valid.
     * @param input a file or folder name (relative or absolute)
     */
    public void setInput(final String input) {
        File file = new File(input);
        if (file.exists()) {
            if (file.isDirectory() && file.list().length == 0) {
                throw new IllegalArgumentException("Folder " + input + " is empty");
            }
        } else {
            throw new IllegalArgumentException("Input file or folder " + input + " not found");
        }
        _input = file;
    }

    /**
     * @param cobolSourceFileEncoding the character set used to encode the input COBOL source files
     */
    public void setCobolSourceFileEncoding(final String cobolSourceFileEncoding) {
        _cobolSourceFileEncoding = cobolSourceFileEncoding;
    }

    /**
     * Check the output parameter and keep it only if it is valid.
     * @param output a file or folder name (relative or absolute)
     */
    public void setOutput(final String output) {
        File file = new File(output);
        if (!file.exists()) {
            if (!file.mkdir()) {
                throw new IllegalArgumentException("Output folder " + output + " cannot be created");
            }
        } else {
            if (!file.isDirectory()) {
                throw new IllegalArgumentException("File " + output + " is not a folder");
            }
        }
        _output = file;
    }

    /**
     * Gather all parameters into a context object.
     * @return a parameter context to be used throughout all code
     */
    public Cob2XsdContext getContext() {
        return _context;
    }

    /**
     * @return the file or folder containing COBOL code to translate to XSD
     */
    public File getInput() {
        return _input;
    }

    /**
     * @return the character set used to encode the input COBOL source files
     */
    public String getCobolSourceFileEncoding() {
        return _cobolSourceFileEncoding;
    }

    /**
     * @return the folder containing translated XML Schema
     */
    public File getOutput() {
        return _output;
    }

}
