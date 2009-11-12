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
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

import com.legstar.cob2xsd.Cob2XsdContext;
import com.legstar.cob2xsd.CobolStructureToXsd;

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

    /** A folder containing translated XML Schema. Defaults to schema relative folder. */
    private File _output = new File("schema");

    /** Set of translation options to use.    */
    private Cob2XsdContext _context = new Cob2XsdContext();;

    /** Line separator (OS specific).*/
    public static final String LS = System.getProperty("line.separator");

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
        Options options = createOptions();
        if (args != null && args.length > 0) {
            CommandLineParser parser = new PosixParser();
            try {
                CommandLine line = parser.parse(options, args);
                if (processLine(line, options)) {
                    execute(_input, _output);
                }
                return;
            } catch (ParseException e) {
                System.err.println("Parsing failed.  Reason: " + e.getMessage());
            }
        }
        produceHelp(options);
    }

    /**
     * @param options options available
     */
    private void produceHelp(final Options options) {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("java -jar legstar-cob2xsd-" + getVersion() + "-exe.jar", options);
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

        Option output = new Option("o", "output", true,
        "folder receiving the translated XML schema");
        options.addOption(output);

        Option targetNamespace = new Option("t", "targetNamespace", true,
        "Target namespace for generated XML schema");
        options.addOption(targetNamespace);

        Option mapConditionsToFacets = new Option("m", "mapConditionsToFacets", false,
                "Whether COBOL conditions (level 88) should be mapped to facets."
                + " Facets restrict the content which might not be desirable");
        options.addOption(mapConditionsToFacets);

        Option nameConflictPrependParentName = new Option("n", "nameConflictPrependParentName", false,
                "Whether parent complex type name should be prepended in case of name conflict"
                + " (otherwise, the COBOL source line will be appended)");
        options.addOption(nameConflictPrependParentName);

        Option addLegStarAnnotations = new Option("a", "addLegStarAnnotations", false,
        "Whether we should generate COBOL/JAXB annotations");
        options.addOption(addLegStarAnnotations);

        Option jaxbPackageName = new Option("p", "jaxbPackageName", true,
        "the JAXB package name for generated Java classes");
        options.addOption(jaxbPackageName);

        Option jaxbTypeClassesSuffix = new Option("s", "jaxbTypeClassesSuffix", true,
        "The JAXB type name prefix (generated JAXB class names will have this suffix)");
        options.addOption(jaxbTypeClassesSuffix);

        Option decimalPointIsComma = new Option("decimalPointIsComma",
        "Whether COBOL comma is the decimal point (DECIMAL-POINT IS COMMA clause in the SPECIAL-NAMES)");
        options.addOption(decimalPointIsComma);

        Option isNSymbolDbcs = new Option("isNSymbolDbcs",
        "The COBOL NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if false");
        options.addOption(isNSymbolDbcs);

        Option currencySymbol = new Option("currencySymbol", true,
        "The COBOL currency symbol used (CURRENCY SIGN clause in the SPECIAL-NAMES)");
        options.addOption(currencySymbol);

        Option customXslt = new Option("x", "customXslt", true,
        "Optional XSLT transform stylesheet for XML schema customization");
        options.addOption(customXslt);

        return options;
    }

    /**
     * Process the command line options selected.
     * @param line the parsed command line
     * @param options available
     * @return false if processing needs to stop, true if its ok to continue
     */
    protected boolean processLine(final CommandLine line, final Options options) {
        if (line.hasOption("version")) {
            System.out.println("version " + getVersion());
            return false;
        }
        if (line.hasOption("help")) {
            produceHelp(options);
            return false;
        }
        if (line.hasOption("targetNamespace")) {
            getContext().setTargetNamespace(line.getOptionValue("targetNamespace").trim());
        }
        if (line.hasOption("addLegStarAnnotations")) {
            getContext().setAddLegStarAnnotations(true);
        }
        if (line.hasOption("nameConflictPrependParentName")) {
            getContext().setNameConflictPrependParentName(true);
        }
        if (line.hasOption("mapConditionsToFacets")) {
            getContext().setMapConditionsToFacets(true);
        }
        if (line.hasOption("jaxbPackageName")) {
            getContext().setJaxbPackageName(line.getOptionValue("jaxbPackageName").trim());
        }
        if (line.hasOption("jaxbTypeClassesSuffix")) {
            getContext().setJaxbTypeClassesSuffix(line.getOptionValue("jaxbTypeClassesSuffix").trim());
        }
        if (line.hasOption("decimalPointIsComma")) {
            getContext().setDecimalPointIsComma(true);
        }
        if (line.hasOption("isNSymbolDbcs")) {
            getContext().setNSymbolDbcs(true);
        }
        if (line.hasOption("currencySymbol")) {
            getContext().setCurrencySymbol(line.getOptionValue("currencySymbol").trim().charAt(0));
        }

        if (line.hasOption("customXslt")) {
            String customXslt = line.getOptionValue("customXslt").trim();
            File customXsltFile = new File(customXslt);
            if (!customXsltFile.exists() || !customXsltFile.isFile()) {
                return false;
            }
        }

        if (line.hasOption("input")) {
            String input = line.getOptionValue("input").trim();
            if (!setInput(input)) {
                return false;
            }
        }
        if (line.hasOption("output")) {
            String output = line.getOptionValue("output").trim();
            if (!setOutput(output)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Translate a single file or all files from an input folder.
     * Place results in the output folder.
     * @param input the input COBOL file or folder
     * @param targetDir the output folder where XML schema file must go
     */
    protected void execute(final File input, final File targetDir) {
        try {
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(getContext());
            if (_input != null && _output != null) {
                if (input.isFile()) {
                    cob2xsd.translate(input, targetDir);
                } else {
                    for (File cobolFile : input.listFiles()) {
                        if (cobolFile.isFile()) {
                            cob2xsd.translate(cobolFile, targetDir);
                        }
                    }
                }
            } else {
                System.err.println("No input or output was specified.");
            }
        } catch (Exception e) {
            System.err.println("Exception caught: " + e.getMessage());
        }

    }

    /**
     * Pick up the version from the properties file.
     * @return the product version
     */
    public static String getVersion() {
        InputStreamReader stream = null;
        try {
            Properties version = new Properties();
            stream = new InputStreamReader(
                    CobolStructureToXsdMain.class.getResourceAsStream(
                            VERSION_FILE_NAME));
            version.load(stream);
            return  version.getProperty("version");
        } catch (IOException e) {
            System.err.println("Unable to locate COBOL reserved word file " + VERSION_FILE_NAME
                    + ". Will not check for COBOL reserved words");
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException e) {
                    System.err.println("Unable to close stream");
                }
            }
        }
        return null;
    }

    /**
     * Check the input parameter and keep it only if it is valid.
     * @param input a file or folder name (relative or absolute)
     * @return false if input is invalid
     */
    public boolean setInput(final String input) {
        File file = new File(input);
        if (!file.exists()) {
            System.err.println("Input file or folder " + input + " not found");
            return false;
        } else {
            if (file.isDirectory() && file.list().length == 0) {
                System.err.println("Folder " + input + " is empty");
                return false;
            } else {
                _input = file;
                return true;
            }
        }

    }

    /**
     * Check the output parameter and keep it only if it is valid.
     * @param output a file or folder name (relative or absolute)
     * @return false if output is invalid
     */
    public boolean setOutput(final String output) {
        File file = new File(output);
        if (!file.exists()) {
            if (!file.mkdir()) {
                System.err.println("Output folder " + output + " cannot be created");
                return false;
            } else {
                _output = file;
                return true;
            }
        } else {
            if (!file.isDirectory()) {
                System.err.println("File " + output + " is not a folder");
                return false;
            } else {
                _output = file;
                return true;
            }
        }
    }

    /**
     * Gather all parameters into a context object.
     * @return a parameter context to be used throughout all code
     */
    private Cob2XsdContext getContext() {
        return _context;
    }

}
