package com.legstar.cob2xsd.exe;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
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
                if (processLine(line)) {
                    execute(_input, _output);
                }
                return;
            } catch (ParseException e) {
                System.err.println("Parsing failed.  Reason: " + e.getMessage());
            }
        }
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("cob2xsd", options);
    }

    /**
     * @return the command line options
     */
    protected Options createOptions() {
        Options options = new Options();

        Option version = new Option("v", "version", false, "print the version information and exit");
        options.addOption(version);

        Option input = new Option("i", "input", true,
        "file or folder holding the COBOL code to translate. Name is relative or absolute");
        options.addOption(input);

        Option output = new Option("o", "output", true,
        "folder receiving the translated XML schema");
        options.addOption(output);

        Option targetNamespace = new Option("targetNamespace", true,
        "Target namespace for generated XML schema");
        options.addOption(targetNamespace);

        Option mapConditionsToFacets = new Option("mapConditionsToFacets",
        "Whether COBOL conditions (level 88) should be mapped to facets."
                + " Facets restrict the content which might not be desirable");
        options.addOption(mapConditionsToFacets);

        Option addLegStarAnnotations = new Option("addLegStarAnnotations",
        "Whether we should generate COBOL/JAXB annotations");
        options.addOption(addLegStarAnnotations);

        Option jaxbPackageName = new Option("jaxbPackageName", true,
        "the JAXB package name for generated Java classes");
        options.addOption(jaxbPackageName);

        Option jaxbTypeClassesSuffix = new Option("jaxbTypeClassesSuffix", true,
        "The JAXB type name prefix (generated JAXB class names will have this suffix)");
        options.addOption(jaxbTypeClassesSuffix);

        Option decimalPointIsComma = new Option("decimalPointIsComma",
        "Whether comma is the decimal point (DECIMAL-POINT IS COMMA clause in the SPECIAL-NAMES)");
        options.addOption(decimalPointIsComma);

        Option isNSymbolDbcs = new Option("isNSymbolDbcs",
        "The NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if false");
        options.addOption(isNSymbolDbcs);

        Option currencySymbol = new Option("currencySymbol", true,
        "The currency symbol used (CURRENCY SIGN clause in the SPECIAL-NAMES)");
        options.addOption(currencySymbol);

        Option customXslt = new Option("customXslt", true,
        "optional XSLT transform for XML schema customization");
        options.addOption(customXslt);

        return options;
    }

    /**
     * Process the command line options selected.
     * @param line the parsed command line
     * @return false if processing needs to stop, true if its ok to continue
     */
    protected boolean processLine(final CommandLine line) {
        if (line.hasOption("version")) {
            System.out.println("version " + getVersion());
            return false;
        }
        if (line.hasOption("targetNamespace")) {
            setTargetNamespace(line.getOptionValue("targetNamespace").trim());
        }
        if (line.hasOption("addLegStarAnnotations")) {
            setAddLegStarAnnotations(true);
        }
        if (line.hasOption("mapConditionsToFacets")) {
            setMapConditionsToFacets(true);
        }
        if (line.hasOption("jaxbPackageName")) {
            setJaxbPackageName(line.getOptionValue("jaxbPackageName").trim());
        }
        if (line.hasOption("jaxbTypeClassesSuffix")) {
            setJaxbTypeClassesSuffix(line.getOptionValue("jaxbTypeClassesSuffix").trim());
        }
        if (line.hasOption("decimalPointIsComma")) {
            setDecimalPointIsComma(true);
        }
        if (line.hasOption("isNSymbolDbcs")) {
            setNSymbolDbcs(true);
        }
        if (line.hasOption("currencySymbol")) {
            setCurrencySymbol(line.getOptionValue("currencySymbol").trim().charAt(0));
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

   /**
    * @return the currency symbol used (CURRENCY SIGN clause in the SPECIAL-NAMES)
    */
   public char getCurrencySymbol() {
       return getContext().getCurrencySymbol();
   }

   /**
    * @param currencySymbol the currency symbol used (CURRENCY SIGN clause in the SPECIAL-NAMES)
    */
   public void setCurrencySymbol(final char currencySymbol) {
       getContext().setCurrencySymbol(currencySymbol);
   }

   /**
    * @return the NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if false
    */
   public boolean isNSymbolDbcs() {
       return getContext().isNSymbolDbcs();
   }

   /**
    * @param nSymbolDbcs the NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if false
    */
   public void setNSymbolDbcs(final boolean nSymbolDbcs) {
       getContext().setNSymbolDbcs(nSymbolDbcs);
   }

   /**
    * @return whether comma is the decimal point (DECIMAL-POINT IS COMMA clause in the SPECIAL-NAMES)
    */
   public boolean decimalPointIsComma() {
       return getContext().decimalPointIsComma();
   }

   /**
    * @param decimalPointIsComma whether comma is the decimal point
    *  (DECIMAL-POINT IS COMMA clause in the SPECIAL-NAMES)
    */
   public void setDecimalPointIsComma(final boolean decimalPointIsComma) {
       getContext().setDecimalPointIsComma(decimalPointIsComma);
   }

   /**
    * @return whether we should generate COBOL/JAXB annotations
    */
   public boolean addLegStarAnnotations() {
       return getContext().addLegStarAnnotations();
   }

   /**
    * @param addLegStarAnnotations whether we should generate COBOL/JAXB annotations
    */
   public void setAddLegStarAnnotations(final boolean addLegStarAnnotations) {
       getContext().setAddLegStarAnnotations(addLegStarAnnotations);
   }

   /**
    * @return the JAXB package name for generated Java classes
    */
   public String getJaxbPackageName() {
       return getContext().getJaxbPackageName();
   }

   /**
    * @return the JAXB type name prefix (generated JAXB class names will have this suffix)
    */
   public String getJaxbTypeClassesSuffix() {
       return getContext().getJaxbTypeClassesSuffix();
   }

   /**
    * @param jaxbPackageName the JAXB package name for generated Java classes
    */
   public void setJaxbPackageName(final String jaxbPackageName) {
       getContext().setJaxbPackageName(jaxbPackageName);
   }

   /**
    * @param jaxbTypeClassesSuffix the JAXB type name prefix (generated JAXB class names will have this suffix)
    */
   public void setJaxbTypeClassesSuffix(final String jaxbTypeClassesSuffix) {
       getContext().setJaxbTypeClassesSuffix(jaxbTypeClassesSuffix);
   }

   /**
    * @return the target namespace for generated XML schema
    */
   public String getTargetNamespace() {
       return getContext().getTargetNamespace();
   }

   /**
    * @param targetNamespace the target namespace for generated XML schema
    */
   public void setTargetNamespace(final String targetNamespace) {
       getContext().setTargetNamespace(targetNamespace);
   }

   /**
    * @return whether COBOL conditions (level 88) should be mapped to facets. Facets 
    * restrict the content which might not be desirable
    */
   public boolean mapConditionsToFacets() {
       return getContext().mapConditionsToFacets();
   }

   /**
    * @param mapConditionsToFacets Whether COBOL conditions (level 88) should be mapped to facets. Facets 
    * restrict the content which might not be desirable
    */
   public void setMapConditionsToFacets(final boolean mapConditionsToFacets) {
       getContext().setMapConditionsToFacets(mapConditionsToFacets);
   }

   /**
    * @return an optional XSLT transform for XML schema customization
    */
   public File getCustomXslt() {
       return getContext().getCustomXslt();
   }

   /**
    * @param customXslt an optional XSLT transform for XML schema customization
    */
   public void setCustomXslt(final File customXslt) {
       getContext().setCustomXslt(customXslt);
   }

}
