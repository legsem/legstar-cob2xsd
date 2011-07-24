/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cob2xsd.exe;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.PosixParser;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.antlr.RecognizerException;
import com.legstar.cob2xsd.Cob2XsdModel;
import com.legstar.cob2xsd.CobolStructureToXsd;
import com.legstar.cob2xsd.XsdGenerationException;

/**
 * COBOL structure to XML schema executable.
 * <p/>
 * This is the main class for the executable jar. It takes options from the
 * command line and calls the {@link CobolStructureToXsd} API.
 * <p/>
 * Usage: <code>
 * java -jar legstar-cob2xsd-x.y.z-exe.jar -i&lt;input file or folder&gt; -o&lt;output folder&gt;
 * </code>
 * 
 */
public class CobolStructureToXsdMain {

    /** The version properties file name. */
    private static final String VERSION_FILE_NAME = "/com/legstar/cob2xsd/version.properties";

    /** The default input. */
    private static final String DEFAULT_INPUT_FOLDER = "cobol";

    /** The default output. */
    private static final String DEFAULT_OUTPUT_FOLDER = "schema";

    /** The default output. */
    private static final String DEFAULT_CONFIG_FILE = "conf/cob2xsd.properties";

    /** A file containing parameters. */
    private File _configFile;

    /**
     * A file or folder containing COBOL code to translate to XSD. Defaults to
     * cobol relative folder.
     */
    private File _input;

    /**
     * A folder containing translated XML Schema. Defaults to schema relative
     * folder.
     */
    private File _output;

    /** Set of translation options to use. */
    private Cob2XsdModel _model;

    /** Line separator (OS specific). */
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
     * If no options are passed, prints the help. Help is also printed if the
     * command line options are invalid.
     * 
     * @param args translator options
     */
    public void execute(final String[] args) {
        try {
            Options options = createOptions();
            if (collectOptions(options, args)) {
                setDefaults();
                loadModel();
                execute(getInput(), getOutput());
            }
        } catch (Exception e) {
            _log.error("Transformers generator failure", e);
            throw new RuntimeException(e);
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
    protected boolean collectOptions(final Options options, final String[] args)
            throws Exception {
        if (args != null && args.length > 0) {
            CommandLineParser parser = new PosixParser();
            CommandLine line = parser.parse(options, args);
            return processLine(line, options);
        }
        return true;
    }

    /**
     * Make sure mandatory parameters have default values.
     */
    protected void setDefaults() {
        if (getConfigFile() == null) {
            setConfigFile(DEFAULT_CONFIG_FILE);
        }
        if (getInput() == null) {
            setInput(DEFAULT_INPUT_FOLDER);
        }
        if (getOutput() == null) {
            setOutput(DEFAULT_OUTPUT_FOLDER);
        }
    }

    /**
     * A configuration file is expected. We load it into the generator model.
     * 
     * @throws XsdGenerationException if configuration file missing or file
     *             corrupt
     */
    protected void loadModel() throws XsdGenerationException {
        try {
            if (getConfigFile() == null) {
                _model = new Cob2XsdModel();
            } else {
                Properties config = new Properties();
                config.load(new FileInputStream(getConfigFile()));
                _model = new Cob2XsdModel(config);
            }
        } catch (FileNotFoundException e) {
            throw new XsdGenerationException(e);
        } catch (IOException e) {
            throw new XsdGenerationException(e);
        }
    }

    /**
     * @param options options available
     * @throws Exception if help cannot be produced
     */
    protected void produceHelp(final Options options) throws Exception {
        HelpFormatter formatter = new HelpFormatter();
        String version = getVersion();
        formatter.printHelp(
                "java -jar legstar-cob2xsd-"
                        + version.substring(0, version.indexOf(' '))
                        + "-exe.jar followed by:", options);
    }

    /**
     * @return the command line options
     */
    protected Options createOptions() {
        Options options = new Options();

        Option version = new Option("v", "version", false,
                "print the version information and exit");
        options.addOption(version);

        Option help = new Option("h", "help", false,
                "print the options available");
        options.addOption(help);

        Option configFile = new Option("c", "config", true,
                "path to configuration file");
        options.addOption(configFile);

        Option input = new Option("i", "input", true,
                "file or folder holding the COBOL code to translate."
                        + " Name is relative or absolute");
        options.addOption(input);

        Option output = new Option("o", "output", true,
                "folder or file receiving the translated XML schema");
        options.addOption(output);

        return options;
    }

    /**
     * Process the command line options selected.
     * 
     * @param line the parsed command line
     * @param options available
     * @return false if processing needs to stop, true if its ok to continue
     * @throws Exception if line cannot be processed
     */
    protected boolean processLine(final CommandLine line, final Options options)
            throws Exception {
        if (line.hasOption("version")) {
            System.out.println("version " + getVersion());
            return false;
        }
        if (line.hasOption("help")) {
            produceHelp(options);
            return false;
        }
        if (line.hasOption("config")) {
            setConfigFile(line.getOptionValue("config").trim());
        }
        if (line.hasOption("input")) {
            setInput(line.getOptionValue("input").trim());
        }
        if (line.hasOption("output")) {
            setOutput(line.getOptionValue("output").trim());
        }

        return true;
    }

    /**
     * Translate a single file or all files from an input folder. Place results
     * in the output folder.
     * 
     * @param input the input COBOL file or folder
     * @param target the output folder or file where XML schema file must go
     * @throws XsdGenerationException if XML schema cannot be generated
     */
    protected void execute(final File input, final File target)
            throws XsdGenerationException {

        try {
            _log.info("Started translation from COBOL to XML Schema");
            _log.info("Taking COBOL from      : " + input);
            _log.info("Output XML Schema to   : " + target);
            _log.info("Options in effect      : " + getModel().toString());

            if (input.isFile()) {
                if (FilenameUtils.getExtension(target.getPath()).length() == 0) {
                    FileUtils.forceMkdir(target);
                }
                translate(input, target);
            } else {
                FileUtils.forceMkdir(target);
                for (File cobolFile : input.listFiles()) {
                    if (cobolFile.isFile()) {
                        translate(cobolFile, target);
                    }
                }
            }
            _log.info("Finished translation");
        } catch (IOException e) {
            throw new XsdGenerationException(e);
        }

    }

    /**
     * Translates a single COBOL source file.
     * 
     * @param cobolFile COBOL source file
     * @param target target file or folder
     * @throws XsdGenerationException if parser fails
     */
    protected void translate(final File cobolFile, final File target)
            throws XsdGenerationException {
        try {
            _log.info("Translation started for: " + cobolFile);

            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(getModel());
            File xmlSchemaFile = cob2xsd.translate(cobolFile, target);
            for (String errorMessage : cob2xsd.getErrorHistory()) {
                _log.warn(errorMessage);
            }

            _log.info("Result XML Schema is   : " + xmlSchemaFile);
        } catch (RecognizerException e) {
            throw new XsdGenerationException(e);
        }
    }

    /**
     * Pick up the version from the properties file.
     * 
     * @return the product version
     * @throws IOException if version cannot be identified
     */
    protected String getVersion() throws IOException {
        InputStream stream = null;
        try {
            Properties version = new Properties();
            stream = CobolStructureToXsdMain.class
                    .getResourceAsStream(VERSION_FILE_NAME);
            version.load(stream);
            return version.getProperty("version");
        } finally {
            if (stream != null) {
                stream.close();
            }
        }
    }

    /**
     * @return the file containing parameters
     */
    public File getConfigFile() {
        return _configFile;
    }

    /**
     * Check the config parameter and keep it only if it is valid.
     * <p/>
     * If the default properties file is not there, we run with the defaults.
     * 
     * @param config a file name (relative or absolute)
     */
    public void setConfigFile(final String config) {
        if (config == null) {
            throw (new IllegalArgumentException(
                    "You must provide a configuration file"));
        }
        File file = new File(config);
        if (file.exists()) {
            if (file.isDirectory()) {
                throw new IllegalArgumentException("Folder " + config
                        + " is not a configuration file");
            }
        } else {
            if (config.equals(DEFAULT_CONFIG_FILE)) {
                file = null;
            } else {
                throw new IllegalArgumentException("Configuration file "
                        + config + " not found");
            }
        }
        setConfigFile(file);
    }

    /**
     * @param configFile the file containing parameters to set
     */
    public void setConfigFile(final File configFile) {
        _configFile = configFile;
    }

    /**
     * Check the input parameter and keep it only if it is valid.
     * 
     * @param input a file or folder name (relative or absolute)
     */
    public void setInput(final String input) {
        if (input == null) {
            throw (new IllegalArgumentException(
                    "You must provide a COBOL source folder or file"));
        }
        File file = new File(input);
        if (file.exists()) {
            if (file.isDirectory() && file.list().length == 0) {
                throw new IllegalArgumentException("Folder " + input
                        + " is empty");
            }
        } else {
            throw new IllegalArgumentException("Input file or folder " + input
                    + " not found");
        }
        _input = file;
    }

    /**
     * Check the output parameter and keep it only if it is valid.
     * 
     * @param output a file or folder name (relative or absolute)
     */
    public void setOutput(final String output) {
        if (output == null) {
            throw (new IllegalArgumentException(
                    "You must provide a target directory or file"));
        }
        _output = new File(output);
    }

    /**
     * Gather all parameters into a model object.
     * 
     * @return a parameter model to be used throughout all code
     */
    public Cob2XsdModel getModel() {
        return _model;
    }

    /**
     * @return the file or folder containing COBOL code to translate to XSD
     */
    public File getInput() {
        return _input;
    }

    /**
     * @return the folder containing translated XML Schema
     */
    public File getOutput() {
        return _output;
    }

}
