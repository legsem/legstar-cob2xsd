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
package com.legstar.cob2xsd.task;

import java.io.File;
import java.io.PrintStream;
import java.util.Vector;

import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildEvent;
import org.apache.tools.ant.BuildListener;
import org.apache.tools.ant.BuildLogger;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.ProjectHelper;
import org.apache.tools.ant.types.FileSet;
import org.w3c.dom.Document;

import com.legstar.cob2xsd.AbstractXsdTester;
import com.legstar.cob2xsd.Cob2XsdModel;
import com.legstar.cob2xsd.Cob2XsdModel.CodeFormat;
import com.legstar.codegen.CodeGenUtil;

/**
 * Test the generation model.
 * 
 */
public class CobolStructureToXsdTaskTest extends AbstractXsdTester {

    /** The parameters set. */
    private Cob2XsdModel _model;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** {@inheritDoc} */
    public void setUp() throws Exception {
        super.setUp();
        try {
            CodeGenUtil.initVelocity();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        CodeGenUtil.checkDirectory(GEN_ANT_DIR, true);
        _model = new Cob2XsdModel();
    }

    /**
     * Generate an ant script and run it to produce an XML Schema.
     * 
     * @throws Exception if ant cannot be generated
     */
    public void testBuildCob2Xsd() throws Exception {

        _model.setProductLocation("../../../..");
        _model.setProbeFile(new File("probe.file.tmp"));
        _model.setCobolSourceFilePath(COBOL_SAMPLES_DIR + "/LSFILEAE");
        _model.setCobolSourceFileEncoding("UTF-8");
        _model.setTargetDir(GEN_XSD_DIR);
        _model.setTargetXsdFileName("lsfileae.xsd");

        _model.setTargetNamespace("http://legstar.com/test/coxb/lsfileae");
        _model.setAddLegStarAnnotations(true);
        /* Backward compatibility */
        _model.setElementNamesStartWithUppercase(true);
        _model.setQuoteIsQuote(false);

        runAnt(genAntScriptAsFile());

        File xsdFile = new File(GEN_XSD_DIR, _model.getTargetXsdFileName());
        Document result = getXMLSchemaAsDoc(xsdFile);
        Document expected = getXMLSchemaAsDoc(new File(XSD_SAMPLES_DIR, _model
                .getTargetXsdFileName().toLowerCase()));
        compare(xsdFile.getName(), expected, result);
    }

    /**
     * Generate an ant script with append base file name option and run it to
     * produce an XML Schema.
     * 
     * @throws Exception if ant cannot be generated
     */
    public void testBuildCob2XsdWithAppendBaseFileNameOption() throws Exception {

        final Project project = new Project();
        project.addBuildListener(new TestLogger());
        project.setCoreLoader(this.getClass().getClassLoader());
        project.init();
        CobolStructureToXsdTask task = new CobolStructureToXsdTask();
        task.setProject(project);
        FileSet fileset = task.createFileset();
        fileset.setProject(project);
        fileset.setFile(new File(COBOL_SAMPLES_DIR + "/LSFILEAE"));
        File targetXsdFile = new File(GEN_XSD_DIR, "lsfileae.xsd");
        task.setTarget(targetXsdFile);
        task.setTargetNamespace("http://legstar.com/test/coxb");
        task.setAppendBaseFileNameToNamespace(true);
        task.setAddLegStarAnnotations(true);
        /* Backward compatibility */
        task.setElementNamesStartWithUppercase(true);
        task.setQuoteIsQuote(false);

        task.execute();

        Document result = getXMLSchemaAsDoc(targetXsdFile);
        Document expected = getXMLSchemaAsDoc(new File(XSD_SAMPLES_DIR,
                targetXsdFile.getName()));
        compare(targetXsdFile.getName(), expected, result);
    }

    /**
     * Test the COBOL formatting.
     * 
     * @throws Exception if test fails
     */
    public void testCobolFormat() throws Exception {
        String result = genAntScriptAsString();
        assertTrue(result.contains("codeFormat=\"FIXED_FORMAT\""));
        assertTrue(result.contains("startColumn=\"7\""));
        assertTrue(result.contains("endColumn=\"72\""));

        _model.setStartColumn(1);
        _model.setEndColumn(66);
        _model.setCobolSourceFileEncoding("ISO-8859-1");
        result = genAntScriptAsString();
        assertTrue(result.contains("startColumn=\"1\""));
        assertTrue(result.contains("endColumn=\"66\""));
        assertTrue(result.contains("cobolSourceFileEncoding=\"ISO-8859-1\""));

        _model.setCodeFormat(CodeFormat.FREE_FORMAT);
        result = genAntScriptAsString();
        assertTrue(result.contains("codeFormat=\"FREE_FORMAT\""));

    }

    /**
     * Test the XSD options.
     * 
     * @throws Exception if test fails
     */
    public void testXsdOptions() throws Exception {
        String result = genAntScriptAsString();
        assertTrue(result.contains("xsdEncoding=\"UTF-8\""));
        assertFalse(result.contains("targetNamespace=\""));
        assertTrue(result.contains("mapConditionsToFacets=\"false\""));
        assertTrue(result.contains("nameConflictPrependParentName=\"false\""));
        assertTrue(result.contains("elementNamesStartWithUppercase=\"false\""));

        _model.setXsdEncoding("ISO-8859-1");
        _model.setTargetNamespace("test/targetNamespace");
        _model.setMapConditionsToFacets(true);
        _model.setNameConflictPrependParentName(true);
        _model.setElementNamesStartWithUppercase(true);
        result = genAntScriptAsString();
        assertTrue(result.contains("xsdEncoding=\"ISO-8859-1\""));
        assertTrue(result.contains("targetNamespace=\"test/targetNamespace\""));
        assertTrue(result.contains("mapConditionsToFacets=\"true\""));
        assertTrue(result.contains("nameConflictPrependParentName=\"true\""));
        assertTrue(result.contains("elementNamesStartWithUppercase=\"true\""));

        _model.setCustomXsltFileName("src/test/resources/xslt/alltypes.xsl");
        result = genAntScriptAsString();
        assertTrue(result
                .contains("customXsltFileName=\"src/test/resources/xslt/alltypes.xsl\""));

    }

    /**
     * Test the LegStar annotations options.
     * 
     * @throws Exception if test fails
     */
    public void testLegStarAnnotationsOptions() throws Exception {
        String result = genAntScriptAsString();
        assertTrue(result.contains("addLegStarAnnotations=\"false\""));

        _model.setAddLegStarAnnotations(true);
        result = genAntScriptAsString();
        assertTrue(result.contains("addLegStarAnnotations=\"true\""));

    }

    /**
     * Test the COBOL compiler options.
     * 
     * @throws Exception if test fails
     */
    public void testCobolCompilerOptions() throws Exception {
        String result = genAntScriptAsString();
        assertTrue(result.contains("currencySign=\"$\""));
        assertTrue(result.contains("currencySymbol=\"$\""));
        assertTrue(result.contains("nSymbolDbcs=\"false\""));
        assertTrue(result.contains("decimalPointIsComma=\"false\""));
        assertTrue(result.contains("quoteIsQuote=\"true\""));

        _model.setCurrencySign("€");
        _model.setCurrencySymbol("€");
        _model.setNSymbolDbcs(true);
        _model.setDecimalPointIsComma(true);
        _model.setQuoteIsQuote(false);
        result = genAntScriptAsString();
        assertTrue(result.contains("currencySign=\"€\""));
        assertTrue(result.contains("currencySymbol=\"€\""));
        assertTrue(result.contains("nSymbolDbcs=\"true\""));
        assertTrue(result.contains("decimalPointIsComma=\"true\""));
        assertTrue(result.contains("quoteIsQuote=\"false\""));

    }

    /**
     * Generates an ant script from a VLC template.
     * 
     * @return the script as a string
     * @throws Exception if generation fails
     */
    protected String genAntScriptAsString() throws Exception {
        File resultFile = CodeGenUtil.getFile(GEN_ANT_DIR, "build.xml");
        _model.generateBuild(resultFile);

        String result = FileUtils.readFileToString(resultFile, "UTF-8");

        if (_log.isDebugEnabled()) {
            _log.debug(result);
        }
        return result;
    }

    /**
     * Generates an ant script from a VLC template.
     * 
     * @return the script as a string
     * @throws Exception if generation fails
     */
    protected File genAntScriptAsFile() throws Exception {
        File resultFile = CodeGenUtil.getFile(GEN_ANT_DIR, "build.xml");
        _model.generateBuild(resultFile);

        String result = FileUtils.readFileToString(resultFile, "UTF-8");

        if (_log.isDebugEnabled()) {
            _log.debug(result);
        }
        return resultFile;
    }

    /**
     * Execute an ant script.
     * 
     * @param buildFile the ant script
     * @throws Exception if ant script execution fails
     */
    protected void runAnt(final File buildFile) throws Exception {
        final Project project = new Project();
        project.addBuildListener(new TestLogger());
        project.setCoreLoader(this.getClass().getClassLoader());
        project.init();
        ProjectHelper helper = ProjectHelper.getProjectHelper();
        project.addReference("ant.projectHelper", helper);
        helper.parse(project, buildFile);
        Vector < String > targets = new Vector < String >();
        targets.addElement(project.getDefaultTarget());
        project.executeTargets(targets);
    }

    /**
     * Used to route ant messages to our log.
     * 
     */
    private class TestLogger implements BuildListener, BuildLogger {

        /** {@inheritDoc} */
        public void buildFinished(final BuildEvent event) {
            if (event.getException() == null) {
                _log.debug("Build finished.");
            } else {
                _log.debug("Build finished.", event.getException());
            }
        }

        /** {@inheritDoc} */
        public void buildStarted(final BuildEvent event) {
            _log.debug("Build started.");

        }

        /** {@inheritDoc} */
        public void messageLogged(final BuildEvent event) {
            _log.debug(event.getMessage());
        }

        /** {@inheritDoc} */
        public void targetFinished(final BuildEvent event) {
            _log.debug("targetFinished.");
            if (event.getException() == null) {
                _log.debug("targetFinished " + event.getTarget().getName());
            } else {
                _log.debug("targetFinished " + event.getTarget().getName(),
                        event.getException());
            }
        }

        /** {@inheritDoc} */
        public void targetStarted(final BuildEvent event) {
            _log.debug("targetStarted " + event.getTarget().getName());
        }

        /** {@inheritDoc} */
        public void taskFinished(final BuildEvent event) {
            _log.debug("taskFinished.");
        }

        /** {@inheritDoc} */
        public void taskStarted(final BuildEvent event) {
            _log.debug("taskStarted.");
        }

        /** {@inheritDoc} */
        public void setEmacsMode(final boolean emacsMode) {
            _log.debug("setEmacsMode.");
        }

        /** {@inheritDoc} */
        public void setErrorPrintStream(final PrintStream err) {
            _log.debug("setErrorPrintStream.");
        }

        /** {@inheritDoc} */
        public void setMessageOutputLevel(final int level) {
            _log.debug("setMessageOutputLevel.");
        }

        /** {@inheritDoc} */
        public void setOutputPrintStream(final PrintStream output) {
            _log.debug("setMessageOutputLevel.");
        }

    }

}
