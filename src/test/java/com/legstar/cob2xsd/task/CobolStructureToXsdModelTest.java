/*******************************************************************************
 * Copyright (c) 2009 LegSem.
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
import org.w3c.dom.Document;

import com.legstar.cob2xsd.AbstractXsdTester;
import com.legstar.cob2xsd.Cob2XsdContext.CodeFormat;
import com.legstar.codegen.CodeGenUtil;

/**
 * Test the generation model.
 *
 */
public class CobolStructureToXsdModelTest extends AbstractXsdTester {

    /** The parameters set.*/
    private CobolStructureToXsdModel _model;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**  @{inheritDoc}*/
    public void setUp() throws Exception {
        super.setUp();
        try {
            CodeGenUtil.initVelocity();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        CodeGenUtil.checkDirectory(GEN_ANT_DIR, true);
        _model = new CobolStructureToXsdModel();
    }

    /**
     * Generate an ant script and run it to produce an XML Schema.
     * @throws Exception if ant cannot be generated
     */
    public void testBuildCob2Xsd() throws Exception {

        _model.setProductLocation("../../../..");
        _model.setProbeFile(new File("probe.file.tmp"));
        _model.setCobolSourceFilePath(COBOL_SAMPLES_DIR + "/LSFILEAE");
        _model.setTargetDir(GEN_XSD_DIR);
        _model.setTargetXsdFileName("lsfileae.xsd");
        
        _model.getContext().setTargetNamespace("http://legstar.com/test/coxb/lsfileae");
        _model.getContext().setAddLegStarAnnotations(true);
        /* Backward compatibility */
        _model.getContext().setElementNamesStartWithUppercase(true);
        _model.getContext().setQuoteIsQuote(false);
        
        runAnt(genAntScriptAsFile());
        
        File xsdFile = new File(GEN_XSD_DIR, _model.getTargetXsdFileName());
        Document result = getXMLSchemaAsDoc(xsdFile);
        Document expected = getXMLSchemaAsDoc(new File(XSD_SAMPLES_DIR, _model
                .getTargetXsdFileName().toLowerCase()));
        compare(xsdFile.getName(), expected, result);
    }
    
    /**
     * Test the COBOL formatting.
     */
    public void testCobolFormat() throws Exception {
        String result = genAntScriptAsString();
        assertTrue(result.contains("codeFormat=\"FIXED_FORMAT\""));
        assertTrue(result.contains("startColumn=\"7\""));
        assertTrue(result.contains("endColumn=\"72\""));
        
        _model.getContext().setStartColumn(1);
        _model.getContext().setEndColumn(66);
        result = genAntScriptAsString();
        assertTrue(result.contains("startColumn=\"1\""));
        assertTrue(result.contains("endColumn=\"66\""));
        
        _model.getContext().setCodeFormat(CodeFormat.FREE_FORMAT);
        result = genAntScriptAsString();
        assertTrue(result.contains("codeFormat=\"FREE_FORMAT\""));
      
    }
    
    /**
     * Test the XSD options.
     */
    public void testXsdOptions() throws Exception {
        String result = genAntScriptAsString();
        assertTrue(result.contains("xsdEncoding=\"UTF-8\""));
        assertTrue(result.contains("targetNamespace=\"http://www.acme.com/test\""));
        assertTrue(result.contains("mapConditionsToFacets=\"false\""));
        assertTrue(result.contains("nameConflictPrependParentName=\"false\""));
        assertTrue(result.contains("elementNamesStartWithUppercase=\"false\""));
        
        _model.getContext().setXsdEncoding("ISO-8859-1");
        _model.getContext().setTargetNamespace("test/targetNamespace");
        _model.getContext().setMapConditionsToFacets(true);
        _model.getContext().setNameConflictPrependParentName(true);
        _model.getContext().setElementNamesStartWithUppercase(true);
        result = genAntScriptAsString();
        assertTrue(result.contains("xsdEncoding=\"ISO-8859-1\""));
        assertTrue(result.contains("targetNamespace=\"test/targetNamespace\""));
        assertTrue(result.contains("mapConditionsToFacets=\"true\""));
        assertTrue(result.contains("nameConflictPrependParentName=\"true\""));
        assertTrue(result.contains("elementNamesStartWithUppercase=\"true\""));
        
        _model.getContext().setCustomXsltFileName("src/test/resources/xslt/alltypes.xsl");
        result = genAntScriptAsString();
        assertTrue(result.contains("customXsltFileName=\"src/test/resources/xslt/alltypes.xsl\""));
      
    }

    /**
     * Test the LegStar annotations options.
     */
    public void testLegStarAnnotationsOptions() throws Exception {
        String result = genAntScriptAsString();
        assertTrue(result.contains("addLegStarAnnotations=\"false\""));
        
        _model.getContext().setAddLegStarAnnotations(true);
         result = genAntScriptAsString();
         assertTrue(result.contains("addLegStarAnnotations=\"true\""));
        
    }

    /**
     * Test the COBOL compiler options.
     */
    public void testCobolCompilerOptions() throws Exception {
        String result = genAntScriptAsString();
        assertTrue(result.contains("currencySign=\"$\""));
        assertTrue(result.contains("currencySymbol=\"$\""));
        assertTrue(result.contains("nSymbolDbcs=\"false\""));
        assertTrue(result.contains("decimalPointIsComma=\"false\""));
        assertTrue(result.contains("quoteIsQuote=\"true\""));
        
        _model.getContext().setCurrencySign("£");
        _model.getContext().setCurrencySymbol("€");
        _model.getContext().setNSymbolDbcs(true);
        _model.getContext().setDecimalPointIsComma(true);
        _model.getContext().setQuoteIsQuote(false);
         result = genAntScriptAsString();
         assertTrue(result.contains("currencySign=\"£\""));
         assertTrue(result.contains("currencySymbol=\"€\""));
         assertTrue(result.contains("nSymbolDbcs=\"true\""));
         assertTrue(result.contains("decimalPointIsComma=\"true\""));
         assertTrue(result.contains("quoteIsQuote=\"false\""));
        
    }

    /**
     * Generates an ant script from a VLC template.
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
     * @param buildFile the ant script
     * @throws Exception if ant script execution fails
     */
    protected void runAnt(File buildFile) throws Exception {
        final Project project = new Project();
        project.addBuildListener(new TestLogger());
        project.setCoreLoader(this.getClass().getClassLoader());
        project.init();
        ProjectHelper helper = ProjectHelper.getProjectHelper();
        project.addReference("ant.projectHelper", helper);
        helper.parse(project, buildFile);
        Vector<String> targets = new Vector<String>();
        targets.addElement(project.getDefaultTarget());
        project.executeTargets(targets);
    }
    
    /**
     * Used to route ant messages to our log.
     *
     */
    private class TestLogger implements BuildListener, BuildLogger {

        public void buildFinished(BuildEvent event) {
            if (event.getException() == null) {
                _log.debug("Build finished.");
            } else {
                _log.debug("Build finished.", event.getException());
            }
        }

        public void buildStarted(BuildEvent event) {
            _log.debug("Build started.");
            
        }

        public void messageLogged(BuildEvent event) {
            _log.debug(event.getMessage());
        }

        public void targetFinished(BuildEvent event) {
            _log.debug("targetFinished.");
            if (event.getException() == null) {
                _log.debug("targetFinished " + event.getTarget().getName());
            } else {
                _log.debug("targetFinished " + event.getTarget().getName(), event.getException());
            }
        }

        public void targetStarted(BuildEvent event) {
            _log.debug("targetStarted " + event.getTarget().getName());
        }

        public void taskFinished(BuildEvent event) {
            _log.debug("taskFinished.");
        }

        public void taskStarted(BuildEvent event) {
            _log.debug("taskStarted.");
        }

        public void setEmacsMode(boolean emacsMode) {
            _log.debug("setEmacsMode.");
        }

        public void setErrorPrintStream(PrintStream err) {
            _log.debug("setErrorPrintStream.");
        }

        public void setMessageOutputLevel(int level) {
            _log.debug("setMessageOutputLevel.");
        }

        public void setOutputPrintStream(PrintStream output) {
            _log.debug("setMessageOutputLevel.");
        }
        
    }

}
