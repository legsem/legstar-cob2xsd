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

import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import junit.framework.TestCase;

import com.legstar.codegen.CodeGenUtil;

/**
 * Test the generation model.
 *
 */
public class CobolStructureToXsdModelTest extends TestCase {

    /** Ant scripts files will be generated here. */
    public static final File GEN_ANT_DIR = new File("target/src/gen/ant");

    /** XML schema files will be generated here. */
    public static final File GEN_XSD_DIR = new File("target/src/gen/schema");

    /** Reference ant scripts files are taken from here. */
    public static final File TEST_ANT_DIR = new File("src/test/resources/ant");

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** @{inheritDoc}*/
    public void setUp() {
        try {
            CodeGenUtil.initVelocity();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        CodeGenUtil.checkDirectory(GEN_ANT_DIR, true);
    }

    /**
     * Generate an ant script capable of producing the binding artifacts.
     * @throws Exception if ant cannot be generated
     */
    public void testBuildCoxb() throws Exception {

        CobolStructureToXsdModel model = new CobolStructureToXsdModel();

        model.setProductLocation("/Users/Fady/sandbox/legstar");
        model.setProbeFile(new File("probe.file.tmp"));
        model.setCobolSourceFilePath("/Users/Fady/sandbox/cobol/LSFILEAE");
        model.setTargetDir(GEN_XSD_DIR);
        model.setTargetXsdFileName("lsfileae.xsd");
        
        model.getContext().setXsdEncoding("ISO-8859-1");
        model.getContext().setTargetNamespace("test/targetNamespace");
        model.getContext().setMapConditionsToFacets(true);
        model.getContext().setCustomXsltFileName("src/test/resources/xslt/alltypes.xsl");
        model.getContext().setNameConflictPrependParentName(true);
        model.getContext().setElementNamesStartWithUppercase(true);
        
        model.getContext().setAddLegStarAnnotations(true);
        model.getContext().setJaxbPackageName("test.jaxbPackageName");
        model.getContext().setJaxbTypeClassesSuffix("TestJaxbTypeClassesSuffix");
        
        model.getContext().setCurrencySign("£");
        model.getContext().setCurrencySymbol("€");
        model.getContext().setCurrencySymbol("€");
        model.getContext().setNSymbolDbcs(true);
        model.getContext().setDecimalPointIsComma(true);
        model.getContext().setQuoteIsQuote(false);

        String antScriptName = "build.xml";
        
        File resultFile = CodeGenUtil.getFile(GEN_ANT_DIR, antScriptName);
        model.generateBuild(resultFile);
        
        String result = FileUtils.readFileToString(resultFile, "UTF-8");
        
        if (_log.isDebugEnabled()) {
            _log.debug(result);
        }
        /* expected reference is a unix style file. */
        result = result.replace(CodeGenUtil.CRLF, "\n");
        result = result.replace('\\', '/');
        
        String expected = FileUtils.readFileToString(new File(TEST_ANT_DIR, antScriptName), "UTF-8");
        assertEquals(expected, result);
    }
}
