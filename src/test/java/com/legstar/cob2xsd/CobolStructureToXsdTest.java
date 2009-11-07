package com.legstar.cob2xsd;


import java.io.File;

import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import junit.framework.TestCase;

/**
 * Test the COBOL to XSD API.
 *
 */
public class CobolStructureToXsdTest extends TestCase {
    
    /** All files from this folder will be tested. */
    private static final String COBOL_SAMPLES_DIR = "src/test/resources/cobol";
    
    /** Folder holding expected results. */
    private static final String XSD_SAMPLES_DIR = "src/test/resources/schema";
    
    /** Where generated schemas are stored (cleanable location).*/
    private static final String XSD_GEN_DIR = "target/generated-sources/schema";

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Go through all the samples and chack.
     * @throws Exception if test fails
     */
    public void testAllSamples() throws Exception {
        Cob2XsdContext context = new Cob2XsdContext();
        File cobolDir = new File(COBOL_SAMPLES_DIR);
        File xsdGenDir = new File(XSD_GEN_DIR);
        xsdGenDir.mkdir();
        for (File cobolFile : cobolDir.listFiles()) {
            if (cobolFile.isFile()) {
                _log.debug("Translating " + cobolFile);
                String name = cobolFile.getName().toLowerCase();
                context.setAddLegStarAnnotations(true);
                context.setJaxbPackageName("com.legstar.test.coxb." + name);
                context.setTargetNamespace("http://legstar.com/test/coxb/" + name);
                CobolStructureToXsd translator = new CobolStructureToXsd(context);
                File xsdFile = translator.translate(cobolFile, xsdGenDir);
                if (_log.isDebugEnabled()) {
                    _log.debug("Result:\n" + FileUtils.readFileToString(xsdFile));
                }
            }
        }
    }

}
