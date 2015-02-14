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
package com.legstar.cob2xsd;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Document;

/**
 * Test the COBOL to XSD API.
 * 
 */
public class Cob2XsdIOTest extends AbstractXsdTester {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** True when references should be created. */
    private static final boolean CREATE_REFERENCES = false;

    /**
     * Go through all the samples and check with backward compatibility.
     * 
     * @throws Exception if test fails
     */
    public void testAllSamples() throws Exception {
        Cob2XsdModel model = new Cob2XsdModel();
        File cobolDir = new File(COBOL_SAMPLES_DIR);
        File xsdGenDir = GEN_XSD_DIR;
        xsdGenDir.mkdir();
        for (File cobolFile : cobolDir.listFiles()) {
            if (cobolFile.isFile()) {
                _log.debug("Translating " + cobolFile);
                String name = cobolFile.getName().toLowerCase();
                File custmXslt = new File(XSLT_SAMPLES_DIR, name + ".xsl");
                if (custmXslt.exists()) {
                    model.setCustomXsltFileName(custmXslt.getPath());
                } else {
                    model.setCustomXsltFileName(null);
                }
                model.setAddLegStarAnnotations(true);
                model.setTargetNamespace("http://legstar.com/test/coxb/" + name);

                /* Backward compatibility */
                model.setElementNamesStartWithUppercase(true);
                model.setQuoteIsQuote(false);

                Cob2XsdIO translator = new Cob2XsdIO(model);
                File xsdFile = translator
                        .translate(cobolFile, xsdGenDir, false);
                if (_log.isDebugEnabled()) {
                    _log.debug("Result:\n"
                            + FileUtils.readFileToString(xsdFile));
                }
                File xsdRefFile = new File(XSD_SAMPLES_DIR, cobolFile.getName()
                        + ".xsd");
                if (CREATE_REFERENCES) {
                    FileUtils.copyFile(xsdFile, xsdRefFile);
                } else {
                    Document result = getXMLSchemaAsDoc(xsdFile);
                    Document expected = getXMLSchemaAsDoc(xsdRefFile);
                    compare(xsdFile.getName(), expected, result);
                }
            }
        }
    }

    /**
     * Check that the XML Schema produced has the correct encoding from a file
     * standpoint. Not using commons-io on purpose.
     */
    public void testFileOutputEncoding() {
        BufferedReader in = null;
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            model.setTargetNamespace("http://www.mycompany.com/test");
            model.setCobolSourceFileEncoding("UTF-8");
            model.setXsdEncoding("UTF-8");
            model.setAddLegStarAnnotations(true);
            Cob2XsdIO cob2xsd = new Cob2XsdIO(model);
            File tempCobolFile = File.createTempFile("test", ".cob");
            tempCobolFile.deleteOnExit();
            BufferedWriter out = new BufferedWriter(new OutputStreamWriter(
                    new FileOutputStream(tempCobolFile), "UTF8"));
            out.write("       01 A.\n           02 B PIC G(4) VALUE '牛年快乐'.");
            out.flush();
            out.close();
            File xmlSchema = cob2xsd.translate(tempCobolFile, GEN_XSD_DIR,
                    false);
            in = new BufferedReader(new InputStreamReader(new FileInputStream(
                    xmlSchema), "UTF8"));
            String line;
            while ((line = in.readLine()) != null) {
                if (line.contains("cobolName=\"B\"")) {
                    assertTrue(line.contains("value=\"牛年快乐\""));
                }
            }

        } catch (Exception e) {
            e.printStackTrace();
            fail();
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    e.printStackTrace();
                    fail();
                }
            }
        }

    }

}
