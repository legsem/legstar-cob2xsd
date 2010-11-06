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

import java.io.File;

import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Document;

/**
 * Test the COBOL to XSD API.
 * 
 */
public class CobolStructureToXsdTest extends AbstractXsdTester {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

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
                model.setTargetNamespace("http://legstar.com/test/coxb/"
                        + name);

                /* Backward compatibility */
                model.setElementNamesStartWithUppercase(true);
                model.setQuoteIsQuote(false);

                CobolStructureToXsd translator = new CobolStructureToXsd(
                        model);
                File xsdFile = translator.translate(cobolFile, xsdGenDir);
                if (_log.isDebugEnabled()) {
                    _log.debug("Result:\n"
                            + FileUtils.readFileToString(xsdFile));
                }
                Document result = getXMLSchemaAsDoc(xsdFile);
                Document expected = getXMLSchemaAsDoc(new File(XSD_SAMPLES_DIR,
                        name.toLowerCase() + ".xsd"));
                compare(xsdFile.getName(), expected, result);
            }
        }
    }

}
