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
package com.legstar.cob2xsd;


import java.io.File;
import java.io.FileReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;


import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.custommonkey.xmlunit.Diff;
import org.custommonkey.xmlunit.Difference;
import org.custommonkey.xmlunit.DifferenceConstants;
import org.custommonkey.xmlunit.DifferenceListener;
import org.custommonkey.xmlunit.XMLTestCase;
import org.custommonkey.xmlunit.XMLUnit;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;

/**
 * Test the COBOL to XSD API.
 *
 */
public class CobolStructureToXsdTest extends XMLTestCase {

    /** All files from this folder will be tested. */
    private static final String COBOL_SAMPLES_DIR = "src/test/resources/cobol";

    /** Folder holding expected results. */
    private static final String XSD_SAMPLES_DIR = "src/test/resources/schema";

    /** Where generated schemas are stored (cleanable location).*/
    private static final String XSD_GEN_DIR = "target/generated-sources/schema";

    /** Folder holding test XSLT. */
    private static final String XSLT_SAMPLES_DIR = "src/test/resources/xslt";

    /** DOM document factory. */
    private DocumentBuilder _docBuilder;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());
    
    /** {@inheritDoc}*/
    public void setUp() throws Exception {
        DocumentBuilderFactory docFac = DocumentBuilderFactory.newInstance();
        docFac.setNamespaceAware(true);
        docFac.setIgnoringElementContentWhitespace(true);
        _docBuilder = docFac.newDocumentBuilder();
    }

    /**
     * Go through all the samples and check with backward compatibility.
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
                File custmXslt = new File(XSLT_SAMPLES_DIR, name + ".xsl");
                if (custmXslt.exists()) {
                    context.setCustomXsltFileName(custmXslt.getPath());
                } else {
                    context.setCustomXsltFileName(null);
                }
                context.setAddLegStarAnnotations(true);
                context.setTargetNamespace("http://legstar.com/test/coxb/" + name);
                
                /* Backward compatibility */
                context.setElementNamesStartWithUppercase(true);
                context.setQuoteIsQuote(false);
                
                CobolStructureToXsd translator = new CobolStructureToXsd(context);
                File xsdFile = translator.translate(cobolFile, xsdGenDir);
                if (_log.isDebugEnabled()) {
                    _log.debug("Result:\n" + FileUtils.readFileToString(xsdFile));
                }
                Document result = getXMLSchemaAsDoc(xsdFile);
                Document expected = getXMLSchemaAsDoc(new File(XSD_SAMPLES_DIR, name.toLowerCase() + ".xsd"));
                compare(xsdFile.getName(), expected, result);
            }
        }
    }

    /**
     * Customization of the XmlUnit assertXMLEqual.
     * @param xsdFileName XML schema file name
     * @param expected expected result
     * @param result actual result
     */
    private void compare(
            final String xsdFileName,
            final Document expected,
            final Document result) {
        boolean oldIgnore = XMLUnit.getIgnoreWhitespace();
        XMLUnit.setIgnoreWhitespace(true);
        try {
            Diff d = new Diff(expected, result);
            MyDifferenceListener listener = new MyDifferenceListener();
            d.overrideDifferenceListener(listener);
            assertTrue(xsdFileName + ": expected pieces to be similar, " + d.toString(), d.similar());
//            if (!d.similar()) {
//                _log.error("expected pieces to be similar, " + d.toString());
//            }
        } finally {
            XMLUnit.setIgnoreWhitespace(oldIgnore);
        }
    }

    /**
     * XmlUnit difference listener needed because attribute values in XML schema
     * might be prefixed by a namespace prefix therefore failing comparison
     * tests while actually identical.
     *
     */
    class MyDifferenceListener implements DifferenceListener {

        /** {@inheritDoc}*/
        public int differenceFound(final Difference difference) {
            if (difference.getId() == DifferenceConstants.ATTR_VALUE_ID) {
                if (compareNoPrefix(difference.getControlNodeDetail().getValue(),
                        difference.getTestNodeDetail().getValue())) {
                    return RETURN_IGNORE_DIFFERENCE_NODES_IDENTICAL;
                }
            }
            return RETURN_ACCEPT_DIFFERENCE;
        }

        /** {@inheritDoc}*/
        public void skippedComparison(final Node control, final Node test) {
        }
    }
    
    /**
     * Compares 2 strings modulo a potential namespace prefix.
     * @param expected the expected string
     * @param result the actual result
     * @return true if both strings are identical modulo a namespace prefix
     */
    private boolean compareNoPrefix(final String expected, final String result) {
        String e = (expected.indexOf(":") > -1) ? expected.substring(expected.indexOf(":") + 1) : expected;
        String r = (result.indexOf(":") > -1) ? result.substring(result.indexOf(":") + 1) : result;
        return e.equals(r);
    }

    /**
     * Pick an XSD from the file system and return it as a DOM.
     * @param xsdFile the XSD file
     * @return an XML DOM
     * @throws Exception if loaf fails
     */
    public Document getXMLSchemaAsDoc(final File xsdFile) throws Exception {
        InputSource is = new InputSource();
        is.setCharacterStream(
                new FileReader(xsdFile));
        return _docBuilder.parse(is);
    }


}
