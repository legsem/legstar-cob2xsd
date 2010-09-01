package com.legstar.cob2xsd;

import java.io.File;
import java.io.FileReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.commons.io.FileUtils;
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
 * Generic test methods used when XSD are generated and need to be compared with
 * reference XSDs.
 * 
 */
public class AbstractXsdTester extends XMLTestCase {

    /** All files from this folder will be tested. */
    public static final String COBOL_SAMPLES_DIR = "src/test/resources/cobol";

    /** Folder holding expected results. */
    public static final String XSD_SAMPLES_DIR = "src/test/resources/schema";

    /** Folder holding test XSLT. */
    public static final String XSLT_SAMPLES_DIR = "src/test/resources/xslt";

    /** Where generated schemas are stored (cleanable location).*/
    public static final File GEN_XSD_DIR = new File("target/src/gen/schema");

    /** Ant scripts files will be generated here (cleanable location). */
    public static final File GEN_ANT_DIR = new File("target/src/gen/ant");

    /** DOM document factory. */
    protected DocumentBuilder _docBuilder;

    /** {@inheritDoc}*/
    public void setUp() throws Exception {
        DocumentBuilderFactory docFac = DocumentBuilderFactory.newInstance();
        docFac.setNamespaceAware(true);
        docFac.setIgnoringElementContentWhitespace(true);
        _docBuilder = docFac.newDocumentBuilder();

        GEN_XSD_DIR.mkdirs();
        FileUtils.cleanDirectory(GEN_XSD_DIR);
        GEN_ANT_DIR.mkdirs();
        FileUtils.cleanDirectory(GEN_ANT_DIR);
    }

    /**
     * Customization of the XmlUnit assertXMLEqual.
     * @param xsdFileName XML schema file name
     * @param expected expected result
     * @param result actual result
     */
    protected void compare(
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
