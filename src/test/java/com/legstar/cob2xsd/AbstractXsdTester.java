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
import java.io.FileReader;
import java.io.StringReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.commons.io.FileUtils;
import org.custommonkey.xmlunit.DetailedDiff;
import org.custommonkey.xmlunit.Diff;
import org.custommonkey.xmlunit.XMLTestCase;
import org.custommonkey.xmlunit.XMLUnit;
import org.w3c.dom.Document;
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

    /** Where generated schemas are stored (cleanable location). */
    public static final File GEN_XSD_DIR = new File("target/src/gen/schema");

    /** Ant scripts files will be generated here (cleanable location). */
    public static final File GEN_ANT_DIR = new File("target/src/gen/ant");

    /** Used for XML validation. */
    public static final String JAXP_SCHEMA_LANGUAGE =
            "http://java.sun.com/xml/jaxp/properties/schemaLanguage";

    /** Used for XML validation. */
    public static final String W3C_XML_SCHEMA =
            "http://www.w3.org/2001/XMLSchema";

    /** Used for XML validation. */
    public static final String JAXP_SCHEMA_SOURCE =
            "http://java.sun.com/xml/jaxp/properties/schemaSource";

    /** Used for XML validation. */
    public static final String W3C_XML_SCHEMA_SOURCE =
            "http://www.w3.org/2001/XMLSchema.xsd";

    /** DOM document builder factory. */
    protected DocumentBuilderFactory _docFac;

    /** DOM document factory. */
    protected DocumentBuilder _docBuilder;

    /** {@inheritDoc} */
    public void setUp() throws Exception {
        _docFac = DocumentBuilderFactory.newInstance();
        _docFac.setNamespaceAware(true);
        _docFac.setValidating(false);
        _docFac.setIgnoringElementContentWhitespace(true);
        _docFac.setAttribute(JAXP_SCHEMA_LANGUAGE, W3C_XML_SCHEMA);
        _docFac.setAttribute(JAXP_SCHEMA_SOURCE, W3C_XML_SCHEMA_SOURCE);
        _docBuilder = _docFac.newDocumentBuilder();

        GEN_XSD_DIR.mkdirs();
        FileUtils.cleanDirectory(GEN_XSD_DIR);
        GEN_ANT_DIR.mkdirs();
        FileUtils.cleanDirectory(GEN_ANT_DIR);
    }

    /**
     * Customization of the XmlUnit assertXMLEqual.
     * 
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
            DetailedDiff d = new DetailedDiff(new Diff(expected, result));
            assertTrue(xsdFileName + ": expected pieces to be identical, "
                    + d.toString(), d.identical());
        } finally {
            XMLUnit.setIgnoreWhitespace(oldIgnore);
        }
    }

    /**
     * Compare result to expected in the case where the XML is serialized as
     * strings (beware that results are pretty printed and therefore will hold
     * CR and/or LF characters).
     * 
     * @param expected the expected XML Schema as a string
     * @param result the result XML schema as a string
     * @throws Exception if comparison fails
     */
    protected void compare(
            final String expected,
            final String result) throws Exception {
        InputSource expectedSrce = new InputSource(new StringReader(expected));
        InputSource resultSrce = new InputSource(new StringReader(result
                .replace("\r", "").replace("\n", "")));
        compare("",
                _docBuilder.parse(expectedSrce),
                _docBuilder.parse(resultSrce));

    }

    /**
     * Pick an XSD from the file system and return it as a DOM.
     * 
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
