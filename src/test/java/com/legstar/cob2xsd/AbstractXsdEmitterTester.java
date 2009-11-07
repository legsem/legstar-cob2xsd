package com.legstar.cob2xsd;

import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaCollection;
import org.apache.ws.commons.schema.XmlSchemaForm;
import org.custommonkey.xmlunit.XMLTestCase;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

import com.legstar.cobol.model.CobolDataItem;

/**
 * General purpose testing helpers.
 *
 */
public class AbstractXsdEmitterTester extends XMLTestCase {

    /** DOM document factory. */
    private DocumentBuilder _docBuilder;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** {@inheritDoc}*/
    public void setUp() throws Exception {
        DocumentBuilderFactory docFac = DocumentBuilderFactory.newInstance();
        docFac.setNamespaceAware(true);
        _docBuilder = docFac.newDocumentBuilder();
    }

    /**
     * Helper that emits XML schema from a COBOL data item and checks result.
     * @param expected the expected XML Schema (without the schema element for simplicity)
     * @param dataItem the COBOL data item
     * @param withLegStarAnnotations true if LegStar annotations are to be added
     */
    public void emitAndCheck(
            final String expected,
            final CobolDataItem dataItem,
            final boolean withLegStarAnnotations) {
        Cob2XsdContext context = new Cob2XsdContext();
        if (withLegStarAnnotations) {
            context.setAddLegStarAnnotations(true);
            context.setJaxbPackageName("com.legstar.test");
        }
        XmlSchema xsd = getXmlSchema();
        XsdEmitter emitter = new XsdEmitter(xsd, _docBuilder, context);
        XsdDataItem xsdDataItem = new XsdDataItem(dataItem, context, null, new ArrayList < String >());
        xsd.getItems().add(emitter.createXmlSchemaType(xsdDataItem));
        check(expected, xsd, withLegStarAnnotations);
    }
    
    /**
     * @return an empty XML schema for testing
     */
    public XmlSchema getXmlSchema() {
        XmlSchema xsd = new XmlSchema("http://legstar.com/test", new XmlSchemaCollection());
        xsd.setElementFormDefault(new XmlSchemaForm(XmlSchemaForm.QUALIFIED));
        return xsd;
    }

    /**
     * Helper that checks a result XML schema against an expected one.
     * @param expected the expected XML Schema (without the schema element for simplicity)
     * @param xsd the XML schema result
     * @param withLegStarAnnotations true if LegStar annotations are to be added
     */
    public void check(
            final String expected,
            final XmlSchema xsd,
            final boolean withLegStarAnnotations) {
        if (_log.isDebugEnabled()) {
            StringWriter writer = new StringWriter();
            xsd.write(writer);
            _log.debug("result:\n" + writer.toString());
        }
        try {
            assertXMLEqual(
                    getExpectedXMLSchema(expected, withLegStarAnnotations),
                    xsd.getSchemaDocument());
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }
    /**
     * Wrap the expected content in a complete XML schema and make it a DOM.
     * @param expected the XML Schema content
     * @param withLegStarAnnotations true if LegStar annotations are to be added
     * @return a DOM document
     * @throws Exception if something goes wrong
     */
    public Document getExpectedXMLSchema(
            final String expected,
            final boolean withLegStarAnnotations) throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append("<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                + " xmlns:tns=\"http://legstar.com/test\""
                + " attributeFormDefault=\"unqualified\""
                + " elementFormDefault=\"qualified\""
                + " targetNamespace=\"http://legstar.com/test\"");
        if (withLegStarAnnotations) {
            sb.append(" xmlns:cb=\"http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd\"");
            sb.append(" xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\"");
            sb.append(" xmlns:ns0=\"http://java.sun.com/xml/ns/jaxb\"");
            sb.append(" ns0:extensionBindingPrefixes=\"cb\"");
            sb.append(" xmlns:ns1=\"http://java.sun.com/xml/ns/jaxb\"");
            sb.append(" ns1:version=\"2.0\"");
        }
        sb.append(">");
        sb.append(expected);
        sb.append("</schema>");
        InputSource is = new InputSource();
        is.setCharacterStream(
                new StringReader(sb.toString()));
        return _docBuilder.parse(is);
    }

}
