package com.legstar.cob2xsd;

import java.io.StringReader;
import java.io.StringWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaCollection;
import org.custommonkey.xmlunit.XMLTestCase;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

import com.legstar.cobol.model.CobolDataItem;
import com.legstar.cobol.model.CobolDataItem.Usage;

/**
 * Test the CobolDataItemToXSD class.
 *
 */
public class XsdEmitterTest extends XMLTestCase {

    /** DOM document factory. */
    private DocumentBuilder _docBuilder;

    /** Translator options.*/
    private Cob2XsdContext _context;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** {@inheritDoc}*/
    public void setUp() throws Exception {
        DocumentBuilderFactory docFac = DocumentBuilderFactory.newInstance();
        docFac.setNamespaceAware(true);
        _docBuilder = docFac.newDocumentBuilder();
        _context = new Cob2XsdContext();
    }

    /**
     * Empty COBOL item.
     */
    public void testAnEmptyCobolItem() {
        emitAndCheck(
                "<complexType name=\"Filler\"><sequence/></complexType>",
                new CobolDataItem());
    }

    /**
     * A structure containing a single string item.
     */
    public void testString() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(getACobolElementaryItem("A-STRING", "X(5)", Usage.DISPLAY));
        emitAndCheck(
                "<complexType name=\"Filler\">"
                + "<sequence>"
                + "<element name=\"aString\">"
                + "<simpleType>"
                + "<restriction base=\"string\">"
                + "<length value=\"5\"/>"
                + "<pattern value=\"^.{0,5}$\"/>"
                + "</restriction>"
                + "</simpleType>"
                + "</element>"
                + "</sequence>"
                + "</complexType>",
                struct);
    }

    /**
     * A short numeric items.
     */
    public void testShort() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(getACobolElementaryItem("NUM1", "S9(4)", Usage.BINARY));
        emitAndCheck(
                "<complexType name=\"Filler\">"
                + "<sequence>"
                + "<element name=\"num1\">"
                + "<simpleType>"
                + "<restriction base=\"short\">"
                + "<totalDigits value=\"4\"/>"
                + "<minInclusive value=\"-9999\"/>"
                + "<maxInclusive value=\"9999\"/>"
                + "</restriction>"
                + "</simpleType>"
                + "</element>"
                + "</sequence>"
                + "</complexType>",
                struct);
    }

    /**
     * An unsignedShort numeric item.
     * There should be no minInclusive/maxInclusive because its native binary.
     */
    public void testUnsignedShort() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(getACobolElementaryItem("NUM1", "9(2)", Usage.NATIVEBINARY));
        emitAndCheck(
                "<complexType name=\"Filler\">"
                + "<sequence>"
                + "<element name=\"num1\">"
                + "<simpleType>"
                + "<restriction base=\"unsignedShort\">"
                + "<totalDigits value=\"2\"/>"
                + "</restriction>"
                + "</simpleType>"
                + "</element>"
                + "</sequence>"
                + "</complexType>"
                ,
                struct);
    }

    /**
     * An int numeric item.
     */
    public void testInt() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(getACobolElementaryItem("NUM1", "S9(9)", Usage.BINARY));
        emitAndCheck(
                "<complexType name=\"Filler\">"
                + "<sequence>"
                + "<element name=\"num1\">"
                + "<simpleType>"
                + "<restriction base=\"int\">"
                + "<totalDigits value=\"9\"/>"
                + "<minInclusive value=\"-999999999\"/>"
                + "<maxInclusive value=\"999999999\"/>"
                + "</restriction>"
                + "</simpleType>"
                + "</element>"
                + "</sequence>"
                + "</complexType>",
                struct);
    }

    /**
     * An unsigned int numeric item.
     */
    public void testUnsignedInt() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(getACobolElementaryItem("NUM1", "9(9)", Usage.DISPLAY));
        emitAndCheck(
                "<complexType name=\"Filler\">"
                + "<sequence>"
                + "<element name=\"num1\">"
                + "<simpleType>"
                + "<restriction base=\"unsignedInt\">"
                + "<totalDigits value=\"9\"/>"
                + "<minInclusive value=\"0\"/>"
                + "<maxInclusive value=\"999999999\"/>"
                + "</restriction>"
                + "</simpleType>"
                + "</element>"
                + "</sequence>"
                + "</complexType>",
                struct);
    }

    /**
     * A long numeric item.
     */
    public void testLong() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(getACobolElementaryItem("NUM1", "S9(18)", Usage.BINARY));
        emitAndCheck(
                "<complexType name=\"Filler\">"
                + "<sequence>"
                + "<element name=\"num1\">"
                + "<simpleType>"
                + "<restriction base=\"long\">"
                + "<totalDigits value=\"18\"/>"
                + "<minInclusive value=\"-999999999999999999\"/>"
                + "<maxInclusive value=\"999999999999999999\"/>"
                + "</restriction>"
                + "</simpleType>"
                + "</element>"
                + "</sequence>"
                + "</complexType>",
                struct);
    }

    /**
     * An unsigned long numeric item.
     */
    public void testUnsignedLong() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(getACobolElementaryItem("NUM1", "9(12)", Usage.NATIVEBINARY));
        emitAndCheck(
                "<complexType name=\"Filler\">"
                + "<sequence>"
                + "<element name=\"num1\">"
                + "<simpleType>"
                + "<restriction base=\"unsignedLong\">"
                + "<totalDigits value=\"12\"/>"
                + "</restriction>"
                + "</simpleType>"
                + "</element>"
                + "</sequence>"
                + "</complexType>",
                struct);
    }

    /**
     * A very long numeric item (ARITH(EXTEND) compiler option).
     */
    public void testInteger() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(getACobolElementaryItem("NUM1", "S9(31)", Usage.DISPLAY));
        emitAndCheck(
                "<complexType name=\"Filler\">"
                + "<sequence>"
                + "<element name=\"num1\">"
                + "<simpleType>"
                + "<restriction base=\"integer\">"
                + "<totalDigits value=\"31\"/>"
                + "<minInclusive value=\"-9999999999999999999999999999999\"/>"
                + "<maxInclusive value=\"9999999999999999999999999999999\"/>"
                + "</restriction>"
                + "</simpleType>"
                + "</element>"
                + "</sequence>"
                + "</complexType>",
                struct);
    }

    /**
     * A decimal numeric item.
     */
    public void testDecimal() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(getACobolElementaryItem("NUM1", "S9(8)V99", Usage.PACKEDDECIMAL));
        emitAndCheck(
                "<complexType name=\"Filler\">"
                + "<sequence>"
                + "<element name=\"num1\">"
                + "<simpleType>"
                + "<restriction base=\"decimal\">"
                + "<totalDigits value=\"10\"/>"
                + "<fractionDigits value=\"2\"/>"
                + "<minInclusive value=\"-99999999.99\"/>"
                + "<maxInclusive value=\"99999999.99\"/>"
                + "</restriction>"
                + "</simpleType>"
                + "</element>"
                + "</sequence>"
                + "</complexType>",
                struct);
    }

    /**
     * A float numeric item.
     */
    public void testFloat() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(getACobolElementaryItem("NUM1", null, Usage.SINGLEFLOAT));
        emitAndCheck(
                "<complexType name=\"Filler\">"
                + "<sequence>"
                + "<element name=\"num1\">"
                + "<simpleType>"
                + "<restriction base=\"float\">"
                + "</restriction>"
                + "</simpleType>"
                + "</element>"
                + "</sequence>"
                + "</complexType>",
                struct);
    }

    /**
     * A double numeric item.
     */
    public void testDouble() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(getACobolElementaryItem("NUM1", null, Usage.DOUBLEFLOAT));
        emitAndCheck(
                "<complexType name=\"Filler\">"
                + "<sequence>"
                + "<element name=\"num1\">"
                + "<simpleType>"
                + "<restriction base=\"double\">"
                + "</restriction>"
                + "</simpleType>"
                + "</element>"
                + "</sequence>"
                + "</complexType>",
                struct);
    }

    /**
     * An array.
     */
    public void testArray() {
        CobolDataItem struct = new CobolDataItem();
        CobolDataItem array = getACobolElementaryItem("A-STRING", "A(5)99", Usage.DISPLAY);
        array.setMinOccurs(0);
        array.setMaxOccurs(3);
        struct.getChildren().add(array);
        emitAndCheck(
                "<complexType name=\"Filler\">"
                + "<sequence>"
                + "<element maxOccurs=\"3\" minOccurs=\"0\" name=\"aString\">"
                + "<simpleType>"
                + "<restriction base=\"string\">"
                + "<length value=\"7\"/>"
                + "<pattern value=\"^[a-zA-Z\\s]{0,5}\\d{0,2}$\"/>"
                + "</restriction>"
                + "</simpleType>"
                + "</element>"
                + "</sequence>"
                + "</complexType>",
                struct);
    }
    /**
     * @return a COBOL data item.
     * @param cobolName the desired COBOL name
     * @param picture the picture clause
     * @param usage the usage
     */
    private CobolDataItem getACobolElementaryItem(final String cobolName, final String picture, final Usage usage) {
        CobolDataItem cobolItem = new CobolDataItem();
        cobolItem.setCobolName(cobolName);
        cobolItem.setPicture(picture);
        cobolItem.setUsage(usage);
        return cobolItem;
    }

    /**
     * Helper that emits XML schema from a COBOL data item and checks result.
     * @param expected the expected XML Schema (without the schema element for simplicity)
     * @param dataItem the COBOL data item
     */
    private void emitAndCheck(final String expected, final CobolDataItem dataItem) {
        XmlSchema xsd = new XmlSchema("http://legstar.com/test", new XmlSchemaCollection());
        XsdEmitter emitter = new XsdEmitter(xsd, _docBuilder, _context);
        XsdDataItem xsdDataItem = new XsdDataItem(dataItem, _context);
        xsd.getItems().add(emitter.createXmlSchemaType(xsdDataItem));
        if (_log.isDebugEnabled()) {
            StringWriter writer = new StringWriter();
            xsd.write(writer);
            _log.debug("result:\n" + writer.toString());
        }
        try {
            assertXMLEqual(getExpectedXMLSchema(expected), xsd.getSchemaDocument());
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Wrap the expected content in a complete XML schema and make it a DOM.
     * @param expected the XML Schema content
     * @return a DOM document
     * @throws Exception if something goes wrong
     */
    private Document getExpectedXMLSchema(final String expected) throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append("<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                + " xmlns:tns=\"http://legstar.com/test\""
                + " attributeFormDefault=\"unqualified\""
                + " elementFormDefault=\"unqualified\""
                + " targetNamespace=\"http://legstar.com/test\">");
        sb.append(expected);
        sb.append("</schema>");
        InputSource is = new InputSource();
        is.setCharacterStream(
                new StringReader(sb.toString()));
        return _docBuilder.parse(is);
    }


}
