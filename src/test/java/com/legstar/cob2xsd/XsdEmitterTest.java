package com.legstar.cob2xsd;

import com.legstar.cobol.model.CobolDataItem;
import com.legstar.cobol.model.CobolDataItem.Usage;

/**
 * Test the CobolDataItemToXSD class.
 *
 */
public class XsdEmitterTest extends AbstractXsdEmitterTester {

    /**
     * Empty COBOL item.
     */
    public void testAnEmptyCobolItem() {
        emitAndCheck(
                "<complexType name=\"Filler\"><sequence/></complexType>",
                new CobolDataItem(), false);
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
                + "<maxLength value=\"5\"/>"
                + "</restriction>"
                + "</simpleType>"
                + "</element>"
                + "</sequence>"
                + "</complexType>",
                struct, false);
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
                struct, false);
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
                struct, false);
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
                struct, false);
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
                struct, false);
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
                struct, false);
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
                struct, false);
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
                struct, false);
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
                struct, false);
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
                struct, false);
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
                struct, false);
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
                + "<maxLength value=\"7\"/>"
                + "<pattern value=\"^[a-zA-Z\\s]{0,5}\\d{0,2}$\"/>"
                + "</restriction>"
                + "</simpleType>"
                + "</element>"
                + "</sequence>"
                + "</complexType>",
                struct, false);
    }

    /**
     * Test generation of LegStar annotations.
     */
    public void testAnnotations() {
        CobolDataItem struct = new CobolDataItem();
        struct.getChildren().add(getACobolElementaryItem("A-STRING", "X(5)", Usage.DISPLAY));
        emitAndCheck(
                ""
                + "<annotation>"
                + "<appinfo>"
                + "<jaxb:schemaBindings>"
                + "<jaxb:package name=\"com.legstar.test\"/>"
                + "</jaxb:schemaBindings>"
                + "</appinfo>"
                + "</annotation>"
                + "<complexType name=\"Filler\">"
                + "<sequence>"
                + "<element name=\"aString\">"
                + "<annotation>"
                + "<appinfo>"
                + "<cb:cobolElement cobolName=\"A-STRING\" levelNumber=\"1\""
                        + " picture=\"X(5)\" type=\"ALPHANUMERIC_ITEM\" usage=\"DISPLAY\"/>"
                + "</appinfo>"
                + "</annotation>"
                + "<simpleType>"
                + "<restriction base=\"string\">"
                + "<maxLength value=\"5\"/>"
                + "</restriction>"
                + "</simpleType>"
                + "</element>"
                + "</sequence>"
                + "</complexType>",
                struct, true);
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

}
