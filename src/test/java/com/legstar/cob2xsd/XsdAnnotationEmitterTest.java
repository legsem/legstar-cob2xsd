package com.legstar.cob2xsd;

import java.util.ArrayList;

import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaAnnotation;

import com.legstar.cobol.model.CobolDataItem;
import com.legstar.cobol.model.CobolDataItem.Usage;

/**
 * Test the XSD annotations produced.
 *
 */
public class XsdAnnotationEmitterTest extends AbstractXsdEmitterTester {

    /**
     * Schema level annotations happen at instantiation time.
     * Check that we load the annotations properly.
     */
    public void testInstantiation() {
        Cob2XsdContext context = new Cob2XsdContext();
        context.setJaxbPackageName("jaxb.package.name");
        XmlSchema xsd = getXmlSchema();
        new XsdAnnotationEmitter(xsd, context);
        check(
                "<annotation>"
                + "<appinfo>"
                + "<jaxb:schemaBindings>"
                + "<jaxb:package name=\"jaxb.package.name\"/>"
                + "</jaxb:schemaBindings>"
                + "</appinfo>"
                + "</annotation>"
                , xsd, true);
    }

    /**
     * Test with a JAXB type suffix parameter.
     */
    public void testInstantiationWithTypeSuffix() {
        Cob2XsdContext context = new Cob2XsdContext();
        context.setJaxbPackageName("jaxb.package.name");
        context.setJaxbTypeClassesSuffix("Type");
        XmlSchema xsd = getXmlSchema();
        new XsdAnnotationEmitter(xsd, context);
        check(
                "<annotation>"
                + "<appinfo>"
                + "<jaxb:schemaBindings>"
                + "<jaxb:package name=\"jaxb.package.name\"/>"
                + "<jaxb:nameXmlTransform>"
                + "<jaxb:typeName suffix=\"Type\"/>"
                + "</jaxb:nameXmlTransform>"
                + "</jaxb:schemaBindings>"
                + "</appinfo>"
                + "</annotation>"
                , xsd, true);
    }

    /**
     * Test with an empty JAXB type suffix parameter.
     */
    public void testInstantiationWithEmptyTypeSuffix() {
        Cob2XsdContext context = new Cob2XsdContext();
        context.setJaxbPackageName("jaxb.package.name");
        context.setJaxbTypeClassesSuffix("");
        XmlSchema xsd = getXmlSchema();
        new XsdAnnotationEmitter(xsd, context);
        check(
                "<annotation>"
                + "<appinfo>"
                + "<jaxb:schemaBindings>"
                + "<jaxb:package name=\"jaxb.package.name\"/>"
                + "</jaxb:schemaBindings>"
                + "</appinfo>"
                + "</annotation>"
                , xsd, true);
    }
    /**
     * Test a group item.
     */
    public void testGroupItem() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.getChildren().add(new CobolDataItem("CHILD-NAME"));
        emitAnnotationAndCheck(dataItem,
        "<cb:cobolElement cobolName=\"COBOL-NAME\" levelNumber=\"1\" type=\"GROUP_ITEM\"/>");
    }

    /**
     * Test an elementary item with usage.
     */
    public void testUsage() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setUsage(Usage.DOUBLEFLOAT);
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " type=\"DOUBLE_FLOAT_ITEM\""
                + " usage=\"COMP-2\"/>");
    }

    /**
     * Test an elementary item with picture.
     */
    public void testPicture() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("NNNN/NN");
        dataItem.setUsage(Usage.NATIONAL);
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " picture=\"NNNN/NN\""
                + " type=\"NATIONAL_ITEM\""
                + " usage=\"NATIONAL\"/>");
    }

    /**
     * Test an elementary item with justified right.
     */
    public void testJustifiedRight() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X");
        dataItem.setJustifiedRight(true);
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " picture=\"X\""
                + " type=\"ALPHANUMERIC_ITEM\""
                + " justifiedRight=\"true\"/>");
    }

    /**
     * Test an elementary item with total digits.
     */
    public void testTotalDigits() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("99");
        dataItem.setUsage(Usage.PACKEDDECIMAL);
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " picture=\"99\""
                + " type=\"PACKED_DECIMAL_ITEM\""
                + " usage=\"PACKED-DECIMAL\""
                + " totalDigits=\"2\""
                + " signed=\"false\""
                + "/>");
    }

    /**
     * Test an elementary item with fraction digits.
     */
    public void testFractionDigits() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("S99V9");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " picture=\"S99V9\""
                + " type=\"ZONED_DECIMAL_ITEM\""
                + " totalDigits=\"3\""
                + " fractionDigits=\"1\""
                + " signed=\"true\""
                + "/>");
    }

    /**
     * Test an elementary item with sign leading.
     */
    public void testSignLeading() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("S99V9");
        dataItem.setSignLeading(true);
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " picture=\"S99V9\""
                + " type=\"ZONED_DECIMAL_ITEM\""
                + " totalDigits=\"3\""
                + " fractionDigits=\"1\""
                + " signed=\"true\""
                + " signLeading=\"true\""
                + "/>");
    }

    /**
     * Test an elementary item with sign separate.
     */
    public void testSignSeparate() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("S99V9");
        dataItem.setSignLeading(true);
        dataItem.setSignSeparate(true);
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " picture=\"S99V9\""
                + " type=\"ZONED_DECIMAL_ITEM\""
                + " totalDigits=\"3\""
                + " fractionDigits=\"1\""
                + " signed=\"true\""
                + " signLeading=\"true\""
                + " signSeparate=\"true\""
                + "/>");
    }

    /**
     * Test an array item.
     */
    public void testArray() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setMaxOccurs(3);
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " maxOccurs=\"3\""
                + " minOccurs=\"3\""
                + " type=\"GROUP_ITEM\""
                + "/>");
    }

    /**
     * Test an array item with depending on.
     */
    public void testArrayDependingOn() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setMinOccurs(0);
        dataItem.setMaxOccurs(1);
        dataItem.setDependingOn("COBOL-LEN");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " dependingOn=\"COBOL-LEN\""
                + " maxOccurs=\"1\""
                + " minOccurs=\"0\""
                + " type=\"GROUP_ITEM\""
                + "/>");
    }

    /**
     * Test an item redefining another.
     */
    public void testRedefines() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-REDEFINING");
        dataItem.setRedefines("COBOL-REDEFINED");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-REDEFINING\""
                + " levelNumber=\"1\""
                + " redefines=\"COBOL-REDEFINED\""
                + " type=\"GROUP_ITEM\""
                + "/>");
    }
    
    /**
     * Test an item with value ZERO.
     */
    public void testValueZERO() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.addValue("ZEROS");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " value=\"0\""
                + " type=\"GROUP_ITEM\""
                + "/>");
    }

    /**
     * Test an item with value SPACE.
     */
    public void testValueSPACE() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.addValue("SPACE");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " value=\"\""
                + " type=\"GROUP_ITEM\""
                + "/>");
    }

    /**
     * Test an item with value HIGH-VALUE.
     */
    public void testValueHIGHVALUE() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.addValue("HIGH-VALUES");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " picture=\"X(3)\""
                + " value=\"0xFFFFFF\""
                + " type=\"ALPHANUMERIC_ITEM\""
                + "/>");
    }

    /**
     * Test an item with value LOW-VALUE.
     */
    public void testValueLOWVALUE() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.addValue("LOW-VALUES");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " picture=\"X(3)\""
                + " value=\"0x000000\""
                + " type=\"ALPHANUMERIC_ITEM\""
                + "/>");
    }

    /**
     * Test an item with value LOW-VALUE.
     */
    public void testValueNULLS() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.addValue("NULLS");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " picture=\"X(3)\""
                + " value=\"0x000000\""
                + " type=\"ALPHANUMERIC_ITEM\""
                + "/>");
    }

    /**
     * Test an item with value QUOTES.
     */
    public void testValueQUOTES() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.addValue("QUOTES");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " picture=\"X(3)\""
                + " value=\"&quot;\""
                + " type=\"ALPHANUMERIC_ITEM\""
                + "/>");
    }
    /**
     * Test an item with value APOST.
     */
    public void testValueAPOST() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.addValue("apost");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " picture=\"X(3)\""
                + " value=\"&apos;\""
                + " type=\"ALPHANUMERIC_ITEM\""
                + "/>");
    }
    /**
     * Test an item with value ALL.
     */
    public void testValueALL() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.addValue("ALL");
        dataItem.addValue("A");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " picture=\"X(3)\""
                + " value=\"AAA\""
                + " type=\"ALPHANUMERIC_ITEM\""
                + "/>");
    }

    /**
     * Test an item with value ALL applied to a figurative constant.
     */
    public void testValueALLFigurative() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.addValue("ALL");
        dataItem.addValue("QUOTE");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " picture=\"X(3)\""
                + " value=\"&quot;&quot;&quot;\""
                + " type=\"ALPHANUMERIC_ITEM\""
                + "/>");
    }
    /**
     * Test an item with a numeric.
     */
    public void testValueNumeric() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setUsage(Usage.PACKEDDECIMAL);
        dataItem.addValue("-125.63");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " usage=\"PACKED-DECIMAL\""
                + " value=\"-125.63\""
                + " type=\"PACKED_DECIMAL_ITEM\""
                + "/>");
    }

    /**
     * Test an item with a alpha escaped.
     */
    public void testValueEscaped() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.addValue("<A>");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " picture=\"X(3)\""
                + " value=\"&lt;A&gt;\""
                + " type=\"ALPHANUMERIC_ITEM\""
                + "/>");
    }

    /**
     * Test an item with a value that is a delimited literal.
     */
    public void testValueDelimitedLiteral() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        dataItem.setPicture("X(3)");
        dataItem.addValue("\"ABD\"");
        emitAnnotationAndCheck(dataItem,
                "<cb:cobolElement cobolName=\"COBOL-NAME\""
                + " levelNumber=\"1\""
                + " picture=\"X(3)\""
                + " value=\"ABD\""
                + " type=\"ALPHANUMERIC_ITEM\""
                + "/>");
    }
    /**
     * Helper to check item level annotation emission.
     * Get item level annotations. We hook them directly in the schema because
     * that simplifies testing.
     * @param dataItem a COBOL data item
     * @param expected expected annotations as a string
     */
    private void emitAnnotationAndCheck(final CobolDataItem dataItem, final String expected) {
        Cob2XsdContext context = new Cob2XsdContext();
        context.setAddLegStarAnnotations(true);
        context.setJaxbPackageName("jaxb.package.name");
        XmlSchema xsd = getXmlSchema();
        XsdAnnotationEmitter emitter = new XsdAnnotationEmitter(xsd, context);
        XsdDataItem xsdDataItem = new XsdDataItem(
                dataItem, context, null, new ArrayList < String >());
        XmlSchemaAnnotation annotation = emitter.createLegStarAnnotation(xsdDataItem);
        xsd.setAnnotation(annotation);
        check(
                "<annotation>"
                + "<appinfo>"
                + expected
                + "</appinfo>"
                + "</annotation>"
                , xsd, true);
    }


}
