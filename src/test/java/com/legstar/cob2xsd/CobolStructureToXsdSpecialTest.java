package com.legstar.cob2xsd;

import java.io.File;

import junit.framework.TestCase;

/**
 * Additional tests for CobolStructureToXsd. These are kept outside
 *  {@link CobolStructureToXsdTest} to keep things simple.
 *
 */
public class CobolStructureToXsdSpecialTest extends TestCase {

    /** Line separator (OS specific).*/
    public static final String LS = System.getProperty("line.separator");

    /**
     * Invoke COBOL structure to XML Schema snippet used in the documentation.
     */
    public void testSampleSnippet() {
        try {
            Cob2XsdContext context = new Cob2XsdContext();
            context.setTargetNamespace("http://www.mycompany.com/test");
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            String xmlSchema = cob2xsd.translate("       01 A.\n           02 B PIC X.");
            assertEquals(
                    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" + LS
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">" + LS
                    + "    <complexType name=\"A\">" + LS
                    + "        <sequence>" + LS
                    + "            <element name=\"b\">" + LS
                    + "                <simpleType>" + LS
                    + "                    <restriction base=\"string\">" + LS
                    + "                        <maxLength value=\"1\"/>" + LS
                    + "                    </restriction>" + LS
                    + "                </simpleType>" + LS
                    + "            </element>" + LS
                    + "        </sequence>" + LS
                    + "    </complexType>" + LS
                    + "</schema>" + LS
                    ,
                    xmlSchema);
        } catch (CobolStructureToXsdException e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test the output encoding.
     */
    public void testOutputEncoding() {
        try {
            Cob2XsdContext context = new Cob2XsdContext();
            context.setTargetNamespace("http://www.mycompany.com/test");
            context.setXsdEncoding("ISO-8859-1");
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            String xmlSchema = cob2xsd.translate("       01 A.\n           02 B PIC X.");
            assertEquals(
                    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"no\"?>" + LS
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">" + LS
                    + "    <complexType name=\"A\">" + LS
                    + "        <sequence>" + LS
                    + "            <element name=\"b\">" + LS
                    + "                <simpleType>" + LS
                    + "                    <restriction base=\"string\">" + LS
                    + "                        <maxLength value=\"1\"/>" + LS
                    + "                    </restriction>" + LS
                    + "                </simpleType>" + LS
                    + "            </element>" + LS
                    + "        </sequence>" + LS
                    + "    </complexType>" + LS
                    + "</schema>" + LS
                    ,
                    xmlSchema);
        } catch (CobolStructureToXsdException e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test the output encoding with customization.
     */
    public void testOutputEncodingPlusCustomization() {
        try {
            Cob2XsdContext context = new Cob2XsdContext();
            context.setTargetNamespace("http://www.mycompany.com/test");
            context.setXsdEncoding("ISO-8859-1");
            context.setAddLegStarAnnotations(true);
            File custmXslt = new File("src/test/resources/xslt", "alltypes.xsl");
            context.setCustomXslt(custmXslt);
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            String xmlSchema = cob2xsd.translate("       01 A.\n           02 S-BINARY PIC X.");
            assertEquals(
                    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"no\"?>" + LS
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:cb=\"http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd\""
                    + " xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " jaxb:extensionBindingPrefixes=\"cb\""
                    + " jaxb:version=\"2.0\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">" + LS
                    + "    <annotation>" + LS
                    + "        <appinfo>" + LS
                    + "            <jaxb:schemaBindings>" + LS
                    + "                <jaxb:package name=\"com.acme.test\"/>" + LS
                    + "            </jaxb:schemaBindings>" + LS
                    + "        </appinfo>" + LS
                    + "    </annotation>" + LS
                    + "    <complexType name=\"A\">" + LS
                    + "        <sequence>" + LS
                    + "            <element name=\"sBinary\">" + LS
                    + "                <annotation>" + LS
                    + "                    <appinfo>" + LS
                    + "                        <cb:cobolElement cobolName=\"S-BINARY\" levelNumber=\"2\""
                    + " picture=\"X\" srceLine=\"2\" type=\"OCTET_STREAM_ITEM\"/>" + LS
                    + "                    </appinfo>" + LS
                    + "                </annotation>" + LS
                    + "                <simpleType>" + LS
                    + "                    <restriction base=\"hexBinary\">" + LS
                    + "                        <maxLength value=\"1\"/>" + LS
                    + "                    </restriction>" + LS
                    + "                </simpleType>" + LS
                    + "            </element>" + LS
                    + "        </sequence>" + LS
                    + "    </complexType>" + LS
                    + "</schema>" + LS
                    ,
                    xmlSchema);
        } catch (CobolStructureToXsdException e) {
            e.printStackTrace();
            fail();
        }
    }
}
