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

import com.legstar.cob2xsd.Cob2XsdModel.CodeFormat;

/**
 * Additional tests for Cob2Xsd. These are kept outside {@link Cob2XsdIOTest} to
 * keep things simple.
 * 
 */
public class Cob2XsdSpecialTest extends AbstractXsdTester {

    /**
     * Check input file validation.
     */
    public void testInputFileValidation() {
        try {
            Cob2Xsd cob2xsd = new Cob2Xsd();
            cob2xsd.checkCobolSourceFile(null);
            fail();
        } catch (Exception e) {
            assertEquals(
                    "java.io.IOException: You must provide a COBOL source file",
                    e.toString());
        }
        try {
            Cob2Xsd cob2xsd = new Cob2Xsd();
            cob2xsd.checkCobolSourceFile(new File("toto"));
            fail();
        } catch (Exception e) {
            assertEquals(
                    "java.io.IOException: COBOL source  file toto not found",
                    e.toString());
        }
    }

    /**
     * Check output file/folder validation.
     */
    public void testOutputFileValidation() {
        try {
            Cob2Xsd cob2xsd = new Cob2Xsd();
            cob2xsd.checkTarget(null);
            fail();
        } catch (Exception e) {
            assertEquals(
                    "java.io.IOException: You must provide a target directory or file",
                    e.toString());
        }
        try {
            Cob2Xsd cob2xsd = new Cob2Xsd();
            cob2xsd.checkTarget(new File("toto"));
            fail();
        } catch (Exception e) {
            assertEquals("java.io.IOException: Target folder toto not found",
                    e.toString());
        }
        try {
            Cob2Xsd cob2xsd = new Cob2Xsd();
            cob2xsd.checkTarget(new File("toto.xsd"));
        } catch (Exception e) {
            fail(e.toString());
        }
    }

    /**
     * Invoke COBOL structure to XML Schema snippet used in the documentation.
     */
    public void testSampleSnippet() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            model.setTargetNamespace("http://www.mycompany.com/test");
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd
                    .translate("       01 A.\n           02 B PIC X.");
            compare("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">"
                    + "    <complexType name=\"A\">"
                    + "        <sequence>"
                    + "            <element name=\"b\">"
                    + "                <simpleType>"
                    + "                    <restriction base=\"string\">"
                    + "                        <maxLength value=\"1\"/>"
                    + "                    </restriction>"
                    + "                </simpleType>"
                    + "            </element>"
                    + "        </sequence>"
                    + "    </complexType>"
                    + "    <element name=\"a\" type=\"tns:A\"/>" + "</schema>",
                    xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test the output encoding.
     */
    public void testOutputEncoding() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            model.setTargetNamespace("http://www.mycompany.com/test");
            model.setXsdEncoding("ISO-8859-1");
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd
                    .translate("       01 A.\n           02 B PIC X.");
            compare("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"no\"?>"
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">"
                    + "    <complexType name=\"A\">"
                    + "        <sequence>"
                    + "            <element name=\"b\">"
                    + "                <simpleType>"
                    + "                    <restriction base=\"string\">"
                    + "                        <maxLength value=\"1\"/>"
                    + "                    </restriction>"
                    + "                </simpleType>"
                    + "            </element>"
                    + "        </sequence>"
                    + "    </complexType>"
                    + "    <element name=\"a\" type=\"tns:A\"/>" + "</schema>",
                    xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test the output encoding with customization.
     */
    public void testOutputEncodingPlusCustomization() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            model.setTargetNamespace("http://www.mycompany.com/test");
            model.setXsdEncoding("ISO-8859-1");
            model.setAddLegStarAnnotations(true);
            model.setCustomXsltFileName("src/test/resources/xslt/alltypes.xsl");
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd
                    .translate("       01 A.\n           02 S-BINARY PIC X.");
            compare("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"no\"?>"

                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:cb=\"http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">"
                    + "    <complexType name=\"A\">"
                    + "        <sequence>"
                    + "            <element name=\"sBinary\">"
                    + "                <annotation>"
                    + "                    <appinfo>"
                    + "                        <cb:cobolElement cobolName=\"S-BINARY\" levelNumber=\"2\""
                    + " picture=\"X\" srceLine=\"2\" type=\"OCTET_STREAM_ITEM\"/>"
                    + "                    </appinfo>"
                    + "                </annotation>"
                    + "                <simpleType>"
                    + "                    <restriction base=\"hexBinary\">"
                    + "                        <maxLength value=\"1\"/>"
                    + "                    </restriction>"
                    + "                </simpleType>"
                    + "            </element>"
                    + "        </sequence>"
                    + "    </complexType>"
                    + "    <element name=\"a\" type=\"tns:A\">"
                    + "        <annotation>"
                    + "            <appinfo>"
                    + "                <cb:cobolElement cobolName=\"A\" levelNumber=\"1\""
                    + " srceLine=\"1\" type=\"GROUP_ITEM\"/>"
                    + "            </appinfo>"
                    + "        </annotation>"
                    + "    </element>" + "</schema>", xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test combinations of conditions and figurative constants.
     */
    public void testConditionsWithFigurativeConstants() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            model.setTargetNamespace("http://www.mycompany.com/test");
            model.setMapConditionsToFacets(true);
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd
                    .translate("       01 DFHCOMMAREA.\n"
                            + "          05 E-FIELD-1        PIC X(5).\n"
                            + "             88 ISEMPTY VALUE ALL SPACES.\n"
                            + "          05 E-FIELD-2        PIC X(5).\n"
                            + "             88 INRANGE VALUE ALL \"A\" THROUGH ALL \"C\".\n");
            compare("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">"
                    + "    <complexType name=\"Dfhcommarea\">"
                    + "        <sequence>"
                    + "            <element name=\"eField1\">"
                    + "                <simpleType>"
                    + "                    <restriction base=\"string\">"
                    + "                        <maxLength value=\"5\"/>"
                    + "                        <enumeration value=\"     \"/>"
                    + "                    </restriction>"
                    + "                </simpleType>"
                    + "            </element>"
                    + "            <element name=\"eField2\">"
                    + "                <simpleType>"
                    + "                    <restriction base=\"string\">"
                    + "                        <maxLength value=\"5\"/>"
                    + "                        <minInclusive value=\"AAAAA\"/>"
                    + "                        <maxInclusive value=\"CCCCC\"/>"
                    + "                    </restriction>"
                    + "                </simpleType>"
                    + "            </element>"
                    + "        </sequence>"
                    + "    </complexType>"
                    + "    <element name=\"dfhcommarea\" type=\"tns:Dfhcommarea\"/>"
                    + "</schema>", xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test identifiers starting with digit.
     */
    public void testIdentifierStartsWithDigit() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            model.setTargetNamespace("http://www.mycompany.com/test");
            model.setMapConditionsToFacets(true);
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd.translate("        01  5500-REC-01.\n"
                    + "          05 5500-REC-TYPE      PIC X(01).\n"
                    + "          05 5500-PLAN-NUM      PIC X(06).");
            compare("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">"
                    + "    <complexType name=\"C5500Rec01\">"
                    + "        <sequence>"
                    + "            <element name=\"c5500RecType\">"
                    + "                <simpleType>"
                    + "                    <restriction base=\"string\">"
                    + "                        <maxLength value=\"1\"/>"
                    + "                    </restriction>"
                    + "                </simpleType>"
                    + "            </element>"
                    + "            <element name=\"c5500PlanNum\">"
                    + "                <simpleType>"
                    + "                    <restriction base=\"string\">"
                    + "                        <maxLength value=\"6\"/>"
                    + "                    </restriction>"
                    + "                </simpleType>"
                    + "            </element>"
                    + "        </sequence>"
                    + "    </complexType>"
                    + "    <element name=\"c5500Rec01\" type=\"tns:C5500Rec01\"/>"
                    + "</schema>", xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test case where 2 primitive types with same name appear in 2 different
     * complex types.
     */
    public void testPrimitiveTypesNameConflict() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            model.setTargetNamespace("http://www.mycompany.com/test");
            model.setMapConditionsToFacets(true);
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd.translate("        01  REC-01.\n"
                    + "            05 REC-TYPE      PIC X(01).\n"
                    + "        01  REC-02.\n"
                    + "            05 REC-TYPE      PIC X(01).\n");
            compare("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">"
                    + "    <complexType name=\"Rec01\">"
                    + "        <sequence>"
                    + "            <element name=\"recType\">"
                    + "                <simpleType>"
                    + "                    <restriction base=\"string\">"
                    + "                        <maxLength value=\"1\"/>"
                    + "                    </restriction>"
                    + "                </simpleType>"
                    + "            </element>"
                    + "        </sequence>"
                    + "    </complexType>"
                    + "    <element name=\"rec01\" type=\"tns:Rec01\"/>"
                    + "    <complexType name=\"Rec02\">"
                    + "        <sequence>"
                    + "            <element name=\"recType\">"
                    + "                <simpleType>"
                    + "                    <restriction base=\"string\">"
                    + "                        <maxLength value=\"1\"/>"
                    + "                    </restriction>"
                    + "                </simpleType>"
                    + "            </element>"
                    + "        </sequence>"
                    + "    </complexType>"
                    + "    <element name=\"rec02\" type=\"tns:Rec02\"/>"
                    + "</schema>"

            , xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test case where 2 complex types with same name appear in 2 different
     * complex types.
     */
    public void testComplexTypesNameConflict() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            model.setTargetNamespace("http://www.mycompany.com/test");
            model.setMapConditionsToFacets(true);
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd.translate("        01  REC-01.\n"
                    + "            05 REC-TYPE.\n"
                    + "                10 FIELD1      PIC X(01).\n"
                    + "        01  REC-02.\n" + "            05 REC-TYPE.\n"
                    + "                10 FIELD2      PIC X(01).\n");
            compare("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">"
                    + "    <complexType name=\"Rec01\">"
                    + "        <sequence>"
                    + "            <element name=\"recType\" type=\"tns:RecType2\"/>"
                    + "        </sequence>"
                    + "    </complexType>"
                    + "    <complexType name=\"RecType2\">"
                    + "        <sequence>"
                    + "            <element name=\"field1\">"
                    + "                <simpleType>"
                    + "                    <restriction base=\"string\">"
                    + "                        <maxLength value=\"1\"/>"
                    + "                    </restriction>"
                    + "                </simpleType>"
                    + "            </element>"
                    + "        </sequence>"
                    + "    </complexType>"
                    + "    <element name=\"rec01\" type=\"tns:Rec01\"/>"
                    + "    <complexType name=\"Rec02\">"
                    + "        <sequence>"
                    + "            <element name=\"recType\" type=\"tns:RecType5\"/>"
                    + "        </sequence>"
                    + "    </complexType>"
                    + "    <complexType name=\"RecType5\">"
                    + "        <sequence>"
                    + "            <element name=\"field2\">"
                    + "                <simpleType>"
                    + "                    <restriction base=\"string\">"
                    + "                        <maxLength value=\"1\"/>"
                    + "                    </restriction>"
                    + "                </simpleType>"
                    + "            </element>"
                    + "        </sequence>"
                    + "    </complexType>"
                    + "    <element name=\"rec02\" type=\"tns:Rec02\"/>"
                    + "</schema>"

            , xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test case where a group item has a PICTURE attribute. This gives a COBOL
     * compilation issue but is often used by users to check the product
     * reactions so we'd better warn about it.
     */
    public void testGroupItemWithPictureClause() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            model.setTargetNamespace("http://www.mycompany.com/test");
            model.setMapConditionsToFacets(true);
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd.translate("        01  REC-01.\n"
                    + "            05 REC-TYPE      PIC X(01).\n"
                    + "                10 FIELD1      PIC X(01).\n");
            compare("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">"
                    + "    <complexType name=\"Rec01\">"
                    + "        <sequence>"
                    + "            <element name=\"recType\">"
                    + "                <simpleType>"
                    + "                    <restriction base=\"string\">"
                    + "                        <maxLength value=\"1\"/>"
                    + "                    </restriction>"
                    + "                </simpleType>"
                    + "            </element>"
                    + "        </sequence>"
                    + "    </complexType>"
                    + "    <element name=\"rec01\" type=\"tns:Rec01\"/>"
                    + "</schema>"

            , xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test a COBOL source that is not fixed.
     */
    public void testFreeFormat() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            model.setCodeFormat(CodeFormat.FREE_FORMAT);
            model.setTargetNamespace("http://www.mycompany.com/test");
            model.setMapConditionsToFacets(true);
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd
                    .translate("*\n"
                            + "01  WS71-HEADER.\n"
                            + "      05  WS71-HEADER-ID        PIC X(4)  VALUE '$HD$'.\n"
                            + "*    05  WS71-TRANS-DESC       PIC X(43) VALUE SPACES.\n"
                            + "      05  WS73-INVOICE-NO.\n"
                            + "*234567890123456789012345678901234567890123456789012345678901234567890123456789\n"
                            + "                                           07  WS73-INVOICE-PREF     PIC X(4).\n");
            compare("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">"
                    + "    <complexType name=\"Ws71Header\">"
                    + "        <sequence>"
                    + "            <element name=\"ws71HeaderId\">"
                    + "                <simpleType>"
                    + "                    <restriction base=\"string\">"
                    + "                        <maxLength value=\"4\"/>"
                    + "                    </restriction>"
                    + "                </simpleType>"
                    + "            </element>"
                    + "            <element name=\"ws73InvoiceNo\" type=\"tns:Ws73InvoiceNo\"/>"
                    + "        </sequence>"
                    + "    </complexType>"
                    + "    <complexType name=\"Ws73InvoiceNo\">"
                    + "        <sequence>"
                    + "            <element name=\"ws73InvoicePref\">"
                    + "                <simpleType>"
                    + "                    <restriction base=\"string\">"
                    + "                        <maxLength value=\"4\"/>"
                    + "                    </restriction>"
                    + "                </simpleType>"
                    + "            </element>"
                    + "        </sequence>"
                    + "    </complexType>"
                    + "    <element name=\"ws71Header\" type=\"tns:Ws71Header\"/>"
                    + "</schema>", xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test a no targetNamespace.
     */
    public void testNoTargetNamespace() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            model.setCodeFormat(CodeFormat.FREE_FORMAT);
            model.setMapConditionsToFacets(true);
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd
                    .translate("*\n"
                            + "01  WS71-HEADER.\n"
                            + "      05  WS71-HEADER-ID        PIC X(4)  VALUE '$HD$'.\n"
                            + "      05  WS73-INVOICE-NO.\n"
                            + "          07  WS73-INVOICE-PREF     PIC X(4).\n");
            compare("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
                    + "<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\""
                    + " elementFormDefault=\"unqualified\">"
                    + "    <xsd:complexType name=\"Ws71Header\">"
                    + "        <xsd:sequence>"
                    + "            <xsd:element name=\"ws71HeaderId\">"
                    + "                <xsd:simpleType>"
                    + "                    <xsd:restriction base=\"xsd:string\">"
                    + "                        <xsd:maxLength value=\"4\"/>"
                    + "                    </xsd:restriction>"
                    + "                </xsd:simpleType>"
                    + "            </xsd:element>"
                    + "            <xsd:element name=\"ws73InvoiceNo\" type=\"Ws73InvoiceNo\"/>"
                    + "        </xsd:sequence>"
                    + "    </xsd:complexType>"
                    + "    <xsd:complexType name=\"Ws73InvoiceNo\">"
                    + "        <xsd:sequence>"
                    + "            <xsd:element name=\"ws73InvoicePref\">"
                    + "                <xsd:simpleType>"
                    + "                    <xsd:restriction base=\"xsd:string\">"
                    + "                        <xsd:maxLength value=\"4\"/>"
                    + "                    </xsd:restriction>"
                    + "                </xsd:simpleType>"
                    + "            </xsd:element>"
                    + "        </xsd:sequence>"
                    + "    </xsd:complexType>"
                    + "    <xsd:element name=\"ws71Header\" type=\"Ws71Header\"/>"
                    + "</xsd:schema>", xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test for issue 40 Parsing error on RECORD CONTAINS 58 TO 183 CHARACTERS.
     */
    public void testRecordContainsTo() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            model.setCodeFormat(CodeFormat.FREE_FORMAT);
            model.setMapConditionsToFacets(true);
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd.translate("*\n" + "FD OUTPUT-FILE\n"
                    + "RECORDING MODE IS V\n" + "BLOCK CONTAINS 2 RECORDS\n"
                    + "RECORD CONTAINS 58 TO 183 CHARACTERS.\n"
                    + "COPY ABCD.01  CUSTOMER-DATA.\n"
                    + "  COPY EFG .  05 CUSTOMER-ID             PIC 9(6).\n");
            compare("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
                    + "<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\""
                    + " elementFormDefault=\"unqualified\">"
                    + "    <xsd:complexType name=\"CustomerData\">"
                    + "        <xsd:sequence>"
                    + "            <xsd:element name=\"customerId\">"
                    + "                <xsd:simpleType>"
                    + "                    <xsd:restriction base=\"xsd:unsignedInt\">"
                    + "                        <xsd:totalDigits value=\"6\"/>"
                    + "                    </xsd:restriction>"
                    + "                </xsd:simpleType>"
                    + "            </xsd:element>"
                    + "        </xsd:sequence>"
                    + "    </xsd:complexType>"
                    + "    <xsd:element name=\"customerData\" type=\"CustomerData\"/>"
                    + "</xsd:schema>", xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test for issue 51 USAGE clause should inherited.
     */
    public void testUsageInheritance() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            model.setCodeFormat(CodeFormat.FREE_FORMAT);
            model.setMapConditionsToFacets(true);
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd.translate("*\n"
                    + "01  A                         COMP-3.\n"
                    + "    05  B     PIC S9(13)V99.\n");
            compare("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
                    + "<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\""
                    + " elementFormDefault=\"unqualified\">"
                    + "    <xsd:complexType name=\"A\">"
                    + "        <xsd:sequence>"
                    + "            <xsd:element name=\"b\">"
                    + "                <xsd:simpleType>"
                    + "                    <xsd:restriction base=\"xsd:decimal\">"
                    + "                        <xsd:totalDigits value=\"15\"/>"
                    + "                        <xsd:fractionDigits value=\"2\"/>"
                    + "                    </xsd:restriction>"
                    + "                </xsd:simpleType>"
                    + "            </xsd:element>"
                    + "        </xsd:sequence>"
                    + "    </xsd:complexType>"
                    + "    <xsd:element name=\"a\" type=\"A\"/>"
                    + "</xsd:schema>", xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test for issue 48: Dates in DATE-WRITTEN directive are misinterpreted..
     */
    public void testDateWrittenIssue() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd.translate("*\n"
                    + "       ID DIVISION.\n"
                    + "       DATE-WRITTEN.              04 GENNAIO 2005.\n"
                    + "       DATA DIVISION.\n"
                    + "       01 A. 02  B     PIC S9(13)V99.\n");
            compare("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
                    + "<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\""
                    + " elementFormDefault=\"unqualified\">"
                    + "    <xsd:complexType name=\"A\">"
                    + "        <xsd:sequence>"
                    + "            <xsd:element name=\"b\">"
                    + "                <xsd:simpleType>"
                    + "                    <xsd:restriction base=\"xsd:decimal\">"
                    + "                        <xsd:totalDigits value=\"15\"/>"
                    + "                        <xsd:fractionDigits value=\"2\"/>"
                    + "                    </xsd:restriction>"
                    + "                </xsd:simpleType>"
                    + "            </xsd:element>"
                    + "        </xsd:sequence>"
                    + "    </xsd:complexType>"
                    + "    <xsd:element name=\"a\" type=\"A\"/>"
                    + "</xsd:schema>", xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test for Issue 49: Value strings containing delimiter are not parsed.
     */
    public void testValueStringWithDelimiter() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd
                    .translate("*\n"
                            + "       01 A. 02  B     PIC X(56) VALUE 'CONTO N. W '.\n");
            compare("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
                    + "<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\""
                    + " elementFormDefault=\"unqualified\">"
                    + "    <xsd:complexType name=\"A\">"
                    + "        <xsd:sequence>"
                    + "            <xsd:element name=\"b\">"
                    + "                <xsd:simpleType>"
                    + "                    <xsd:restriction base=\"xsd:string\">"
                    + "                        <xsd:maxLength value=\"56\"/>"
                    + "                    </xsd:restriction>"
                    + "                </xsd:simpleType>"
                    + "            </xsd:element>"
                    + "        </xsd:sequence>"
                    + "    </xsd:complexType>"
                    + "    <xsd:element name=\"a\" type=\"A\"/>"
                    + "</xsd:schema>", xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test for Issue 50: Must disambiguate siblings with same name
     */
    public void testMustDisambiguateSiblingsWithSameName() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd.translate("*\n"
                    + "       01 FILLER. 02  F PIC X. 02  F PIC X.\n");
            compare("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
                    + "<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\""
                    + " elementFormDefault=\"unqualified\">"
                    + "    <xsd:complexType name=\"Filler2\">"
                    + "        <xsd:sequence>"
                    + "            <xsd:element name=\"f0\">"
                    + "                <xsd:simpleType>"
                    + "                    <xsd:restriction base=\"xsd:string\">"
                    + "                        <xsd:maxLength value=\"1\"/>"
                    + "                    </xsd:restriction>"
                    + "                </xsd:simpleType>"
                    + "            </xsd:element>"
                    + "            <xsd:element name=\"f1\">"
                    + "                <xsd:simpleType>"
                    + "                    <xsd:restriction base=\"xsd:string\">"
                    + "                        <xsd:maxLength value=\"1\"/>"
                    + "                    </xsd:restriction>"
                    + "                </xsd:simpleType>"
                    + "            </xsd:element>"
                    + "        </xsd:sequence>"
                    + "    </xsd:complexType>"
                    + "    <xsd:element name=\"filler2\" type=\"Filler2\"/>"
                    + "</xsd:schema>", xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test for Issue 58: Must accept orphan items without root parent
     */
    public void testMustAcceptOrphans() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            model.setIgnoreOrphanPrimitiveElements(false);
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd
                    .translate("        10  A PIC S9(4) COMP.\n"
                            + "        10  B PIC S9(04) COMP.\n");
            compare("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
                    + "<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" elementFormDefault=\"unqualified\">"
                    + "    <xsd:element name=\"a\">"
                    + "        <xsd:simpleType>"
                    + "            <xsd:restriction base=\"xsd:short\">"
                    + "                <xsd:totalDigits value=\"4\"/>"
                    + "            </xsd:restriction>"
                    + "        </xsd:simpleType>"
                    + "    </xsd:element>"
                    + "    <xsd:element name=\"b\">"
                    + "        <xsd:simpleType>"
                    + "            <xsd:restriction base=\"xsd:short\">"
                    + "                <xsd:totalDigits value=\"4\"/>"
                    + "            </xsd:restriction>"
                    + "        </xsd:simpleType>"
                    + "    </xsd:element>"
                    + "</xsd:schema>", xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test for Issue 61: Level 88 translations containing VALUE THRU
     */
    public void testLevel88WithValueThru() {
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            model.setMapConditionsToFacets(true);
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd
                    .translate("        01  COMMAREA."
                            + "        05  PIB-SECURITY-CODE PIC 9(3) COMP-4.\n"
                            + "        88  PIB-TECH-USER               VALUE  1.\n"
                            + "        88  PIB-MASTER-USER             VALUE  1 THRU   9.\n"
                            + "        88  PIB-SYSTEM-USER             VALUE 10 THRU  19.\n");
            compare("<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" elementFormDefault=\"unqualified\">"
                    + "    <xsd:complexType name=\"Commarea\">"
                    + "        <xsd:sequence>"
                    + "            <xsd:element name=\"pibSecurityCode\">"
                    + "                <xsd:simpleType>"
                    + "                    <xsd:restriction base=\"xsd:unsignedShort\">"
                    + "                        <xsd:totalDigits value=\"3\"/>"
                    + "                        <xsd:enumeration value=\"1\"/>"
                    + "                        <xsd:minInclusive value=\"1\"/>"
                    + "                        <xsd:maxInclusive value=\"9\"/>"
                    + "                    </xsd:restriction>"
                    + "                </xsd:simpleType>"
                    + "            </xsd:element>"
                    + "        </xsd:sequence>"
                    + "    </xsd:complexType>"
                    + "    <xsd:element name=\"commarea\" type=\"Commarea\"/>"
                    + "</xsd:schema>", xmlSchema);
        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }
}
