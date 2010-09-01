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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import com.legstar.antlr.RecognizerException;
import com.legstar.cob2xsd.Cob2XsdContext.CodeFormat;

/**
 * Additional tests for CobolStructureToXsd. These are kept outside
 *  {@link CobolStructureToXsdTest} to keep things simple.
 *
 */
public class CobolStructureToXsdSpecialTest extends AbstractXsdTester {

    /** Line separator (OS specific).*/
    public static final String LS = System.getProperty("line.separator");

    /**
     * Check input file validation.
     */
    public void testInputFileValidation() {
        try {
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd();
            cob2xsd.checkCobolSourceFile(null);
            fail();
        } catch (Exception e) {
            assertEquals("java.io.IOException: You must provide a COBOL source file",
                    e.toString());
        }
        try {
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd();
            cob2xsd.checkCobolSourceFile(new File("toto"));
            fail();
        } catch (Exception e) {
            assertEquals("java.io.IOException: COBOL source  file toto not found",
                    e.toString());
        }
    }

    /**
     * Check output file/folder validation.
     */
    public void testOutputFileValidation() {
        try {
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd();
            cob2xsd.checkTarget(null);
            fail();
        } catch (Exception e) {
            assertEquals("java.io.IOException: You must provide a target directory or file",
                    e.toString());
        }
        try {
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd();
            cob2xsd.checkTarget(new File("toto"));
            fail();
        } catch (Exception e) {
            assertEquals("java.io.IOException: Target folder toto not found",
                    e.toString());
        }
        try {
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd();
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
        } catch (XsdGenerationException e) {
            e.printStackTrace();
            fail();
        } catch (RecognizerException e) {
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
        } catch (XsdGenerationException e) {
            e.printStackTrace();
            fail();
        } catch (RecognizerException e) {
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
            context.setCustomXsltFileName("src/test/resources/xslt/alltypes.xsl");
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            String xmlSchema = cob2xsd.translate("       01 A.\n           02 S-BINARY PIC X.");
            assertEquals(
                    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"no\"?>" + LS
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:cb=\"http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">" + LS
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
        } catch (XsdGenerationException e) {
            e.printStackTrace();
            fail();
        } catch (RecognizerException e) {
            e.printStackTrace();
            fail();
        }
    }
    
    /**
     * Check that the XML Schema produced has the correct encoding from a file
     *  standpoint.
     * Not using commons-io on purpose.
     */
    public void testFileOutputEncoding() {
        try {
            Cob2XsdContext context = new Cob2XsdContext();
            context.setTargetNamespace("http://www.mycompany.com/test");
            context.setXsdEncoding("UTF-8");
            context.setAddLegStarAnnotations(true);
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            File tempCobolFile = File.createTempFile("test", ".cob");
            tempCobolFile.deleteOnExit();
            BufferedWriter out = new BufferedWriter(
                    new OutputStreamWriter(
                            new FileOutputStream(tempCobolFile), "UTF8"));
            out.write("       01 A.\n           02 B PIC G(4) VALUE '牛年快乐'.");
            out.flush();
            out.close();
            File xmlSchema = cob2xsd.translate(
                    tempCobolFile, "UTF-8", GEN_XSD_DIR);
            BufferedReader in = new BufferedReader(
                    new InputStreamReader(
                            new FileInputStream(xmlSchema), "UTF8"));
            String line;
            while ((line = in.readLine()) != null) {
                if (line.contains("cb:cobolElement")) {
                    assertTrue(line.contains("value=\"牛年快乐\""));
                }
            }
            in.close();
            
        } catch (XsdGenerationException e) {
            e.printStackTrace();
            fail();
        } catch (IOException e) {
            e.printStackTrace();
            fail();
        } catch (RecognizerException e) {
            e.printStackTrace();
            fail();
        }
       
    }

    /**
     * Test combinations of conditions and figurative constants.
     */
    public void testConditionsWithFigurativeConstants() {
        try {
            Cob2XsdContext context = new Cob2XsdContext();
            context.setTargetNamespace("http://www.mycompany.com/test");
            context.setMapConditionsToFacets(true);
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            String xmlSchema = cob2xsd.translate(
                    "       01 DFHCOMMAREA.\n"
                    + "          05 E-FIELD-1        PIC X(5).\n"
                    + "             88 ISEMPTY VALUE ALL SPACES.\n"
                    + "          05 E-FIELD-2        PIC X(5).\n"
                    + "             88 INRANGE VALUE ALL \"A\" THROUGH ALL \"C\".\n"
                    );
            assertEquals(
                    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" + LS
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">" + LS
                    + "    <complexType name=\"Dfhcommarea\">" + LS
                    + "        <sequence>" + LS
                    + "            <element name=\"eField1\">" + LS
                    + "                <simpleType>" + LS
                    + "                    <restriction base=\"string\">" + LS
                    + "                        <maxLength value=\"5\"/>" + LS
                    + "                        <enumeration value=\"     \"/>" + LS
                    + "                    </restriction>" + LS
                    + "                </simpleType>" + LS
                    + "            </element>" + LS
                    + "            <element name=\"eField2\">" + LS
                    + "                <simpleType>" + LS
                    + "                    <restriction base=\"string\">" + LS
                    + "                        <maxLength value=\"5\"/>" + LS
                    + "                        <minInclusive value=\"AAAAA\"/>" + LS
                    + "                        <maxInclusive value=\"CCCCC\"/>" + LS
                    + "                    </restriction>" + LS
                    + "                </simpleType>" + LS
                    + "            </element>" + LS
                    + "        </sequence>" + LS
                    + "    </complexType>" + LS
                    + "</schema>" + LS
                    ,
                    xmlSchema);
        } catch (XsdGenerationException e) {
            e.printStackTrace();
            fail();
        } catch (RecognizerException e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test identifiers starting with digit.
     */
    public void testIdentifierStartsWithDigit() {
        try {
            Cob2XsdContext context = new Cob2XsdContext();
            context.setTargetNamespace("http://www.mycompany.com/test");
            context.setMapConditionsToFacets(true);
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            String xmlSchema = cob2xsd.translate(
                    "        01  5500-REC-01.\n"
                    + "          05 5500-REC-TYPE      PIC X(01).\n"
                    + "          05 5500-PLAN-NUM      PIC X(06)."
                    );
            assertEquals(
                    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" + LS
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">" + LS
                    + "    <complexType name=\"C5500Rec01\">" + LS
                    + "        <sequence>" + LS
                    + "            <element name=\"c5500RecType\">" + LS
                    + "                <simpleType>" + LS
                    + "                    <restriction base=\"string\">" + LS
                    + "                        <maxLength value=\"1\"/>" + LS
                    + "                    </restriction>" + LS
                    + "                </simpleType>" + LS
                    + "            </element>" + LS
                    + "            <element name=\"c5500PlanNum\">" + LS
                    + "                <simpleType>" + LS
                    + "                    <restriction base=\"string\">" + LS
                    + "                        <maxLength value=\"6\"/>" + LS
                    + "                    </restriction>" + LS
                    + "                </simpleType>" + LS
                    + "            </element>" + LS
                    + "        </sequence>" + LS
                    + "    </complexType>" + LS
                    + "</schema>" + LS
                    ,
                    xmlSchema);
        } catch (XsdGenerationException e) {
            e.printStackTrace();
            fail();
        } catch (RecognizerException e) {
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
            Cob2XsdContext context = new Cob2XsdContext();
            context.setTargetNamespace("http://www.mycompany.com/test");
            context.setMapConditionsToFacets(true);
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            String xmlSchema = cob2xsd.translate(
                    "        01  REC-01.\n"
                    + "            05 REC-TYPE      PIC X(01).\n"
                    + "        01  REC-02.\n"
                    + "            05 REC-TYPE      PIC X(01).\n"
                    );
            assertEquals(
                    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" + LS
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">" + LS
                    + "    <complexType name=\"Rec01\">" + LS
                    + "        <sequence>" + LS
                    + "            <element name=\"recType\">" + LS
                    + "                <simpleType>" + LS
                    + "                    <restriction base=\"string\">" + LS
                    + "                        <maxLength value=\"1\"/>" + LS
                    + "                    </restriction>" + LS
                    + "                </simpleType>" + LS
                    + "            </element>" + LS
                    + "        </sequence>" + LS
                    + "    </complexType>" + LS
                    + "    <complexType name=\"Rec02\">" + LS
                    + "        <sequence>" + LS
                    + "            <element name=\"recType\">" + LS
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
        } catch (RecognizerException e) {
            e.printStackTrace();
            fail();
        } catch (XsdGenerationException e) {
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
            Cob2XsdContext context = new Cob2XsdContext();
            context.setTargetNamespace("http://www.mycompany.com/test");
            context.setMapConditionsToFacets(true);
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            String xmlSchema = cob2xsd.translate(
                    "        01  REC-01.\n"
                    + "            05 REC-TYPE.\n"
                    + "                10 FIELD1      PIC X(01).\n"
                    + "        01  REC-02.\n"
                    + "            05 REC-TYPE.\n"
                    + "                10 FIELD2      PIC X(01).\n"
                    );
            assertEquals(
                    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" + LS
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">" + LS
                    + "    <complexType name=\"Rec01\">" + LS
                    + "        <sequence>" + LS
                    + "            <element name=\"recType\" type=\"tns:RecType2\"/>" + LS
                    + "        </sequence>" + LS
                    + "    </complexType>" + LS
                    + "    <complexType name=\"RecType2\">" + LS
                    + "        <sequence>" + LS
                    + "            <element name=\"field1\">" + LS
                    + "                <simpleType>" + LS
                    + "                    <restriction base=\"string\">" + LS
                    + "                        <maxLength value=\"1\"/>" + LS
                    + "                    </restriction>" + LS
                    + "                </simpleType>" + LS
                    + "            </element>" + LS
                    + "        </sequence>" + LS
                    + "    </complexType>" + LS
                    + "    <complexType name=\"Rec02\">" + LS
                    + "        <sequence>" + LS
                    + "            <element name=\"recType\" type=\"tns:RecType5\"/>" + LS
                    + "        </sequence>" + LS
                    + "    </complexType>" + LS
                    + "    <complexType name=\"RecType5\">" + LS
                    + "        <sequence>" + LS
                    + "            <element name=\"field2\">" + LS
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
        } catch (RecognizerException e) {
            e.printStackTrace();
            fail();
        } catch (XsdGenerationException e) {
            e.printStackTrace();
            fail();
        }
    }

    /**
     * Test case where a group item has a PICTURE attribute. This gives a COBOL
     * compilation issue but is often used by users to check the product reactions
     * so we'd better warn about it.
     */
    public void testGroupItemWithPictureClause() {
        try {
            Cob2XsdContext context = new Cob2XsdContext();
            context.setTargetNamespace("http://www.mycompany.com/test");
            context.setMapConditionsToFacets(true);
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            String xmlSchema = cob2xsd.translate(
                    "        01  REC-01.\n"
                    + "            05 REC-TYPE      PIC X(01).\n"
                    + "                10 FIELD1      PIC X(01).\n"
                    );
            assertEquals(
                    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" + LS
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">" + LS
                    + "    <complexType name=\"Rec01\">" + LS
                    + "        <sequence>" + LS
                    + "            <element name=\"recType\">" + LS
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
        } catch (RecognizerException e) {
            e.printStackTrace();
            fail();
        } catch (XsdGenerationException e) {
            e.printStackTrace();
            fail();
        }
    }
    /**
     * Test a COBOL source that is not fixed.
     */
    public void testFreeFormat() {
        try {
            Cob2XsdContext context = new Cob2XsdContext();
            context.setCodeFormat(CodeFormat.FREE_FORMAT);
            context.setTargetNamespace("http://www.mycompany.com/test");
            context.setMapConditionsToFacets(true);
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            String xmlSchema = cob2xsd.translate(
                    "*\n"
                    + "01  WS71-HEADER.\n"
                    + "      05  WS71-HEADER-ID        PIC X(4)  VALUE '$HD$'.\n"
                    + "*    05  WS71-TRANS-DESC       PIC X(43) VALUE SPACES.\n"
                    + "      05  WS73-INVOICE-NO.\n"
                    + "*234567890123456789012345678901234567890123456789012345678901234567890123456789\n"
                    + "                                           07  WS73-INVOICE-PREF     PIC X(4).\n"
                    );
            assertEquals(
                    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" + LS
                    + "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                    + " xmlns:tns=\"http://www.mycompany.com/test\""
                    + " elementFormDefault=\"qualified\""
                    + " targetNamespace=\"http://www.mycompany.com/test\">" + LS
                    + "    <complexType name=\"Ws71Header\">" + LS
                    + "        <sequence>" + LS
                    + "            <element name=\"ws71HeaderId\">" + LS
                    + "                <simpleType>" + LS
                    + "                    <restriction base=\"string\">" + LS
                    + "                        <maxLength value=\"4\"/>" + LS
                    + "                    </restriction>" + LS
                    + "                </simpleType>" + LS
                    + "            </element>" + LS
                    + "            <element name=\"ws73InvoiceNo\" type=\"tns:Ws73InvoiceNo\"/>" + LS
                    + "        </sequence>" + LS
                    + "    </complexType>" + LS
                    + "    <complexType name=\"Ws73InvoiceNo\">" + LS
                    + "        <sequence>" + LS
                    + "            <element name=\"ws73InvoicePref\">" + LS
                    + "                <simpleType>" + LS
                    + "                    <restriction base=\"string\">" + LS
                    + "                        <maxLength value=\"4\"/>" + LS
                    + "                    </restriction>" + LS
                    + "                </simpleType>" + LS
                    + "            </element>" + LS
                    + "        </sequence>" + LS
                    + "    </complexType>" + LS
                    + "</schema>" + LS
                    ,
                    xmlSchema);
        } catch (RecognizerException e) {
            e.printStackTrace();
            fail();
        } catch (XsdGenerationException e) {
            e.printStackTrace();
            fail();
        }
    }
}
