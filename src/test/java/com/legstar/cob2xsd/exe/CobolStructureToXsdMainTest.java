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
package com.legstar.cob2xsd.exe;

import junit.framework.TestCase;

import org.apache.commons.cli.Options;

import com.legstar.cob2xsd.Cob2XsdContext.CodeFormat;

/**
 * Test the standalone jar.
 * 
 */
public class CobolStructureToXsdMainTest extends TestCase {

    /**
     * Test without arguments.
     */
    public void testNoArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertTrue(main.collectOptions(options, null));
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with help argument.
     */
    public void testHelpArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertFalse(main.collectOptions(options, new String[] { "-h" }));
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with bad input file.
     */
    public void testWrongInputArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            main.collectOptions(options, new String[] { "-i nope" });
            fail();
        } catch (Exception e) {
            assertEquals(
                    "java.lang.IllegalArgumentException: Input file or folder nope not found",
                    e.toString());
        }
    }

    /**
     * Test with unsupported argument.
     */
    public void testUnsupportedArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            main.collectOptions(options, new String[] { "- #" });
        } catch (Exception e) {
            assertEquals(
                    "org.apache.commons.cli.UnrecognizedOptionException: Unrecognized option: - #",
                    e.toString());
        }
    }

    /**
     * Test with addLegStarAnnotations argument.
     */
    public void testAddLegStarAnnotationsArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertEquals(false, main.getContext().addLegStarAnnotations());
            assertTrue(main.collectOptions(options, new String[] { "-a" }));
            assertEquals(true, main.getContext().addLegStarAnnotations());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with cobolSourceFileEncoding argument.
     */
    public void testCobolSourceFileEncodingArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertEquals(null, main.getCobolSourceFileEncoding());
            assertTrue(main.collectOptions(options,
                    new String[] { "-c ISO-8859-1" }));
            assertEquals("ISO-8859-1", main.getCobolSourceFileEncoding());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with currencySymbol argument.
     */
    public void testCurrencySymbolArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertEquals("$", main.getContext().getCurrencySymbol());
            assertTrue(main.collectOptions(options, new String[] { "-w £" }));
            assertEquals("£", main.getContext().getCurrencySymbol());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with decimalPointIsComma argument.
     */
    public void testDecimalPointIsCommaArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertEquals(false, main.getContext().decimalPointIsComma());
            assertTrue(main.collectOptions(options, new String[] { "-d" }));
            assertEquals(true, main.getContext().decimalPointIsComma());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with xsdEncoding argument.
     */
    public void testXsdEncodingArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertEquals("UTF-8", main.getContext().getXsdEncoding());
            assertTrue(main.collectOptions(options,
                    new String[] { "-e ISO-8859-1" }));
            assertEquals("ISO-8859-1", main.getContext().getXsdEncoding());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with input argument.
     */
    public void testInputEncodingArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertEquals("cobol", main.getInput().getName());
            assertTrue(main.collectOptions(options,
                    new String[] { "-i target" }));
            assertEquals("target", main.getInput().getName());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with mapConditionsToFacets argument.
     */
    public void testMapConditionsToFacetsArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertEquals(false, main.getContext().mapConditionsToFacets());
            assertTrue(main.collectOptions(options, new String[] { "-m" }));
            assertEquals(true, main.getContext().mapConditionsToFacets());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with nameConflictPrependParentName argument.
     */
    public void testNameConflictPrependParentNameArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertEquals(false, main.getContext()
                    .nameConflictPrependParentName());
            assertTrue(main.collectOptions(options, new String[] { "-n" }));
            assertEquals(true, main.getContext()
                    .nameConflictPrependParentName());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with nSymbolDbcs argument.
     */
    public void testNSymbolDbcsArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertEquals(false, main.getContext().nSymbolDbcs());
            assertTrue(main.collectOptions(options, new String[] { "-z" }));
            assertEquals(true, main.getContext().nSymbolDbcs());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with output argument.
     */
    public void testOutputEncodingArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertEquals("schema", main.getOutput().getName());
            assertTrue(main.collectOptions(options,
                    new String[] { "-o target" }));
            assertEquals("target", main.getOutput().getName());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with quoteIsApost argument.
     */
    public void testQuoteIsApostArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertEquals(true, main.getContext().quoteIsQuote());
            assertTrue(main.collectOptions(options, new String[] { "-q" }));
            assertEquals(false, main.getContext().quoteIsQuote());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with targetNamespace argument.
     */
    public void testTargetNamespaceArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertNull(main.getContext().getTargetNamespace());
            assertTrue(main.collectOptions(options,
                    new String[] { "-t http://zombi.org" }));
            assertEquals("http://zombi.org", main.getContext()
                    .getTargetNamespace());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with elementNamesStartWithUppercase argument.
     */
    public void testElementNamesStartWithUppercaseArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertEquals(false, main.getContext()
                    .elementNamesStartWithUppercase());
            assertTrue(main.collectOptions(options, new String[] { "-u" }));
            assertEquals(true, main.getContext()
                    .elementNamesStartWithUppercase());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with customXsltFileName argument.
     */
    public void testCustomXsltFileNameArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertEquals(null, main.getContext().getCustomXsltFileName());
            assertTrue(main.collectOptions(options,
                    new String[] { "-x src/main/resources/xslt/custom.xsl" }));
            assertEquals("src/main/resources/xslt/custom.xsl", main
                    .getContext().getCustomXsltFileName());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with code format arguments.
     */
    public void testCodeFormatArgument() {
        try {
            CobolStructureToXsdMain main = new CobolStructureToXsdMain();
            Options options = main.createOptions();
            assertEquals(CodeFormat.FIXED_FORMAT, main.getContext()
                    .getCodeFormat());
            assertTrue(main.collectOptions(options, new String[] { "-f free" }));
            assertEquals(CodeFormat.FREE_FORMAT.toString(), main.getContext()
                    .getCodeFormat().toString());
            assertEquals(7, main.getContext().getStartColumn());
            assertEquals(72, main.getContext().getEndColumn());
            assertTrue(main.collectOptions(options, new String[] { "-l 1",
                    "-r 66" }));
            assertEquals(1, main.getContext().getStartColumn());
            assertEquals(66, main.getContext().getEndColumn());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }
}
