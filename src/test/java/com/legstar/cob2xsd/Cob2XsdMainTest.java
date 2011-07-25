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

import junit.framework.TestCase;

import org.apache.commons.cli.Options;
import org.apache.commons.io.FileUtils;

import com.legstar.cob2xsd.Cob2XsdMain;

/**
 * Test the executable jar.
 * 
 */
public class Cob2XsdMainTest extends TestCase {

    /**
     * Test without arguments.
     */
    public void testNoArgument() {
        try {
            Cob2XsdMain main = new Cob2XsdMain();
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
            Cob2XsdMain main = new Cob2XsdMain();
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
            Cob2XsdMain main = new Cob2XsdMain();
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
            Cob2XsdMain main = new Cob2XsdMain();
            Options options = main.createOptions();
            main.collectOptions(options, new String[] { "- #" });
        } catch (Exception e) {
            assertEquals(
                    "org.apache.commons.cli.UnrecognizedOptionException: Unrecognized option: - #",
                    e.toString());
        }
    }

    /**
     * Test with configuration argument.
     */
    public void testConfigurationArgument() {
        Cob2XsdMain main = new Cob2XsdMain();
        try {
            main.execute(new String[] { "-c",
                    "src/main/resources/conf/cob2xsd.properties", "-i",
                    "src/test/resources/cobol/LSFILEAE", "-o",
                    "target/gen/myfile.xsd" });
            assertTrue(new File("target/gen/myfile.xsd").exists());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with append base file name option.
     */
    public void testAppendBaseFileNameToNamespace() {
        Cob2XsdMain main = new Cob2XsdMain();
        try {
            main.execute(new String[] { "-c",
                    "src/main/resources/conf/cob2xsd.properties", "-i",
                    "src/test/resources/cobol/LSFILEAE", "-o",
                    "target/gen/myfile.xsd", "-a" });
            File result = new File("target/gen/myfile.xsd");
            assertTrue(result.exists());
            assertTrue(FileUtils.readFileToString(result).contains(
                    "xmlns:tns=\"http://legstar.com/test/coxb/lsfileae\""));
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }
}
