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

import com.legstar.antlr.RecognizerException;

import junit.framework.TestCase;

/**
 * Check how various types of errors are handled at the API level.
 *
 */
public class CobolStructureToXsdErrorHandlingTest extends TestCase {
    
    /**
     * Cleaning might get errors.
     */
    public void testCleanerErrors() {
        try {
            Cob2XsdContext context = new Cob2XsdContext();
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            cob2xsd.translate("^ù@");
        } catch (XsdGenerationException e) {
            fail();
        } catch (RecognizerException e) {
            assertEquals("No data descriptions found. Are you sure this is COBOL source?",
                    e.getMessage());
        }
    }

    /**
     * Lexing might get errors but they are all recovered from.
     */
    public void testLexerErrors() {
        try {
            Cob2XsdContext context = new Cob2XsdContext();
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            cob2xsd.translate("       1 ^ù@.");
            assertEquals("line 1:12 Syntax error in last COBOL clause", cob2xsd.getErrorHistory().get(0));
        } catch (XsdGenerationException e) {
            fail();
        } catch (RecognizerException e) {
            fail();
        }
    }

    /**
     * Parsing might get UnwantedTokenException.
     */
    public void testParserUnwantedTokenException() {
        try {
            Cob2XsdContext context = new Cob2XsdContext();
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            cob2xsd.translate("       01 01.");
        } catch (XsdGenerationException e) {
            fail();
        } catch (RecognizerException e) {
            assertEquals("Parsing failed. 1 syntax errors."
                    + " Last error was line 1:10 unexpected token '01' expecting PERIOD",
                    e.getMessage());
        }
    }

    /**
     * Parsing might get MismatchedTokenException.
     */
    public void testParserMismatchedTokenException() {
        try {
            Cob2XsdContext context = new Cob2XsdContext();
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            cob2xsd.translate("       88 A PIC X.");
        } catch (XsdGenerationException e) {
            fail();
        } catch (RecognizerException e) {
            assertEquals("Parsing failed. 1 syntax errors."
                    + " Last error was line 1:12 unexpected token 'PIC' expecting VALUE_KEYWORD",
                    e.getMessage());
        }
    }

    /**
     * Parsing might get MismatchedTokenException and reaches end of file.
     */
    public void testParserMismatchedTokenExceptionEOF() {
        try {
            Cob2XsdContext context = new Cob2XsdContext();
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            cob2xsd.translate("       01 A PIC X");
        } catch (XsdGenerationException e) {
            fail();
        } catch (RecognizerException e) {
            assertEquals("Parsing failed. 1 syntax errors."
                    + " Last error was line 0:-1 reached end of file looking for PERIOD",
                    e.getMessage());
        }
    }

    /**
     * Parsing might get EarlyExitException.
     */
    public void testParserEarlyExitException() {
        try {
            Cob2XsdContext context = new Cob2XsdContext();
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            cob2xsd.translate("       01 A PIC.");
        } catch (XsdGenerationException e) {
            fail();
        } catch (RecognizerException e) {
            assertEquals("Parsing failed. 1 syntax errors."
                    + " Last error was line 1:15 required tokens not found at input '.'",
                    e.getMessage());
        }
    }

}
