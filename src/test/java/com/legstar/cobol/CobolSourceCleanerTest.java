package com.legstar.cobol;

import com.legstar.antlr.RecognizerException;



/**
 * Test the source cleaner class.
 *
 */
public class CobolSourceCleanerTest extends AbstractCobolTester {

    
    /**
     * Null input case.
     */
    public void testNull() {
        try {
            clean(null);
            fail();
        } catch (RecognizerException e) {
            assertEquals("COBOL source was null", e.getMessage());
        }
    }
    
    /**
     * Empty input case.
     */
    public void testEmpty() {
        try {
            clean("");
            fail();
        } catch (RecognizerException e) {
            assertEquals("No data descriptions between columns 7 and 72."
                    + " Are you sure this is COBOL source?", e.getMessage());
        }
    }
    
    /**
     * Check that special characters are removed.
     */
    public void testCleaningSequenceNumbers() {
        /*       0        1         2         3         4         5         6         7  */
        /*       123456789012345678901234567890123456789012345678901234567890123456789012*/
        assertEquals(
                "",
                CobolSourceCleaner.cleanLine(
                "", 7, 72));

        assertEquals(
                "",
                CobolSourceCleaner.cleanLine(
                "123456", 7, 72));

        assertEquals(
                "01 A.",
                CobolSourceCleaner.cleanLine(
                "01 A.", 1, 66));

        assertEquals(
                "      -",
                CobolSourceCleaner.cleanLine(
                "123456-", 7, 72));

        assertEquals(
                "      -                                                              ABC",
                CobolSourceCleaner.cleanLine(
                "123456-                                                              ABC123456", 7, 72));
    }
    
    /**
     * Check that long separators are trimmed.
     */
    public void testCleanLongSeparators() {

        assertEquals(
                "       01 A  PIC  X(5)  VALUE  5.",
                CobolSourceCleaner.cleanLine(
                        "123456 01 A, PIC; X(5), VALUE, 5.", 7, 72));
    }

    /**
     * Test that DATA DIVISION is properly delineated.
     */
    public void testDataDivision() {
        
        CobolSourceCleaner cleaner = new CobolSourceCleaner(getErrorHandler());
        CobolSourceCleaner.CleaningContext context = new CobolSourceCleaner.CleaningContext();
        assertTrue(cleaner.isDataDivision("", 7, context));
        assertFalse(cleaner.isDataDivision(" PROCEDURE DIVISION", 7, context));
        assertFalse(cleaner.isDataDivision("whatever", 7, context));
        context = new CobolSourceCleaner.CleaningContext();
        assertFalse(cleaner.isDataDivision("PROCEDURE DIVISION", 1, context));
        assertEquals("Procedure division found. The rest of the source code will be ignored.",
                getErrorHandler().getErrorMessages().get(0));
    }
    
    /**
     * Check that we correctly identify end of statements.
     */
    public void testEndStatementDetection() {
        
        CobolSourceCleaner.CleaningContext context = new CobolSourceCleaner.CleaningContext();

        removeExtraneousCharactersAndCheck(" 01 A PIC 9.9.", " 01 A PIC 9.9.", context);
        assertTrue(context.isLookingForLevel());
        removeExtraneousCharactersAndCheck(" 01 A PIC X. ", " 01 A PIC X. ", context);
        assertTrue(context.isLookingForLevel());
    }

    /**
     * Check cleaning of simple lines.
     */
    public void testCleanSimpleLine() {
        
        CobolSourceCleaner.CleaningContext context = new CobolSourceCleaner.CleaningContext();

        removeExtraneousCharactersAndCheck("", "", context);
        removeExtraneousCharactersAndCheck(" 01.", " 01.", context);
        assertTrue(context.isLookingForLevel());
        removeExtraneousCharactersAndCheck(" 01", " 01", context);
        assertFalse(context.isLookingForLevel());
        removeExtraneousCharactersAndCheck(" .", " .", context);
        assertTrue(context.isLookingForLevel());
        removeExtraneousCharactersAndCheck(" 01 A.", " 01 A.", context);
        assertTrue(context.isLookingForLevel());
        removeExtraneousCharactersAndCheck(" 01   A.", " 01   A.", context);
        assertTrue(context.isLookingForLevel());
        removeExtraneousCharactersAndCheck(" 01   A  .", " 01   A  .", context);
        assertTrue(context.isLookingForLevel());
        removeExtraneousCharactersAndCheck("blabla", "", context);
        assertEquals("Extraneous characters ignored: blabla", getErrorHandler().getErrorMessages().get(0));
        assertTrue(context.isLookingForLevel());
        removeExtraneousCharactersAndCheck("       01  FILEA.   COPY DFH0CFIL.", "       01  FILEA. ", context);
        assertEquals("Extraneous characters ignored:   COPY DFH0CFIL.", getErrorHandler().getErrorMessages().get(1));
        assertTrue(context.isLookingForLevel());
    }
    
    /**
     * Test cleaning of multi statement line.
     */
    public void testMultiStatementLine() {
        
        CobolSourceCleaner.CleaningContext context = new CobolSourceCleaner.CleaningContext();

        removeExtraneousCharactersAndCheck(" 01 A. 02 B.", " 01 A. 02 B.", context);
        removeExtraneousCharactersAndCheck(" 01 A. 02 B.03 C.", " 01 A. 02 B.03 C.", context);
        /* Extraneous characters past closed statement should be wiped out*/
        removeExtraneousCharactersAndCheck(" 01 A. 02 B. blabla 03 C.", " 01 A. 02 B.        03 C.", context);
        removeExtraneousCharactersAndCheck(" 01. 02 B.", " 01. 02 B.", context);

    }

    /**
     * Test cleaning of multi line stements.
     */
    public void testMultiLineStatement() {
        
        CobolSourceCleaner.CleaningContext context = new CobolSourceCleaner.CleaningContext();

        removeExtraneousCharactersAndCheck(" 01 A", " 01 A", context);
        assertFalse(context.isLookingForLevel());
        /* Whatever is within the statement should remain untouched */
        removeExtraneousCharactersAndCheck("blabla", "blabla", context);
        assertFalse(context.isLookingForLevel());
        /* Extraneous characters past the closing statement should be wiped out */
        removeExtraneousCharactersAndCheck(". blabla", ". ", context);
        assertTrue(context.isLookingForLevel());

    }

    /**
     * Helper method.
     * @param line line of code
     * @param expected expected cleaneup result
     * @param context cleaning context
     */
    private void removeExtraneousCharactersAndCheck(
            final String line,
            final String expected,
            final CobolSourceCleaner.CleaningContext context) {
        CobolSourceCleaner cleaner = new CobolSourceCleaner(getErrorHandler());
        assertEquals(expected, cleaner.removeExtraneousCharacters(line, context));
    }

    /**
     * Test cleaning on a complete program source.
     */
    public void testCompleteCleaning() {
        
        /*         0        1         2         3         4         5         6         7  */
        /*         123456789012345678901234567890123456789012345678901234567890123456789012*/
        cleanAndCheck(
                ""
                + "123456 PROCESS XOPTS(APOST)" + LS
                + "       IDENTIFICATION DIVISION." + LS
                + "       PROGRAM-ID. LSFILEAE." + LS
                + "      * OVERVIEW                                                      *" + LS
                + "COPY 'JOE' REPLACING ==A== BY ==B==." + LS
                + "123456 01 DFHCOMMAREA.                                                  123456" + LS
                + "EJECT" + LS
                + "          05 COM-NUMBER         PIC 9(6)." + LS
                + "          05 COM-DATE" + LS
                + "" + LS
                + "             PIC X(8)." + LS
                + "          05 COM-AMOUNT         PIC X(8)." + LS
                + "          05 A value" + LS
                + "                     \"AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEE" + LS
                + "      -              \"GGGGGGGGGGHHHHHHHHHHIIIIIIIIIIJJJJJJJJJJKKKKKKKKKK" + LS
                + "      -              \"LLLLLLLLLLMMMMMMMMMM\"." + LS
                + "       PROCEDURE DIVISION." + LS
                + "          MOVE" + LS
                + "             05 TO B." + LS
                ,
                ""
                + "" + LS
                + "" + LS
                + "" + LS
                + "" + LS
                + "" + LS
                + "       01 DFHCOMMAREA." + LS
                + "" + LS
                + "          05 COM-NUMBER         PIC 9(6)." + LS
                + "          05 COM-DATE" + LS
                + "" + LS
                + "             PIC X(8)." + LS
                + "          05 COM-AMOUNT         PIC X(8)." + LS
                + "          05 A value" + LS
                + "                     \"AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEE" + LS
                + "      -              \"GGGGGGGGGGHHHHHHHHHHIIIIIIIIIIJJJJJJJJJJKKKKKKKKKK" + LS
                + "      -              \"LLLLLLLLLLMMMMMMMMMM\"." + LS
                + "" + LS
                + "" + LS
                + "" + LS
        );
    }
    
    /**
     * Test effect of cleaning on source code starting at column1.
     */
    public void testCodeStartingColumnOne() {
        cleanAndCheck(
                "01   PO-RECORD1." + LS
                + "     05 RECORD-TYPE  PIC 9 VALUE 1."
                ,
                "" + LS
                + "      5 RECORD-TYPE  PIC 9 VALUE 1." + LS
                );
        
    }
    /**
     * Test removal of comments even containing valid data descriptions.
     */
    public void testCommentWithValidDataDescription() {
        cleanAndCheck(
                ""
                + "        01   PO-RECORD1." + LS
                + "      * 01   PO-RECORD2." + LS
                + "      / 01   PO-RECORD3."
                ,
                "        01   PO-RECORD1." + LS
                + "" + LS
                + "" + LS
                );
        
    }
}
