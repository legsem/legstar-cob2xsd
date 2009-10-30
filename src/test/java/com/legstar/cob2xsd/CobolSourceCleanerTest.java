package com.legstar.cob2xsd;


import com.legstar.cob2xsd.CobolSourceCleaner.CleaningContext;

/**
 * Test the source cleaner class.
 *
 */
public class CobolSourceCleanerTest extends AbstractCob2XsdTester {

    /**
     * Check that special characters are removed.
     */
    public void testCleaningSequenceNumbers() {
        /*       0        1         2         3         4         5         6         7  */
        /*       123456789012345678901234567890123456789012345678901234567890123456789012*/
        assertEquals(
                "",
                CobolSourceCleaner.removeLineSequenceNumbering(
                ""));

        assertEquals(
                "",
                CobolSourceCleaner.removeLineSequenceNumbering(
                "123456"));

        assertEquals(
                "      *",
                CobolSourceCleaner.removeLineSequenceNumbering(
                "123456*"));

        assertEquals(
                "      *                                                              ABC",
                CobolSourceCleaner.removeLineSequenceNumbering(
                "123456*                                                              ABC123456"));
    }

    /**
     * Check that special characters are removed.
     */
    public void testCleaningSpecialChars() {
        assertEquals("one unwanted char here",
                CobolSourceCleaner.removeUnwantedCharacters(
                        "one unwanted char here" + (char) 0x1A));
        assertEquals("one unwanted char here and here",
                CobolSourceCleaner.removeUnwantedCharacters(
                        "one unwanted char here" + (char) 0x1A + " and here" + (char) 0x1A));
    }

    /**
     * Check that data descriptions are correctly identified.
     */
    public void testDataDescriptionDetection() {
        CleaningContext dataDescription = new CleaningContext();
        CobolSourceCleaner.setCleaningContext(" not a data description 01 . line", dataDescription);
        assertFalse(dataDescription.isDataDescriptionStarted());
        assertFalse(dataDescription.isDataDescriptionEnded());

        dataDescription = new CleaningContext();
        CobolSourceCleaner.setCleaningContext(" 01 a data description single line.", dataDescription);
        assertTrue(dataDescription.isDataDescriptionStarted());
        assertTrue(dataDescription.isDataDescriptionEnded());

        dataDescription = new CleaningContext();
        CobolSourceCleaner.setCleaningContext("      01 a data description single line.   ", dataDescription);
        assertTrue(dataDescription.isDataDescriptionStarted());
        assertTrue(dataDescription.isDataDescriptionEnded());

        dataDescription = new CleaningContext();
        CobolSourceCleaner.setCleaningContext("      01 a data description '..99' multiline line   ", dataDescription);
        assertTrue(dataDescription.isDataDescriptionStarted());
        assertFalse(dataDescription.isDataDescriptionEnded());
        CobolSourceCleaner.setCleaningContext("      01 a .. continuation   ", dataDescription);
        assertTrue(dataDescription.isDataDescriptionStarted());
        assertFalse(dataDescription.isDataDescriptionEnded());
        CobolSourceCleaner.setCleaningContext("      finally th end.   ", dataDescription);
        assertTrue(dataDescription.isDataDescriptionStarted());
        assertTrue(dataDescription.isDataDescriptionEnded());
    }

    /**
     * Test cleaning on a complete program source.
     */
    public void testCompleteCleaning() {
        
        cleanAndCheck(null, null);
        cleanAndCheck("", "");
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
}
