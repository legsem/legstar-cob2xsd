package com.legstar.cob2xsd;

import java.io.File;

import org.apache.commons.io.FileUtils;

/**
 * Check recognition on a complete program source.
 *
 */
public class CompleteProgramsParserTest extends AbstractCob2XsdTester {
    
    /** Location of COBOL samples.*/
    private File sampleFolder = new File("src/test/resources/cobol");
    
    /**
     * Try the LSFILEAE sample.
     * @throws Exception if test fails
     */
    public void testLsfileae() throws Exception {
        parseAndCheck(
                FileUtils.readFileToString(new File(sampleFolder, "LSFILEAE"))
                , "(DATA_ITEM (LEVEL 01) (NAME FILEA)"
                + " (DATA_ITEM (LEVEL 77) (NAME RESPONSE) (PICTURE S9(8))))"
                + " (DATA_ITEM (LEVEL 01) (NAME DFHCOMMAREA)"
                + " (DATA_ITEM (LEVEL 05) (NAME COM-NUMBER) (PICTURE 9(6)))"
                + " (DATA_ITEM (LEVEL 05) (NAME COM-PERSONAL)"
                + " (DATA_ITEM (LEVEL 10) (NAME COM-NAME) (PICTURE X(20)))"
                + " (DATA_ITEM (LEVEL 10) (NAME COM-ADDRESS) (PICTURE X(20)))"
                + " (DATA_ITEM (LEVEL 10) (NAME COM-PHONE) (PICTURE X(8))))"
                + " (DATA_ITEM (LEVEL 05) (NAME COM-DATE) (PICTURE X(8)))"
                + " (DATA_ITEM (LEVEL 05) (NAME COM-AMOUNT) (PICTURE X(8)))"
                + " (DATA_ITEM (LEVEL 05) (NAME COM-COMMENT) (PICTURE X(9))))"
        );
    }

    /**
     * Try the DPLARCHT sample.
     * @throws Exception if test fails
     */
    public void testDplarcht() throws Exception {
        parseAndCheck(
                FileUtils.readFileToString(new File(sampleFolder, "DPLARCHT"))
                , "(DATA_ITEM (LEVEL 01) (NAME FILEA)"
                + " (DATA_ITEM (LEVEL 77) (NAME RESPONSE) (PICTURE S9(8))))"
                + " (DATA_ITEM (LEVEL 01) (NAME DFHCOMMAREA)"
                + " (DATA_ITEM (LEVEL 05) (NAME COM-NUMBER) (PICTURE 9(6)))"
                + " (DATA_ITEM (LEVEL 05) (NAME COM-PERSONAL)"
                + " (DATA_ITEM (LEVEL 10) (NAME COM-NAME) (PICTURE X(20)))"
                + " (DATA_ITEM (LEVEL 10) (NAME COM-ADDRESS) (PICTURE X(20)))"
                + " (DATA_ITEM (LEVEL 10) (NAME COM-PHONE) (PICTURE X(8))))"
                + " (DATA_ITEM (LEVEL 05) (NAME COM-DATE) (PICTURE X(8)))"
                + " (DATA_ITEM (LEVEL 05) (NAME COM-AMOUNT) (PICTURE X(8)))"
                + " (DATA_ITEM (LEVEL 05) (NAME COM-COMMENT) (PICTURE X(9))))"
        );
    }

}
