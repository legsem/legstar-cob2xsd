package com.legstar.cobol.utils;

import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.legstar.cobol.model.PictureSymbol;

import junit.framework.TestCase;

/**
 * Test the pPictureUtil class.
 *
 */
public class PictureUtilTest extends TestCase {

    /**
     * Test character length calculation based on picture clause.
     */
    public void testCalcLengthFromPicture() {
        assertEquals(0, getLengthFromPicture("", false));
        assertEquals(1, getLengthFromPicture("A", false));
        assertEquals(2, getLengthFromPicture("AB", false));
        assertEquals(3, getLengthFromPicture("ABE", false));
        assertEquals(4, getLengthFromPicture("ABEG", false));
        assertEquals(5, getLengthFromPicture("ABEGN", false));
        assertEquals(5, getLengthFromPicture("ABEGNP", false));
        assertEquals(5, getLengthFromPicture("ABEGNPS", false));
        assertEquals(5, getLengthFromPicture("ABEGNPSV", false));
        assertEquals(6, getLengthFromPicture("ABEGNPSVX", false));
        assertEquals(7, getLengthFromPicture("ABEGNPSVXZ", false));
        assertEquals(8, getLengthFromPicture("ABEGNPSVXZ9", false));
        assertEquals(9, getLengthFromPicture("ABEGNPSVXZ90", false));
        assertEquals(10, getLengthFromPicture("ABEGNPSVXZ90/", false));
        assertEquals(11, getLengthFromPicture("ABEGNPSVXZ90/,", false));
        assertEquals(12, getLengthFromPicture("ABEGNPSVXZ90/,.", false));
        assertEquals(13, getLengthFromPicture("ABEGNPSVXZ90/,.+", false));
        assertEquals(14, getLengthFromPicture("ABEGNPSVXZ90/,.+-", false));
        assertEquals(16, getLengthFromPicture("ABEGNPSVXZ90/,.+-CR", false));
        assertEquals(18, getLengthFromPicture("ABEGNPSVXZ90/,.+-CRDB", false));
        assertEquals(19, getLengthFromPicture("ABEGNPSVXZ90/,.+-CRDB*", false));
        assertEquals(20, getLengthFromPicture("ABEGNPSVXZ90/,.+-CRDB*$", false));
        assertEquals(21, getLengthFromPicture("ABEGNPSVXZ90/,.+-CRDB*$", true));
        
    }
    
    /**
     * Check that characters are correctly counted in picture strings.
     */
    public void testGetPictureCharOccurences() {
        assertEquals(0,
                getPictureCharOccurences('Z', ""));
        assertEquals(1,
                getPictureCharOccurences('Z', "Z"));
        assertEquals(1,
                getPictureCharOccurences('Z', "Z9"));
        assertEquals(2,
                getPictureCharOccurences('Z', "ZZ"));
        assertEquals(2,
                getPictureCharOccurences('Z', "ZZ(1)"));
        assertEquals(2,
                getPictureCharOccurences('Z', "ZZ( 1 )"));
        assertEquals(3,
                getPictureCharOccurences('Z', "ZZ(2)"));
        assertEquals(4,
                getPictureCharOccurences('Z', "ZZ(2)Z"));
        assertEquals(26,
                getPictureCharOccurences('Z', "ZZ(2)Z(23)"));
        assertEquals(27,
                getPictureCharOccurences('Z', "zz(2)z9/z(23)"));
        assertEquals(1,
                getPictureCharOccurences('D', "zz(2)z9/z(23)DB"));
        assertEquals(0,
                getPictureCharOccurences('B', "zz(2)z9/z(23)DB"));
        
    }
    
    /**
     * Test the picture parsing.
     */
    public void testParsePicture() {
        assertEquals("[]", getParsedPicture(""));
        assertEquals("[{symbol:Z,occurs:1}]", getParsedPicture("Z"));
        assertEquals("[{symbol:Z,occurs:2}]", getParsedPicture("ZZ"));
        assertEquals("[{symbol:Z,occurs:4}]", getParsedPicture("ZZ(3)"));
        assertEquals("[{symbol:Z,occurs:4}]", getParsedPicture("ZZ( 3 )"));
        assertEquals("[{symbol:Z,occurs:4}, {symbol:C,occurs:1}]", getParsedPicture("ZZ(3)CR"));
        assertEquals("[{symbol:Z,occurs:4}, {symbol:D,occurs:1}, {symbol:Z,occurs:2}]",
                getParsedPicture("ZZ(3)DBZ(2)"));
        
    }
    
    /**
     * Test the regex generation from a picture clause.
     */
    public void testRegexFromPicture() {
        assertTrue(checkRegexFromPicture("A", "a"));
        assertFalse(checkRegexFromPicture("A", "9"));
        assertFalse(checkRegexFromPicture("A", "ab"));
        assertTrue(checkRegexFromPicture("A(2)", "a "));
        assertFalse(checkRegexFromPicture("A(2)", "a b"));
        assertTrue(checkRegexFromPicture("X(5)", null));
        assertTrue(checkRegexFromPicture("S999PPP", "-1000"));
        assertTrue(checkRegexFromPicture("$9(5).9(2)CR", "$1000.99CR"));
        assertTrue(checkRegexFromPicture("$9(5).9(2)DB", "$100.9  "));
        assertTrue(checkRegexFromPicture("+,+++,999.99", "-123,456.78"));
        assertTrue(checkRegexFromPicture("$$$9.99", "$0.12"));
        assertFalse(checkRegexFromPicture("$$$9.99", "$0.12A"));
        assertTrue(checkRegexFromPicture("Z(3)", "123"));
        assertTrue(checkRegexFromPicture("Z,ZZZ.ZZ+", "123.45+"));
        assertTrue(checkRegexFromPicture("****.99", "****.00"));
        assertTrue(checkRegexFromPicture("90/9.99", "50/6.12"));
    }
    
    /**
     * Helper to calculate character length from a picture clause.
     * @param picture picture clause
     * @param isSignSeparate true if sign is separate
     * @return return the character length derived from picture
     */
    private int getLengthFromPicture(final String picture, final boolean isSignSeparate) {
        Map < Character, Integer > charNum =
            PictureUtil.getPictureCharOccurences(picture, '$');
        return PictureUtil.calcLengthFromPicture(charNum, isSignSeparate, '$');
    }

    /**
     * Helper to count a single character occurrences in a string.
     * @param character the character to look for
     * @param picture the picture string
     * @return the number of occurrences
     */
    private int getPictureCharOccurences(final char character, final String picture) {
        Map < Character, Integer > charNum =
            PictureUtil.getPictureCharOccurences(picture, '$');
        return charNum.get(character);
    }
    
    /**
     * Helper to parse a picture string and return the result.
     * @param picture picture string
     * @return the stringified list of picture symbols and number of occurrences
     */
    private String getParsedPicture(final String picture) {
        List < PictureSymbol > result = PictureUtil.parsePicture(picture, '$');
        return result.toString();
    }
    
    /**
     * Turns a picture into a regex and then checks that the test value matches.
     * A special case is when regex returned is null (meaning no restriction) in
     * this case the testValue must be null.
     * @param picture the COBOL picture clause
     * @param testValue the test value
     * @return true if there is a match
     */
    private boolean checkRegexFromPicture(final String picture, final String testValue) {
        String regex = PictureUtil.getRegexFromPicture(picture, '$');
        if (regex != null) {
            Pattern pattern = Pattern.compile(regex);
            Matcher matcher = pattern.matcher(testValue);
            return matcher.find();
        } else {
            return (testValue == null);
        }
    }
    
}
