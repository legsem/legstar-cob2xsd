package com.legstar.cobol.utils;

import junit.framework.TestCase;

/**
 * Test the ValueUtil class.
 *
 */
public class ValueUtilTest extends TestCase {
    
    /**
     * Check that delimiters are stripped correctly.
     */
    public void testStripDelimiters() {
        assertEquals("no'delimiters", ValueUtil.stripDelimiters("no'delimiters"));
        assertEquals(" quote\"\"delimiter ", ValueUtil.stripDelimiters("\" quote\"\"delimiter \""));
        assertEquals(" quote''delimiter ", ValueUtil.stripDelimiters("' quote''delimiter '"));
    }
    
    
    /**
     * Test filling with characters.
     */
    public void testFill() {
        assertEquals("AAAAA", ValueUtil.fill(null, "A", 5));
        assertEquals("0xFFFFFFFFFF", ValueUtil.fill("0x", "FF", 5));
    }
    
    /**
     * Test resolving figurative constants.
     */
    public void testResolveFigurative() {
        assertEquals("A", ValueUtil.resolveFigurative("'A'", 5, true));
        assertEquals("9", ValueUtil.resolveFigurative("9", 5, true));
        assertEquals("0", ValueUtil.resolveFigurative("zero", 5, true));
        assertEquals(" ", ValueUtil.resolveFigurative("spaces", 5, true));
        assertEquals("\"", ValueUtil.resolveFigurative("quote", 5, true));
        assertEquals("'", ValueUtil.resolveFigurative("quote", 5, false));
        assertEquals("'", ValueUtil.resolveFigurative("apost", 5, true));
        assertEquals("0x0000000000", ValueUtil.resolveFigurative("LOW-VALUE", 5, true));
        assertEquals("0xFFFFFFFFFF", ValueUtil.resolveFigurative("HIGH-VALUE", 5, true));
        assertEquals("0x0000000000", ValueUtil.resolveFigurative("NULLS", 5, true));
        assertEquals("     ", ValueUtil.resolveFigurative("ALL SPACES", 5, true));
        assertEquals("00000", ValueUtil.resolveFigurative("ALL ZERO", 5, true));
        assertEquals("0x0000000000", ValueUtil.resolveFigurative("ALL LOW-VALUE", 5, true));
        assertEquals("AAAAA", ValueUtil.resolveFigurative("ALL 'A'", 5, true));
        assertEquals("ABABA", ValueUtil.resolveFigurative("ALL 'AB'", 5, true));
        
    }

}
