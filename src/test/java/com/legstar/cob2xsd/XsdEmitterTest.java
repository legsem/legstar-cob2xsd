package com.legstar.cob2xsd;

import junit.framework.TestCase;

/**
 * Test the CobolDataItemToXSD class.
 *
 */
public class XsdEmitterTest extends TestCase {
    
    /**
     * Test XML element name derived from COBOL name.
     */
    public void testFormatName() {
        
        assertEquals("", XsdEmitter.formatName("", true));
        assertEquals("A", XsdEmitter.formatName("A", true));
        assertEquals("Ab", XsdEmitter.formatName("AB", true));
        assertEquals("Ab9C", XsdEmitter.formatName("AB9C", true));
        assertEquals("Ab9Cd", XsdEmitter.formatName("AB9CD", true));
        assertEquals("Ab9CdE", XsdEmitter.formatName("AB9CD-E", true));
        assertEquals("Ab9CdEf", XsdEmitter.formatName("AB9CD-EF", true));
        assertEquals("ab9CdEf", XsdEmitter.formatName("AB9CD-EF", false));
        
    }

}
