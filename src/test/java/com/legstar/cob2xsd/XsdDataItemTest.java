package com.legstar.cob2xsd;

import com.legstar.cobol.model.CobolDataItem;
import com.legstar.cobol.model.CobolDataItem.Usage;

import junit.framework.TestCase;

/**
 * Test the XSD mapping of elementary COBOL data items.
 *
 */
public class XsdDataItemTest extends TestCase {
    
    /** Translator options. */
    private Cob2XsdContext _context = new Cob2XsdContext();
    
    /**
     * Test XML element name derived from COBOL name.
     */
    public void testFormatName() {
        
        assertEquals("", XsdDataItem.formatTypeName(""));
        assertEquals("A", XsdDataItem.formatTypeName("A"));
        assertEquals("Ab", XsdDataItem.formatTypeName("AB"));
        assertEquals("Ab9C", XsdDataItem.formatTypeName("AB9C"));
        assertEquals("Ab9Cd", XsdDataItem.formatTypeName("AB9CD"));
        assertEquals("Ab9CdE", XsdDataItem.formatTypeName("AB9CD-E"));
        assertEquals("Ab9CdEf", XsdDataItem.formatTypeName("AB9CD-EF"));
        
    }
    /**
     * Test group items.
     */
    public void testGroupItems() {
        CobolDataItem dataItem = new CobolDataItem();

        dataItem.setCobolName("COBOL-NAME");
        dataItem.getChildren().add(new CobolDataItem());
        XsdDataItem mapper = new XsdDataItem(dataItem, _context);
        assertEquals("GROUP_ITEM", mapper.getCobolType().toString());
        assertEquals("COMPLEX", mapper.getXsdType().toString());
        assertEquals("CobolName", mapper.getXsdTypeName());
        assertEquals("cobolName", mapper.getXsdElementName());
    }

    /**
     * See if COBOL USAGE clause is mapped correctly.
     */
    public void testSetFromUsage() {
        CobolDataItem dataItem = new CobolDataItem();

        dataItem.setUsage(Usage.BINARY);
        XsdDataItem mapper = new XsdDataItem(dataItem, _context);
        assertEquals("BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("INTEGER", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.NATIVEBINARY);
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("NATIVE_BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("INTEGER", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.SINGLEFLOAT);
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("SINGLE_FLOAT_ITEM", mapper.getCobolType().toString());
        assertEquals("FLOAT", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.DOUBLEFLOAT);
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("DOUBLE_FLOAT_ITEM", mapper.getCobolType().toString());
        assertEquals("DOUBLE", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.PACKEDDECIMAL);
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("PACKED_DECIMAL_ITEM", mapper.getCobolType().toString());
        assertEquals("DECIMAL", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.INDEX);
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("INDEX_ITEM", mapper.getCobolType().toString());
        assertEquals("HEXBINARY", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.POINTER);
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("POINTER_ITEM", mapper.getCobolType().toString());
        assertEquals("HEXBINARY", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.PROCEDUREPOINTER);
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("PROC_POINTER_ITEM", mapper.getCobolType().toString());
        assertEquals("HEXBINARY", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.FUNCTIONPOINTER);
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("FUNC_POINTER_ITEM", mapper.getCobolType().toString());
        assertEquals("HEXBINARY", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.DISPLAY);
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("ALPHANUMERIC_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.DISPLAY1);
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("DBCS_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.NATIONAL);
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("NATIONAL_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

    }
    
    /**
     * Test deriving type from picture clause.
     */
    public void testSetFromPicture() {
        CobolDataItem dataItem = new CobolDataItem();

        dataItem.setPicture("A");
        XsdDataItem mapper = new XsdDataItem(dataItem, _context);
        assertEquals("ALPHABETIC_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("X");
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("ALPHANUMERIC_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("X9");
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("ALPHANUMERIC_EDITED_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("G");
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("DBCS_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("N");
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("NATIONAL_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("E");
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("EXTERNAL_FLOATING_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("BZ0,+-CRDB$");
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("NUMERIC_EDITED_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
    }
    
    /**
     * Test setting numeric attributes from usage and picture.
     */
    public void testSetNumericAttributes() {
        CobolDataItem dataItem = new CobolDataItem();

        dataItem.setPicture("99.9");
        dataItem.setUsage(Usage.DISPLAY);
        XsdDataItem mapper = new XsdDataItem(dataItem, _context);
        assertEquals("ZONED_DECIMAL_ITEM", mapper.getCobolType().toString());
        assertEquals("DECIMAL", mapper.getXsdType().toString());
        assertEquals(3, mapper.getTotalDigits());
        assertEquals(1, mapper.getFractionDigits());
        assertEquals("0", mapper.getMinInclusive());
        assertEquals("99.9", mapper.getMaxInclusive());

        dataItem.setPicture("S99.9");
        dataItem.setUsage(Usage.PACKEDDECIMAL);
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("PACKED_DECIMAL_ITEM", mapper.getCobolType().toString());
        assertEquals("DECIMAL", mapper.getXsdType().toString());
        assertEquals(3, mapper.getTotalDigits());
        assertEquals(1, mapper.getFractionDigits());
        assertEquals("-99.9", mapper.getMinInclusive());
        assertEquals("99.9", mapper.getMaxInclusive());

        dataItem.setPicture("S999");
        dataItem.setUsage(Usage.BINARY);
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("SHORT", mapper.getXsdType().toString());
        assertEquals(3, mapper.getTotalDigits());
        assertEquals(0, mapper.getFractionDigits());
        assertEquals("-999", mapper.getMinInclusive());
        assertEquals("999", mapper.getMaxInclusive());

        dataItem.setPicture("999999999");
        dataItem.setUsage(Usage.NATIVEBINARY);
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("NATIVE_BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("UINT", mapper.getXsdType().toString());
        assertEquals(9, mapper.getTotalDigits());
        assertEquals(0, mapper.getFractionDigits());
        assertEquals(null, mapper.getMinInclusive());
        assertEquals(null, mapper.getMaxInclusive());

        dataItem.setPicture("S99999999999");
        dataItem.setUsage(Usage.BINARY);
        mapper = new XsdDataItem(dataItem, _context);
        assertEquals("BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("LONG", mapper.getXsdType().toString());
        assertEquals(11, mapper.getTotalDigits());
        assertEquals(0, mapper.getFractionDigits());
        assertEquals("-99999999999", mapper.getMinInclusive());
        assertEquals("99999999999", mapper.getMaxInclusive());
    }
    
}
