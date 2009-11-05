package com.legstar.cob2xsd.xsd;

import com.legstar.cob2xsd.model.CobolDataItem;
import com.legstar.cob2xsd.model.CobolDataItem.Usage;
import com.legstar.cob2xsd.xsd.XsdDataItem;

import junit.framework.TestCase;

/**
 * Test the XSD mapping of elementary COBOL data items.
 *
 */
public class XsdDataItemTest extends TestCase {
    
    /**
     * See if COBOL USAGE clause is mapped correctly.
     */
    public void testSetFromUsage() {
        CobolDataItem dataItem = new CobolDataItem();

        dataItem.setUsage(Usage.BINARY);
        XsdDataItem mapper = new XsdDataItem(dataItem);
        assertEquals("BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("INTEGER", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.NATIVEBINARY);
        mapper = new XsdDataItem(dataItem);
        assertEquals("NATIVE_BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("INTEGER", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.SINGLEFLOAT);
        mapper = new XsdDataItem(dataItem);
        assertEquals("SINGLE_FLOAT_ITEM", mapper.getCobolType().toString());
        assertEquals("FLOAT", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.DOUBLEFLOAT);
        mapper = new XsdDataItem(dataItem);
        assertEquals("DOUBLE_FLOAT_ITEM", mapper.getCobolType().toString());
        assertEquals("DOUBLE", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.PACKEDDECIMAL);
        mapper = new XsdDataItem(dataItem);
        assertEquals("PACKED_DECIMAL_ITEM", mapper.getCobolType().toString());
        assertEquals("DECIMAL", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.INDEX);
        mapper = new XsdDataItem(dataItem);
        assertEquals("INDEX_ITEM", mapper.getCobolType().toString());
        assertEquals("HEXBINARY", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.POINTER);
        mapper = new XsdDataItem(dataItem);
        assertEquals("POINTER_ITEM", mapper.getCobolType().toString());
        assertEquals("HEXBINARY", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.PROCEDUREPOINTER);
        mapper = new XsdDataItem(dataItem);
        assertEquals("PROC_POINTER_ITEM", mapper.getCobolType().toString());
        assertEquals("HEXBINARY", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.FUNCTIONPOINTER);
        mapper = new XsdDataItem(dataItem);
        assertEquals("FUNC_POINTER_ITEM", mapper.getCobolType().toString());
        assertEquals("HEXBINARY", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.DISPLAY);
        mapper = new XsdDataItem(dataItem);
        assertEquals("ALPHANUMERIC_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.DISPLAY1);
        mapper = new XsdDataItem(dataItem);
        assertEquals("DBCS_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.NATIONAL);
        mapper = new XsdDataItem(dataItem);
        assertEquals("NATIONAL_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

    }
    
    /**
     * Test deriving type from picture clause.
     */
    public void testSetFromPicture() {
        CobolDataItem dataItem = new CobolDataItem();

        dataItem.setPicture("A");
        XsdDataItem mapper = new XsdDataItem(dataItem);
        assertEquals("ALPHABETIC_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("X");
        mapper = new XsdDataItem(dataItem);
        assertEquals("ALPHANUMERIC_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("X9");
        mapper = new XsdDataItem(dataItem);
        assertEquals("ALPHANUMERIC_EDITED_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("G");
        mapper = new XsdDataItem(dataItem);
        assertEquals("DBCS_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("N");
        mapper = new XsdDataItem(dataItem);
        assertEquals("NATIONAL_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("E");
        mapper = new XsdDataItem(dataItem);
        assertEquals("EXTERNAL_FLOATING_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("BZ0,+-CRDB$");
        mapper = new XsdDataItem(dataItem);
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
        XsdDataItem mapper = new XsdDataItem(dataItem);
        assertEquals("ZONED_DECIMAL_ITEM", mapper.getCobolType().toString());
        assertEquals("DECIMAL", mapper.getXsdType().toString());
        assertEquals(3, mapper.getTotalDigits());
        assertEquals(1, mapper.getFractionDigits());
        assertEquals("0", mapper.getMinInclusive());
        assertEquals("99.9", mapper.getMaxInclusive());

        dataItem.setPicture("S99.9");
        dataItem.setUsage(Usage.PACKEDDECIMAL);
        mapper = new XsdDataItem(dataItem);
        assertEquals("PACKED_DECIMAL_ITEM", mapper.getCobolType().toString());
        assertEquals("DECIMAL", mapper.getXsdType().toString());
        assertEquals(3, mapper.getTotalDigits());
        assertEquals(1, mapper.getFractionDigits());
        assertEquals("-99.9", mapper.getMinInclusive());
        assertEquals("99.9", mapper.getMaxInclusive());

        dataItem.setPicture("S999");
        dataItem.setUsage(Usage.BINARY);
        mapper = new XsdDataItem(dataItem);
        assertEquals("BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("SHORT", mapper.getXsdType().toString());
        assertEquals(3, mapper.getTotalDigits());
        assertEquals(0, mapper.getFractionDigits());
        assertEquals("-999", mapper.getMinInclusive());
        assertEquals("999", mapper.getMaxInclusive());

        dataItem.setPicture("999999999");
        dataItem.setUsage(Usage.NATIVEBINARY);
        mapper = new XsdDataItem(dataItem);
        assertEquals("NATIVE_BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("UINT", mapper.getXsdType().toString());
        assertEquals(9, mapper.getTotalDigits());
        assertEquals(0, mapper.getFractionDigits());
        assertEquals(null, mapper.getMinInclusive());
        assertEquals(null, mapper.getMaxInclusive());

        dataItem.setPicture("S99999999999");
        dataItem.setUsage(Usage.BINARY);
        mapper = new XsdDataItem(dataItem);
        assertEquals("BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("LONG", mapper.getXsdType().toString());
        assertEquals(11, mapper.getTotalDigits());
        assertEquals(0, mapper.getFractionDigits());
        assertEquals("-99999999999", mapper.getMinInclusive());
        assertEquals("99999999999", mapper.getMaxInclusive());
    }
    
}
