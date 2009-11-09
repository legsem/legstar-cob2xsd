package com.legstar.cob2xsd;

import java.util.ArrayList;
import java.util.List;

import com.legstar.cob2xsd.XsdDataItem.XsdType;
import com.legstar.cobol.model.CobolDataItem;
import com.legstar.cobol.model.CobolDataItem.DataEntryType;
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
        
        List < String > uniqueXsdTypeNames = new ArrayList < String >();
        assertEquals("", XsdDataItem.formatTypeName(new CobolDataItem(""), uniqueXsdTypeNames));
        assertEquals("A", XsdDataItem.formatTypeName(new CobolDataItem("A"), uniqueXsdTypeNames));
        assertEquals("Ab", XsdDataItem.formatTypeName(new CobolDataItem("AB"), uniqueXsdTypeNames));
        assertEquals("Ab9C", XsdDataItem.formatTypeName(new CobolDataItem("AB9C"), uniqueXsdTypeNames));
        assertEquals("Ab9Cd", XsdDataItem.formatTypeName(new CobolDataItem("AB9CD"), uniqueXsdTypeNames));
        assertEquals("Ab9CdE", XsdDataItem.formatTypeName(new CobolDataItem("AB9CD-E"), uniqueXsdTypeNames));
        assertEquals("Ab9CdEf", XsdDataItem.formatTypeName(new CobolDataItem("AB9CD-EF"), uniqueXsdTypeNames));
        
        /* Test name conflict resolution */
        CobolDataItem cobolDataItem = new CobolDataItem("AB9CD-EF");
        cobolDataItem.setSrceLine(18);
        assertEquals("Ab9CdEf18", XsdDataItem.formatTypeName(cobolDataItem, uniqueXsdTypeNames));
        
    }
    /**
     * Test group items.
     */
    public void testGroupItems() {
        CobolDataItem dataItem = new CobolDataItem();

        dataItem.setCobolName("COBOL-NAME");
        dataItem.getChildren().add(new CobolDataItem());
        XsdDataItem mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
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
        XsdDataItem mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("INTEGER", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.NATIVEBINARY);
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("NATIVE_BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("INTEGER", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.SINGLEFLOAT);
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("SINGLE_FLOAT_ITEM", mapper.getCobolType().toString());
        assertEquals("FLOAT", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.DOUBLEFLOAT);
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("DOUBLE_FLOAT_ITEM", mapper.getCobolType().toString());
        assertEquals("DOUBLE", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.PACKEDDECIMAL);
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("PACKED_DECIMAL_ITEM", mapper.getCobolType().toString());
        assertEquals("DECIMAL", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.INDEX);
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("INDEX_ITEM", mapper.getCobolType().toString());
        assertEquals("HEXBINARY", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.POINTER);
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("POINTER_ITEM", mapper.getCobolType().toString());
        assertEquals("HEXBINARY", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.PROCEDUREPOINTER);
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("PROC_POINTER_ITEM", mapper.getCobolType().toString());
        assertEquals("HEXBINARY", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.FUNCTIONPOINTER);
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("FUNC_POINTER_ITEM", mapper.getCobolType().toString());
        assertEquals("HEXBINARY", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.DISPLAY);
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("ALPHANUMERIC_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.DISPLAY1);
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("DBCS_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

        dataItem.setUsage(Usage.NATIONAL);
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("NATIONAL_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());

    }
    
    /**
     * Test deriving type from picture clause.
     */
    public void testSetFromPicture() {
        CobolDataItem dataItem = new CobolDataItem();

        dataItem.setPicture("A");
        XsdDataItem mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("ALPHABETIC_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("X");
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("ALPHANUMERIC_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("X9");
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("ALPHANUMERIC_EDITED_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("G");
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("DBCS_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("N");
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("NATIONAL_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("E");
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("EXTERNAL_FLOATING_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("BZ0,+-CRDB$");
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("NUMERIC_EDITED_ITEM", mapper.getCobolType().toString());
        assertEquals("STRING", mapper.getXsdType().toString());
        
        dataItem.setPicture("99");
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("ZONED_DECIMAL_ITEM", mapper.getCobolType().toString());
        assertEquals("USHORT", mapper.getXsdType().toString());
    }
    
    /**
     * Test setting numeric attributes from usage and picture.
     */
    public void testSetNumericAttributes() {
        CobolDataItem dataItem = new CobolDataItem();

        dataItem.setPicture("99.9");
        dataItem.setUsage(Usage.DISPLAY);
        XsdDataItem mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("ZONED_DECIMAL_ITEM", mapper.getCobolType().toString());
        assertEquals("DECIMAL", mapper.getXsdType().toString());
        assertEquals(3, mapper.getTotalDigits());
        assertEquals(1, mapper.getFractionDigits());
        assertEquals("99.9", mapper.getPicture());

        dataItem.setPicture("S99.9");
        dataItem.setUsage(Usage.PACKEDDECIMAL);
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("PACKED_DECIMAL_ITEM", mapper.getCobolType().toString());
        assertEquals("DECIMAL", mapper.getXsdType().toString());
        assertEquals(3, mapper.getTotalDigits());
        assertEquals(1, mapper.getFractionDigits());

        dataItem.setPicture("S999");
        dataItem.setUsage(Usage.BINARY);
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("SHORT", mapper.getXsdType().toString());
        assertEquals(3, mapper.getTotalDigits());
        assertEquals(0, mapper.getFractionDigits());

        dataItem.setPicture("999999999");
        dataItem.setUsage(Usage.NATIVEBINARY);
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("NATIVE_BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("UINT", mapper.getXsdType().toString());
        assertEquals(9, mapper.getTotalDigits());
        assertEquals(0, mapper.getFractionDigits());

        dataItem.setPicture("S99999999999");
        dataItem.setUsage(Usage.BINARY);
        mapper = new XsdDataItem(dataItem, _context, null, new ArrayList < String >());
        assertEquals("BINARY_ITEM", mapper.getCobolType().toString());
        assertEquals("LONG", mapper.getXsdType().toString());
        assertEquals(11, mapper.getTotalDigits());
        assertEquals(0, mapper.getFractionDigits());
    }
    
    /**
     * Test what happens when the ODOObject is not found.
     */
    public void testUpdateDependencyNoMatch() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        CobolDataItem child1 = new CobolDataItem("DEPENDON-NAME");
        child1.setDependingOn("ODO-OBJECT-NAME");
        
        dataItem.getChildren().add(child1);
        
        new XsdDataItem(
                dataItem, _context, null, new ArrayList < String >());
        
    }
    
    /**
     * Test with ODO object in direct parent.
     */
    public void testUpdateDependencyInParent() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-NAME");
        CobolDataItem child0 = new CobolDataItem("SIBLING-NAME");
        CobolDataItem child1 = new CobolDataItem("ODO-OBJECT-NAME");
        CobolDataItem child2 = new CobolDataItem("DEPENDON-NAME");
        child2.setDependingOn("ODO-OBJECT-NAME");
        
        dataItem.getChildren().add(child0);
        dataItem.getChildren().add(child1);
        dataItem.getChildren().add(child2);
        
        XsdDataItem xsdDataItem = new XsdDataItem(
                dataItem, _context, null, new ArrayList < String >());
        assertFalse(xsdDataItem.getChildren().get(0).isODOObject());
        assertTrue(xsdDataItem.getChildren().get(1).isODOObject());
        assertFalse(xsdDataItem.getChildren().get(2).isODOObject());
        
    }
    
    /**
     * Test with ODO object in ancestor.
     */
    public void testUpdateDependencyInAncestor() {
        CobolDataItem grandParent = new CobolDataItem("GRAND-PARENT-NAME");
        CobolDataItem odo = new CobolDataItem("ODO-OBJECT-NAME");
        grandParent.getChildren().add(odo);

        CobolDataItem parent = new CobolDataItem("PARENT-NAME");
        CobolDataItem dep = new CobolDataItem("DEPENDON-NAME");
        dep.setDependingOn("ODO-OBJECT-NAME");
        parent.getChildren().add(dep);

        grandParent.getChildren().add(parent);
        
        XsdDataItem xsdGrandParent = new XsdDataItem(
                grandParent, _context, null, new ArrayList < String >());
        
        assertTrue(xsdGrandParent.getChildren().get(0).isODOObject());
        
    }

    /**
     * Test with Redefined object in ancestor.
     */
    public void testUpdateRedefinitionInAncestor() {
        CobolDataItem grandParent = new CobolDataItem("GRAND-PARENT-NAME");
        CobolDataItem redefined = new CobolDataItem("REDEFINED-OBJECT-NAME");
        grandParent.getChildren().add(redefined);

        CobolDataItem parent = new CobolDataItem("PARENT-NAME");
        CobolDataItem redefining = new CobolDataItem("REDEFINING-NAME");
        redefining.setRedefines("REDEFINED-OBJECT-NAME");
        parent.getChildren().add(redefining);

        grandParent.getChildren().add(parent);
        
        XsdDataItem xsdGrandParent = new XsdDataItem(
                grandParent, _context, null, new ArrayList < String >());
        
        assertTrue(xsdGrandParent.getChildren().get(0).isRedefined());
        
    }

    /**
     * Test a RENAMES data entry.
     */
    public void testRenames() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-RENAME");
        dataItem.setDataEntryType(DataEntryType.RENAMES);
        
        XsdDataItem xsdDataItem = new XsdDataItem(
                dataItem, _context, null, new ArrayList < String >());
        assertTrue(xsdDataItem.getXsdType() == null);
        
    }
    
    /**
     * Test a CONDITION data entry.
     */
    public void testCondition() {
        CobolDataItem dataItem = new CobolDataItem("COBOL-RENAME");
        dataItem.setDataEntryType(DataEntryType.CONDITION);
        
        XsdDataItem xsdDataItem = new XsdDataItem(
                dataItem, _context, null, new ArrayList < String >());
        assertEquals(XsdType.ENUM, xsdDataItem.getXsdType());
        
    }
}
