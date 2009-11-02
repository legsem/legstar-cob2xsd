package com.legstar.cob2xsd;

/**
 * Test the tree walker emitter. The tree walker builds a data entry model.
 *
 */
public class CobolStructureXsdEmitterTest extends AbstractCob2XsdTester {

    /**
     * A hierarchy.
     */
    public void testSimpleHierarchy() {
        emitAndCheck(
                "       01 A." + LS + "       05 B."
                , 
        "[{level:1,cobolName:A,children:[{level:5,cobolName:B,srceLine:2}],srceLine:1}]");
    }

    /**
     * Redefines clause.
     */
    public void testRedefines() {
        emitAndCheck(
                "      01 B REDEFINES A."
                , 
        "[{level:1,cobolName:B,redefines:A,srceLine:1}]");
    }

    /**
     * Blank when zero clause.
     */
    public void testBlankWhenZero() {
        emitAndCheck(
                "      01 A BLANK WHEN ZERO."
                , 
                "[{level:1,cobolName:A,isBlankWhenZero:true,srceLine:1}]");
    }

    /**
     * External clause.
     */
    public void testExternal() {
        emitAndCheck(
                "      01 A EXTERNAL."
                , 
                "[{level:1,cobolName:A,isExternal:true,srceLine:1}]");
    }

    /**
     * Global clause.
     */
    public void testGlobal() {
        emitAndCheck(
                "      01 A GLOBAL."
                , 
                "[{level:1,cobolName:A,isGlobal:true,srceLine:1}]");
    }

    /**
     * Global clause.
     */
    public void testGroupUsageNational() {
        emitAndCheck(
                "      01 A GROUP-USAGE NATIONAL."
                , 
                "[{level:1,cobolName:A,groupUsageNational:true,srceLine:1}]");
    }

    /**
     * Justified right clause.
     */
    public void testJustifiedRight() {
        emitAndCheck(
                "      01 A JUST RIGHT."
                , 
                "[{level:1,cobolName:A,isJustifiedRight:true,srceLine:1}]");
    }

    /**
     * Sign clause.
     */
    public void testSign() {
        emitAndCheck(
                "      01 A SIGN IS LEADING."
                , 
                "[{level:1,cobolName:A,isSign:true,isSignLeading:true,isSignSeparate:false,srceLine:1}]");
        emitAndCheck(
                "      01 A SIGN TRAILING SEPARATE."
                , 
                "[{level:1,cobolName:A,isSign:true,isSignLeading:false,isSignSeparate:true,srceLine:1}]");
    }
 
    /**
     * Synchronized clause.
     */
    public void testSynchronized() {
        emitAndCheck(
                "      01 A SYNC."
                , 
                "[{level:1,cobolName:A,isSynchronized:true,srceLine:1}]");
    }

    /**
     * Usage clause.
     */
    public void testUsage() {
        emitAndCheck(
                "      01 A USAGE IS COMP."
                , 
                "[{level:1,cobolName:A,usage:BINARY,srceLine:1}]");

        emitAndCheck(
                "      01 A USAGE COMPUTATIONAL-1."
                , 
                "[{level:1,cobolName:A,usage:SINGLEFLOAT,srceLine:1}]");

        emitAndCheck(
                "      01 A COMP-2."
                , 
                "[{level:1,cobolName:A,usage:DOUBLEFLOAT,srceLine:1}]");

        emitAndCheck(
                "      01 A COMP-3."
                , 
                "[{level:1,cobolName:A,usage:PACKEDDECIMAL,srceLine:1}]");

        emitAndCheck(
                "      01 A COMP-5."
                , 
                "[{level:1,cobolName:A,usage:NATIVEBINARY,srceLine:1}]");

        emitAndCheck(
                "      01 A USAGE DISPLAY."
                , 
                "[{level:1,cobolName:A,usage:DISPLAY,srceLine:1}]");

        emitAndCheck(
                "      01 A DISPLAY-1."
                , 
                "[{level:1,cobolName:A,usage:DISPLAY1,srceLine:1}]");

        emitAndCheck(
                "      01 A INDEX."
                , 
                "[{level:1,cobolName:A,usage:INDEX,srceLine:1}]");

        emitAndCheck(
                "      01 A NATIONAL."
                , 
                "[{level:1,cobolName:A,usage:NATIONAL,srceLine:1}]");

        emitAndCheck(
                "      01 A POINTER."
                , 
                "[{level:1,cobolName:A,usage:POINTER,srceLine:1}]");

        emitAndCheck(
                "      01 A PROCEDURE-POINTER."
                , 
                "[{level:1,cobolName:A,usage:PROCEDUREPOINTER,srceLine:1}]");

        emitAndCheck(
                "      01 A FUNCTION-POINTER."
                , 
                "[{level:1,cobolName:A,usage:FUNCTIONPOINTER,srceLine:1}]");
    }

    /**
     * Value clause.
     */
    public void testValue() {
        emitAndCheck(
                "      01 A VALUE +0.99E-02."
                , 
                "[{level:1,cobolName:A,values:[+0.99E-02],srceLine:1}]");
        
        emitAndCheck(
                "      01 A VALUE -95.25."
                , 
                "[{level:1,cobolName:A,values:[-95.25],srceLine:1}]");
        
        emitAndCheck(
                "      01 A VALUE 19."
                , 
                "[{level:1,cobolName:A,values:[19],srceLine:1}]");
        
        emitAndCheck(
                "      01 A VALUE -25."
                , 
                "[{level:1,cobolName:A,values:[-25],srceLine:1}]");
        
        emitAndCheck(
                "      01 A VALUE \"pazuzu\"."
                , 
                "[{level:1,cobolName:A,values:[\"pazuzu\"],srceLine:1}]");
        
        emitAndCheck(
                "      01 A VALUE 'pazuzu'."
                , 
                "[{level:1,cobolName:A,values:['pazuzu'],srceLine:1}]");
        
        emitAndCheck(
                "      01 A VALUE X'FF95'."
                , 
                "[{level:1,cobolName:A,values:[X'FF95'],srceLine:1}]");
        
        emitAndCheck(
                "      01 A VALUE Z'ab'."
                , 
                "[{level:1,cobolName:A,values:[Z'ab'],srceLine:1}]");
        
        emitAndCheck(
                "      01 A VALUE G'ab'."
                , 
                "[{level:1,cobolName:A,values:[G'ab'],srceLine:1}]");
        
        emitAndCheck(
                "      01 A VALUE N'ab'."
                , 
                "[{level:1,cobolName:A,values:[N'ab'],srceLine:1}]");
        
        emitAndCheck(
                "      01 A VALUE NX'FF95'."
                , 
                "[{level:1,cobolName:A,values:[NX'FF95'],srceLine:1}]");
        
        emitAndCheck(
                "      01 A VALUE ZERO."
                , 
                "[{level:1,cobolName:A,values:[ZERO],srceLine:1}]");
        
        emitAndCheck(
                "      01 A VALUE SPACES."
                , 
                "[{level:1,cobolName:A,values:[SPACES],srceLine:1}]");
        
        emitAndCheck(
                "      01 A VALUE HIGH-VALUE."
                , 
                "[{level:1,cobolName:A,values:[HIGH-VALUE],srceLine:1}]");
        
        emitAndCheck(
                "      01 A VALUE low-value."
                , 
                "[{level:1,cobolName:A,values:[low-value],srceLine:1}]");
        
        emitAndCheck(
                "      01 A VALUE QUOTE."
                , 
                "[{level:1,cobolName:A,values:[QUOTE],srceLine:1}]");
        
        emitAndCheck(
                "      01 A VALUE ALL 'A'."
                , 
                "[{level:1,cobolName:A,values:[ALL,'A'],srceLine:1}]");

        emitAndCheck(
                "      01 A VALUE NULL."
                , 
                "[{level:1,cobolName:A,values:[NULL],srceLine:1}]");
    }

    /**
     * Date format clause.
     */
    public void testDateFormat() {
        emitAndCheck(
                "      01 DATE-FIELD DATE FORMAT YYXXXX PICTURE 9(6)" + LS
                + "                VALUE IS 450101."
                , 
                "[{level:1,cobolName:DATE-FIELD,picture:\"9(6)\",values:[450101],dateFormat:YYXXXX,srceLine:1}]");
    }

    /**
     * Occurs clause.
     */
    public void testOccurs() {
        emitAndCheck(
                "      01 A OCCURS 3."
                , 
                "[{level:1,cobolName:A,maxOccurs:3,srceLine:1}]");
        emitAndCheck(
                "      01 A OCCURS 0 TO 3 DEPENDING B."
                , 
                "[{level:1,cobolName:A,minOccurs:0,maxOccurs:3,dependingOn:B,srceLine:1}]");

        emitAndCheck(
                "      01 Z OCCURS 3 INDEXED BY A, B."
                , 
                "[{level:1,cobolName:Z,maxOccurs:3,indexes:[A,B],srceLine:1}]");

        emitAndCheck(
                "      01 Z OCCURS 3 ASCENDING KEY IS WAGE-RATE EMPLOYEE-NO."
                , 
                "[{level:1,cobolName:Z,maxOccurs:3,ascendingKeys:[WAGE-RATE,EMPLOYEE-NO],srceLine:1}]");

        emitAndCheck(
                "      01 Z OCCURS 3 ASCENDING A B DESCENDING C."
                , 
                "[{level:1,cobolName:Z,maxOccurs:3,ascendingKeys:[A,B],descendingKeys:[C],srceLine:1}]");
    }

    /**
     * Renames clause.
     */
    public void testRenames() {
        emitAndCheck(
                "      66 A RENAMES B."
                , 
                "[{level:66,cobolName:A,renamesSubject:B,srceLine:1}]");

        emitAndCheck(
                "      66 A RENAMES B THRU C."
                , 
                "[{level:66,cobolName:A,renamesSubjectRange:{from:B,to:C},srceLine:1}]");
    }

    /**
     * Condition clause.
     */
    public void testCondition() {
        emitAndCheck(
                "      88 A VALUE \"99\"."
                , 
                "[{level:88,cobolName:A,conditionLiterals:[\"99\"],srceLine:1}]");

        emitAndCheck(
                "      88 A VALUE 1, 2."
                , 
                "[{level:88,cobolName:A,conditionLiterals:[1,2],srceLine:1}]");

        emitAndCheck(
                "      88 A VALUE 3 THRU 12."
                , 
                "[{level:88,cobolName:A,conditionRanges:[{from:3,to:12}],srceLine:1}]");

        emitAndCheck(
                "      88 A VALUE 3 THRU 12, 16 THRU 18."
                , 
                "[{level:88,cobolName:A,conditionRanges:[{from:3,to:12},{from:16,to:18}],srceLine:1}]");
    }
}
