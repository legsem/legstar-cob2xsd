package com.legstar.cob2xsd;

/**
 * Test the cob2xsd parser.
 *
 */
public class Cob2XsdParserTest extends AbstractCob2XsdTester {
    
    /**
     * Just a simple level.
     */
    public void testLevel() {
        parseAndCheck(
                "01."
                ,
                "(DATA_ITEM (LEVEL 01))");
    }

    /**
     * Just a simple level/name detection.
     */
    public void testLevelAndName() {
        parseAndCheck(
                "01 MYNAME."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME MYNAME))");
    }

    /**
     * Same as above with lowercase name.
     */
    public void testLevelAndLowercaseName() {
        parseAndCheck(
                "01 myName."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName))");
    }
 
    /**
     * A simple structure.
     */
    public void testStructure() {
        parseAndCheck(
                ""
                + "       01 myName." + LS
                + "          02 FILLER."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (DATA_ITEM (LEVEL 02) (NAME FILLER)))");
    }

    /**
     * A simple structure.
     */
    public void testTwoIndependentStructures() {
        parseAndCheck(
                ""
                + "       01 A." + LS
                + "          02 B." + LS
                + "          02 C." + LS
                + "            03 D." + LS
                + "          02 E." + LS
                + "       01 F." + LS
                + "          02 G." + LS
                + "            03 H." + LS
                + "            03 I." + LS
                + "          02 J." + LS
                ,
                ""
                + "(DATA_ITEM (LEVEL 01) (NAME A)" 
                + " (DATA_ITEM (LEVEL 02) (NAME B))" 
                + " (DATA_ITEM (LEVEL 02) (NAME C)" 
                + " (DATA_ITEM (LEVEL 03) (NAME D)))" 
                + " (DATA_ITEM (LEVEL 02) (NAME E)))" 
                + " (DATA_ITEM (LEVEL 01) (NAME F)" 
                + " (DATA_ITEM (LEVEL 02) (NAME G)" 
                + " (DATA_ITEM (LEVEL 03) (NAME H))" 
                + " (DATA_ITEM (LEVEL 03) (NAME I)))" 
                + " (DATA_ITEM (LEVEL 02) (NAME J)))");
    }

    /**
     * A structure with sequence rupture.
     */
    public void testStructureSequenceRupture() {
        parseAndCheck(
                ""
                + "       01 A." + LS
                + "          02 B." + LS
                + "             04 C." + LS
                + "          03 D." + LS
                ,
                "(DATA_ITEM (LEVEL 01) (NAME A)"
                + " (DATA_ITEM (LEVEL 02) (NAME B)"
                + " (DATA_ITEM (LEVEL 04) (NAME C))"
                + " (DATA_ITEM (LEVEL 03) (NAME D))"
                + "))");
    }

    /**
     * Test a rename clause.
     */
    public void testRename() {
        parseAndCheck(
                "66 NEWNAME RENAMES OLDNAME."
                ,
                "(RENAME (LEVEL 66) (NAME NEWNAME) (LITERAL OLDNAME))");
    }
    
    /**
     * Test a rename clause with a range.
     */
    public void testRenameThrough() {
        parseAndCheck(
                "66 NEWNAME RENAMES OLDSTART THROUGH OLDEND."
                ,
                "(RENAME (LEVEL 66) (NAME NEWNAME) (RANGE OLDSTART OLDEND))");
    }

    /**
     * Test a rename clause in context.
     */
    public void testRenameInContext() {
        parseAndCheck(
                ""
                + "       01 RECORD-I." + LS
                + "            05 DN-1." + LS
                + "            05 DN-2." + LS
                + "            05 DN-3." + LS
                + "       66 DN-6 RENAMES DN-1 THROUGH DN-3." + LS
                + "       01 RECORD-II." + LS
                ,
                "(DATA_ITEM (LEVEL 01) (NAME RECORD-I)"
                + " (DATA_ITEM (LEVEL 05) (NAME DN-1))"
                + " (DATA_ITEM (LEVEL 05) (NAME DN-2))"
                + " (DATA_ITEM (LEVEL 05) (NAME DN-3))"
                + " (RENAME (LEVEL 66) (NAME DN-6) (RANGE DN-1 DN-3)))"
                + " (DATA_ITEM (LEVEL 01) (NAME RECORD-II))");
    }

    /**
     * Test a condition clause with one value.
     */
    public void testConditionOneValue() {
        parseAndCheck(
                "88 CONDITION VALUE 99."
                ,
                "(CONDITION (LEVEL 88) (NAME CONDITION) (LITERAL 99))");
    }

    /**
     * Test a condition clause with multiple value.
     */
    public void testConditionMultipleValues() {
        parseAndCheck(
                "88 A VALUES ARE \"1\" \"2\"."
                ,
                "(CONDITION (LEVEL 88) (NAME A) (LITERAL \"1\") (LITERAL \"2\"))");
    }

    /**
     * Test a condition clause with range.
     */
    public void testConditionRangeValues() {
        parseAndCheck(
                "88 Q VALUE 13 THRU 19."
                ,
                "(CONDITION (LEVEL 88) (NAME Q) (RANGE 13 19))");
    }

    /**
     * Make sure a condition is bound to its parent.
     */
    public void testConditionInContext() {
        parseAndCheck(
                ""
                + "       01 RECORD-I." + LS
                + "            05 DN-1." + LS
                + "               88 COND-1 VALUE 5." + LS
                + "            05 DN-3."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME RECORD-I)"
                + " (DATA_ITEM (LEVEL 05) (NAME DN-1)"
                + " (CONDITION (LEVEL 88) (NAME COND-1) (LITERAL 5)))"
                + " (DATA_ITEM (LEVEL 05) (NAME DN-3)))");
    }
 
    /**
     * Test an item redefining another.
     */
    public void testRedefines() {
        parseAndCheck(
                ""
                + "       01 A." + LS
                + "       01 B REDEFINES A."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME A)) (DATA_ITEM (LEVEL 01) (NAME B) (REDEFINES A))");
    }

    /**
     * Test blank when zero clause.
     */
    public void testBlankWhenZero() {
        parseAndCheck(
                "01 myName BLANK WHEN ZERO."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) BLANKWHENZERO)");
        parseAndCheck(
                "01 myName BLANK WHEN zeros."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) BLANKWHENZERO)");
        parseAndCheck(
                "01 myName BLANK WHEN zeroes."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) BLANKWHENZERO)");
   }

    /**
     * Test external clause.
     */
    public void testExternal() {
        parseAndCheck(
                "01 myName external."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) EXTERNAL)");
    }

    /**
     * Test global clause.
     */
    public void testGlobal() {
        parseAndCheck(
                "01 myName global."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) GLOBAL)");
    }
 
    /**
     * Test group usage national clause.
     */
    public void testGroupUsageNational() {
        parseAndCheck(
                "01 myName GROUP-USAGE IS NATIONAL."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) GROUPUSAGENATIONAL)");
        parseAndCheck(
                "01 myName GROUP-USAGE NATIONAL."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) GROUPUSAGENATIONAL)");
    }

    /**
     * Test global clause.
     */
    public void testJustifiedRight() {
        parseAndCheck(
                "01 myName just."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) JUSTIFIEDRIGHT)");
        parseAndCheck(
                "01 myName justified right."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) JUSTIFIEDRIGHT)");
    }
 
    /**
     * Test fixed length table clause.
     */
    public void testFixedOccurs() {
        parseAndCheck(
                "01 myName OCCURS 3."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (FIXEDARRAY (HBOUND 3)))");
        parseAndCheck(
                "01 myName OCCURS 3 TIMES."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (FIXEDARRAY (HBOUND 3)))");
    }

    /**
     * Test fixed length table clause with keys.
     */
    public void testFixedOccursWithKeys() {
        parseAndCheck(
                "01 myName OCCURS 3 ASCENDING myKey."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (FIXEDARRAY (HBOUND 3) (KEY ASCENDING myKey)))");

        parseAndCheck(
                "01 myName OCCURS 3 ASCENDING KEY myKey."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (FIXEDARRAY (HBOUND 3) (KEY ASCENDING myKey)))");

        parseAndCheck(
                "01 myName OCCURS 3 ASCENDING KEY IS myKey."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (FIXEDARRAY (HBOUND 3) (KEY ASCENDING myKey)))");

        parseAndCheck(
                "01 myName OCCURS 3 DESCENDING myKey1 myKey2."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (FIXEDARRAY (HBOUND 3) (KEY DESCENDING myKey1) (KEY DESCENDING myKey2)))");
    }

    /**
     * Test fixed length table clause with indexes.
     */
    public void testFixedOccursWithIndexes() {
        parseAndCheck(
                "01 myName OCCURS 3 INDEXED myIndex."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (FIXEDARRAY (HBOUND 3) (INDEX myIndex)))");

        parseAndCheck(
                "01 myName OCCURS 3 INDEXED BY myIndex."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (FIXEDARRAY (HBOUND 3) (INDEX myIndex)))");

        parseAndCheck(
                "01 myName OCCURS 3 INDEXED myIndex1 myIndex2."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (FIXEDARRAY (HBOUND 3) (INDEX myIndex1) (INDEX myIndex2)))");
    }

    /**
     * Test variable length table clause.
     */
    public void testVariableOccurs() {
        parseAndCheck(
                "01 myName OCCURS 3 DEPENDING myDep."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (VARARRAY (HBOUND 3 (DEPENDINGON myDep))))");

        parseAndCheck(
                "01 myName OCCURS 3 DEPENDING ON myDep."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (VARARRAY (HBOUND 3 (DEPENDINGON myDep))))");
    }
 
    /**
     * Test variable length table clause with lower bound.
     */
    public void testVariableOccursWithLowerBound() {
        parseAndCheck(
                "01 myName OCCURS 0 TO 3 DEPENDING myDep ASCENDING myKey."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (VARARRAY (LBOUND 0) (HBOUND 3 (DEPENDINGON myDep)) (KEY ASCENDING myKey)))");

        parseAndCheck(
                "01 myName OCCURS 1 TO 3 DEPENDING ON myDep INDEXED myIndex."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (VARARRAY (LBOUND 1) (HBOUND 3 (DEPENDINGON myDep)) (INDEX myIndex)))");
    }

    /**
     * Test a simple picture clause.
     */
    public void testPictureClause() {
        parseAndCheck(
                "01 myName PIC X."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (PICTURE X))");

        parseAndCheck(
                "01 m99 PIC 9.9."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME m99) (PICTURE 9.9))");

        parseAndCheck(
                "01 myName PIC 99."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (PICTURE 99))");
    }
 
    /**
     * Test a more complex picture clauses.
     */
    public void testComplexPictureClause() {
        parseAndCheck(
                "01 myName PIC $99999.99CR."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (PICTURE $99999.99CR))");

        parseAndCheck(
                "01 myName PIC $9(5).9(2)CR."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (PICTURE $9(5).9(2)CR))");

        parseAndCheck(
                "01 myName PIC X(10)/XX ."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (PICTURE X(10)/XX))");

        parseAndCheck(
                "01 myName PIC X(5)BX(7)."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (PICTURE X(5)BX(7)))");

        parseAndCheck(
                "01 myName PIC GGBBGG."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (PICTURE GGBBGG))");

        parseAndCheck(
                "01 myName PIC +999.99E+99."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (PICTURE +999.99E+99))");

        parseAndCheck(
                "01 myName PIC -$$,$$$,$$$.99CR."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (PICTURE -$$,$$$,$$$.99CR))");

        parseAndCheck(
                "01 myName PIC Z,ZZZ.ZZ+."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (PICTURE Z,ZZZ.ZZ+))");

        parseAndCheck(
                "01 myName PIC $B*,***,***.**BBDB."
                ,
                "(DATA_ITEM (LEVEL 01) (NAME myName) (PICTURE $B*,***,***.**BBDB))");
   }
}
