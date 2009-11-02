package com.legstar.cob2xsd;


/**
 * Test cases for the cob2xsd Lexer.
 *
 */
public class CobolStructureLexerTest extends AbstractCob2XsdTester {

    /**
     * Comments should be skipped from the token stream. Starting white
     * spaces are not part of the comment and are on the hidden channel
     */
    public void testComments() {
        lexAndCheck(
                ""
                + "       01 A" + LS
                + "      * a comment" + LS
                + "       ." + LS
                , "[@0,7:8='01',<INT>,1:7]"
                + "[@1,10:10='A',<DATA_NAME>,1:10]"
                + "[@2,22:22='.',<PERIOD>,3:7]");
        lexAndCheck(
                ""
                + "       01 A" + LS
                + "      / a comment" + LS
                + "       ." + LS
                , "[@0,7:8='01',<INT>,1:7]"
                + "[@1,10:10='A',<DATA_NAME>,1:10]"
                + "[@2,22:22='.',<PERIOD>,3:7]");
    }

    /**
     * Items don't have to be named.
     */
    public void testLevelAlone() {
        lexAndCheck(
                "       01." + LS
                , "[@0,7:8='01',<INT>,1:7]"
                + "[@1,9:9='.',<PERIOD>,1:9]");
    }

    /**
     * An empty string literal.
     */
    public void testEmptyLiteralString() {
        lexAndCheck(
                "       1 A value \"\"" + LS
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:18='\"\"',<ALPHANUM_LITERAL_STRING>,1:17]");
    }

    /**
     * A string literal on a single line.
     */
    public void testSingleLineLiteralString() {
        lexAndCheck(
                "       1 A value \" a literal \"" + LS
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:29='\" a literal \"',<ALPHANUM_LITERAL_STRING>,1:17]");
    }

    /**
     * A string literal on a single line but with a delimiter that
     * occurs in the middle of the literal (and therefore should be
     * interpreted as part of the literal).
     */
    public void testSingleLineLiteralStringWithInnerDelimiter() {
        lexAndCheck(
                "       1 A value \" a li\"\"teral \"" + LS
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:31='\" a li\"\"teral \"',<ALPHANUM_LITERAL_STRING>,1:17]");
    }

    /**
     * Two consecutive strings separated by at least a space should not
     * be confused with a single string.
     */
    public void testSingleLineDoubleString() {
        lexAndCheck(
                "       1 A value \" a li\" \"teral \"" + LS
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:23='\" a li\"',<ALPHANUM_LITERAL_STRING>,1:17]"
                + "[@4,25:32='\"teral \"',<ALPHANUM_LITERAL_STRING>,1:25]");
    }

    /**
     * A multiline string literal. It should be reassembled into
     * a single literal.
     */
    public void testMultilineLiteralString() {
        lexAndCheck(
                "       1 A value" + LS
                + "                     \"AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEE" + LS
                + "      -              \"GGGGGGGGGGHHHHHHHHHHIIIIIIIIIIJJJJJJJJJJKKKKKKKKKK" + LS
                + "      -              \"LLLLLLLLLLMMMMMMMMMM\"." + LS
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,39:208='\"AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEEGGGGGGGGGGHHHHHHHHHH"
                + "IIIIIIIIIIJJJJJJJJJJKKKKKKKKKKLLLLLLLLLLMMMMMMMMMM\"',<ALPHANUM_LITERAL_STRING>,2:21]"
                + "[@4,209:209='.',<PERIOD>,4:43]");
    }

    /**
     * Same as above but with apostrophe delimiters rather than quotes.
     */
    public void testMultilineLiteralStringApost() {
        lexAndCheck(
                "       1 A value" + LS
                + "                     \'AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEE" + LS
                + "      -              \'LLLLLLLLLLMMMMMMMMMM\'."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,39:134=''AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEELLLLLLLLLL"
                + "MMMMMMMMMM'',<ALPHANUM_LITERAL_STRING>,2:21]"
                + "[@4,135:135='.',<PERIOD>,3:43]");
    }


    /**
     * An integer value. Should not be extracted with the period delimiter.
     */
    public void testIntegerLiteral() {
        lexAndCheck(
                "       1 A value 99."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:18='99',<INT>,1:17]"
                + "[@4,19:19='.',<PERIOD>,1:19]");
    }

    /**
     * A value with extra IS keyword.
     */
    public void testIntegerLiteralWithIs() {
        lexAndCheck(
                "       1 A value is 99."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,20:21='99',<INT>,1:20]"
                + "[@4,22:22='.',<PERIOD>,1:22]");
    }


    /**
     * An integer value. This time it should be detected as an INT.
     */
    public void testIntegerLiteralSeparateFromPeriod() {
        lexAndCheck(
                "       1 A value 99 ."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:18='99',<INT>,1:17]"
                + "[@4,20:20='.',<PERIOD>,1:20]");
    }

    /**
     * A decimal value. Might confuse lexers because period is a decimal point.
     */
    public void testDecimalLiteral() {
        lexAndCheck(
                "       1 A value 99.9."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:18='99',<INT>,1:17]"
                + "[@4,19:19='.',<DECIMAL_POINT>,1:19]"
                + "[@5,20:20='9',<INT>,1:20]"
                + "[@6,21:21='.',<PERIOD>,1:21]");
    }

    /**
     * Same as above but without an integer part.
     */
    public void testNoIntegerPartDecimalLiteral() {
        lexAndCheck(
                "       1 A value .99."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:17='.',<DECIMAL_POINT>,1:17]"
                + "[@4,18:19='99',<INT>,1:18]"
                + "[@5,20:20='.',<PERIOD>,1:20]");
    }

    /**
     * Decimal again but with a sign.
     */
    public void testSignedDecimalLiteral() {
        lexAndCheck(
                "       1 A value -99.9."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:19='-99',<SIGNED_INT>,1:17]"
                + "[@4,20:20='.',<DECIMAL_POINT>,1:20]"
                + "[@5,21:21='9',<INT>,1:21]"
                + "[@6,22:22='.',<PERIOD>,1:22]");
    }

    /**
     * The case insensitive char reader should allow keywords to
     * be mixed case without impact on lexer grammar.
     */
    public void testCaseInsensitiveReader() {
        lexAndCheck("       1 A  REDEFINES b.",
                "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,12:20='REDEFINES',<REDEFINES_KEYWORD>,1:12]"
                + "[@3,22:22='b',<DATA_NAME>,1:22]"
                + "[@4,23:23='.',<PERIOD>,1:23]");

        lexAndCheck("       1 A  redeFines b.",
                "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,12:20='redeFines',<REDEFINES_KEYWORD>,1:12]"
                + "[@3,22:22='b',<DATA_NAME>,1:22]"
                + "[@4,23:23='.',<PERIOD>,1:23]");
    }

    /**
     * Date format case.
     */
    public void testDateFormat() {
        lexAndCheck("       1 YY  DATE FORMAT YY.",
                "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:10='YY',<DATA_NAME>,1:9]"
                + "[@2,18:23='FORMAT',<DATE_FORMAT_KEYWORD>,1:18]"
                + "[@3,25:26='YY',<DATE_PATTERN>,1:25]"
                + "[@4,27:27='.',<PERIOD>,1:27]");
    }

    /**
     * A picture case.
     */
    public void testPicture() {
        lexAndCheck("       01 MYVAR PIC 99.",
                "[@0,7:8='01',<INT>,1:7]"
                + "[@1,10:14='MYVAR',<DATA_NAME>,1:10]"
                + "[@2,16:18='PIC',<PICTURE_KEYWORD>,1:16]"
                + "[@3,20:21='99',<PICTURE_PART>,1:20]"
                + "[@4,22:22='.',<PERIOD>,1:22]");
    }

    /**
     * A picture case where the period is separated by a space resulting
     * in lexer identifying the symbol string as an INT.
     */
    public void testPictureSeparateFromPeriod() {
        lexAndCheck("       01 MYVAR PIC 99 .",
                "[@0,7:8='01',<INT>,1:7]"
                + "[@1,10:14='MYVAR',<DATA_NAME>,1:10]"
                + "[@2,16:18='PIC',<PICTURE_KEYWORD>,1:16]"
                + "[@3,20:21='99',<PICTURE_PART>,1:20]"
                + "[@4,23:23='.',<PERIOD>,1:23]");
    }

    /**
     * A picture clause with a decimal point. 
     */
    public void testPictureWithPeriod() {
        lexAndCheck("       01 MYVAR PIC 99.9.",
                "[@0,7:8='01',<INT>,1:7]"
                + "[@1,10:14='MYVAR',<DATA_NAME>,1:10]"
                + "[@2,16:18='PIC',<PICTURE_KEYWORD>,1:16]"
                + "[@3,20:21='99',<PICTURE_PART>,1:20]"
                + "[@4,22:22='.',<DECIMAL_POINT>,1:22]"
                + "[@5,23:23='9',<PICTURE_PART>,1:23]"
                + "[@6,24:24='.',<PERIOD>,1:24]");
    }

    /**
     * A picture clause with a CR or DB type of symbol. 
     */
    public void testPictureWithDoubleCharacters() {
        lexAndCheck("       01 MYVAR PIC 99.9CR.",
                "[@0,7:8='01',<INT>,1:7]"
                + "[@1,10:14='MYVAR',<DATA_NAME>,1:10]"
                + "[@2,16:18='PIC',<PICTURE_KEYWORD>,1:16]"
                + "[@3,20:21='99',<PICTURE_PART>,1:20]"
                + "[@4,22:22='.',<DECIMAL_POINT>,1:22]"
                + "[@5,23:25='9CR',<PICTURE_PART>,1:23]"
                + "[@6,26:26='.',<PERIOD>,1:26]");
    }

    /**
     * More than one statement. 
     */
    public void testMultipleStatements() {
        lexAndCheck(
                "       01 MYVAR1." + LS
                + "        02 MYVAR2 PIC X."
                ,
                "[@0,7:8='01',<INT>,1:7]"
                + "[@1,10:15='MYVAR1',<DATA_NAME>,1:10]"
                + "[@2,16:16='.',<PERIOD>,1:16]"
                + "[@3,27:28='02',<INT>,2:8]"
                + "[@4,30:35='MYVAR2',<DATA_NAME>,2:11]"
                + "[@5,37:39='PIC',<PICTURE_KEYWORD>,2:18]"
                + "[@6,41:41='X',<PICTURE_PART>,2:22]"
                + "[@7,42:42='.',<PERIOD>,2:23]");
    }

    /**
     * Multiple statements and a comment. 
     */
    public void testMultipleStatementsAndComments() {
        lexAndCheck(
                ""
                + "       01 MYVAR1." + LS
                + "      * A comment." + LS
                + "           02 MYVAR2 PIC 99.9."
                ,
                "[@0,7:8='01',<INT>,1:7]"
                + "[@1,10:15='MYVAR1',<DATA_NAME>,1:10]"
                + "[@2,16:16='.',<PERIOD>,1:16]"
                + "[@3,32:33='02',<INT>,3:11]"
                + "[@4,35:40='MYVAR2',<DATA_NAME>,3:14]"
                + "[@5,42:44='PIC',<PICTURE_KEYWORD>,3:21]"
                + "[@6,46:47='99',<PICTURE_PART>,3:25]"
                + "[@7,48:48='.',<DECIMAL_POINT>,3:27]"
                + "[@8,49:49='9',<PICTURE_PART>,3:28]"
                + "[@9,50:50='.',<PERIOD>,3:29]");
    }

    /**
     * An hex string literal.
     */
    public void testHexLiteralString() {
        lexAndCheck(
                "       1 A value X\"FB\"" + LS
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:21='X\"FB\"',<HEX_LITERAL_STRING>,1:17]");
    }

    /**
     * An zero terminated string literal.
     */
    public void testZeroLiteralString() {
        lexAndCheck(
                "       1 A value Z'q'"
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:20='Z'q'',<ZERO_LITERAL_STRING>,1:17]");
    }

    /**
     * An DBCS string literal.
     */
    public void testDBCSLiteralString() {
        lexAndCheck(
                "       1 A value G'q'"
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:20='G'q'',<DBCS_LITERAL_STRING>,1:17]");
    }

    /**
     * An National string literal.
     */
    public void testNationalLiteralString() {
        lexAndCheck(
                "       1 A value N'q'"
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:20='N'q'',<NATIONAL_LITERAL_STRING>,1:17]");
    }

    /**
     * An Hex National string literal.
     */
    public void testHexNationalLiteralString() {
        lexAndCheck(
                "       1 A value NX'F5F7'"
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='value',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:24='NX'F5F7'',<NATIONAL_HEX_LITERAL_STRING>,1:17]");
    }


    /**
     * An Decimal numeric literal.
     */
    public void testDecimalNumericLiteral() {
        lexAndCheck(
                "       1 A  VALUE 99.9."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                + "[@3,18:19='99',<INT>,1:18]"
                + "[@4,20:20='.',<DECIMAL_POINT>,1:20]"
                + "[@5,21:21='9',<INT>,1:21]"
                + "[@6,22:22='.',<PERIOD>,1:22]");
    }

    /**
     * An Float numeric literal.
     */
    public void testFloatNumericLiteral() {
        lexAndCheck(
                "       1 A  VALUE 99.9E56."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                + "[@3,18:19='99',<INT>,1:18]"
                + "[@4,20:20='.',<DECIMAL_POINT>,1:20]"
                + "[@5,21:24='9E56',<FLOAT_PART2>,1:21]"
                + "[@6,25:25='.',<PERIOD>,1:25]");

        lexAndCheck(
                "       1 A  VALUE IS -0.78E+23."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                + "[@3,21:22='-0',<SIGNED_INT>,1:21]"
                + "[@4,23:23='.',<DECIMAL_POINT>,1:23]"
                + "[@5,24:29='78E+23',<FLOAT_PART2>,1:24]"
                + "[@6,30:30='.',<PERIOD>,1:30]");
    }

    /**
     * Test figurative constants recognition.
     */
    public void testFigurativeConstants() {
        lexAndCheck(
                "       1 A VALUE ZERO."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='VALUE',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:20='ZERO',<ZERO_CONSTANT>,1:17]"
                + "[@4,21:21='.',<PERIOD>,1:21]"
        );
        lexAndCheck(
                "       1 A VALUE ZEROS."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='VALUE',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:21='ZEROS',<ZERO_CONSTANT>,1:17]"
                + "[@4,22:22='.',<PERIOD>,1:22]"
        );
        lexAndCheck(
                "       1 A VALUE ZEROES."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='VALUE',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:22='ZEROES',<ZERO_CONSTANT>,1:17]"
                + "[@4,23:23='.',<PERIOD>,1:23]"
        );
        lexAndCheck(
                "       1 A  VALUE SPACE."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                + "[@3,18:22='SPACE',<SPACE_CONSTANT>,1:18]"
                + "[@4,23:23='.',<PERIOD>,1:23]"
        );
        lexAndCheck(
                "       1 A  VALUE SPACES."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                + "[@3,18:23='SPACES',<SPACE_CONSTANT>,1:18]"
                + "[@4,24:24='.',<PERIOD>,1:24]"
        );
        lexAndCheck(
                "       1 A  VALUE HIGH-VALUE."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                + "[@3,18:27='HIGH-VALUE',<HIGH_VALUE_CONSTANT>,1:18]"
                + "[@4,28:28='.',<PERIOD>,1:28]"
        );
        lexAndCheck(
                "       1 A  VALUE HIGH-VALUES."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                + "[@3,18:28='HIGH-VALUES',<HIGH_VALUE_CONSTANT>,1:18]"
                + "[@4,29:29='.',<PERIOD>,1:29]"
        );
        lexAndCheck(
                "       1 A  VALUE LOW-VALUE."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                + "[@3,18:26='LOW-VALUE',<LOW_VALUE_CONSTANT>,1:18]"
                + "[@4,27:27='.',<PERIOD>,1:27]"
        );
        lexAndCheck(
                "       1 A  VALUE LOW-VALUES."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                + "[@3,18:27='LOW-VALUES',<LOW_VALUE_CONSTANT>,1:18]"
                + "[@4,28:28='.',<PERIOD>,1:28]"
        );
        lexAndCheck(
                "       1 A  VALUE QUOTE."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                + "[@3,18:22='QUOTE',<QUOTE_CONSTANT>,1:18]"
                + "[@4,23:23='.',<PERIOD>,1:23]"
        );
        lexAndCheck(
                "       1 A  VALUE QUOTES."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                + "[@3,18:23='QUOTES',<QUOTE_CONSTANT>,1:18]"
                + "[@4,24:24='.',<PERIOD>,1:24]"
        );
        lexAndCheck(
                "       1 A VALUE ALL 'A'."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,11:15='VALUE',<VALUE_KEYWORD>,1:11]"
                + "[@3,17:19='ALL',<ALL_CONSTANT>,1:17]"
                + "[@4,21:23=''A'',<ALPHANUM_LITERAL_STRING>,1:21]"
                + "[@5,24:24='.',<PERIOD>,1:24]"
        );

        lexAndCheck(
                "       1 A  VALUE NULL."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                + "[@3,18:21='NULL',<NULL_CONSTANT>,1:18]"
                + "[@4,22:22='.',<PERIOD>,1:22]"
        );

        lexAndCheck(
                "       1 A  VALUE NULLS."
                , "[@0,7:7='1',<INT>,1:7]"
                + "[@1,9:9='A',<DATA_NAME>,1:9]"
                + "[@2,12:16='VALUE',<VALUE_KEYWORD>,1:12]"
                + "[@3,18:22='NULLS',<NULL_CONSTANT>,1:18]"
                + "[@4,23:23='.',<PERIOD>,1:23]"
        );
    }
    
    /**
     * Helper to test string fragments assembly using a regular expression. 
     */
    public void testReplace() {
        String regex = "(\\r)?\\n(\\s)*\\-(\\s)*(\"|\')";
        assertEquals("ab", "ab".replaceAll(regex, ""));
        assertEquals("", "\r\n-\"".replaceAll(regex, ""));
        assertEquals("", "\n  -  \"".replaceAll(regex, ""));
        assertEquals("\r\n", "\r\n".replaceAll(regex, ""));
        assertEquals("", "\r\n-\'".replaceAll(regex, ""));
        assertEquals("", "\n  - \'".replaceAll(regex, ""));
        assertEquals("EEEEEEEEEEGGGGGGGGGG",
                "EEEEEEEEEE\r\n      -               \"GGGGGGGGGG".replaceAll(regex, ""));
    }

    /**
     * A condition.
     */
    public void testCondition() {
        lexAndCheck(
                "      88 CONDITION VALUE 99."
                , "[@0,6:7='88',<INT>,1:6]"
                + "[@1,9:17='CONDITION',<DATA_NAME>,1:9]"
                + "[@2,19:23='VALUE',<VALUE_KEYWORD>,1:19]"
                + "[@3,25:26='99',<INT>,1:25]"
                + "[@4,27:27='.',<PERIOD>,1:27]");
    }
 
    /**
     * A value clause followed by a keyword.
     */
    public void testValueThenKeyword() {
        lexAndCheck(
                "       01 myName PIC 9 SYNCHRONIZED."
                , "[@0,7:8='01',<INT>,1:7]"
                + "[@1,10:15='myName',<DATA_NAME>,1:10]"
                + "[@2,17:19='PIC',<PICTURE_KEYWORD>,1:17]"
                + "[@3,21:21='9',<PICTURE_PART>,1:21]"
                + "[@4,23:34='SYNCHRONIZED',<SYNCHRONIZED_KEYWORD>,1:23]"
                + "[@5,35:35='.',<PERIOD>,1:35]");
    }

    /**
     * A value clause followed by a keyword.
     */
    public void testUsageThenKeyword() {
        lexAndCheck(
                "       01 hisName USAGE COMPUTATIONAL-1."
                , "[@0,7:8='01',<INT>,1:7]"
                + "[@1,10:16='hisName',<DATA_NAME>,1:10]"
                + "[@2,18:22='USAGE',<USAGE_KEYWORD>,1:18]"
                + "[@3,24:38='COMPUTATIONAL-1',<SINGLE_FLOAT_KEYWORD>,1:24]"
                + "[@4,39:39='.',<PERIOD>,1:39]");

        lexAndCheck(
                "       01 hisName USAGE DISPLAY-1."
                , "[@0,7:8='01',<INT>,1:7]"
                + "[@1,10:16='hisName',<DATA_NAME>,1:10]"
                + "[@2,18:22='USAGE',<USAGE_KEYWORD>,1:18]"
                + "[@3,24:32='DISPLAY-1',<DISPLAY_1_KEYWORD>,1:24]"
                + "[@4,33:33='.',<PERIOD>,1:33]");

        lexAndCheck(
                "       01 hisName DISPLAY."
                , "[@0,7:8='01',<INT>,1:7]"
                + "[@1,10:16='hisName',<DATA_NAME>,1:10]"
                + "[@2,18:24='DISPLAY',<DISPLAY_KEYWORD>,1:18]"
                + "[@3,25:25='.',<PERIOD>,1:25]");
    }

    /**
     * Special separator cases.
     */
    public void testSpecialSeparators() {
        lexAndCheck(
                "       01 hisName INDEXED BY A, B."
                ,
                "[@0,7:8='01',<INT>,1:7]"
                + "[@1,10:16='hisName',<DATA_NAME>,1:10]"
                + "[@2,18:24='INDEXED',<INDEXED_KEYWORD>,1:18]"
                + "[@3,29:30='A',<DATA_NAME>,1:29]"
                + "[@4,32:32='B',<DATA_NAME>,1:32]"
                + "[@5,33:33='.',<PERIOD>,1:33]");

        lexAndCheck(
                "       01 hisName VALUE 1, 2."
                ,
                "[@0,7:8='01',<INT>,1:7]"
                + "[@1,10:16='hisName',<DATA_NAME>,1:10]"
                + "[@2,18:22='VALUE',<VALUE_KEYWORD>,1:18]"
                + "[@3,24:24='1',<INT>,1:24]"
                + "[@4,27:27='2',<INT>,1:27]"
                + "[@5,28:28='.',<PERIOD>,1:28]");
    }
 }
