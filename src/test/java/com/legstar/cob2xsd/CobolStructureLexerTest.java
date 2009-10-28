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
                "      * a comment" + LS
                , "[@0,0:5='      ',<WHITESPACE>,channel=99,1:0]");
        lexAndCheck(
                "      * / comment" + LS
                , "[@0,0:5='      ',<WHITESPACE>,channel=99,1:0]");
    }

    /**
     * Items don't have to be named.
     */
    public void testLevelAlone() {
        lexAndCheck(
                "       01." + LS
                , "[@0,0:6='       ',<WHITESPACE>,channel=99,1:0]"
                + "[@1,7:8='01',<INT>,1:7]"
                + "[@2,9:9='.',<PERIOD>,1:9]"
                + "[@3,10:11='\\r\\n',<NEWLINE>,channel=99,1:10]");
    }

    /**
     * An empty string literal.
     */
    public void testEmptyLiteralString() {
        lexAndCheck(
                "value \"\"" + LS
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:5=' ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,6:7='\"\"',<ALPHANUM_LITERAL_STRING>,1:6]"
                + "[@3,8:9='\\r\\n',<NEWLINE>,channel=99,1:8]");
    }

    /**
     * A string literal on a single line.
     */
    public void testSingleLineLiteralString() {
        lexAndCheck(
                "value \" a literal \"" + LS
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:5=' ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,6:18='\" a literal \"',<ALPHANUM_LITERAL_STRING>,1:6]"
                + "[@3,19:20='\\r\\n',<NEWLINE>,channel=99,1:19]");
    }

    /**
     * A string literal on a single line but with a delimiter that
     * occurs in the middle of the literal (and therefore should be
     * interpreted as part of the literal).
     */
    public void testSingleLineLiteralStringWithInnerDelimiter() {
        lexAndCheck(
                "value \" a li\"\"teral \"" + LS
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:5=' ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,6:20='\" a li\"\"teral \"',<ALPHANUM_LITERAL_STRING>,1:6]"
                + "[@3,21:22='\\r\\n',<NEWLINE>,channel=99,1:21]");
    }

    /**
     * Two consecutive strings separated by at least a space should not
     * be confused with a single string.
     */
    public void testSingleLineDoubleString() {
        lexAndCheck(
                "value \" a li\" \"teral \"" + LS
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:5=' ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,6:12='\" a li\"',<ALPHANUM_LITERAL_STRING>,1:6]"
                + "[@3,13:13=' ',<WHITESPACE>,channel=99,1:13]"
                + "[@4,14:21='\"teral \"',<ALPHANUM_LITERAL_STRING>,1:14]"
                + "[@5,22:23='\\r\\n',<NEWLINE>,channel=99,1:22]");
    }

    /**
     * A multiline string literal. It should be reassembled into
     * a single literal.
     */
    public void testMultilineLiteralString() {
        lexAndCheck(
                "value"
                + "                      \"AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEE" + LS
                + "      -               \"GGGGGGGGGGHHHHHHHHHHIIIIIIIIIIJJJJJJJJJJKKKKKKKKKK" + LS
                + "      -               \"LLLLLLLLLLMMMMMMMMMM\""
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:26='                      ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,27:198='\"AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEEGGGGGGGGGGHHHHHHHHHHIIIIIIIIII"
                + "JJJJJJJJJJKKKKKKKKKKLLLLLLLLLLMMMMMMMMMM\"',<ALPHANUM_LITERAL_STRING>,1:27]");
    }

    /**
     * Same as above but with apostrophe delimiters rather than quotes.
     */
    public void testMultilineLiteralStringApost() {
        lexAndCheck(
                "value"
                + "                      \'AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEE" + LS
                + "      -               \'LLLLLLLLLLMMMMMMMMMM\'"
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:26='                      ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,27:123=''AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEELLLLLLLLLLMMMMMMMMMM'',"
                + "<ALPHANUM_LITERAL_STRING>,1:27]");
    }


    /**
     * An integer value. Should not be extracted with the period delimiter.
     */
    public void testIntegerLiteral() {
        lexAndCheck(
                "value 99."
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:5=' ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,6:7='99',<INT>,1:6]"
                + "[@3,8:8='.',<PERIOD>,1:8]");
    }

    /**
     * A value with extra IS keyword.
     */
    public void testIntegerLiteralWithIs() {
        lexAndCheck(
                "value is 99."
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:5=' ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,8:8=' ',<WHITESPACE>,channel=99,1:8]"
                + "[@3,9:10='99',<INT>,1:9]"
                + "[@4,11:11='.',<PERIOD>,1:11]");
    }


    /**
     * An integer value. This time it should be detected as an INT.
     */
    public void testIntegerLiteralSeparateFromPeriod() {
        lexAndCheck(
                "value 99 ."
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:5=' ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,6:7='99',<INT>,1:6]"
                + "[@3,8:8=' ',<WHITESPACE>,channel=99,1:8]"
                + "[@4,9:9='.',<PERIOD>,1:9]");
    }

    /**
     * A decimal value. Might confuse lexers because period is a decimal point.
     */
    public void testDecimalLiteral() {
        lexAndCheck(
                "value 99.9."
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:5=' ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,6:7='99',<INT>,1:6]"
                + "[@3,8:8='.',<DECIMAL_POINT>,1:8]"
                + "[@4,9:9='9',<INT>,1:9]"
                + "[@5,10:10='.',<PERIOD>,1:10]");
    }

    /**
     * Same as above but without an integer part.
     */
    public void testNoIntegerPartDecimalLiteral() {
        lexAndCheck(
                "value .99."
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:5=' ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,6:6='.',<DECIMAL_POINT>,1:6]"
                + "[@3,7:8='99',<INT>,1:7]"
                + "[@4,9:9='.',<PERIOD>,1:9]");
    }

    /**
     * Decimal again but with a sign.
     */
    public void testSignedDecimalLiteral() {
        lexAndCheck(
                "value -99.9."
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:5=' ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,6:8='-99',<SIGNED_INT>,1:6]"
                + "[@3,9:9='.',<DECIMAL_POINT>,1:9]"
                + "[@4,10:10='9',<INT>,1:10]"
                + "[@5,11:11='.',<PERIOD>,1:11]");
    }

    /**
     * The case insensitive char reader should allow keywords to
     * be mixed case without impact on lexer grammar.
     */
    public void testCaseInsensitiveReader() {
        lexAndCheck("  01 a REDEFINES b.",
                "[@0,0:1='  ',<WHITESPACE>,channel=99,1:0]"
                + "[@1,2:3='01',<INT>,1:2]"
                + "[@2,4:4=' ',<WHITESPACE>,channel=99,1:4]"
                + "[@3,5:5='a',<DATA_NAME>,1:5]"
                + "[@4,6:6=' ',<WHITESPACE>,channel=99,1:6]"
                + "[@5,7:15='REDEFINES',<REDEFINES_KEYWORD>,1:7]"
                + "[@6,16:16=' ',<WHITESPACE>,channel=99,1:16]"
                + "[@7,17:17='b',<DATA_NAME>,1:17]"
                + "[@8,18:18='.',<PERIOD>,1:18]");

        lexAndCheck("  01 a redeFines b.",
                "[@0,0:1='  ',<WHITESPACE>,channel=99,1:0]"
                + "[@1,2:3='01',<INT>,1:2]"
                + "[@2,4:4=' ',<WHITESPACE>,channel=99,1:4]"
                + "[@3,5:5='a',<DATA_NAME>,1:5]"
                + "[@4,6:6=' ',<WHITESPACE>,channel=99,1:6]"
                + "[@5,7:15='redeFines',<REDEFINES_KEYWORD>,1:7]"
                + "[@6,16:16=' ',<WHITESPACE>,channel=99,1:16]"
                + "[@7,17:17='b',<DATA_NAME>,1:17]"
                + "[@8,18:18='.',<PERIOD>,1:18]");
    }

    /**
     * Date format case.
     */
    public void testDateFormat() {
        lexAndCheck("  01 YY DATE FORMAT YY.",
                "[@0,0:1='  ',<WHITESPACE>,channel=99,1:0]"
                + "[@1,2:3='01',<INT>,1:2]"
                + "[@2,4:4=' ',<WHITESPACE>,channel=99,1:4]"
                + "[@3,5:6='YY',<DATA_NAME>,1:5]"
                + "[@4,7:7=' ',<WHITESPACE>,channel=99,1:7]"
                + "[@5,8:18='DATE FORMAT',<DATE_KEYWORD>,1:8]"
                + "[@6,19:19=' ',<WHITESPACE>,channel=99,1:19]"
                + "[@7,20:21='YY',<DATE_PATTERN>,1:20]"
                + "[@8,22:22='.',<PERIOD>,1:22]");
    }

    /**
     * A picture case.
     */
    public void testPicture() {
        lexAndCheck("  01 MYVAR PIC 99.",
                "[@0,0:1='  ',<WHITESPACE>,channel=99,1:0]"
                + "[@1,2:3='01',<INT>,1:2][@2,4:4=' ',<WHITESPACE>,channel=99,1:4]"
                + "[@3,5:9='MYVAR',<DATA_NAME>,1:5]"
                + "[@4,10:10=' ',<WHITESPACE>,channel=99,1:10]"
                + "[@5,11:13='PIC',<PICTURE_KEYWORD>,1:11]"
                + "[@6,14:14=' ',<WHITESPACE>,channel=99,1:14]"
                + "[@7,15:16='99',<PICTURE_PART>,1:15]"
                + "[@8,17:17='.',<PERIOD>,1:17]");
    }

    /**
     * A picture case where the period is separated by a space resulting
     * in lexer identifying the symbol string as an INT.
     */
    public void testPictureSeparateFromPeriod() {
        lexAndCheck("  01 MYVAR PIC 99 .",
                "[@0,0:1='  ',<WHITESPACE>,channel=99,1:0]"
                + "[@1,2:3='01',<INT>,1:2]"
                + "[@2,4:4=' ',<WHITESPACE>,channel=99,1:4]"
                + "[@3,5:9='MYVAR',<DATA_NAME>,1:5]"
                + "[@4,10:10=' ',<WHITESPACE>,channel=99,1:10]"
                + "[@5,11:13='PIC',<PICTURE_KEYWORD>,1:11]"
                + "[@6,14:14=' ',<WHITESPACE>,channel=99,1:14]"
                + "[@7,15:16='99',<PICTURE_PART>,1:15]"
                + "[@8,17:17=' ',<WHITESPACE>,channel=99,1:17]"
                + "[@9,18:18='.',<PERIOD>,1:18]");
    }

    /**
     * A picture clause with a decimal point. 
     */
    public void testPictureWithPeriod() {
        lexAndCheck("  01 MYVAR PIC 99.9.",
                "[@0,0:1='  ',<WHITESPACE>,channel=99,1:0]"
                + "[@1,2:3='01',<INT>,1:2]"
                + "[@2,4:4=' ',<WHITESPACE>,channel=99,1:4]"
                + "[@3,5:9='MYVAR',<DATA_NAME>,1:5]"
                + "[@4,10:10=' ',<WHITESPACE>,channel=99,1:10]"
                + "[@5,11:13='PIC',<PICTURE_KEYWORD>,1:11]"
                + "[@6,14:14=' ',<WHITESPACE>,channel=99,1:14]"
                + "[@7,15:16='99',<PICTURE_PART>,1:15]"
                + "[@8,17:17='.',<DECIMAL_POINT>,1:17]"
                + "[@9,18:18='9',<PICTURE_PART>,1:18]"
                + "[@10,19:19='.',<PERIOD>,1:19]");
    }

    /**
     * A picture clause with a CR or DB type of symbol. 
     */
    public void testPictureWithDoubleCharacters() {
        lexAndCheck("  01 MYVAR PIC 99.9CR.",
                "[@0,0:1='  ',<WHITESPACE>,channel=99,1:0]"
                + "[@1,2:3='01',<INT>,1:2]"
                + "[@2,4:4=' ',<WHITESPACE>,channel=99,1:4]"
                + "[@3,5:9='MYVAR',<DATA_NAME>,1:5]"
                + "[@4,10:10=' ',<WHITESPACE>,channel=99,1:10]"
                + "[@5,11:13='PIC',<PICTURE_KEYWORD>,1:11]"
                + "[@6,14:14=' ',<WHITESPACE>,channel=99,1:14]"
                + "[@7,15:16='99',<PICTURE_PART>,1:15]"
                + "[@8,17:17='.',<DECIMAL_POINT>,1:17]"
                + "[@9,18:20='9CR',<PICTURE_PART>,1:18]"
                + "[@10,21:21='.',<PERIOD>,1:21]");
    }

    /**
     * More than one statement. 
     */
    public void testMultipleStatements() {
        lexAndCheck(
                "  01 MYVAR1." + LS
                + " 02 MYVAR2 PIC X."
                ,
                "[@0,0:1='  ',<WHITESPACE>,channel=99,1:0]"
                + "[@1,2:3='01',<INT>,1:2]"
                + "[@2,4:4=' ',<WHITESPACE>,channel=99,1:4]"
                + "[@3,5:10='MYVAR1',<DATA_NAME>,1:5]"
                + "[@4,11:11='.',<PERIOD>,1:11]"
                + "[@5,12:13='\\r\\n',<NEWLINE>,channel=99,1:12]"
                + "[@6,14:14=' ',<WHITESPACE>,channel=99,2:0]"
                + "[@7,15:16='02',<INT>,2:1]"
                + "[@8,17:17=' ',<WHITESPACE>,channel=99,2:3]"
                + "[@9,18:23='MYVAR2',<DATA_NAME>,2:4]"
                + "[@10,24:24=' ',<WHITESPACE>,channel=99,2:10]"
                + "[@11,25:27='PIC',<PICTURE_KEYWORD>,2:11]"
                + "[@12,28:28=' ',<WHITESPACE>,channel=99,2:14]"
                + "[@13,29:29='X',<PICTURE_PART>,2:15]"
                + "[@14,30:30='.',<PERIOD>,2:16]");
    }

    /**
     * Multiple statements and a comment. 
     */
    public void testMultipleStatementsAndComments() {
        lexAndCheck(
                ""
                + "      * A comment." + LS
                + "       01 MYVAR1." + LS
                + "           02 MYVAR2 PIC 99.9."
                ,
                "[@0,0:5='      ',<WHITESPACE>,channel=99,1:0]"
                + "[@1,20:26='       ',<WHITESPACE>,channel=99,2:0]"
                + "[@2,27:28='01',<INT>,2:7]"
                + "[@3,29:29=' ',<WHITESPACE>,channel=99,2:9]"
                + "[@4,30:35='MYVAR1',<DATA_NAME>,2:10]"
                + "[@5,36:36='.',<PERIOD>,2:16]"
                + "[@6,37:38='\\r\\n',<NEWLINE>,channel=99,2:17]"
                + "[@7,39:49='           ',<WHITESPACE>,channel=99,3:0]"
                + "[@8,50:51='02',<INT>,3:11]"
                + "[@9,52:52=' ',<WHITESPACE>,channel=99,3:13]"
                + "[@10,53:58='MYVAR2',<DATA_NAME>,3:14]"
                + "[@11,59:59=' ',<WHITESPACE>,channel=99,3:20]"
                + "[@12,60:62='PIC',<PICTURE_KEYWORD>,3:21]"
                + "[@13,63:63=' ',<WHITESPACE>,channel=99,3:24]"
                + "[@14,64:65='99',<PICTURE_PART>,3:25]"
                + "[@15,66:66='.',<DECIMAL_POINT>,3:27]"
                + "[@16,67:67='9',<PICTURE_PART>,3:28]"
                + "[@17,68:68='.',<PERIOD>,3:29]");
    }

    /**
     * An hex string literal.
     */
    public void testHexLiteralString() {
        lexAndCheck(
                "value X\"FB\"" + LS
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:5=' ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,6:10='X\"FB\"',<HEX_LITERAL_STRING>,1:6]"
                + "[@3,11:12='\\r\\n',<NEWLINE>,channel=99,1:11]");
    }

    /**
     * An zero terminated string literal.
     */
    public void testZeroLiteralString() {
        lexAndCheck(
                "value Z'q'"
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:5=' ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,6:9='Z'q'',<ZERO_LITERAL_STRING>,1:6]");
    }

    /**
     * An DBCS string literal.
     */
    public void testDBCSLiteralString() {
        lexAndCheck(
                "value G'q'"
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:5=' ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,6:9='G'q'',<DBCS_LITERAL_STRING>,1:6]");
    }

    /**
     * An National string literal.
     */
    public void testNationalLiteralString() {
        lexAndCheck(
                "value N'q'"
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:5=' ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,6:9='N'q'',<NATIONAL_LITERAL_STRING>,1:6]");
    }

    /**
     * An Hex National string literal.
     */
    public void testHexNationalLiteralString() {
        lexAndCheck(
                "value NX'F5F7'"
                , "[@0,0:4='value',<VALUE_KEYWORD>,1:0]"
                + "[@1,5:5=' ',<WHITESPACE>,channel=99,1:5]"
                + "[@2,6:13='NX'F5F7'',<NATIONAL_HEX_LITERAL_STRING>,1:6]");
    }


    /**
     * An Decimal numeric literal.
     */
    public void testDecimalNumericLiteral() {
        lexAndCheck(
                "01 A VALUE 99.9."
                , "[@0,0:1='01',<INT>,1:0]"
                + "[@1,2:2=' ',<WHITESPACE>,channel=99,1:2]"
                + "[@2,3:3='A',<DATA_NAME>,1:3]"
                + "[@3,4:4=' ',<WHITESPACE>,channel=99,1:4]"
                + "[@4,5:9='VALUE',<VALUE_KEYWORD>,1:5]"
                + "[@5,10:10=' ',<WHITESPACE>,channel=99,1:10]"
                + "[@6,11:12='99',<INT>,1:11]"
                + "[@7,13:13='.',<DECIMAL_POINT>,1:13]"
                + "[@8,14:14='9',<INT>,1:14]"
                + "[@9,15:15='.',<PERIOD>,1:15]");
    }

    /**
     * An Float numeric literal.
     */
    public void testFloatNumericLiteral() {
        lexAndCheck(
                "01 A VALUE 99.9E56."
                , "[@0,0:1='01',<INT>,1:0]"
                + "[@1,2:2=' ',<WHITESPACE>,channel=99,1:2]"
                + "[@2,3:3='A',<DATA_NAME>,1:3]"
                + "[@3,4:4=' ',<WHITESPACE>,channel=99,1:4]"
                + "[@4,5:9='VALUE',<VALUE_KEYWORD>,1:5]"
                + "[@5,10:10=' ',<WHITESPACE>,channel=99,1:10]"
                + "[@6,11:12='99',<INT>,1:11]"
                + "[@7,13:13='.',<DECIMAL_POINT>,1:13]"
                + "[@8,14:17='9E56',<FLOAT_PART2>,1:14]"
                + "[@9,18:18='.',<PERIOD>,1:18]");

        lexAndCheck(
                "01 A VALUE IS -0.78E+23."
                , "[@0,0:1='01',<INT>,1:0]"
                + "[@1,2:2=' ',<WHITESPACE>,channel=99,1:2]"
                + "[@2,3:3='A',<DATA_NAME>,1:3]"
                + "[@3,4:4=' ',<WHITESPACE>,channel=99,1:4]"
                + "[@4,5:9='VALUE',<VALUE_KEYWORD>,1:5]"
                + "[@5,10:10=' ',<WHITESPACE>,channel=99,1:10]"
                + "[@6,13:13=' ',<WHITESPACE>,channel=99,1:13]"
                + "[@7,14:15='-0',<SIGNED_INT>,1:14]"
                + "[@8,16:16='.',<DECIMAL_POINT>,1:16]"
                + "[@9,17:22='78E+23',<FLOAT_PART2>,1:17]"
                + "[@10,23:23='.',<PERIOD>,1:23]");
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

}
