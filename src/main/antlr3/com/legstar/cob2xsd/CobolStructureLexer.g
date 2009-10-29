lexer grammar CobolStructureLexer;
/*------------------------------------------------------------------
 * Built from IBM Entreprise COBOL V3R4
 * Parses COBOL WORKING-STORAGE or LINKAGE-SECTION statements only.
 * Source must be cleaned up, nothing but whitespaces should appear
 * before column 7 and after column 72.
 * This is not a validating recognizer. COBOL code is assumed to be
 * valid.
 * TODO:
 * ----
 * Separators such as ', ' or '; ' should be allowed wherever space is a separator
 * Handle Currency signs other than $
 * Handle Decimal point is comma
 * Renames should follow the structure they rename
 * Handle NSYMBOL(DBCS) versus NSYMBOL(NATIONAL)
 *------------------------------------------------------------------*/
/*------------------------------------------------------------------
 * Fuzzy lexing (ignore what is not recognized)
 *------------------------------------------------------------------*/

/*------------------------------------------------------------------
 * Java overrides
 *------------------------------------------------------------------*/
@header {
package com.legstar.cob2xsd;
}

@members {
    /** Keeps track of the last COBOL keyword recognized. This helps
        disambiguate lexing rules. */
    private int lastKeyword = PERIOD;
}
/*------------------------------------------------------------------
 * Lexer grammar
 *------------------------------------------------------------------*/
/*------------------------------------------------------------------
 * Keywords
 *------------------------------------------------------------------*/
RENAMES_KEYWORD           : 'RENAMES' {lastKeyword = $type;};
THROUGH_KEYWORD           : ('THROUGH' | 'THRU') {lastKeyword = $type;};
REDEFINES_KEYWORD         : 'REDEFINES' {lastKeyword = $type;};  
BLANK_KEYWORD             : 'BLANK' {lastKeyword = $type;};
WHEN_KEYWORD              : 'WHEN' {skip();};
ZERO_KEYWORD              : 'ZERO' ('S' | 'ES')? {lastKeyword = $type;};
EXTERNAL_KEYWORD          : 'EXTERNAL' {lastKeyword = $type;};
GLOBAL_KEYWORD            : 'GLOBAL' {lastKeyword = $type;};
GROUP_USAGE_KEYWORD       : 'GROUP-USAGE' {lastKeyword = $type;};
IS_KEYWORD                : 'IS' {skip();};
ARE_KEYWORD               : 'ARE' {skip();};
NATIONAL_KEYWORD          : 'NATIONAL' {lastKeyword = $type;};
JUSTIFIED_KEYWORD         : 'JUSTIFIED' | 'JUST' {lastKeyword = $type;};
RIGHT_KEYWORD             : 'RIGHT' {lastKeyword = $type;};
OCCURS_KEYWORD            : 'OCCURS' {lastKeyword = $type;};
TIMES_KEYWORD             : 'TIMES' {skip();};
TO_KEYWORD                : 'TO' {lastKeyword = $type;};
ASCENDING_KEYWORD         : 'ASCENDING' {lastKeyword = $type;};
DESCENDING_KEYWORD        : 'DESCENDING' {lastKeyword = $type;};
KEY_KEYWORD               : 'KEY' {lastKeyword = $type;};
INDEXED_KEYWORD           : 'INDEXED' {lastKeyword = $type;};
BY_KEYWORD                : 'BY' {skip();};
PICTURE_KEYWORD           : 'PIC' ('TURE')? {lastKeyword = $type;};
DEPENDING_KEYWORD         : 'DEPENDING' {lastKeyword = $type;};
ON_KEYWORD                : 'ON' {skip();};
SIGN_KEYWORD              : 'SIGN' {skip();};
SIGN_LEADING_KEYWORD      : 'LEADING' {lastKeyword = $type;};
SIGN_TRAILING_KEYWORD     : 'TRAILING' {lastKeyword = $type;};
SEPARATE_KEYWORD          : 'SEPARATE' {lastKeyword = $type;};
CHARACTER_KEYWORD         : 'CHARACTER' {skip();};
SYNCHRONIZED_KEYWORD      : 'SYNC' ('HRONIZED')? {lastKeyword = $type;};
LEFT_KEYWORD              : 'LEFT' {lastKeyword = $type;};
USAGE_KEYWORD             : 'USAGE' {lastKeyword = $type;};
BINARY_KEYWORD            : ('BINARY' | 'COMP' ('UTATIONAL')?) {lastKeyword = $type;};  
SINGLE_FLOAT_KEYWORD      : ('COMPUTATIONAL-1' | 'COMP-1') {lastKeyword = $type;}; 
DOUBLE_FLOAT_KEYWORD      : ('COMPUTATIONAL-2' | 'COMP-2') {lastKeyword = $type;};
PACKED_DECIMAL_KEYWORD    : ('PACKED-DECIMAL' | 'COMPUTATIONAL-3' | 'COMP-3') {lastKeyword = $type;};
NATIVE_BINARY_KEYWORD     : ('COMPUTATIONAL-5' | 'COMP-5') {lastKeyword = $type;};
DISPLAY_KEYWORD           : 'DISPLAY' {lastKeyword = $type;};
DISPLAY_1_KEYWORD         : 'DISPLAY-1' {lastKeyword = $type;};
INDEX_KEYWORD             : 'INDEX' {lastKeyword = $type;};
POINTER_KEYWORD           : 'POINTER' {lastKeyword = $type;};
PROCEDURE_POINTER_KEYWORD : 'PROCEDURE-POINTER' {lastKeyword = $type;};
FUNCTION_POINTER_KEYWORD  : 'FUNCTION-POINTER' {lastKeyword = $type;};
VALUE_KEYWORD             : 'VALUE' ('S')? {lastKeyword = $type;};
DATE_KEYWORD              : 'DATE FORMAT' {lastKeyword = $type;};
  
/*------------------------------------------------------------------
 * Period is the data entry delimiter.
 * It might also appear in a PICTURE clause, FLOAT or DECIMAL literal.
 * Fortunately in these cases, it cannot appear as the last character
 * The action here detects these cases and dynamically retype the
 * token produced by the lexer.
 *------------------------------------------------------------------*/
PERIOD
    :
    {
        /* If this period is not followed by a space or a newline, then we consider
         * it is a decimal point and not to be used as a sentence delimiter.*/
        if (input.LA(2) != ' ' && input.LA(2) != '\r' && input.LA(2) != '\n' && input.LA(2) != -1) {
            $type = DECIMAL_POINT;
        } else {
            /* This will set the context as the end of a data entry */
            lastKeyword = PERIOD;
        }
    } '.'
    ;

/*------------------------------------------------------------------
 * Integer literals
 * We may have to retype these tokens which have a broad pattern,
 * depending on context.
 *------------------------------------------------------------------*/
INT :   '0'..'9'+
    {
        if (lastKeyword == PICTURE_KEYWORD) {
            $type = PICTURE_PART;
        }
    }
    ;

SIGNED_INT
    : ('+' | '-') '0'..'9'+
    {
        if (lastKeyword == PICTURE_KEYWORD) {
            $type = PICTURE_PART;
        }
    }
    ;

/*------------------------------------------------------------------
 * Floating point literals are fragmented because DECIMAL_POINT
 * occurs in the mantissa. The first part of the floating point is
 * recognized as an INT or SIGNED_IT and the second part, wich holds
 * at least of digit of the mantissa decimal part and the following
 * exponent is recognized here.
 *------------------------------------------------------------------*/
FLOAT_PART2
    : '0'..'9'+ 'E' ('+' | '-')? '0'..'9'+
    {
        if (lastKeyword == PICTURE_KEYWORD) {
            $type = PICTURE_PART;
        }
    }
    ;

/*------------------------------------------------------------------
 * Date pattern
 * A date pattern such as XX is ambiguous for the lexer because it
 * can also be a DATA_NAME or a PICTURE_STRING. By declaring
 * DATE_PATTERN first we hush the lexer complaining. But now, 
 * everytime the lexer encounters XX it will assume a DATE_PATTERN
 * The post action retypes the token according to context.
 *------------------------------------------------------------------*/
DATE_PATTERN
    : ('X'|'Y')+
    {
        if (lastKeyword != DATE_KEYWORD) {
            if (lastKeyword == PICTURE_KEYWORD) {
                $type = PICTURE_PART;
            } else {
                $type = DATA_NAME;
            }
        }
    }
    ;

/*------------------------------------------------------------------
 * Data item names
 * A data name such as ABE is ambiguous because it might as well be
 * a PICTURE_STRING. We retype the token if that's the case.
 *------------------------------------------------------------------*/
DATA_NAME
    : LETTER (LETTER|'0'..'9'|'-')*
    {
        if (lastKeyword == PICTURE_KEYWORD) {
            $type = PICTURE_PART;
        }
    }
    ;

/*------------------------------------------------------------------
 * Picture value
 * This might be a part from a picture value in case a decimal point
 * is detected. For a PICTURE such as 99.9 the lexer will recognize
 * 3 tokens : PICTURE_STRING DECIMAL_POINT PICTURE_STRING.
 * The complete picture string is reconstructed by the 
 * picture_string parser rule.
 *------------------------------------------------------------------*/
PICTURE_PART
    : PICTURE_CHAR+
    ;
    
/*------------------------------------------------------------------
 * In addition to the characters listed here, a PICTURE can also
 * contain a DECIMAL point. We can't list it here though because
 * the lexer would get confused (period is also the sentence delimiter).
 * TODO $ should not be the only currency sign supported
 *------------------------------------------------------------------*/
fragment
PICTURE_CHAR
    : ('A' | 'B' | 'E' | 'G' | 'N' | 'P' | 'S' | 'V' | 'X' | 'Z' | '9' | '0' | '/' | ',' | '+' | 'C' | 'R' | 'D' | '-' | '*' | '$' | '(' | ')' )
    ;

/*------------------------------------------------------------------
 * Alphanumeric literals are delimited by QUOTE or APOST
 * Escaping is done by duplicating the delimiter. For instance,
 * "aaa""bbbb" is a valid COBOL literal string.
 * Strings can be continued on multiple lines in which case:
 * - The continued line does not terminate with a delimiter
 * - The continuation line has a '-' in column 7
 * when we concatenate fragments from multiple lines, we end up
 * things like "aaa\n  - "bbb" which we manually clean up to become
 * "aaabbb"
 *------------------------------------------------------------------*/
ALPHANUM_LITERAL_STRING
    :   ALPHANUM_LITERAL_FRAGMENT+
    {setText(getText().replaceAll("(\\r)?\\n(\\s)*\\-(\\s)*(\"|\')",""));}
    ;

fragment
ALPHANUM_LITERAL_FRAGMENT
    :   QUOTE (options {greedy=false;} : .)* ( QUOTE | CONTINUED_ALPHANUM_LITERAL_FRAGMENT)
    |   APOST (options {greedy=false;} : .)* ( APOST | CONTINUED_ALPHANUM_LITERAL_FRAGMENT)
    ;

fragment
CONTINUED_ALPHANUM_LITERAL_FRAGMENT
  :  NEWLINE WHITESPACE CONTINUATION_CHAR WHITESPACE? ALPHANUM_LITERAL_FRAGMENT+
  ;

fragment
CONTINUATION_CHAR
    :   {getCharPositionInLine() == 6}?=> '-'
    ;
    
/*------------------------------------------------------------------
 * Hexadecimal literal strings
 *------------------------------------------------------------------*/
HEX_LITERAL_STRING
    :   'X' ALPHANUM_LITERAL_STRING
    ;

/*------------------------------------------------------------------
 * Zero terminated literal strings
 *------------------------------------------------------------------*/
ZERO_LITERAL_STRING
    :   'Z' ALPHANUM_LITERAL_STRING
    ;

/*------------------------------------------------------------------
 * DBCS literal strings
 *------------------------------------------------------------------*/
DBCS_LITERAL_STRING
    :   'G' ALPHANUM_LITERAL_STRING
    ;

/*------------------------------------------------------------------
 * National literal strings
 *------------------------------------------------------------------*/
NATIONAL_LITERAL_STRING
    :   'N' ALPHANUM_LITERAL_STRING
    ;

/*------------------------------------------------------------------
 * National hexadecimal literal strings
 *------------------------------------------------------------------*/
NATIONAL_HEX_LITERAL_STRING
    :   'NX' ALPHANUM_LITERAL_STRING
    ;


/*------------------------------------------------------------------
 * Comments start with '*' or '/' in column 7
 *------------------------------------------------------------------*/
COMMENT options { greedy = false; }
    : {getCharPositionInLine() == 6}?=> (ASTERISK | FSLASH) .* NEWLINE
    { skip(); }
    ;

/*------------------------------------------------------------------
 * Whitespaces, newlines are kept but hidden
 *------------------------------------------------------------------*/
WHITESPACE
    :   SPACE+ { $channel=HIDDEN; }
    ;

NEWLINE
    :   ('\r'? '\n')+  { $channel=HIDDEN; }
    ;

/*------------------------------------------------------------------
 * Fragments
 *------------------------------------------------------------------*/
fragment LETTER     : 'A'..'Z'| 'a'..'z';
fragment SPACE      : ' ' | '\t';
fragment QUOTE      : '"';
fragment APOST      : '\'';
fragment ASTERISK   : '*';
fragment FSLASH     : '/';
/*------------------------------------------------------------------
 * DECIMAL_POINT is reported as a fragment so that the lexer code
 * generator does not complain when it is referenced from other
 * rules. In reality it is an imaginary token set in some cases
 * when a PERIOD is recognized (see PERIOD rule).
 *------------------------------------------------------------------*/
fragment
DECIMAL_POINT 
    :  '.'
    ;

/*------------------------------------------------------------------
 * A non recognized character is dropped
 *------------------------------------------------------------------*/
EVERYTHING_ELSE: . {skip();};