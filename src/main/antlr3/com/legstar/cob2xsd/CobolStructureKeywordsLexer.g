lexer grammar CobolStructureKeywordsLexer;
options {
    filter=true;
}
/*------------------------------------------------------------------
 * This lexer grammar is a secondary lexer used to refine identifiers.
 * A primary lexer calls this one with single token text. If there
 * is a match with one of the keywords here, then a more precise token
 * is returned.
 * This considerably reduces the complexity of the primary lexer when
 * there are lots of keywords.
 * Some keywords are not mandatory (COBOL is full of IS, ARE and WHERE
 * which are supposed to make the language english like but server no
 * other purpose). Such keywords would normally simply be skipped
 * but since we are only a secondary lexer (only the primary one can skip)
 * we need to inform the primary when an optional keyword is encountered
 * hence the use of SKIP_TOKEN.
 *------------------------------------------------------------------*/
/*------------------------------------------------------------------
 * Java overrides
 *------------------------------------------------------------------*/
@header {
package com.legstar.cob2xsd;
}

/*------------------------------------------------------------------
 * COBOL Structures keywords
 *------------------------------------------------------------------*/
RENAMES_KEYWORD           : 'RENAMES';
THROUGH_KEYWORD           : ('THROUGH' | 'THRU');
REDEFINES_KEYWORD         : 'REDEFINES';  
BLANK_KEYWORD             : 'BLANK';
WHEN_KEYWORD              : 'WHEN' {$type = Token.SKIP_TOKEN.getType();};
EXTERNAL_KEYWORD          : 'EXTERNAL';
GLOBAL_KEYWORD            : 'GLOBAL';
GROUP_USAGE_KEYWORD       : 'GROUP-USAGE';
IS_KEYWORD                : 'IS' {$type = Token.SKIP_TOKEN.getType();};
ARE_KEYWORD               : 'ARE' {$type = Token.SKIP_TOKEN.getType();};
NATIONAL_KEYWORD          : 'NATIONAL';
JUSTIFIED_KEYWORD         : 'JUSTIFIED' | 'JUST';
RIGHT_KEYWORD             : 'RIGHT';
OCCURS_KEYWORD            : 'OCCURS';
TIMES_KEYWORD             : 'TIMES' {$type = Token.SKIP_TOKEN.getType();};
TO_KEYWORD                : 'TO';
ASCENDING_KEYWORD         : 'ASCENDING';
DESCENDING_KEYWORD        : 'DESCENDING';
KEY_KEYWORD               : 'KEY';
INDEXED_KEYWORD           : 'INDEXED';
BY_KEYWORD                : 'BY' {$type = Token.SKIP_TOKEN.getType();};
PICTURE_KEYWORD           : 'PIC' ('TURE')?;
DEPENDING_KEYWORD         : 'DEPENDING';
ON_KEYWORD                : 'ON' {$type = Token.SKIP_TOKEN.getType();};
SIGN_KEYWORD              : 'SIGN' {$type = Token.SKIP_TOKEN.getType();};
SIGN_LEADING_KEYWORD      : 'LEADING';
SIGN_TRAILING_KEYWORD     : 'TRAILING';
SEPARATE_KEYWORD          : 'SEPARATE';
CHARACTER_KEYWORD         : 'CHARACTER' {$type = Token.SKIP_TOKEN.getType();};
SYNCHRONIZED_KEYWORD      : 'SYNC' ('HRONIZED')?;
LEFT_KEYWORD              : 'LEFT';
USAGE_KEYWORD             : 'USAGE';
SINGLE_FLOAT_KEYWORD      : ('COMPUTATIONAL-1' | 'COMP-1'); 
DOUBLE_FLOAT_KEYWORD      : ('COMPUTATIONAL-2' | 'COMP-2');
NATIVE_BINARY_KEYWORD     : ('COMPUTATIONAL-5' | 'COMP-5');
PACKED_DECIMAL_KEYWORD    : ('PACKED-DECIMAL' | 'COMPUTATIONAL-3' | 'COMP-3');
BINARY_KEYWORD            : ('BINARY' | 'COMP' ('UTATIONAL')?);  
DISPLAY_1_KEYWORD         : 'DISPLAY-1';
DISPLAY_KEYWORD           : 'DISPLAY';
INDEX_KEYWORD             : 'INDEX';
POINTER_KEYWORD           : 'POINTER';
PROCEDURE_POINTER_KEYWORD : 'PROCEDURE-POINTER';
FUNCTION_POINTER_KEYWORD  : 'FUNCTION-POINTER';
VALUE_KEYWORD             : 'VALUE' ('S')?;
DATE_KEYWORD              : 'DATE' {$type = Token.SKIP_TOKEN.getType();};
DATE_FORMAT_KEYWORD       : 'FORMAT';
 
/*------------------------------------------------------------------
 * Figurative constants
 * ZERO_CONSTANT is also a keyword in BLANK WHEN ZERO
 *------------------------------------------------------------------*/
ZERO_CONSTANT             : 'ZERO' ('S' | 'ES')?;
SPACE_CONSTANT            : 'SPACE' ('S')?;
HIGH_VALUE_CONSTANT       : 'HIGH-VALUE' ('S')?;
LOW_VALUE_CONSTANT        : 'LOW-VALUE' ('S')?;
QUOTE_CONSTANT            : 'QUOTE' ('S')?;
ALL_CONSTANT              : 'ALL';
NULL_CONSTANT             : 'NULL' ('S')?;
