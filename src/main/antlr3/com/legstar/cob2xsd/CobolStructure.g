grammar CobolStructure;
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
 * Produces an Abstract Syntax Tree
 *------------------------------------------------------------------*/
options {
  output = AST;
}

/*------------------------------------------------------------------
 * Imaginary nodes
 *------------------------------------------------------------------*/
tokens {
    DATA_ITEM;
    LEVEL;
    NAME;
    RENAME;
    RANGE;
    LITERAL;
    CONDITION;
    REDEFINES;
    BLANKWHENZERO;
    EXTERNAL;
    GLOBAL;
    GROUPUSAGENATIONAL;
    JUSTIFIEDRIGHT;
    INDEX;
    KEY;
    FIXEDARRAY;
    VARARRAY;
    HBOUND;
    LBOUND;
    DEPENDINGON;
    PICTURE;
    PICTURESTRING;
    SIGN;
    LEADING;
    TRAILING;
    SEPARATE;
    SYNCHRONIZED;
    RIGHT;
    LEFT;
    USAGE;
    BINARY;
    SINGLEFLOAT;
    DOUBLEFLOAT;
    PACKEDDECIMAL;
    NATIVEBINARY;
    DISPLAY;
    DISPLAY1;
    INDEX;
    NATIONAL;
    POINTER;
    PROCEDUREPOINTER;
    FUNCTIONPOINTER;
    VALUE;
    LITERALDECIMALSTRING;
    LITERALFLOATSTRING;
    DATEFORMAT;
}

/*------------------------------------------------------------------
 * Java overrides
 *------------------------------------------------------------------*/
@lexer::header {
package com.legstar.cob2xsd;
import java.util.Hashtable;
import java.util.Map;
}
@header {
package com.legstar.cob2xsd;
}

@lexer::members {
    /** Keeps track of the last COBOL keyword recognized. This helps
        disambiguate lexing rules. */
    private int lastKeyword = PERIOD;
}
@members {
    
    /**
     * Checks if a string contains numerics which fall in a given range.
     * @param str the string holding a numeric value
     * @param lower the lower bound
     * @param higher the upper bound
     * @return true if the string holds a numeric in the range
     */
    public boolean inRange(final String str, final int lower, final int higher) {
        if (str != null && str.length() > 0) {
            try {
                int v = Integer.parseInt(str);
                if (v >= lower && v <= higher) {
                    return true;
                }
            } catch (NumberFormatException e) {
                return false;
            }
        }
        return false;
    }

    /** This AST, built by custom actions, is a hierarchy of DATA_ITEM based on their LEVEL. */
    private Object hTree ;
    
    /** The last subtree built from a DATA_ITEM. */
    private Object hLast;
    
    /**
     * Get the LEVEL for a DATA_ITEM subtree.
     * @param tree the DATA_ITEM subtree
     * @return the DATA_ITEM level
     */
    public int getLevel(final Object tree) {
        Tree level = ((CommonTree) tree).getFirstChildWithType(LEVEL);
        if (level == null) {
            return -1;
        }
        return Integer.parseInt(level.getChild(0).getText());
    }
    
    /**
     * Search the tree hierarchy for the first node with a lower level than the one received.
     * Level 66 renames are a special case where we look for a level 01 to hook them to.
     * @param tree the DATA_ITEM subtree to start from (moving up)
     * @param level the level for which we are looking for a suitable parent
     * @return the DATA_ITEM which can be used as a parent or the Nil node
     */
    public Object getParent(final Object tree, final int level) {
        if (getTreeAdaptor().isNil(tree)) {
            return tree;
        } else {
            int targetLevel = (level == 66 ) ? 02 : level; 
            if (getLevel(tree) < targetLevel) {
                return tree;
            } else {
                return getParent(getTreeAdaptor().getParent(tree), level);
            }
        }
    }

}

/*------------------------------------------------------------------
 * Parser grammar
/*------------------------------------------------------------------
 * Data items are manually added to a hierarchical AST to reproduce
 * the COBOL level hierarchy.
 *------------------------------------------------------------------*/
cobdata
@init {
    /* Initialize the hierarchical AST which will replace the standard one. */
    hTree = getTreeAdaptor().nil();
    hLast = hTree;

}
     :  (data_items)*
     ->{hTree}
     ;
    
/* 
   The AST built from this rule is manually added to the hierarchical AST.
   We override the normal tree rewriting mechanism because it would create
   a flat list of data items, disregarding their level.
*/
data_items
    :   data_entry
        {
            Object parent = getParent(hLast, getLevel($data_entry.tree));
            getTreeAdaptor().addChild(parent, $data_entry.tree);
            hLast = $data_entry.tree;
        }
        ->
    ;

data_entry
    :   data_description_entry
    |   rename_description_entry
    |   condition_description_entry
    ;

/*------------------------------------------------------------------
 * Regular data item entries such as 01 A PIC X.
 *------------------------------------------------------------------*/
data_description_entry 
    :   data_item_level DATA_NAME? clauses* PERIOD
    ->^(DATA_ITEM data_item_level ^(NAME DATA_NAME)? clauses*)
    ;
  
data_item_level
    :   {inRange(input.LT(1).getText(), 1, 50) || inRange(input.LT(1).getText(), 77, 77)}?=> INT
    ->^(LEVEL INT)
    ;

/*------------------------------------------------------------------
 * A rename expression such as: 66 NEWN RENAMES OLD.
 *------------------------------------------------------------------*/
rename_description_entry
    :   rename_level v=DATA_NAME RENAMES_KEYWORD w=DATA_NAME THROUGH_KEYWORD x=DATA_NAME PERIOD
    ->^(RENAME rename_level ^(NAME $v) ^(RANGE $w $x))
    |   rename_level v=DATA_NAME RENAMES_KEYWORD w=DATA_NAME PERIOD
    ->^(RENAME rename_level ^(NAME $v) ^(LITERAL $w))
    ; 

rename_level
    :   {inRange(input.LT(1).getText(), 66, 66)}?=> INT
    ->^(LEVEL INT)
    ;

/*------------------------------------------------------------------
 * A condition such as: 88 TRUE VALUE 1.
 *------------------------------------------------------------------*/
condition_description_entry
    :   condition_level DATA_NAME condition_name_values PERIOD
    ->^(CONDITION condition_level ^(NAME DATA_NAME) condition_name_values)
    ; 

condition_level
    :   {inRange(input.LT(1).getText(), 88, 88)}?=> INT
    ->^(LEVEL INT)
    ;

condition_name_values
    :   VALUE_KEYWORD (IS_KEYWORD | ARE_KEYWORD)? (v+=condition_name_value)+
    ->^($v)+
    ;
    
condition_name_value
    :   v=literal
       (
           THROUGH_KEYWORD w=literal ->^(RANGE $v $w?)
           |                         ->^(LITERAL $v)
        )
    ;

/*------------------------------------------------------------------
 * Regular data description entry clauses.
 *------------------------------------------------------------------*/
clauses 
    :   redefines_clause
    |   blank_when_zero_clause
    |   external_clause
    |   global_clause
    |   group_usage_clause
    |   justified_clause
    |   occurs_clause
    |   picture_clause
    |   sign_clause
    |   synchronized_clause
    |   usage_clause
    |   value_clause
    |   date_format_clause
    ; 

redefines_clause
    :   REDEFINES_KEYWORD DATA_NAME
    ->^(REDEFINES DATA_NAME)
    ;

blank_when_zero_clause
    :   BLANK_KEYWORD WHEN_KEYWORD? ZERO_KEYWORD
    ->^(BLANKWHENZERO)
    ;

external_clause
    :   EXTERNAL_KEYWORD
    ->^(EXTERNAL)
    ;

global_clause
    :   GLOBAL_KEYWORD
    ->^(GLOBAL)
    ;

group_usage_clause
    :   GROUP_USAGE_KEYWORD IS_KEYWORD? NATIONAL_KEYWORD
    ->^(GROUPUSAGENATIONAL)
    ;

justified_clause
    :   JUSTIFIED_KEYWORD RIGHT_KEYWORD?
    ->^(JUSTIFIEDRIGHT)
    ;

occurs_clause
    :   fixed_length_table
    |   variable_length_table
    ;

picture_clause
    :   PICTURE_KEYWORD IS_KEYWORD? picture_string
    ->^(PICTURE picture_string)
    ;

sign_clause
    :   (SIGN_KEYWORD IS_KEYWORD?)? (sign_leading_clause | sign_trailing_clause)
    ->^(SIGN sign_leading_clause? sign_trailing_clause?)
    ;
    
sign_leading_clause
    :   SIGN_LEADING_KEYWORD separate_clause?
    ->^(LEADING separate_clause?)
    ;
    
sign_trailing_clause
    :   SIGN_TRAILING_KEYWORD separate_clause?
    ->^(TRAILING separate_clause?)
    ;
    
separate_clause
    :   SEPARATE_KEYWORD CHARACTER_KEYWORD?
    ->^(SEPARATE)
    ;

synchronized_clause
    :   SYNCHRONIZED_KEYWORD
        (LEFT_KEYWORD ->^(SYNCHRONIZED LEFT)
         | RIGHT_KEYWORD ->^(SYNCHRONIZED RIGHT)
         |               ->^(SYNCHRONIZED)
        )
    ;

usage_clause
    :   (USAGE_KEYWORD IS_KEYWORD?)?
        (
          BINARY_KEYWORD            ->^(USAGE BINARY)
        | SINGLE_FLOAT_KEYWORD      ->^(USAGE SINGLEFLOAT)
        | DOUBLE_FLOAT_KEYWORD      ->^(USAGE DOUBLEFLOAT)
        | PACKED_DECIMAL_KEYWORD    ->^(USAGE PACKEDDECIMAL)
        | NATIVE_BINARY_KEYWORD     ->^(USAGE NATIVEBINARY)
        | DISPLAY_KEYWORD           ->^(USAGE DISPLAY)
        | DISPLAY_1_KEYWORD         ->^(USAGE DISPLAY1)
        | INDEX_KEYWORD             ->^(USAGE INDEX)
        | NATIONAL_KEYWORD          ->^(USAGE NATIONAL)
        | POINTER_KEYWORD           ->^(USAGE POINTER)
        | PROCEDURE_POINTER_KEYWORD ->^(USAGE PROCEDUREPOINTER)
        | FUNCTION_POINTER_KEYWORD  ->^(USAGE FUNCTIONPOINTER)
        )
    ;

value_clause
    : VALUE_KEYWORD IS_KEYWORD? literal
    ->^(VALUE literal)
    ;
    
literal
    : (float_literal)=> float_literal
    | (decimal_literal)=> decimal_literal
    | INT
    | SIGNED_INT
    | ALPHANUM_LITERAL_STRING
    | HEX_LITERAL_STRING
    | ZERO_LITERAL_STRING
    | DBCS_LITERAL_STRING
    | NATIONAL_LITERAL_STRING
    | NATIONAL_HEX_LITERAL_STRING
    ;

date_format_clause
    : DATE_KEYWORD IS_KEYWORD? DATE_PATTERN
    ->^(DATEFORMAT DATE_PATTERN)
    ;
  
/*------------------------------------------------------------------
 * Arrays
 *------------------------------------------------------------------*/
fixed_length_table
    : OCCURS_KEYWORD INT TIMES_KEYWORD? (key_clause)* (index_clause)*
    ->^(FIXEDARRAY ^(HBOUND INT) key_clause* index_clause*)
    ;               

variable_length_table
    : (OCCURS_KEYWORD low_bound)=>OCCURS_KEYWORD low_bound hb=INT TIMES_KEYWORD? DEPENDING_KEYWORD ON_KEYWORD? DATA_NAME (key_clause)* (index_clause)*
    ->^(VARARRAY low_bound ^(HBOUND $hb ^(DEPENDINGON DATA_NAME)) key_clause* index_clause*)
    | OCCURS_KEYWORD hb=INT TIMES_KEYWORD? DEPENDING_KEYWORD ON_KEYWORD? DATA_NAME (key_clause)* (index_clause)*
    ->^(VARARRAY ^(HBOUND $hb ^(DEPENDINGON DATA_NAME)) key_clause* index_clause*)
    ;
    
low_bound
    :   INT TO_KEYWORD 
    ->^(LBOUND INT)
    ;         

key_clause
    : (v=ASCENDING_KEYWORD | v=DESCENDING_KEYWORD) KEY_KEYWORD? IS_KEYWORD? DATA_NAME+
    ->^(KEY $v DATA_NAME)+
    ;
  
index_clause
    : INDEXED_KEYWORD BY_KEYWORD? DATA_NAME+
    ->^(INDEX DATA_NAME)+
    ; 
  
/*------------------------------------------------------------------
 * Picture strings are a special case that is handled as a parser
 * rule when it belongs to the lexer really.
 * The problem is that picture values might contain decimal points
 * that the lexer normally recognizes as sentence delimiters.
 * What we do is emitting a DECIMAL_POINT imaginary token when we
 * encounter this situation instead of a PERIOD. Because DECIMAL_POINT
 * is an imaginary token, only parser rules can handle it. 
 * At tree construction we concatenate the various parts of the
 * picture string which might have been split by the decimal point.
 *------------------------------------------------------------------*/
picture_string
@init {
    StringBuilder sb = new StringBuilder();
}
    :   (v+=PICTURE_PART | v+=DECIMAL_POINT)+
    {
            for (Object o : $v) {
                sb.append(((Token) o).getText());
            }
    }
    ->{new CommonTree(new CommonToken(PICTURESTRING,sb.toString()))}
    ;
    
/*------------------------------------------------------------------
 * Decimals include a DECIMAL_POINT. We need a parser rule to
 * recognize and reconstruct a literal from its parts.
 *------------------------------------------------------------------*/
decimal_literal
@init {
    StringBuilder sb = new StringBuilder();
}
    : (v=SIGNED_INT | v=INT) DECIMAL_POINT w=INT
    {
        if ($v != null) {
            sb.append($v.getText());
        }
        sb.append('.');
        sb.append($w.getText());
    }
    ->{new CommonTree(new CommonToken(LITERALDECIMALSTRING,sb.toString()))}
    ;

/*------------------------------------------------------------------
 * Float literals include a DECIMAL_POINT. We need a parser rule to
 * recognize and reconstruct a literal from its parts.
 *------------------------------------------------------------------*/
float_literal
@init {
    StringBuilder sb = new StringBuilder();
}
    : (v=SIGNED_INT | v=INT) DECIMAL_POINT w=FLOAT_PART2
    {
        if ($v != null) {
            sb.append($v.getText());
        }
        sb.append('.');
        sb.append($w.getText());
    }
    ->{new CommonTree(new CommonToken(LITERALFLOATSTRING,sb.toString()))}
    ;

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
