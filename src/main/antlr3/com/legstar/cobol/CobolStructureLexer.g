lexer grammar CobolStructureLexer;
/*------------------------------------------------------------------
 * Lexer grammar for COBOL structures.
 * Built from IBM Entreprise COBOL V3R4
 * COBOL keywords recognition is delegated to a secondary lexer.
 *------------------------------------------------------------------*/
options {
	tokenVocab=CobolStructureKeywordsLexer;
}
/*------------------------------------------------------------------
 * Java overrides
 *------------------------------------------------------------------*/
@header {
package com.legstar.cob2xsd;
import java.io.IOException;
import java.io.StringReader;
import com.legstar.antlr.ANTLRNoCaseReaderStream;
}

@members {
    /** Keeps track of the last COBOL keyword recognized. This helps
        disambiguate lexing rules. */
    private int lastKeyword = PERIOD;

    /** Secondary lexer specializing in keyword recognition.*/
    private CobolStructureKeywordsLexerImpl keywordLexer
            = new CobolStructureKeywordsLexerImpl();

    /**
     * Asks secondary lexer to check if text is a keyword. If there is a match,
     * makes sure the entire text was matched (as opposed to a substring).
     * If the text matches a keyword that needs to be skipped, skip.
     * @param text the text to match with keywords
     * @param originalType the initial token type
     * @return the keyword type if a match is found otherwise the original type
     * @throws RecognitionException if failed to call secondary lexer
     */
    public int matchKeywords(
            final String text,
            final int originalType) throws RecognitionException {
        try {
            int type = originalType;
            String str = trimSeparator(text);
            keywordLexer.setCharStream( new ANTLRNoCaseReaderStream(
                new StringReader(str)));
            CommonTokenStream kTokens = new CommonTokenStream(keywordLexer);
            List < ? > kTokenl = kTokens.getTokens();
            if (kTokenl.size() > 0) {
                CommonToken kToken = (CommonToken) kTokenl.get(0);
                if (kToken.getText().length() == str.length()) {
                    if (kToken.getType() == Token.SKIP_TOKEN.getType()) {
                        skip();
                    } else {
                        type = kToken.getType();
                        lastKeyword = type;
                    }
                }
            }
            return type;

        } catch (IOException e) {
            throw new RecognitionException(input);
        }
    }
    
    /**
     * COBOL accepts ', ' and '; ' as clause separators. This method
     * removes this separators when they get concatenated to a token text.  
     * @param text the token text
     * @return the text without the trailing separator
     */
    public String trimSeparator(final String text) {
        if (text.length() > 0) {
            char c = text.charAt(text.length() - 1);
            if (c == ',' || c == ';') {
                return text.substring(0, text.length() - 1);
            }
        }
        return text;
    }
}
/*------------------------------------------------------------------
 * Lexer grammar
 *------------------------------------------------------------------*/
/*------------------------------------------------------------------
 * Period is the data entry delimiter.
 * It might also appear in a PICTURE clause, FLOAT or DECIMAL literal.
 * Fortunately in these cases, it cannot appear as the last character.
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
 * recognized as an INT or SIGNED_INT and the second part, wich holds
 * at least one digit of the mantissa decimal part and the following
 * exponent, is recognized here.
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
        if (lastKeyword != DATE_FORMAT_KEYWORD) {
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
 * All COBOL keywords fall into this category. Since COBOL keywords
 * are reserved and cannot be used as DATA_NAME, we check with an
 * auxiliary parser for a match and change the token type accordingly.
 * When the ', ' or '; ' separator is used, it gets concatenated
 * to the data name so we remove that.
 *------------------------------------------------------------------*/
DATA_NAME
    : LETTER (LETTER|'0'..'9'|'-')* (',' | ';')?
    {
        $type = matchKeywords(getText(), $type);
        if ($type == DATA_NAME) {
            if (lastKeyword == PICTURE_KEYWORD) {
                $type = PICTURE_PART;
            } else {
                setText(trimSeparator(getText()));
                if (getText().length() == 0) {
                    skip();
                }
            }
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
 * When the ', ' or '; ' separator is used, it gets concatenated
 * to the picture part so we remove that.
 *------------------------------------------------------------------*/
PICTURE_PART
    : PICTURE_CHAR+
    {
        if (lastKeyword != PICTURE_KEYWORD) {
            $type = DATA_NAME;
        }
        setText(trimSeparator(getText()));
        if (getText().length() == 0) {
            skip();
        }
    }
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
 * when we concatenate fragments from multiple lines, we end up with
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
  :  ('\r'? '\n') SPACE+ CONTINUATION_CHAR SPACE* ALPHANUM_LITERAL_FRAGMENT+
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
 * Whitespaces are not needed by the parser
 *------------------------------------------------------------------*/
WHITESPACE
    :   SPACE+ { skip(); }
    ;

/*------------------------------------------------------------------
 * Newlines are not needed by the parser
 *------------------------------------------------------------------*/
NEWLINE
    :   ('\r'? '\n')+  { skip(); }
    ;


/*------------------------------------------------------------------
 * Fragments
 *------------------------------------------------------------------*/
fragment LETTER     : 'A'..'Z'| 'a'..'z';
fragment SPACE      : ' ' | '\t';
fragment QUOTE      : '"';
fragment APOST      : '\'';
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
