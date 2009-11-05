package com.legstar.cobol.utils;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.legstar.cobol.model.PictureSymbol;

/**
 * Utility class provides methods to introspect COBOL picture clauses.
 *
 */
public final class PictureUtil {
    
    /**
     * Utility class.
     */
    private PictureUtil() {
        
    }

    /**
     * Determines how many times a given character occurs in a picture string.
     * A character can appear standalone or as a factored sequence like X(nn).
     * Unlike all other picture symbols, currency symbols are case sensitive.
     * For example, ’D’ and ’d’ specify different currency symbols.
     * @param picture the picture string
     * @param currencySign the currency sign
     * @return a map of all characters to search for
     */
    public static Map < Character, Integer > getPictureCharOccurences(
            final String picture,
            final char currencySign) {

        Map < Character, Integer > charNum = new HashMap < Character, Integer >();
        charNum.put('A', 0);
        charNum.put('B', 0);
        charNum.put('G', 0);
        charNum.put('N', 0);
        charNum.put('X', 0);
        charNum.put('P', 0);
        charNum.put('Z', 0);
        charNum.put('0', 0);
        charNum.put('/', 0);
        charNum.put('+', 0);
        charNum.put('-', 0);
        charNum.put('*', 0);
        charNum.put('C', 0);
        charNum.put('D', 0);
        charNum.put('.', 0);
        charNum.put(',', 0);
        charNum.put('9', 0);
        charNum.put('E', 0);
        charNum.put('S', 0);
        charNum.put('V', 0);
        charNum.put(currencySign, 0);
        
        List < PictureSymbol > pictureSymbols = parsePicture(picture, currencySign);
        for (PictureSymbol pictureSymbol : pictureSymbols) {
            Integer number = charNum.get(pictureSymbol.getSymbol());
            if (number != null) {
                number += pictureSymbol.getNumber();
                charNum.put(pictureSymbol.getSymbol(), number);
            }
        }

        return charNum;
    }
    
    /**
     * The COBOL picture clause determines the length, in number of characters,
     * for all alphanumeric and numeric-edited data items.
     * <p/>
     * The length evaluated here is not the byte size of the storage needed on
     * z/OS for the data item. It is the number of character positions.
     * 
     * @param charNum map of all characters in the picture string
     * @param isSignSeparate if sign occupies a separated position (no overpunch)
     * @param currencySign the currency sign
     * @return the length, in number of characters, of the data item
     */
    public static int calcLengthFromPicture(
            final Map < Character, Integer > charNum,
            final boolean isSignSeparate,
            final char currencySign) {

        int length = 0;
        
        /* character position occupied by each picture symbol */
        Map < Character, Integer > charLen = new HashMap < Character, Integer >();
        charLen.put('A', 1);
        charLen.put('B', 1);
        charLen.put('G', 1);
        charLen.put('N', 1);
        charLen.put('X', 1);
        charLen.put('P', 0);
        charLen.put('Z', 1);
        charLen.put('0', 1);
        charLen.put('/', 1);
        charLen.put('+', 1);
        charLen.put('-', 1);
        charLen.put('*', 1);
        charLen.put('C', 2);
        charLen.put('D', 2);
        charLen.put('.', 1);
        charLen.put(',', 1);
        charLen.put('9', 1);
        charLen.put('E', 1);
        charLen.put('S', (isSignSeparate) ? 1 : 0);
        charLen.put('V', 0);
        charLen.put(currencySign, 1);
        
        for (Map.Entry < Character, Integer > entry : charNum.entrySet()) {
            length += entry.getValue() * charLen.get(entry.getKey());
        }
        return length;
    }
    
    /**
     * Try to infer a regular expression to match a COBOL picture clause.
     * @param picture the picture clause
     * @param currencySign the currency sign
     * @return a regular expression
     */
    public static String getRegexFromPicture(
            final String picture,
            final char currencySign) {
        StringBuilder result = new StringBuilder();
        result.append('^');
        
        Map < Character, String > charRegex = new HashMap < Character, String >();
        charRegex.put('A', "[a-zA-Z\\s]");
        charRegex.put('B', "\\s");
        charRegex.put('G', ".");
        charRegex.put('N', ".");
        charRegex.put('X', ".");
        charRegex.put('P', "[\\d\\.]");
        charRegex.put('Z', "[1-9\\s]");
        charRegex.put('0', "0");
        charRegex.put('/', "/");
        charRegex.put('+', "[\\+\\-\\d]");
        charRegex.put('-', "[\\+\\-\\d]");
        charRegex.put('*', "[1-9\\*]");
        charRegex.put('C', "(CR|\\s\\s)");
        charRegex.put('D', "(DB|\\s\\s)");
        charRegex.put('.', ".");
        charRegex.put(',', ",");
        charRegex.put('9', "\\d");
        charRegex.put('E', "E");
        charRegex.put('S', "[\\+\\-]");
        charRegex.put('V', "");
        charRegex.put(currencySign, "[\\" + currencySign + "\\d]");
        
        List < PictureSymbol > pictureSymbols = parsePicture(picture, currencySign);
        for (PictureSymbol pictureSymbol : pictureSymbols) {
            String regex = charRegex.get(pictureSymbol.getSymbol());
            if (charRegex != null) {
                result.append(regex);
                int occurs = pictureSymbol.getNumber();
                if (occurs > 1) {
                    result.append("{0," + occurs + "}");
                } else {
                    result.append("?");
                }
            }
        }
        
        result.append('$');
        return result.toString();
    }
    
    /**
     * Parse a COBOL picture clause. Character symbols are returned in the order
     * where they are found in the picture clause. All factoring is resolved and
     * each character is associated with its occurrence number.
     * <p/>
     * For instance: 9(3)V99XX becomes 4 entries in the list for characters 9, V, 9 and X.
     * First 9 occurs 3 times, V occurs 1 time, 9 occurs 2 and X occurs 2.  
     * @param currencySign the currency sign
     * @param picture the COBOL picture clause
     * @return ordered list of symbols appearing in the picture clause with their
     * number of occurrences.
     */
    public static List < PictureSymbol > parsePicture(final String picture, final char currencySign) {
        int factor = 1;
        int factoredNumber = 0;
        boolean factorSequence = false;
        char lastChar = 0;
        PictureSymbol pictureSymbol = null;
        List < PictureSymbol > result = new LinkedList < PictureSymbol >();
        for (int i = 0; i < picture.length(); i++) {
            char c = picture.charAt(i);
            if (c != currencySign) {
                c = Character.toUpperCase(c);
            }
            if (factorSequence) {
                if (c == ')') {
                    pictureSymbol.setNumber(pictureSymbol.getNumber() + factoredNumber - 1);
                    factorSequence = false;
                } else {
                    if  (Character.isDigit(c)) {
                        factoredNumber = factoredNumber * factor + Character.getNumericValue(c);
                        factor *= 10;
                    }
                }
            } else {
                if (c == '(') {
                    factor = 1;
                    factoredNumber = 0;
                    factorSequence = true;
                } else {
                    /* CR and DB are special cases where we need to ignore,
                     * the second character R or B.*/
                    if ((c != 'B' || lastChar != 'D') && (c != 'R' || lastChar != 'C')) {
                        if (c == lastChar) {
                            pictureSymbol.setNumber(pictureSymbol.getNumber() + 1);
                        } else {
                            pictureSymbol = new PictureSymbol(c, 1);
                            result.add(pictureSymbol);
                            lastChar = c;
                        }
                    }
                }
            }
        }
        return result;
    }
    

}
