package com.legstar.cobol.utils;

import java.util.Locale;

import com.legstar.cob2xsd.XsdDataItem;

/**
 * A set of utility methods to manipulate COBOL values.
 *
 */
public final class ValueUtil {

    /**
     * Utility class.
     */
    private ValueUtil() {
        
    }

    /**
     * COBOL values may be figurative constants with no meaning in the
     * XML/java world.
     * @param xsdDataItem COBOL data item decorated with XSD attributes
     * @param quoteIsQuote true if quote is quote, false if quote is apost
     * @return a value suitable for XML
     */
    public static String cleanValue(
            final XsdDataItem xsdDataItem,
            final boolean quoteIsQuote) {

        String value = xsdDataItem.getValues().get(0);
        if (value == null) {
            return null;
        }

        String resolved = ValueUtil.resolveFigurative(
                value, xsdDataItem.getLength(), quoteIsQuote);
        if (resolved != null) {
            return resolved;
        }

        /* 
         * All is a special case where a value is used to fill the
         * data item. That value normally follows in the values list.
         * The following value can also be a figurative constant.
         */
        if (value.toUpperCase(Locale.getDefault()).matches("^ALL$")
                && xsdDataItem.getValues().size() > 1) {
            String allValue = xsdDataItem.getValues().get(1);
            resolved = resolveFigurative(
                    allValue, xsdDataItem.getLength(), quoteIsQuote);
            allValue = (resolved != null) ? resolved : allValue;
            if (allValue != null && allValue.length() > 0) {
                StringBuilder sb = new StringBuilder();
                for (int i = 0; i < (xsdDataItem.getLength() / allValue.length()); i++) {
                    sb.append(allValue);
                }
                return sb.toString();
            }
        }

        /* This is not a figurative constant. Just make sure there are no
         * delimiters left.*/
        return stripDelimiters(value);
    }

    /**
     * Translate individual figurative constants.
     * @param value a potential figurative constant value
     * @param length the target data item length
     * @param quoteIsQuote true if quote is quote, false if quote is apost
     * @return a translated value or null if it is not a  figurative constant
     */
    public static String resolveFigurative(
            final String value,
            final int length,
            final boolean quoteIsQuote) {

        if (value == null) {
            return value;
        }

        if (value.toUpperCase(Locale.getDefault()).matches("^ZERO(S|ES)?$")) {
            return "0";
        }

        /* We avoid filling with spaces because this is the most common
         * initial value for large strings*/
        if (value.toUpperCase(Locale.getDefault()).matches("^SPACES?$")) {
            return "";
        }

        if (value.toUpperCase(Locale.getDefault()).matches("^QUOTES?$")) {
            return quoteIsQuote ? "\"" : "\'";
        }
        if (value.toUpperCase(Locale.getDefault()).matches("^APOST$")) {
            return "\'";
        }

        /* For binary content, we use pseudo hexadecimal representation
         * This is understood downstream by the COBOL binder. */
        if (value.toUpperCase(Locale.getDefault()).matches("^HIGH-VALUES?$")) {
            return fillHex("FF", length);
        }
        if (value.toUpperCase(Locale.getDefault()).matches("^LOW-VALUES?$")) {
            return fillHex("00", length);
        }
        /* Nulls are treated like low-value. */
        if (value.toUpperCase(Locale.getDefault()).matches("^NULLS?$")) {
            return fillHex("00", length);
        }

        return null;

    }

    /**
     * Create a hexadecimal string representation repeating the hexByte
     * sequence the requested number of times.
     * @param hexByte the hexadecimal representation of a byte
     * @param times how many times the hexByte should be repeated
     * @return a string filled with hexByte or empty string
     */
    public static String fillHex(final String hexByte, final int times) {
        if (times < 1) {
            return "";
        }
        StringBuilder sb = new StringBuilder();
        sb.append("0x");
        for (int i = 0; i < times; i++) {
            sb.append(hexByte);
        }
        return sb.toString();
    }


    /**
     * The parser does not strip delimiters from literal strings so we
     * do it here if necessary.
     * @param value a potential literal string
     * @return the literal without delimiters
     */
    public static String stripDelimiters(final String value) {
        if (value != null && value.length() > 1) {
            if (value.charAt(0) == value.charAt(value.length() - 1)
                    && (value.charAt(0) == '\'')) {
                return value.substring(1, value.length() - 1);
            }
            if (value.charAt(0) == value.charAt(value.length() - 1)
                    && (value.charAt(0) == '\"')) {
                return value.substring(1, value.length() - 1);
            }
        }
        return value;
    }

}
