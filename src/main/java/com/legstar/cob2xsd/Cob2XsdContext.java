package com.legstar.cob2xsd;

/**
 * This class gathers execution parameters for the COBOL to XSD utility.
 *
 */
public class Cob2XsdContext {

    /** Default Currency symbol used. */
    public static final char DEFAULT_CURRENCY_SYMBOL = '$';

    /** Default NSYMBOL(DBCS) compiler option. */
    public static final boolean DEFAULT_NSYMBOLDBCS = false;

    /** Default Whether comma is the decimal point. */
    public static final boolean DEFAULT_DECIMALPOINTISCOMMA = false;
    
    /** Currency symbol used (CURRENCY SIGN clause in the SPECIAL-NAMES). */
    private char _currencySymbol = DEFAULT_CURRENCY_SYMBOL;
    
    /** NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if false.*/
    private boolean _nSymbolDbcs = DEFAULT_NSYMBOLDBCS;
    
    /** Whether comma is the decimal point (DECIMAL-POINT IS COMMA clause in the SPECIAL-NAMES). */
    private boolean _decimalPointIsComma = DEFAULT_DECIMALPOINTISCOMMA;

    /** Whether we should generate COBOL/JAXB annotations. */
    private boolean _addLegStarAnnotations = false;
    
    /** The JAXB package name (appears in schema annotations).*/
    private String _jaxbPackageName;

    /** JAXB appends this suffix to all generated types.*/
    private String _jaxbTypeClassesSuffix;

    /**
     * @return the currency symbol used (CURRENCY SIGN clause in the SPECIAL-NAMES)
     */
    public char getCurrencySymbol() {
        return _currencySymbol;
    }

    /**
     * @param currencySymbol the currency symbol used (CURRENCY SIGN clause in the SPECIAL-NAMES)
     */
    public void setCurrencySymbol(final char currencySymbol) {
        _currencySymbol = currencySymbol;
    }

    /**
     * @return the NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if false
     */
    public boolean isNSymbolDbcs() {
        return _nSymbolDbcs;
    }

    /**
     * @param nSymbolDbcs the NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if false
     */
    public void setNSymbolDbcs(final boolean nSymbolDbcs) {
        _nSymbolDbcs = nSymbolDbcs;
    }

    /**
     * @return whether comma is the decimal point (DECIMAL-POINT IS COMMA clause in the SPECIAL-NAMES)
     */
    public boolean decimalPointIsComma() {
        return _decimalPointIsComma;
    }

    /**
     * @param decimalPointIsComma whether comma is the decimal point
     *  (DECIMAL-POINT IS COMMA clause in the SPECIAL-NAMES)
     */
    public void setDecimalPointIsComma(final boolean decimalPointIsComma) {
        _decimalPointIsComma = decimalPointIsComma;
    }

    /**
     * @return whether we should generate COBOL/JAXB annotations
     */
    public boolean addLegStarAnnotations() {
        return _addLegStarAnnotations;
    }

    /**
     * @param addLegStarAnnotations whether we should generate COBOL/JAXB annotations
     */
    public void setAddLegStarAnnotations(final boolean addLegStarAnnotations) {
        _addLegStarAnnotations = addLegStarAnnotations;
    }

    /**
     * @return the JAXB package name for generated Java classes
     */
    public String getJaxbPackageName() {
        return _jaxbPackageName;
    }

    /**
     * @return the JAXB type name prefix (generated JAXB class names will have this suffix)
     */
    public String getJaxbTypeClassesSuffix() {
        return _jaxbTypeClassesSuffix;
    }

    /**
     * @param jaxbPackageName the JAXB package name for generated Java classes
     */
    public void setJaxbPackageName(final String jaxbPackageName) {
        _jaxbPackageName = jaxbPackageName;
    }

    /**
     * @param jaxbTypeClassesSuffix the JAXB type name prefix (generated JAXB class names will have this suffix)
     */
    public void setJaxbTypeClassesSuffix(final String jaxbTypeClassesSuffix) {
        _jaxbTypeClassesSuffix = jaxbTypeClassesSuffix;
    }


}
