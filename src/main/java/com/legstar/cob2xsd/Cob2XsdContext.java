package com.legstar.cob2xsd;

import java.io.File;

/**
 * This class gathers execution parameters for the COBOL to XML Schema utility.
 *
 */
public class Cob2XsdContext {

    /** The default character set used to encode the XML Schema.*/
    public static final String DEFAULT_XSD_ENCODING = "UTF-8";

    /** Default target namespace. */
    public static final String DEFAULT_TARGET_NAMESPACE = "http://www.acme.com/test";

    /** Default JAXB package name. */
    public static final String DEFAULT_JAXB_PACKAGE_NAME = "com.acme.test";

    /** Default Currency symbol used (CURRENCY SIGN clause in the SPECIAL-NAMES). */
    public static final char DEFAULT_CURRENCY_SYMBOL = '$';


    /* -------------------------------------------------------------------
     * XML Schema related options
     * */
    
    /** Character set used to encode the output XML Schema.*/
    private String _xsdEncoding = DEFAULT_XSD_ENCODING;
    
    /** Target namespace for generated XML schema.*/
    private String _targetNamespace = DEFAULT_TARGET_NAMESPACE;

    /** Whether COBOL conditions (level 88) should be mapped to facets. Facets 
     * restrict the content which might not be desirable. */
    private boolean _mapConditionsToFacets = false;

    /** True if parent complex type name should be prepended in case of name conflict
     * (otherwise, the COBOL source line will be appended). */
    private boolean _nameConflictPrependParentName = false;

    /** True if XSD element names should start with an uppercase 
     * (compatible with LegStar 1.2).*/
    private boolean _elementNamesStartWithUppercase = false;

    /** An optional XSLT transform for XML schema customization. */
    private File _customXslt;


    /* -------------------------------------------------------------------
     * LegStar annotations related options
     * */

    /** Whether we should generate COBOL/JAXB annotations. */
    private boolean _addLegStarAnnotations = false;

    /** The JAXB package name (appears in schema annotations).*/
    private String _jaxbPackageName = DEFAULT_JAXB_PACKAGE_NAME;

    /** JAXB appends this suffix to all generated types.*/
    private String _jaxbTypeClassesSuffix;


    /* -------------------------------------------------------------------
     * COBOL compiler related options
     * */

    /** Currency symbol used (CURRENCY SIGN clause in the SPECIAL-NAMES). */
    private char _currencySymbol = DEFAULT_CURRENCY_SYMBOL;

    /** Whether comma is the decimal point (DECIMAL-POINT IS COMMA clause in the SPECIAL-NAMES). */
    private boolean _decimalPointIsComma = false;

    /** COBOL NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if false.*/
    private boolean _nSymbolDbcs = false;

    /** COBOL QUOTE|APOST compiler option. False means APOST.*/
    private boolean _quoteIsQuote = true;


    /* -------------------------------------------------------------------
     * XML Schema related options
     * */

    /**
     * @return the character set used to encode the output XML Schema
     */
    public String getXsdEncoding() {
        return _xsdEncoding;
    }

    /**
     * @param xsdEncoding the character set used to encode the output XML Schema to set
     */
    public void setXsdEncoding(final String xsdEncoding) {
        _xsdEncoding = xsdEncoding;
    }

    /**
     * The target namespace for generated XML schema.
     * @return the target namespace for generated XML schema
     */
    public String getTargetNamespace() {
        return _targetNamespace;
    }

    /**
     * @param targetNamespace the target namespace for generated XML schema
     */
    public void setTargetNamespace(final String targetNamespace) {
        _targetNamespace = targetNamespace;
    }

    /**
     * Whether COBOL conditions (level 88) should be mapped to facets. Facets 
     * restrict the content which might not be desirable.
     * @return whether COBOL conditions (level 88) should be mapped to facets.
     */
    public boolean mapConditionsToFacets() {
        return _mapConditionsToFacets;
    }

    /**
     * @param mapConditionsToFacets Whether COBOL conditions (level 88) should be mapped to facets. Facets 
     * restrict the content which might not be desirable
     */
    public void setMapConditionsToFacets(final boolean mapConditionsToFacets) {
        _mapConditionsToFacets = mapConditionsToFacets;
    }

    /**
     * An optional XSLT transform for XML schema customization.
     * @return an optional XSLT transform for XML schema customization
     */
    public File getCustomXslt() {
        return _customXslt;
    }

    /**
     * @param customXslt an optional XSLT transform for XML schema customization
     */
    public void setCustomXslt(final File customXslt) {
        _customXslt = customXslt;
    }

    /**
     * True if parent complex type name should be prepended in case of name conflict
     * (otherwise, the COBOL source line will be appended).
     * @return true if parent complex type name should be prepended in case of name conflict
     * (otherwise, the COBOL source line will be appended)
     */
    public boolean nameConflictPrependParentName() {
        return _nameConflictPrependParentName;
    }

    /**
     * @param nameConflictPrependParentName true if parent complex type name should be prepended
     * in case of name conflict (otherwise, the COBOL source line will be appended)
     */
    public void setNameConflictPrependParentName(
            final boolean nameConflictPrependParentName) {
        _nameConflictPrependParentName = nameConflictPrependParentName;
    }

    /**
     * True if XSD element names should start with an uppercase
     * (compatible with legstar-schemagen).
     * @return true if XSD element names should start with an uppercase 
     */
    public boolean elementNamesStartWithUppercase() {
        return _elementNamesStartWithUppercase;
    }

    /**
     * @param elementNamesStartWithUppercase true if XSD element names should start with an uppercase 
     * (compatible with LegStar 1.2)
     */
    public void setElementNamesStartWithUppercase(
            final boolean elementNamesStartWithUppercase) {
        _elementNamesStartWithUppercase = elementNamesStartWithUppercase;
    }

    /* -------------------------------------------------------------------
     * LegStar annotations related options
     * */

    /**
     * Whether we should generate LegStar COBOL/JAXB annotations.
     * @return whether we should generate LegStar COBOL/JAXB annotations
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
     * The package name for JAXB generated Java classes.
     * @return the package name for JAXB generated Java classes
     */
    public String getJaxbPackageName() {
        return _jaxbPackageName;
    }

    /**
     * @param jaxbPackageName the JAXB package name for generated Java classes
     */
    public void setJaxbPackageName(final String jaxbPackageName) {
        _jaxbPackageName = jaxbPackageName;
    }

    /**
     * The JAXB type name prefix (generated JAXB class names will have this suffix).
     * @return the JAXB type name prefix (generated JAXB class names will have this suffix)
     */
    public String getJaxbTypeClassesSuffix() {
        return _jaxbTypeClassesSuffix;
    }

    /**
     * @param jaxbTypeClassesSuffix the JAXB type name prefix (generated JAXB class names will have this suffix)
     */
    public void setJaxbTypeClassesSuffix(final String jaxbTypeClassesSuffix) {
        _jaxbTypeClassesSuffix = jaxbTypeClassesSuffix;
    }

    /* -------------------------------------------------------------------
     * COBOL compiler related options
     * */

    /**
     * The COBOL currency symbol used (CURRENCY SIGN clause in the SPECIAL-NAMES).
     * @return the COBOL currency symbol used
     */
    public char getCurrencySymbol() {
        return _currencySymbol;
    }

    /**
     * @param currencySymbol the COBOL currency symbol used (CURRENCY SIGN clause in the SPECIAL-NAMES)
     */
    public void setCurrencySymbol(final char currencySymbol) {
        _currencySymbol = currencySymbol;
    }

    /**
     * Whether comma is the decimal point (DECIMAL-POINT IS COMMA clause in the SPECIAL-NAMES).
     * @return whether comma is the decimal point
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
     * The COBOL NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if false
     * @return the NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if false
     */
    public boolean nSymbolDbcs() {
        return _nSymbolDbcs;
    }

    /**
     * @param nSymbolDbcs the NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if false
     */
    public void setNSymbolDbcs(final boolean nSymbolDbcs) {
        _nSymbolDbcs = nSymbolDbcs;
    }

    /**
     * The COBOL QUOTE|APOST compiler option. False means APOST.
     * @return the COBOL QUOTE|APOST compiler option. False means APOST
     */
    public boolean quoteIsQuote() {
        return _quoteIsQuote;
    }

    /**
     * @param quoteIsQuote the COBOL QUOTE|APOST compiler option. False means APOST
     */
    public void setQuoteIsQuote(final boolean quoteIsQuote) {
        _quoteIsQuote = quoteIsQuote;
    }

    /** {@inheritDoc}*/
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("xsdEncoding                    : " + getXsdEncoding() + "\n");
        sb.append("targetNamespace                : " + getTargetNamespace() + "\n");
        sb.append("mapConditionsToFacets          : " + mapConditionsToFacets() + "\n");
        sb.append("nameConflictPrependParentName  : " + nameConflictPrependParentName() + "\n");
        sb.append("elementNamesStartWithUppercase : " + elementNamesStartWithUppercase() + "\n");
        sb.append("customXslt                     : " + getCustomXslt() + "\n");
        sb.append("\n");
        sb.append("addLegStarAnnotations          : " + addLegStarAnnotations() + "\n");
        sb.append("jaxbPackageName                : " + getJaxbPackageName() + "\n");
        sb.append("jaxbTypeClassesSuffix          : " + getJaxbTypeClassesSuffix() + "\n");
        sb.append("\n");
        sb.append("currencySymbol                 : " + getCurrencySymbol() + "\n");
        sb.append("decimalPointIsComma            : " + decimalPointIsComma() + "\n");
        sb.append("nSymbolDbcs                    : " + nSymbolDbcs() + "\n");
        sb.append("quoteIsQuote                   : " + quoteIsQuote() + "\n");
        return sb.toString();
    }


}
