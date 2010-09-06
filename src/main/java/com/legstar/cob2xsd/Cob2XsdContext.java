/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cob2xsd;

import java.io.Serializable;

/**
 * This class gathers execution parameters for the COBOL to XML Schema utility.
 * 
 */
public class Cob2XsdContext implements Serializable {

    /** serial ID. */
    private static final long serialVersionUID = 3689777932172778788L;

    /**
     * Default column where fixed format COBOL code starts (inclusive, based 1).
     */
    public static final int DEFAULT_START_COLUMN = 7;

    /** Default column where fixed format COBOL code ends (inclusive, based 1). */
    public static final int DEFAULT_END_COLUMN = 72;

    /** The default character set used to encode the XML Schema. */
    public static final String DEFAULT_XSD_ENCODING = "UTF-8";

    /** Default Currency sign used (CURRENCY SIGN clause in the SPECIAL-NAMES). */
    public static final String DEFAULT_CURRENCY_SIGN = "$";

    /**
     * Default Currency symbol used (CURRENCY PICTURE SYMBOL clause in the
     * SPECIAL-NAMES).
     */
    public static final String DEFAULT_CURRENCY_SYMBOL = DEFAULT_CURRENCY_SIGN;

    /*
     * -------------------------------------------------------------------
     * COBOL source format related options
     */

    /**
     * Source can be in fixed format (sequence numbers, indicator area, area A,
     * area B) or free format.
     */
    public enum CodeFormat {
        /**
         * Fixed is the legacy format, free, the more recent one.
         */
        FIXED_FORMAT, FREE_FORMAT
    };

    /** Fixed or Free format COBOL source. */
    public CodeFormat _codeFormat = CodeFormat.FIXED_FORMAT;

    /** For fixed format COBOL, position of the indicator area. */
    public int _startColumn = DEFAULT_START_COLUMN;

    /** For fixed format COBOL position of the right margin. */
    public int _endColumn = DEFAULT_END_COLUMN;

    /*
     * -------------------------------------------------------------------
     * XML Schema related options
     */

    /** Character set used to encode the output XML Schema. */
    private String _xsdEncoding = DEFAULT_XSD_ENCODING;

    /** Target namespace for generated XML schema. */
    private String _targetNamespace;

    /**
     * Whether COBOL conditions (level 88) should be mapped to facets. Facets
     * restrict the content which might not be desirable.
     */
    private boolean _mapConditionsToFacets = false;

    /**
     * True if parent complex type name should be prepended in case of name
     * conflict
     * (otherwise, the COBOL source line will be appended).
     */
    private boolean _nameConflictPrependParentName = false;

    /**
     * True if XSD element names should start with an uppercase
     * (compatible with LegStar 1.2).
     */
    private boolean _elementNamesStartWithUppercase = false;

    /** An optional XSLT transform for XML schema customization. */
    private String _customXsltFileName;

    /*
     * -------------------------------------------------------------------
     * LegStar annotations related options
     */

    /** Whether we should generate COBOL/JAXB annotations. */
    private boolean _addLegStarAnnotations = false;

    /*
     * -------------------------------------------------------------------
     * COBOL compiler related options
     */

    /** Currency sign used (CURRENCY SIGN clause in the SPECIAL-NAMES). */
    private String _currencySign = DEFAULT_CURRENCY_SIGN;

    /**
     * Currency symbol used (CURRENCY PICTURE SYMBOL clause in the
     * SPECIAL-NAMES).
     */
    private String _currencySymbol = DEFAULT_CURRENCY_SYMBOL;

    /**
     * Whether comma is the decimal point (DECIMAL-POINT IS COMMA clause in the
     * SPECIAL-NAMES).
     */
    private boolean _decimalPointIsComma = false;

    /** COBOL NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if false. */
    private boolean _nSymbolDbcs = false;

    /** COBOL QUOTE|APOST compiler option. False means APOST. */
    private boolean _quoteIsQuote = true;

    /*
     * -------------------------------------------------------------------
     * COBOL source format related options
     */
    /**
     * @return the Fixed or Free format COBOL source
     */
    public CodeFormat getCodeFormat() {
        return _codeFormat;
    }

    /**
     * @param cobolFormat the Fixed or Free format COBOL source to set
     */
    public void setCodeFormat(final CodeFormat cobolFormat) {
        _codeFormat = cobolFormat;
    }

    /**
     * @return the position of the indicator area for fixed format COBOL
     */
    public int getStartColumn() {
        return _startColumn;
    }

    /**
     * @param startColumn the position of the indicator area for fixed format
     *            COBOL
     */
    public void setStartColumn(final int startColumn) {
        _startColumn = startColumn;
    }

    /**
     * @return the position of the right margin for fixed format COBOL
     */
    public int getEndColumn() {
        return _endColumn;
    }

    /**
     * @param endColumn the position of the right margin for fixed format COBOL
     */
    public void setEndColumn(final int endColumn) {
        _endColumn = endColumn;
    }

    /*
     * -------------------------------------------------------------------
     * XML Schema related options
     */

    /**
     * @return the character set used to encode the output XML Schema
     */
    public String getXsdEncoding() {
        return _xsdEncoding;
    }

    /**
     * @param xsdEncoding the character set used to encode the output XML Schema
     *            to set
     */
    public void setXsdEncoding(final String xsdEncoding) {
        _xsdEncoding = xsdEncoding;
    }

    /**
     * The target namespace for generated XML schema.
     * 
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
     * 
     * @return whether COBOL conditions (level 88) should be mapped to facets.
     */
    public boolean mapConditionsToFacets() {
        return _mapConditionsToFacets;
    }

    /**
     * @param mapConditionsToFacets Whether COBOL conditions (level 88) should
     *            be mapped to facets. Facets
     *            restrict the content which might not be desirable
     */
    public void setMapConditionsToFacets(final boolean mapConditionsToFacets) {
        _mapConditionsToFacets = mapConditionsToFacets;
    }

    /**
     * An optional XSLT transform for XML schema customization.
     * 
     * @return an optional XSLT transform for XML schema customization
     */
    public String getCustomXsltFileName() {
        return _customXsltFileName;
    }

    /**
     * @param customXsltFileName an optional XSLT transform for XML schema
     *            customization
     */
    public void setCustomXsltFileName(final String customXsltFileName) {
        _customXsltFileName = customXsltFileName;
    }

    /**
     * True if parent complex type name should be prepended in case of name
     * conflict
     * (otherwise, the COBOL source line will be appended).
     * 
     * @return true if parent complex type name should be prepended in case of
     *         name conflict
     *         (otherwise, the COBOL source line will be appended)
     */
    public boolean nameConflictPrependParentName() {
        return _nameConflictPrependParentName;
    }

    /**
     * @param nameConflictPrependParentName true if parent complex type name
     *            should be prepended
     *            in case of name conflict (otherwise, the COBOL source line
     *            will be appended)
     */
    public void setNameConflictPrependParentName(
            final boolean nameConflictPrependParentName) {
        _nameConflictPrependParentName = nameConflictPrependParentName;
    }

    /**
     * True if XSD element names should start with an uppercase
     * (compatible with legstar-schemagen).
     * 
     * @return true if XSD element names should start with an uppercase
     */
    public boolean elementNamesStartWithUppercase() {
        return _elementNamesStartWithUppercase;
    }

    /**
     * @param elementNamesStartWithUppercase true if XSD element names should
     *            start with an uppercase
     *            (compatible with LegStar 1.2)
     */
    public void setElementNamesStartWithUppercase(
            final boolean elementNamesStartWithUppercase) {
        _elementNamesStartWithUppercase = elementNamesStartWithUppercase;
    }

    /*
     * -------------------------------------------------------------------
     * LegStar annotations related options
     */

    /**
     * Whether we should generate LegStar COBOL/JAXB annotations.
     * 
     * @return whether we should generate LegStar COBOL/JAXB annotations
     */
    public boolean addLegStarAnnotations() {
        return _addLegStarAnnotations;
    }

    /**
     * @param addLegStarAnnotations whether we should generate COBOL/JAXB
     *            annotations
     */
    public void setAddLegStarAnnotations(final boolean addLegStarAnnotations) {
        _addLegStarAnnotations = addLegStarAnnotations;
    }

    /*
     * -------------------------------------------------------------------
     * COBOL compiler related options
     */

    /**
     * The COBOL currency sign used (CURRENCY SIGN clause in the SPECIAL-NAMES).
     * 
     * @return the COBOL currency sign used
     */
    public String getCurrencySign() {
        return _currencySign;
    }

    /**
     * @param currencySign the COBOL currency sign used (CURRENCY SIGN clause in
     *            the SPECIAL-NAMES)
     */
    public void setCurrencySign(final String currencySign) {
        if (currencySign == null || currencySign.length() == 0) {
            throw new IllegalArgumentException(
                    "Currency sign cannot be null or empty");
        }
        _currencySign = currencySign;
    }

    /**
     * The COBOL currency symbol used (CURRENCY PICTURE SYMBOL clause in the
     * SPECIAL-NAMES).
     * 
     * @return the COBOL currency symbol used
     */
    public String getCurrencySymbol() {
        return _currencySymbol;
    }

    /**
     * @param currencySymbol the COBOL currency symbol used (CURRENCY PICTURE
     *            SYMBOL clause in the SPECIAL-NAMES)
     */
    public void setCurrencySymbol(final String currencySymbol) {
        if (currencySymbol == null || currencySymbol.length() == 0) {
            throw new IllegalArgumentException(
                    "Currency symbol cannot be null or empty");
        }
        _currencySymbol = currencySymbol;
    }

    /**
     * Whether comma is the decimal point (DECIMAL-POINT IS COMMA clause in the
     * SPECIAL-NAMES).
     * 
     * @return whether comma is the decimal point
     */
    public boolean decimalPointIsComma() {
        return _decimalPointIsComma;
    }

    /**
     * @param decimalPointIsComma whether comma is the decimal point
     *            (DECIMAL-POINT IS COMMA clause in the SPECIAL-NAMES)
     */
    public void setDecimalPointIsComma(final boolean decimalPointIsComma) {
        _decimalPointIsComma = decimalPointIsComma;
    }

    /**
     * The COBOL NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if
     * false
     * 
     * @return the NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if
     *         false
     */
    public boolean nSymbolDbcs() {
        return _nSymbolDbcs;
    }

    /**
     * @param nSymbolDbcs the NSYMBOL(DBCS) compiler option. Assume
     *            NSYMBOL(NATIONAL) if false
     */
    public void setNSymbolDbcs(final boolean nSymbolDbcs) {
        _nSymbolDbcs = nSymbolDbcs;
    }

    /**
     * The COBOL QUOTE|APOST compiler option. False means APOST.
     * 
     * @return the COBOL QUOTE|APOST compiler option. False means APOST
     */
    public boolean quoteIsQuote() {
        return _quoteIsQuote;
    }

    /**
     * @param quoteIsQuote the COBOL QUOTE|APOST compiler option. False means
     *            APOST
     */
    public void setQuoteIsQuote(final boolean quoteIsQuote) {
        _quoteIsQuote = quoteIsQuote;
    }

    /** {@inheritDoc} */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("{");
        sb.append("sourceFormat: " + getCodeFormat() + ", ");
        sb.append("startColumn: " + getStartColumn() + ", ");
        sb.append("endColumn: " + getEndColumn() + ", ");
        sb.append("xsdEncoding: " + getXsdEncoding() + ", ");
        sb.append("targetNamespace: " + getTargetNamespace() + ", ");
        sb.append("mapConditionsToFacets: " + mapConditionsToFacets() + ", ");
        sb.append("nameConflictPrependParentName: "
                + nameConflictPrependParentName() + ", ");
        sb.append("elementNamesStartWithUppercase: "
                + elementNamesStartWithUppercase() + ", ");
        sb.append("customXslt: " + getCustomXsltFileName() + ", ");
        sb.append("addLegStarAnnotations: " + addLegStarAnnotations() + ", ");
        sb.append("currencySign: " + getCurrencySign() + ", ");
        sb.append("currencySymbol: " + getCurrencySymbol() + ", ");
        sb.append("decimalPointIsComma: " + decimalPointIsComma() + ", ");
        sb.append("nSymbolDbcs: " + nSymbolDbcs() + ", ");
        sb.append("quoteIsQuote: " + quoteIsQuote() + ", ");
        sb.append("}");
        return sb.toString();
    }

}
