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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Properties;

import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.codegen.models.SourceToXsdCobolModel;

/**
 * This class gathers execution parameters for the COBOL to XML Schema utility.
 * <p>
 * The class is also capable of generating a fully configured ANT script to run
 * the ant version of the utility with the current parameter set.
 * 
 */
public class Cob2XsdModel extends SourceToXsdCobolModel {

    /** This generator name. */
    public static final String C2S_GENERATOR_NAME = "LegStar COBOL to XML Schema generator";

    /** This velocity template. */
    public static final String C2S_VELOCITY_MACRO_NAME = "vlc/build-cob2xsd-xml.vm";

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

    /* ====================================================================== */
    /* Following are default field values. = */
    /* ====================================================================== */

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

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
    /* ====================================================================== */

    /** Fixed or Free format COBOL source. */
    public static final String CODE_FORMAT = "codeFormat";

    /** For fixed format COBOL, position of the indicator area. */
    public static final String START_COLUMN = "startColumn";

    /** For fixed format COBOL position of the right margin. */
    public static final String END_COLUMN = "endColumn";

    /** Character set used to encode the source COBOL file. */
    public static final String COBOL_SOURCE_FILE_ENCODING = "cobolSourceFileEncoding";

    /** Character set used to encode the output XML Schema. */
    public static final String XSD_ENCODING = "xsdEncoding";

    /** Target namespace for generated XML schema. */
    public static final String TARGET_NAMESPACE = "targetNamespace";

    /** Whether COBOL conditions (level 88) should be mapped to facets. */
    public static final String MAP_CONDITIONS_TO_FACETS = "mapConditionsToFacets";

    /**
     * True if parent complex type name should be prepended in case of name
     * conflict.
     */
    public static final String NAME_CONFLICT_PREPEND_PARENT_NAME = "nameConflictPrependParentName";

    /** True if XSD element names should start with an uppercase. */
    public static final String ELEMENT_NAMES_START_WITH_UPPERCASE = "elementNamesStartWithUppercase";

    /** True if we should ignore primitive data items without a parent group. */
    public static final String IGNORE_ORPHAN_PRIMITIVE_ELEMENTS = "ignoreOrphanPrimitiveElements";

    /** An optional XSLT transform for XML schema customization. */
    public static final String CUSTOM_XSLT_FILENAME = "customXsltFileName";

    /** Whether we should generate COBOL/JAXB annotations. */
    public static final String ADD_LEGSTAR_ANNOTATIONS = "addLegStarAnnotations";

    /** Currency sign used (CURRENCY SIGN clause in the SPECIAL-NAMES). */
    public static final String CURRENCY_SIGN = "currencySign";

    /** Currency symbol used (CURRENCY PICTURE SYMBOL clause). */
    public static final String CURRENCY_SYMBOL = "currencySymbol";

    /** Whether comma is the decimal point (DECIMAL-POINT IS COMMA clause). */
    public static final String DECIMAL_POINT_IS_COMMA = "decimalPointIsComma";

    /** COBOL NSYMBOL(DBCS) compiler option. */
    public static final String NSYMBOL_DBCS = "nSymbolDbcs";

    /** COBOL QUOTE|APOST compiler option. */
    public static final String QUOTE_IS_QUOTE = "quoteIsQuote";

    /* ====================================================================== */
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */

    /*
     * ----------------------------------------------------------------------
     * COBOL source format related options
     */

    /** Fixed or Free format COBOL source. */
    private CodeFormat _codeFormat = CodeFormat.FIXED_FORMAT;

    /** For fixed format COBOL, position of the indicator area. */
    private int _startColumn = DEFAULT_START_COLUMN;

    /** For fixed format COBOL, position of the right margin. */
    private int _endColumn = DEFAULT_END_COLUMN;

    /**
     * Character set used to encode the input COBOL source files. Null means
     * default character set.
     */
    private String _cobolSourceFileEncoding;

    /*
     * ----------------------------------------------------------------------
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
     * conflict (otherwise, the COBOL source line will be appended).
     */
    private boolean _nameConflictPrependParentName = false;

    /**
     * True if XSD element names should start with an uppercase (compatible with
     * LegStar 1.2).
     */
    private boolean _elementNamesStartWithUppercase = false;

    /** An optional XSLT transform for XML schema customization. */
    private String _customXsltFileName;

    /**
     * Ignore primitive data items which are not attached to a parent group.
     */
    private boolean _ignoreOrphanPrimitiveElements = true;

    /*
     * ----------------------------------------------------------------------
     * LegStar annotations related options
     */

    /** Whether we should generate COBOL annotations. */
    private boolean _addLegStarAnnotations = false;

    /*
     * ----------------------------------------------------------------------
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

    /* ====================================================================== */
    /* Following are this class fields that are non persistent. = */
    /* ====================================================================== */

    /** The full path to the cobol source file. */
    private String _cobolSourceFilePath;

    /** Whether velocity is already initialized. */
    private boolean _velocityInitialized;

    /**
     * A no-Arg constructor.
     */
    public Cob2XsdModel() {
    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public Cob2XsdModel(final Properties props) {
        setCodeFormat(CodeFormat.valueOf(getString(props, CODE_FORMAT,
                CodeFormat.FIXED_FORMAT.toString())));
        setStartColumn(getInt(props, START_COLUMN, DEFAULT_START_COLUMN));
        setEndColumn(getInt(props, END_COLUMN, DEFAULT_END_COLUMN));
        setCobolSourceFileEncoding(getString(props, COBOL_SOURCE_FILE_ENCODING,
                null));
        setXsdEncoding(getString(props, XSD_ENCODING, DEFAULT_XSD_ENCODING));
        setTargetNamespace(getString(props, TARGET_NAMESPACE, null));
        setMapConditionsToFacets(getBoolean(props, MAP_CONDITIONS_TO_FACETS,
                false));
        setNameConflictPrependParentName(getBoolean(props,
                NAME_CONFLICT_PREPEND_PARENT_NAME, false));
        setElementNamesStartWithUppercase(getBoolean(props,
                ELEMENT_NAMES_START_WITH_UPPERCASE, false));
        setIgnoreOrphanPrimitiveElements(getBoolean(props,
                IGNORE_ORPHAN_PRIMITIVE_ELEMENTS, true));
        setCustomXsltFileName(getString(props, CUSTOM_XSLT_FILENAME, null));
        setAddLegStarAnnotations(getBoolean(props, ADD_LEGSTAR_ANNOTATIONS,
                false));
        setCurrencySign(getString(props, CURRENCY_SIGN, DEFAULT_CURRENCY_SIGN));
        setCurrencySymbol(getString(props, CURRENCY_SYMBOL,
                DEFAULT_CURRENCY_SYMBOL));
        setDecimalPointIsComma(getBoolean(props, DECIMAL_POINT_IS_COMMA, false));
        setNSymbolDbcs(getBoolean(props, NSYMBOL_DBCS, false));
        setQuoteIsQuote(getBoolean(props, QUOTE_IS_QUOTE, true));
    }

    /**
     * Creates an ant build script file ready for XSD generation.
     * 
     * @param targetFile the script file that must be created
     * @throws CodeGenMakeException if generation fails
     */
    public final void generateBuild(final File targetFile)
            throws CodeGenMakeException {
        Writer w = null;
        try {
            if (!_velocityInitialized) {
                CodeGenUtil.initVelocity();
                _velocityInitialized = true;
            }
            VelocityContext context = CodeGenUtil
                    .getContext(C2S_GENERATOR_NAME);
            context.put("antModel", this);
            w = new OutputStreamWriter(new FileOutputStream(targetFile),
                    "UTF-8");
            Velocity.mergeTemplate(C2S_VELOCITY_MACRO_NAME, "UTF-8", context, w);
        } catch (IOException e) {
            throw new CodeGenMakeException(e);
        } catch (Exception e) {
            throw new CodeGenMakeException(e);
        } finally {
            if (w != null) {
                try {
                    w.close();
                } catch (IOException e) {
                    throw new CodeGenMakeException(e);
                }
            }
        }
    }

    /**
     * @return the full path to the COBOL source file
     */
    public final String getCobolSourceFilePath() {
        return _cobolSourceFilePath;
    }

    /**
     * @param cobolSourceFilePath the full path to the COBOL source file to set
     */
    public final void setCobolSourceFilePath(final String cobolSourceFilePath) {
        _cobolSourceFilePath = cobolSourceFilePath;
    }

    /**
     * @return the target XML schema file
     */
    public final File getTargetXsdFile() {
        if (getTargetXsdFileName() == null) {
            return null;
        }
        if (getTargetDir() == null) {
            return new File(getTargetXsdFileName());
        } else {
            return new File(getTargetDir(), getTargetXsdFileName());
        }
    }

    /*
     * ------------------------------------------------------------------- COBOL
     * source format related options
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

    /**
     * @return the character set used to encode the input COBOL source files
     */
    public String getCobolSourceFileEncoding() {
        return _cobolSourceFileEncoding;
    }

    /**
     * @param cobolSourceFileEncoding the character set used to encode the input
     *            COBOL source files
     */
    public void setCobolSourceFileEncoding(final String cobolSourceFileEncoding) {
        this._cobolSourceFileEncoding = cobolSourceFileEncoding;
    }

    /*
     * ------------------------------------------------------------------- XML
     * Schema related options
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
     *            be mapped to facets. Facets restrict the content which might
     *            not be desirable
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
     * conflict (otherwise, the COBOL source line will be appended).
     * 
     * @return true if parent complex type name should be prepended in case of
     *         name conflict (otherwise, the COBOL source line will be appended)
     */
    public boolean nameConflictPrependParentName() {
        return _nameConflictPrependParentName;
    }

    /**
     * @param nameConflictPrependParentName true if parent complex type name
     *            should be prepended in case of name conflict (otherwise, the
     *            COBOL source line will be appended)
     */
    public void setNameConflictPrependParentName(
            final boolean nameConflictPrependParentName) {
        _nameConflictPrependParentName = nameConflictPrependParentName;
    }

    /**
     * True if XSD element names should start with an uppercase (compatible with
     * legstar-schemagen).
     * 
     * @return true if XSD element names should start with an uppercase
     */
    public boolean elementNamesStartWithUppercase() {
        return _elementNamesStartWithUppercase;
    }

    /**
     * @param elementNamesStartWithUppercase true if XSD element names should
     *            start with an uppercase (compatible with LegStar 1.2)
     */
    public void setElementNamesStartWithUppercase(
            final boolean elementNamesStartWithUppercase) {
        _elementNamesStartWithUppercase = elementNamesStartWithUppercase;
    }

    /**
     * Ignore primitive data items which are not attached to a parent group.
     * 
     * @return true if primitive data items without a parent group are ignored
     */
    public boolean ignoreOrphanPrimitiveElements() {
        return _ignoreOrphanPrimitiveElements;
    }

    /**
     * Ignore primitive data items which are not attached to a parent group.
     * 
     * @param ignoreOrphanPrimitiveElements set to true to ignore primitive data
     *            items without a parent group item
     */
    public void setIgnoreOrphanPrimitiveElements(
            boolean ignoreOrphanPrimitiveElements) {
        _ignoreOrphanPrimitiveElements = ignoreOrphanPrimitiveElements;
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
     * ------------------------------------------------------------------- COBOL
     * compiler related options
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

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = super.toProperties();
        if (getCodeFormat() != null) {
            putString(props, CODE_FORMAT, getCodeFormat().toString());
        }
        putInt(props, START_COLUMN, getStartColumn());
        putInt(props, END_COLUMN, getEndColumn());
        if (getCobolSourceFileEncoding() != null) {
            putString(props, COBOL_SOURCE_FILE_ENCODING,
                    getCobolSourceFileEncoding());
        }
        if (getXsdEncoding() != null) {
            putString(props, XSD_ENCODING, getXsdEncoding());
        }
        if (getTargetNamespace() != null) {
            putString(props, TARGET_NAMESPACE, getTargetNamespace());
        }
        putBoolean(props, MAP_CONDITIONS_TO_FACETS, mapConditionsToFacets());
        putBoolean(props, NAME_CONFLICT_PREPEND_PARENT_NAME,
                nameConflictPrependParentName());
        putBoolean(props, ELEMENT_NAMES_START_WITH_UPPERCASE,
                elementNamesStartWithUppercase());
        putBoolean(props, IGNORE_ORPHAN_PRIMITIVE_ELEMENTS,
                ignoreOrphanPrimitiveElements());
        if (getCustomXsltFileName() != null) {
            putString(props, CUSTOM_XSLT_FILENAME, getCustomXsltFileName());
        }
        putBoolean(props, ADD_LEGSTAR_ANNOTATIONS, addLegStarAnnotations());
        if (getCurrencySign() != null) {
            putString(props, CURRENCY_SIGN, getCurrencySign());
        }
        if (getCurrencySymbol() != null) {
            putString(props, CURRENCY_SYMBOL, getCurrencySymbol());
        }
        putBoolean(props, DECIMAL_POINT_IS_COMMA, decimalPointIsComma());
        putBoolean(props, NSYMBOL_DBCS, nSymbolDbcs());
        putBoolean(props, QUOTE_IS_QUOTE, quoteIsQuote());
        return props;
    }

}
