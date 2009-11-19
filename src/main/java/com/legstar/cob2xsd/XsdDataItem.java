package com.legstar.cob2xsd;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.cobol.model.CobolDataItem;
import com.legstar.cobol.model.CobolDataItem.DataEntryType;
import com.legstar.cobol.model.CobolDataItem.Range;
import com.legstar.cobol.model.CobolDataItem.Usage;
import com.legstar.cobol.utils.PictureUtil;
import com.legstar.coxb.CobolType;

/**
 * XML Schema attributes derived from a COBOL data item.
 * <p/>
 * Acts as a facade to the {@link CobolDataItem} type.
 * This class is constructed from a CobolDataItem. All XML Schema attributes
 * are derived at construction time.
 * <p/>
 * The isODOObject and isRedefined properties are the only ones that
 * are not set at construction time (And therefore have setters).
 * This is because these members can be set only when some data item
 * downstream happens to reference this one.
 * 
 */
public class XsdDataItem {

    /** The COBOL data item this facade is built from. */
    private CobolDataItem _cobolDataItem;

    /** XSD simple built-in types.*/
    public enum XsdType {
        /** Maps XML Schema types.*/
        COMPLEX, ENUM, STRING, HEXBINARY, SHORT, USHORT, INT, UINT, LONG, ULONG, INTEGER, DECIMAL, FLOAT, DOUBLE
    };

    /** The parent data item or null if root. */
    private XsdDataItem _parent;

    /** Ordered list of direct children.*/
    private List < XsdDataItem > _children = new LinkedList < XsdDataItem >();

    /** XML schema type mapping the COBOL data item.*/
    private XsdType _xsdType;

    /** XML Schema element name. */
    private String _xsdElementName;

    /** Complex types are named (as opposed to anonymous). */
    private String _xsdTypeName;

    /** A derived COBOL type used in annotations.*/
    private CobolType _cobolType;

    /** For xsd:string and xsd:hexBinary. */
    private int _length = -1;

    /** For xsd:string derived from numeric edited. */
    private String _pattern;

    /** For xsd numeric types, the total number of digits. */
    private int _totalDigits = -1;

    /** For xsd numeric types, the fractional digits. */
    private int _fractionDigits = -1;

    /** Determines if a numeric item is signed or unsigned.*/
    private boolean _isSigned;

    /* Array boundaries have a different semantic in XSD. MinOccurs defaults
     * to MaxOccurs in COBOL while, it defaults to 1 in XSD. The meaning of
     * maxOccurs = 1 is also different: it is meant as an array of 1 dimension
     * in COBOL while it means "not an array" in XSD.*/
    /** Arrays minimum number of occurrences. */
    private int _minOccurs = 1;

    /** Arrays maximum number of occurrences. */
    private int _maxOccurs = 1;

    /** True if some array size downstream depends on this data item value.*/
    private boolean _isODOObject;

    /** True if some item downstream redefines this data item.*/
    private boolean _isRedefined;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());


    /**
     * COBOL data item is analyzed at construction time and all XSD attributes are 
     * derived from the COBOL attributes.
     * @param cobolDataItem the COBOL elementary data item
     * @param context the translator options in effect
     * @param parent the parent data item or null if root
     * @param nonUniqueCobolNames a list of non unique COBOL names used to detect name collisions
     */
    public XsdDataItem(
            final CobolDataItem cobolDataItem,
            final Cob2XsdContext context,
            final XsdDataItem parent,
            final List < String > nonUniqueCobolNames) {

        _cobolDataItem = cobolDataItem;
        _parent = parent;
        _xsdElementName = formatElementName(cobolDataItem, context);
        _xsdTypeName = formatTypeName(_xsdElementName, cobolDataItem,
                nonUniqueCobolNames, context, parent);

        switch (cobolDataItem.getDataEntryType()) {
        case DATA_DESCRIPTION:
            setDataDescription(cobolDataItem, context, nonUniqueCobolNames);
            break;
        case RENAMES:
           /* COBOL renames don't map to an XSD type. */
            _log.warn("Unhandled data entry type " + cobolDataItem.toString());
            break;
        case CONDITION:
            /* Will map to an enumeration facet. */
            _xsdType = XsdType.ENUM;
            if (context.mapConditionsToFacets() && getConditionRanges().size() > 1) {
                _log.warn("Condition with multiple ranges cannot be mapped to enumeration facet "
                        + cobolDataItem.toString());
            }
            break;
        default:
            _log.error("Unrecognized data entry type " + cobolDataItem.toString());
        }


    }

    /**
     * Setup a regular data description entry, elementary data items and groups.
     * @param cobolDataItem the COBOL elementary data item
     * @param context the translator options in effect
     * @param nonUniqueCobolNames a list of non unique COBOL names used to detect name collisions
     */
    protected void setDataDescription(
            final CobolDataItem cobolDataItem,
            final Cob2XsdContext context,
            final List < String > nonUniqueCobolNames) {

        if (cobolDataItem.getUsage() != null) {
            setFromUsage(cobolDataItem.getUsage());
        }
        if (cobolDataItem.getPicture() != null) {
            setFromPicture(
                    cobolDataItem.getPicture(),
                    cobolDataItem.isSignSeparate(),
                    context.getCurrencySymbol().charAt(0),
                    context.nSymbolDbcs(),
                    context.decimalPointIsComma());
        }

        /* If the xsdType is not set yet, then this is not an elementary data item.*/
        if (_xsdType == null) {
            _xsdType = XsdType.COMPLEX;
            _cobolType = CobolType.GROUP_ITEM;
        }

        /* Set XSD minOccurs/maxOccurs from COBOL. If no minOccurs set
         * in COBOL, this is a fixed size array. */
        if (cobolDataItem.getMaxOccurs() > 0) {
            _maxOccurs = cobolDataItem.getMaxOccurs();
            if (cobolDataItem.getMinOccurs() > -1) {
                _minOccurs = cobolDataItem.getMinOccurs();
            } else {
                _minOccurs = cobolDataItem.getMaxOccurs();
            }
        }

        /* Inform object upstream that someone depends on him.*/
        if (getDependingOn() != null && getParent() != null) {
            getParent().updateDependency(getDependingOn());
        }

        /* Inform object upstream that someone redefines him.*/
        if (getRedefines() != null && getParent() != null) {
            getParent().updateRedefinition(getRedefines());
        }

        /* Create the list of children by decorating the COBOL item children */
        for (CobolDataItem child : cobolDataItem.getChildren()) {
            _children.add(new XsdDataItem(child, context, this, nonUniqueCobolNames));
        }

    }

    /**
     * Called when some child (or child of a child) has a DEPENDING ON clause.
     * We look up our children for an item matching the COBOL name of the depending on
     * object.
     * If found, we update its isODOObject member, otherwise we propagate the
     * request to our own parent. 
     * @param cobolName the depending on object.
     */
    public void updateDependency(final String cobolName) {
        boolean found = false;
        for (XsdDataItem child : getChildren()) {
            if (child.getCobolName().equals(cobolName)) {
                child.setIsODOObject(true);
                found = true;
                break;
            }
        }
        if (!found && getParent() != null) {
            getParent().updateDependency(cobolName);
        }
    }

    /**
     * Called when some child (or child of a child) has a REDEFINES clause.
     * We look up our children for an item matching the COBOL name of the REDEFINES
     * object.
     * If found, we update its isRedefined member, otherwise we propagate the
     * request to our own parent. 
     * @param cobolName the redefines object.
     */
    public void updateRedefinition(final String cobolName) {
        boolean found = false;
        for (XsdDataItem child : getChildren()) {
            if (child.getCobolName().equals(cobolName)) {
                child.setIsRedefined(true);
                found = true;
                break;
            }
        }
        if (!found && getParent() != null) {
            getParent().updateRedefinition(cobolName);
        }
    }

    /**
     * Derive XML schema attributes from a COBOL usage clause.
     * This gives a rough approximation of the XSD type because the picture
     * clause usually carries info that needs to be further analyzed to determine
     * a more precise type.
     * If no usage clause, we assume there will be a picture clause.
     * @param usage COBOL usage clause
     */
    private void setFromUsage(final Usage usage) {
        switch (usage) {
        case BINARY:
            _cobolType = CobolType.BINARY_ITEM;
            _xsdType = XsdType.INTEGER;
            break;
        case NATIVEBINARY:
            _cobolType = CobolType.NATIVE_BINARY_ITEM;
            _xsdType = XsdType.INTEGER;
            break;
        case SINGLEFLOAT:
            _cobolType = CobolType.SINGLE_FLOAT_ITEM;
            _xsdType = XsdType.FLOAT;
            break;
        case DOUBLEFLOAT:
            _cobolType = CobolType.DOUBLE_FLOAT_ITEM;
            _xsdType = XsdType.DOUBLE;
            break;
        case PACKEDDECIMAL:
            _cobolType = CobolType.PACKED_DECIMAL_ITEM;
            _xsdType = XsdType.DECIMAL;
            break;
        case INDEX:
            _cobolType = CobolType.INDEX_ITEM;
            _xsdType = XsdType.HEXBINARY;
            break;
        case POINTER:
            _cobolType = CobolType.POINTER_ITEM;
            _xsdType = XsdType.HEXBINARY;
            break;
        case PROCEDUREPOINTER:
            _cobolType = CobolType.PROC_POINTER_ITEM;
            _xsdType = XsdType.HEXBINARY;
            break;
        case FUNCTIONPOINTER:
            _cobolType = CobolType.FUNC_POINTER_ITEM;
            _xsdType = XsdType.HEXBINARY;
            break;
        case DISPLAY:
            _cobolType = CobolType.ALPHANUMERIC_ITEM;
            _xsdType = XsdType.STRING;
            break;
        case DISPLAY1:
            _cobolType = CobolType.DBCS_ITEM;
            _xsdType = XsdType.STRING;
            break;
        case NATIONAL:
            _cobolType = CobolType.NATIONAL_ITEM;
            _xsdType = XsdType.STRING;
            break;
        default:
            _log.error("Unrecognized usage clause " + toString());
        }
    }

    /**
     * Derive XML schema attributes from a COBOL picture.
     * @param picture the picture clause
     * @param isSignSeparate if sign occupies a separated position (no overpunch)
     * @param currencyChar the currency sign
     * @param nSymbolDbcs true if COBOL compiler option NSYMBOL(DBCS)
     * @param decimalPointIsComma if COBOL compiler option DECIMAL POINT IS COMMA
     */
    private void setFromPicture(
            final String picture,
            final boolean isSignSeparate,
            final char currencyChar,
            final boolean nSymbolDbcs,
            final boolean decimalPointIsComma) {

        char comma = (decimalPointIsComma) ? '.' : ',';

        Map < Character, Integer > charNum =
            PictureUtil.getPictureCharOccurences(picture, currencyChar);

        _length = PictureUtil.calcLengthFromPicture(charNum, isSignSeparate, currencyChar);
        _pattern = PictureUtil.getRegexFromPicture(picture, currencyChar);

        if ((charNum.get('A') + charNum.get('X')) > 0) {
            if ((charNum.get('9') + charNum.get('B') + charNum.get('0') + charNum.get('/')) > 0) {
                _cobolType = CobolType.ALPHANUMERIC_EDITED_ITEM;
            } else {
                if (charNum.get('X') == 0) {
                    _cobolType = CobolType.ALPHABETIC_ITEM;
                } else {
                    _cobolType = CobolType.ALPHANUMERIC_ITEM;
                }
            }
            _xsdType = XsdType.STRING;
            return;
        }

        if (charNum.get('G') > 0) {
            _cobolType = CobolType.DBCS_ITEM;
            _xsdType = XsdType.STRING;
            return;
        }

        if (charNum.get('N') > 0) {
            if (nSymbolDbcs) {
                _cobolType = CobolType.DBCS_ITEM;
            } else {
                _cobolType = CobolType.NATIONAL_ITEM;
            }
            _xsdType = XsdType.STRING;
            return;
        }

        /* TODO Was previously mapped to xsd:float but requires more analysis*/
        if (charNum.get('E') > 0) {
            _cobolType = CobolType.EXTERNAL_FLOATING_ITEM;
            _xsdType = XsdType.STRING;
            return;
        }

        if ((charNum.get('/')
                + charNum.get('B')
                + charNum.get('Z')
                + charNum.get('0')
                + charNum.get(comma)
                + charNum.get('*')
                + charNum.get('+')
                + charNum.get('-')
                + charNum.get('C')  /* CR */
                + charNum.get('D')  /* DB */
                + charNum.get(currencyChar)) > 0) {
            _cobolType = CobolType.NUMERIC_EDITED_ITEM;
            _xsdType = XsdType.STRING;
            return;
        }

        /* At this stage we are left with pure numeric picture clauses.*/

        /* If usage was DISPLAY, we can now refine since we now know it is a
         * numeric not an alphanumeric. */
        if (_cobolType == null || _cobolType == CobolType.ALPHANUMERIC_ITEM) {
            _cobolType = CobolType.ZONED_DECIMAL_ITEM;
        }
        setNumericAttributes(picture, currencyChar, decimalPointIsComma);

    }

    /**
     * Once we have identified the COBOL data item as being numeric, this
     * will perform more analysis on the picture clause to extract such
     * info as integer part, decimal part and sign.
     * <p/>
     * The fractionDigits corresponds to digits past the decimal point.
     * The totalDigits is the integer part + fractionDigits;
     * <p/>
     * Once digits are identified we can further refine the choice of
     * XML schema type and a set of associated facets.
     * @param picture a purely numeric picture clause
     * @param currencyChar the currency sign
     * @param decimalPointIsComma true if decimal point is comma
     */
    private void setNumericAttributes(
            final String picture,
            final char currencyChar,
            final boolean decimalPointIsComma) {
        char decimalPoint = (decimalPointIsComma) ? ',' : '.';

        /* Look for the integer part (digits before the decimal point)*/
        int iV = picture.indexOf('V');
        if (iV == -1) {
            iV = picture.indexOf(decimalPoint);
        }
        Map < Character, Integer > intCharNum;
        Map < Character, Integer > decCharNum;
        if (iV > 0) {
            intCharNum = PictureUtil.getPictureCharOccurences(
                    picture.substring(0, iV), currencyChar);
            decCharNum = PictureUtil.getPictureCharOccurences(
                    picture.substring(iV), currencyChar);
            _fractionDigits = decCharNum.get('9');
            _totalDigits = intCharNum.get('9') + _fractionDigits;
        } else {
            intCharNum = PictureUtil.getPictureCharOccurences(picture, currencyChar);
            _fractionDigits = 0;
            _totalDigits = intCharNum.get('9');
        }

        _isSigned = (intCharNum.get('S') > 0) ? true : false;

        if (_fractionDigits == 0) {
            if (_totalDigits < 5) {
                _xsdType = (_isSigned) ? XsdType.SHORT : XsdType.USHORT;
            } else if (_totalDigits < 10) {
                _xsdType = (_isSigned) ? XsdType.INT : XsdType.UINT;
            } else if (_totalDigits < 20) {
                _xsdType = (_isSigned) ? XsdType.LONG : XsdType.ULONG;
            } else {
                _xsdType = XsdType.INTEGER;
            }

        } else {
            _xsdType = XsdType.DECIMAL;
        }

    }

    /**
     * Turn a COBOL name to a unique XSD type name.
     * <p/>
     * Complex type names customarily start with an uppercase character.
     * <p/>
     * The proposed name might be conflicting with another so we disambiguate xsd type
     * names with one of 2 options:
     * <ul>
     * <li>Appending the COBOL source line number (compatible with legstar-schemagen)</li>
     * <li>Appending the parent XSD type name</li>
     * </ul>
     * 
     * @param cobolDataItem the COBOL data item
     * @param elementName the element name built from the COBOL name
     * @param nonUniqueCobolNames a list of non unique COBOL names
     * @param context the translator options
     * @param parent used to resolve potential name conflict
     * @return a nice XML type name
     */
    public static String formatTypeName(
            final String elementName,
            final CobolDataItem cobolDataItem,
            final List < String > nonUniqueCobolNames,
            final Cob2XsdContext context,
            final XsdDataItem parent) {

        StringBuilder sb = new StringBuilder();
        sb.append(Character.toUpperCase(elementName.charAt(0)));
        sb.append(elementName.substring(1));
        
        if (nonUniqueCobolNames.contains(cobolDataItem.getCobolName())) {
            if (context.nameConflictPrependParentName()) {
                if (parent != null) {
                    sb.insert(0, parent.getXsdTypeName());
                }
            } else {
                sb.append(cobolDataItem.getSrceLine());
            }
        }

        return sb.toString();
    }
    
    /**
     * Turn a COBOL name to an XSD element name.
     * <p/>
     * This is not strictly necessary if there is no name conflict as the only requirement
     * for an XML schema element name is to be an NCName (non columnized name) which is
     * a superset of valid COBOL names.
     * <p/>
     * COBOL names look ugly in XML schema though. They are often uppercased and use
     * hyphens extensively. XML schema names they will have to be transformed later
     * to java identifiers so we try to get as close as possible to a naming convention
     * that suits XML Schema as well as Java.
     * <p>
     * So we remove hyphens.
     * We lower case all characters which are not word breakers. Word breakers are
     * hyphens and numerics. This creates Camel style names.
     * Element names customarily start with a lowercase character.
     * <p/>
     * COBOL FILLERs are a particular case because there might be more than one in the
     * same parent group. So what we do is systematically append the COBOL source line
     * number so that these become unique names.
     * @param cobolDataItem the original COBOL data item
     * @param context the translator options
     * @return an XML schema element name
     */
    public static String formatElementName(
            final CobolDataItem cobolDataItem,
            final Cob2XsdContext context) {
        
        String cobolName = cobolDataItem.getCobolName();
        if (cobolName.equalsIgnoreCase("FILLER")) {
            return "filler" + cobolDataItem.getSrceLine();
        }
        
        StringBuilder sb = new StringBuilder();
        boolean wordBreaker = (context.elementNamesStartWithUppercase()) ? true : false;
        for (int i = 0; i < cobolName.length(); i++) {
            char c = cobolName.charAt(i);
            if (c != '-') {
                if (Character.isDigit(c)) {
                    sb.append(c);
                    wordBreaker = true;
                } else {
                    if (wordBreaker) {
                        sb.append(Character.toUpperCase(c));
                    } else {
                        sb.append(Character.toLowerCase(c));
                    }
                    wordBreaker = false;
                }
            } else {
                wordBreaker = true;
            }
        }
        return sb.toString();
    }

    /**
     * @return the XML schema type mapping the COBOL data item
     */
    public XsdType getXsdType() {
        return _xsdType;
    }

    /**
     * @return the A derived COBOL type used in annotations
     */
    public CobolType getCobolType() {
        return _cobolType;
    }

    /**
     * @return the For xsd:string and xsd:hexBinary
     */
    public int getLength() {
        return _length;
    }

    /**
     * @return the For xsd:string derived from numeric edited
     */
    public String getPattern() {
        return _pattern;
    }

    /**
     * @return the For xsd numeric types, the total number of digits
     */
    public int getTotalDigits() {
        return _totalDigits;
    }

    /**
     * @return the For xsd numeric types, the fractional digits
     */
    public int getFractionDigits() {
        return _fractionDigits;
    }

    /**
     * @return the XML Schema element name
     */
    public String getXsdElementName() {
        return _xsdElementName;
    }

    /**
     * @return the XML Schema type name
     */
    public String getXsdTypeName() {
        return _xsdTypeName;
    }

    /**
     * @return the ordered list of direct children
     */
    public List < XsdDataItem > getChildren() {
        return _children;
    }
    /**
     * @return the minimum number of occurrences (XSD semantic)
     */
    public int getMinOccurs() {
        return _minOccurs;
    }

    /**
     * @return the minimum number of occurrences (COBOL semantic)
     */
    public int getCobolMinOccurs() {
        return _cobolDataItem.getMinOccurs();
    }

    /**
     * @return the maximum number of occurrences (XSD semantic)
     */
    public int getMaxOccurs() {
        return _maxOccurs;
    }

    /**
     * @return the maximum number of occurrences (COBOL semantic)
     */
    public int getCobolMaxOccurs() {
        return _cobolDataItem.getMaxOccurs();
    }

    /**
     * @return the Level in the hierarchy
     */
    public int getLevelNumber() {
        return _cobolDataItem.getLevelNumber();
    }
    /**
     * @return the Cobol element name
     */
    public String getCobolName() {
        return _cobolDataItem.getCobolName();
    }
    /**
     * @return the Cobol picture clause
     */
    public String getPicture() {
        return _cobolDataItem.getPicture();
    }

    /**
     * @return the Cobol usage clause (enum)
     */
    public Usage getUsage() {
        return _cobolDataItem.getUsage();
    }

    /**
     * @return the Cobol generic usage.
     * This is needed because COBOL usage values are not accepted as java identifiers.
     */
    public String getUsageForCobol() {
        switch (getUsage()) {
        case BINARY:
            return "BINARY";
        case SINGLEFLOAT:
            return "COMP-1";
        case DOUBLEFLOAT:
            return "COMP-2";
        case PACKEDDECIMAL:
            return "PACKED-DECIMAL";
        case NATIVEBINARY:
            return "COMP-5";
        case DISPLAY:
            return "DISPLAY";
        case DISPLAY1:
            return "DISPLAY-1";
        case INDEX:
            return "INDEX";
        case NATIONAL:
            return "NATIONAL";
        case POINTER:
            return "POINTER";
        case PROCEDUREPOINTER:
            return "PROCEDURE-POINTER";
            /** Function pointer. */
        case FUNCTIONPOINTER:
            return "FUNCTION-POINTER";
        default:
            return null;
        }
    }

    /**
     * @return true if String is right justified
     */
    public boolean isJustifiedRight() {
        return _cobolDataItem.isJustifiedRight();
    }

    /**
     * @return true if sign clause specifies sign in leading byte (false means trailing byte)
     */
    public boolean isSignLeading() {
        return _cobolDataItem.isSignLeading();
    }

    /**
     * @return true if sign clause specifies sign in separate byte (overpunch)
     */
    public boolean isSignSeparate() {
        return _cobolDataItem.isSignSeparate();
    }

    /**
     * @return the Cobol element giving array actual size
     */
    public String getDependingOn() {
        return _cobolDataItem.getDependingOn();
    }

    /**
     * @return true if a numeric item is signed
     */
    public boolean isSigned() {
        return _isSigned;
    }

    /**
     * @return the Cobol element sharing same memory location
     */
    public String getRedefines() {
        return _cobolDataItem.getRedefines();
    }

    /**
     * @return the Cobol value clause
     */
    public List < String > getValues() {
        return _cobolDataItem.getValues();
    }

    /**
     * @return the Line number in the original source file 
     */
    public int getSrceLine() {
        return _cobolDataItem.getSrceLine();
    }

    /**
     * @return the data entry type. Could also be inferred from the level
     */
    public DataEntryType getDataEntryType() {
        return _cobolDataItem.getDataEntryType();
    }

    /**
     * @return the one or more literal values of a condition
     */
    public List < String > getConditionLiterals() {
        return _cobolDataItem.getConditionLiterals();
    }

    /**
     * @return the one or more ranges of literal values of a condition
     */
    public List < Range >  getConditionRanges() {
        return _cobolDataItem.getConditionRanges();
    }

    /**
     * @return the parent data item or null if root
     */
    public XsdDataItem getParent() {
        return _parent;
    }

    /**
     * @return true if some array size downstream depends on this data item value
     */
    public boolean isODOObject() {
        return _isODOObject;
    }

    /**
     * @return true if some item downstream redefines this data item
     */
    public boolean isRedefined() {
        return _isRedefined;
    }

    /**
     * @param isODOObject true if some array size downstream depends on this data item value
     */
    public void setIsODOObject(final boolean isODOObject) {
        _isODOObject = isODOObject;
    }

    /**
     * @param isRedefined true if some item downstream redefines this data item
     */
    public void setIsRedefined(final boolean isRedefined) {
        _isRedefined = isRedefined;
    }
    
    /** {@inheritDoc}*/
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append('{');
        sb.append("typeName:" + getXsdTypeName());
        sb.append(',');
        sb.append("type:" + getXsdType().toString());
        sb.append(',');
        sb.append(_cobolDataItem.toString());
        return sb.toString();
        
    }

}
