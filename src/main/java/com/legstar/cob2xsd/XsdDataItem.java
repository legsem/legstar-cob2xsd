package com.legstar.cob2xsd;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.cobol.model.CobolDataItem;
import com.legstar.cobol.model.CobolDataItem.Usage;
import com.legstar.cobol.utils.PictureUtil;
import com.legstar.coxb.CobolType;

/**
 * XML Schema attributes derived from a COBOL data item.
 * Acts as a facade to the CobolDataItem type.
 * This class is constructed from a CobolDataItem. All XSD attributes
 * are derived at construction time.
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
        COMPLEX, STRING, HEXBINARY, SHORT, USHORT, INT, UINT, LONG, ULONG, INTEGER, DECIMAL, FLOAT, DOUBLE
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

    /** Lower bound for a numeric. */
    private String _minInclusive;

    /** Upper bound for a numeric. */
    private String _maxInclusive;

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
     * @param uniqueXsdTypeNames a list of unique type names used to detect name collisions
     */
    public XsdDataItem(
            final CobolDataItem cobolDataItem,
            final Cob2XsdContext context,
            final XsdDataItem parent,
            final List < String > uniqueXsdTypeNames) {

        _cobolDataItem = cobolDataItem;
        _parent = parent;

        _xsdTypeName = formatTypeName(cobolDataItem, uniqueXsdTypeNames);
        _xsdElementName = Character.toLowerCase(_xsdTypeName.charAt(0))
        + _xsdTypeName.substring(1);

        if (cobolDataItem.getChildren().size() > 0) {
            _xsdType = XsdType.COMPLEX;
            _cobolType = CobolType.GROUP_ITEM;
        } else {
            if (cobolDataItem.getUsage() != null) {
                setFromUsage(cobolDataItem.getUsage());
            }
            if (cobolDataItem.getPicture() != null) {
                setFromPicture(
                        cobolDataItem.getPicture(),
                        cobolDataItem.isSignSeparate(),
                        context.getCurrencySymbol(),
                        context.isNSymbolDbcs(),
                        context.decimalPointIsComma());
            }
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
            _children.add(new XsdDataItem(child, context, this, uniqueXsdTypeNames));
        }

        /* This should not happen. The COBOL type was not identified */
        if (_cobolType == null) {
            _cobolType = CobolType.GROUP_ITEM;
            _xsdType = XsdType.COMPLEX;
            _log.warn("Unable to identify COBOL type for " + cobolDataItem);
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
            _cobolType = CobolType.ALPHANUMERIC_ITEM;
            _xsdType = XsdType.STRING;
        }
    }

    /**
     * Derive XML schema attributes from a COBOL picture.
     * @param picture the picture clause
     * @param isSignSeparate if sign occupies a separated position (no overpunch)
     * @param currencySign the currency sign
     * @param nSymbolDbcs true if COBOL compiler option NSYMBOL(DBCS)
     * @param decimalPointIsComma if COBOL compiler option DECIMAL POINT IS COMMA
     */
    private void setFromPicture(
            final String picture,
            final boolean isSignSeparate,
            final char currencySign,
            final boolean nSymbolDbcs,
            final boolean decimalPointIsComma) {

        char comma = (decimalPointIsComma) ? '.' : ',';

        Map < Character, Integer > charNum =
            PictureUtil.getPictureCharOccurences(picture, currencySign);

        _length = PictureUtil.calcLengthFromPicture(charNum, isSignSeparate, currencySign);
        _pattern = PictureUtil.getRegexFromPicture(picture, currencySign);

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
                + charNum.get(currencySign)) > 0) {
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
        setNumericAttributes(picture, currencySign, decimalPointIsComma);

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
     * @param currencySign the currency sign
     * @param decimalPointIsComma true if decimal point is comma
     */
    private void setNumericAttributes(
            final String picture,
            final char currencySign,
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
                    picture.substring(0, iV), currencySign);
            decCharNum = PictureUtil.getPictureCharOccurences(
                    picture.substring(iV), currencySign);
            _fractionDigits = decCharNum.get('9');
            _totalDigits = intCharNum.get('9') + _fractionDigits;
        } else {
            intCharNum = PictureUtil.getPictureCharOccurences(picture, currencySign);
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

        /* For numerics with further constraints populate minInclusive
         * and maxInclusive. */
        if (_cobolType != CobolType.NATIVE_BINARY_ITEM) {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < (_totalDigits - _fractionDigits); i++) {
                sb.append('9');
            }
            if (_fractionDigits > 0) {
                sb.append('.');
                for (int i = 0; i < _fractionDigits; i++) {
                    sb.append('9');
                }
            }
            if (_isSigned) {
                _minInclusive = '-' + sb.toString();
            } else {
                _minInclusive = "0";
            }
            _maxInclusive = sb.toString();
        }
    }

    /**
     * Turn a COBOL name to an XSD type name.
     * <p/>
     * This is not strictly necessary as the only requirement for an XML schema element
     * name is to be an NCName (non columnized name) which is a superset of valid
     * COBOL names.
     * <p/>
     * COBOL names look ugly in XML schema though. They are often uppercased and hyphens,
     * even if they are valid for NCNames, will have to be transformed again when the 
     * XML schema is mapped to java.
     * So we remove hyphens.
     * We lower case all characters which are not word breakers. Word breakers are
     * hyphens and numerics. This creates Camel style names.
     * Complex type names customarily start with uppercase.
     * <p/>
     * The proposed name might be conflicting with another so we disambiguate xsd type
     * names by appending the source line number.
     * 
     * @param dataItem the original COBOL data item
     * @param uniqueXsdTypeNames a list of unique type names
     * @return a nice XML type name
     */
    public static String formatTypeName(
            final CobolDataItem dataItem,
            final List < String > uniqueXsdTypeNames) {

        StringBuilder sb = new StringBuilder();
        boolean wordBreaker = true;
        for (int i = 0; i < dataItem.getCobolName().length(); i++) {
            char c = dataItem.getCobolName().charAt(i);
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

        if (uniqueXsdTypeNames.contains(sb.toString())) {
            sb.append(dataItem.getSrceLine());
        }
        uniqueXsdTypeNames.add(sb.toString());
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
     * @return the Lower bound for a numeric
     */
    public String getMinInclusive() {
        return _minInclusive;
    }

    /**
     * @return the Upper bound for a numeric
     */
    public String getMaxInclusive() {
        return _maxInclusive;
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

}
