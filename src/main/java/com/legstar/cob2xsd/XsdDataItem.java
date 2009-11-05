package com.legstar.cob2xsd;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.legstar.cobol.model.CobolDataItem;
import com.legstar.cobol.model.CobolDataItem.Usage;
import com.legstar.cobol.utils.PictureUtil;
import com.legstar.coxb.CobolType;

/**
 * XML Schema attributes derived from a COBOL data item.
 * Acts as a facade to the CobolDataItem type.
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


    /**
     * COBOL data item is analyzed at construction time.
     * @param dataItem the COBOL elementary data item
     * @param context the translator options in effect
     */
    public XsdDataItem(final CobolDataItem dataItem, final Cob2XsdContext context) {

        _cobolDataItem = dataItem;
        
        _xsdTypeName = formatTypeName(dataItem.getCobolName());
        _xsdElementName = Character.toLowerCase(_xsdTypeName.charAt(0)) + _xsdTypeName.substring(1);

        /* By default an item is a structure */
        _xsdType = XsdType.COMPLEX;
        _cobolType = CobolType.GROUP_ITEM;
        
        /* Create the list of children by decorating the COBOL item children */
        for (CobolDataItem child : dataItem.getChildren()) {
            _children.add(new XsdDataItem(child, context));
        }

        if (dataItem.getUsage() != null) {
            setFromUsage(dataItem.getUsage());
        }
        if (dataItem.getPicture() != null) {
            setFromPicture(
                    dataItem.getPicture(),
                    dataItem.isSignSeparate(),
                    context.getCurrencySymbol(),
                    context.isNSymbolDbcs(),
                    context.decimalPointIsComma());
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

        /* Usage was DISPLAY, we can now refine since we now know it is a
         * numeric not an alphanumeric. */
        if (_cobolType == CobolType.ALPHANUMERIC_ITEM) {
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

        boolean signed = (intCharNum.get('S') > 0) ? true : false;

        if (_fractionDigits == 0) {
            if (_totalDigits < 5) {
                _xsdType = (signed) ? XsdType.SHORT : XsdType.USHORT;
            } else if (_totalDigits < 10) {
                _xsdType = (signed) ? XsdType.INT : XsdType.UINT;
            } else if (_totalDigits < 20) {
                _xsdType = (signed) ? XsdType.LONG : XsdType.ULONG;
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
            if (signed) {
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
     * @param cobolName the original COBOL name
     * @return a nice XML type name
     */
    public static String formatTypeName(final String cobolName) {

        StringBuilder sb = new StringBuilder();
        boolean wordBreaker = true;
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
     * @return the minimum number of occurrences
     */
    public int getMinOccurs() {
        return _cobolDataItem.getMinOccurs();
    }

    /**
     * @return the maximum number of occurrences
     */
    public int getMaxOccurs() {
        return _cobolDataItem.getMaxOccurs();
    }

}
