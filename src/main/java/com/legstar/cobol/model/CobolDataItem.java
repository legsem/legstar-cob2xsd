package com.legstar.cobol.model;

import java.util.LinkedList;
import java.util.List;

/**
 * Model of a COBOL data entry.
 *
 */
public class CobolDataItem {

    /** Level in the hierarchy this element was parsed from. */
    private int _levelNumber = 1;

    /** Cobol element name. */
    private String _cobolName = "FILLER";

    /** These correspond to the 3 different formats for data entries. */
    public enum DataEntryType { DATA_DESCRIPTION, RENAMES, CONDITION };

    /** The data entry type. Could also be inferred from the level. */
    private DataEntryType _dataEntryType = DataEntryType.DATA_DESCRIPTION;

    /** Cobol element sharing same memory location. */
    private String _redefines;

    /** Blank when zero clause. */
    private boolean _blankWhenZero;

    /** External clause. */
    private boolean _isExternal;

    /** Global clause. */
    private boolean _isGlobal;

    /** Group usage national clause. */
    private boolean _groupUsageNational;

    /** String justification. */
    private boolean _isJustifiedRight;

    /** Cobol picture clause. */
    private String _picture;

    /** Sign clause. */
    private boolean _isSign;

    /** Sign clause specifies sign in leading byte or trailing byte. */
    private boolean _isSignLeading;

    /** Sign clause specifies if sign clause specifies sign in separate byte (overpunch). */
    private boolean _isSignSeparate;

    /** Item must be synchronized on natural boundary in storage. */
    private boolean _isSynchronized;

    /** Arrays minimum number of occurrences. A value of -1 means this was not
     * explicitly set. */
    private int _minOccurs = -1;

    /** Arrays maximum number of occurrences. */
    private int _maxOccurs = -1;

    /** Cobol element giving array actual size. */
    private String _dependingOn;

    /** Cobol indexed by sub-clauses.*/
    private List < String > _indexes = new LinkedList < String >();

    /** Cobol ascending key is sub-clauses.*/
    private List < String > _ascendingKeys = new LinkedList < String >();

    /** Cobol descending key is sub-clauses.*/
    private List < String > _descendingKeys = new LinkedList < String >();

    /** Cobol usage. */
    private Usage _usage = null;

    /** Cobol value clause. Conditions might have multiple values. */
    private List < String > _values = new LinkedList < String >();

    /** Cobol date format clause.*/
    private String _dateFormat;

    /** Line number in the original source file this element was parsed from. */
    private int _srceLine;

    /** Ordered list of direct children.*/
    private List < CobolDataItem > _children = new LinkedList < CobolDataItem >();

    /** Used with RENAMES clause when there is a single subject.*/
    private String _renamesSubject;

    /** Used with RENAMES clause when there is a range of subjects.*/
    private Range _renamesSubjectRange;

    /** Used with condition clauses for one or more literal values.*/
    private List < String > _conditionLiterals = new LinkedList < String >();

    /** Used with condition clauses for one or more ranges of literal values.*/
    private List < Range > _conditionRanges = new LinkedList < Range >();

    /** Floating point or fixed numerics. */
    public enum Usage {
        /** COMP. */
        BINARY,
        /** COMP-1. */
        SINGLEFLOAT,
        /** COMP-2. */
        DOUBLEFLOAT,
        /** COMP-3. */
        PACKEDDECIMAL,
        /** COMP-5. */
        NATIVEBINARY,
        /** DISPLAY. */
        DISPLAY,
        /** DBCS. */
        DISPLAY1,
        /** INDEX. */
        INDEX,
        /** UTF-16. */
        NATIONAL,
        /** Pointer. */
        POINTER,
        /** Procedure pointer. */
        PROCEDUREPOINTER,
        /** Function pointer. */
        FUNCTIONPOINTER
    };

    /**
     * No argument constructor.
     */
    public CobolDataItem() {
        
    }

    /**
     * No argument constructor.
     * @param cobolName the data item COBOL name
     */
    public CobolDataItem(final String cobolName) {
        _cobolName = cobolName;
    }

    /**
     * @return the Level in the hierarchy
     */
    public int getLevelNumber() {
        return _levelNumber;
    }

    /**
     * @param levelNumber the Level in the hierarchy to set
     */
    public void setLevelNumber(final int levelNumber) {
        _levelNumber = levelNumber;
    }

    /**
     * @return the Cobol element name
     */
    public String getCobolName() {
        return _cobolName;
    }

    /**
     * @param cobolName the Cobol element name to set
     */
    public void setCobolName(final String cobolName) {
        _cobolName = cobolName;
    }

    /**
     * @return the data entry type. Could also be inferred from the level
     */
    public DataEntryType getDataEntryType() {
        return _dataEntryType;
    }

    /**
     * @param dataEntryType the data entry type to set
     */
    public void setDataEntryType(final DataEntryType dataEntryType) {
        _dataEntryType = dataEntryType;
    }

    /**
     * @return the Cobol element sharing same memory location
     */
    public String getRedefines() {
        return _redefines;
    }

    /**
     * @param redefines Cobol element sharing same memory location to set
     */
    public void setRedefines(final String redefines) {
        _redefines = redefines;
    }

    /**
     * @return the blank when zero clause
     */
    public boolean isBlankWhenZero() {
        return _blankWhenZero;
    }

    /**
     * @param blankWhenZero the blank when zero clause to set
     */
    public void setBlankWhenZero(final boolean blankWhenZero) {
        _blankWhenZero = blankWhenZero;
    }

    /**
     * @return the external clause
     */
    public boolean isExternal() {
        return _isExternal;
    }

    /**
     * @param external the external clause to set
     */
    public void setExternal(final boolean external) {
        _isExternal = external;
    }

    /**
     * @return the global clause
     */
    public boolean isGlobal() {
        return _isGlobal;
    }

    /**
     * @param global the global clause to set
     */
    public void setGlobal(final boolean global) {
        _isGlobal = global;
    }

    /**
     * @return the groupusagenational clause
     */
    public boolean isGroupUsageNational() {
        return _groupUsageNational;
    }

    /**
     * @param groupUsageNational the group usage national clause to set
     */
    public void setGroupUsageNational(final boolean groupUsageNational) {
        _groupUsageNational = groupUsageNational;
    }

    /**
     * @return true if String is right justified
     */
    public boolean isJustifiedRight() {
        return _isJustifiedRight;
    }

    /**
     * @param justifiedRight true if String is right justified
     */
    public void setJustifiedRight(
            final boolean justifiedRight) {
        _isJustifiedRight = justifiedRight;
    }

    /**
     * @return the Cobol picture clause
     */
    public String getPicture() {
        return _picture;
    }

    /**
     * @param picture the Cobol picture clause to set
     */
    public void setPicture(final String picture) {
        _picture = picture;
    }

    /**
     * @return sign clause
     */
    public boolean isSign() {
        return _isSign;
    }

    /**
     * @param isSign sign clause to set
     */
    public void setSign(final boolean isSign) {
        _isSign = isSign;
    }

    /**
     * @return true if sign clause specifies sign in leading byte (false means trailing byte)
     */
    public boolean isSignLeading() {
        return _isSignLeading;
    }

    /**
     * @param isSignLeading true itrue if sign clause specifies sign in leading byte (false means trailing byte)
     */
    public void setSignLeading(final boolean isSignLeading) {
        _isSignLeading = isSignLeading;
    }

    /**
     * @return true if sign clause specifies sign in separate byte (overpunch)
     */
    public boolean isSignSeparate() {
        return _isSignSeparate;
    }

    /**
     * @param isSignSeparate true if sign clause specifies sign in separate byte (overpunch)
     */
    public void setSignSeparate(final boolean isSignSeparate) {
        _isSignSeparate = isSignSeparate;
    }

    /**
     * @return true if the item must be synchronized on natural boundary in storage
     */
    public boolean isSynchronized() {
        return _isSynchronized;
    }

    /**
     * @param isSynchronized true if the item must be synchronized on natural boundary in storage
     */
    public void setSynchronized(final boolean isSynchronized) {
        _isSynchronized = isSynchronized;
    }

    /**
     * @return the minimum number of occurrences
     */
    public int getMinOccurs() {
        return _minOccurs;
    }

    /**
     * @param minOccurs the minimum number of occurrences to set
     */
    public void setMinOccurs(final int minOccurs) {
        _minOccurs = minOccurs;
    }

    /**
     * @return the maximum number of occurrences
     */
    public int getMaxOccurs() {
        return _maxOccurs;
    }

    /**
     * @param maxOccurs the maximum number of occurrences to set
     */
    public void setMaxOccurs(final int maxOccurs) {
        _maxOccurs = maxOccurs;
    }

    /**
     * @return the Cobol element giving array actual size
     */
    public String getDependingOn() {
        return _dependingOn;
    }

    /**
     * @param dependingOn the Cobol element giving array actual size to set
     */
    public void setDependingOn(final String dependingOn) {
        _dependingOn = dependingOn;
    }

    /**
     * @return the cobol indexed by sub-clauses
     */
    public List < String >  getIndexes() {
        return _indexes;
    }

    /**
     * @param indexes the cobol indexed by sub-clauses to set
     */
    public void setIndexes(final List < String > indexes) {
        _indexes = indexes;
    }

    /**
     * @param index a cobol index to add
     */
    public void addIndex(final String index) {
        _indexes.add(index);
    }

    /**
     * @return the cobol ascending key is sub-clauses
     */
    public List < String >  getAscendingKeys() {
        return _ascendingKeys;
    }

    /**
     * @param ascendingKeys the cobol ascending key is sub-clauses to set
     */
    public void setAscendingKeys(final List < String > ascendingKeys) {
        _ascendingKeys = ascendingKeys;
    }

    /**
     * @param ascendingKey a cobol ascending key to add
     */
    public void addAscendingKey(final String ascendingKey) {
        _ascendingKeys.add(ascendingKey);
    }

    /**
     * @return the cobol descending key is sub-clauses
     */
    public List < String >  getDescendingKeys() {
        return _descendingKeys;
    }

    /**
     * @param descendingKeys the cobol descending key is sub-clauses to set
     */
    public void setDescendingKeys(final List < String > descendingKeys) {
        _descendingKeys = descendingKeys;
    }

    /**
     * @param descendingKey a cobol descending key to add
     */
    public void addDescendingKey(final String descendingKey) {
        _descendingKeys.add(descendingKey);
    }

    /**
     * @return the Cobol usage clause (enum)
     */
    public Usage getUsage() {
        return _usage;
    }

    /**
     * @param usage the Cobol usage to set
     */
    public void setUsage(final Usage usage) {
        _usage = usage;
    }

    /**
     * @return the Cobol value clause
     */
    public List < String > getValues() {
        return _values;
    }

    /**
     * @param values the Cobol values to set
     */
    public void setValues(final List < String > values) {
        _values = values;
    }

    /**
     * @param value a cobol value to add
     */
    public void addValue(final String value) {
        _values.add(value);
    }

    /**
     * @return the cobol date format clause
     */
    public String getDateFormat() {
        return _dateFormat;
    }

    /**
     * @param dateFormat the cobol date format clause to set
     */
    public void setDateFormat(final String dateFormat) {
        _dateFormat = dateFormat;
    }

    /**
     * @return the Line number in the original source file 
     */
    public int getSrceLine() {
        return _srceLine;
    }

    /**
     * @param srceLine the Line number in the original source file  to set
     */
    public void setSrceLine(final int srceLine) {
        _srceLine = srceLine;
    }

    /**
     * @return the ordered list of direct children
     */
    public List < CobolDataItem > getChildren() {
        return _children;
    }

    /**
     * @param children the ordered list of direct children to set
     */
    public void setChildren(final List < CobolDataItem > children) {
        _children = children;
    }

    /**
     * @return true if this data item is a structure (group).
     */
    public boolean isStructure() {
        return (getChildren().size() == 0);
    }

    /**
     * @return the single subject of a RENAMES clause
     */
    public String getRenamesSubject() {
        return _renamesSubject;
    }

    /**
     * @param renamesSubject the single subject of a RENAMES clause to set
     */
    public void setRenamesSubject(final String renamesSubject) {
        _renamesSubject = renamesSubject;
    }

    /**
     * @return the range of subjects of a RENAMES clause
     */
    public Range getRenamesSubjectRange() {
        return _renamesSubjectRange;
    }

    /**
     * @param renamesSubjectRange the range of subjects of a RENAMES clause to set
     */
    public void setRenamesSubjectRange(final Range renamesSubjectRange) {
        _renamesSubjectRange = renamesSubjectRange;
    }

    /**
     * @return the one or more literal values of a condition
     */
    public List < String > getConditionLiterals() {
        return _conditionLiterals;
    }

    /**
     * @param conditionLiterals the one or more literal values of a condition to set
     */
    public void setConditionLiterals(final List < String >  conditionLiterals) {
        _conditionLiterals = conditionLiterals;
    }

    /**
     * @param conditionLiteral a literal value of a condition to set
     */
    public void addConditionLiterals(final String conditionLiteral) {
        _conditionLiterals.add(conditionLiteral);
    }

    /**
     * @return the one or more ranges of literal values of a condition
     */
    public List < Range >  getConditionRanges() {
        return _conditionRanges;
    }

    /**
     * @param conditionRanges the one or more ranges of literal values of a condition to set
     */
    public void setConditionRanges(final List < Range >  conditionRanges) {
        _conditionRanges = conditionRanges;
    }

    /**
     * @param conditionRange the range of literal values of a condition to set
     */
    public void addConditionRange(final Range conditionRange) {
        _conditionRanges.add(conditionRange);
    }

    /**
     * Represents a range between two literals.
     */
    public static class Range {
        /** Range start. */
        private String _from;
        /** Range stop. */
        private String _to;

        /**
         * Constructor for immutable class.
         * @param from range start
         * @param to range stop
         */
        public Range(final String from, final String to) {
            _from = from;
            _to = to;
        }
        /**
         * @return the range start
         */
        public String getFrom() {
            return _from;
        }
        /**
         * @return the range stop
         */
        public String getTo() {
            return _to;
        }

        /** {@inheritDoc} */
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append('{');
            sb.append("from:" + getFrom());
            sb.append(',');
            sb.append("to:" + getTo());
            sb.append('}');
            return sb.toString();
        }
    }

    /** {@inheritDoc} */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append('{');
        sb.append("level:" + getLevelNumber());
        sb.append(',');
        sb.append("cobolName:" + getCobolName());

        if (getDataEntryType() != null) {
            switch(getDataEntryType()) {
            case DATA_DESCRIPTION:
                toStringDataDescription(sb);
                break;
            case RENAMES:
                toStringRenames(sb);
                break;
            case CONDITION:
                toStringCondition(sb);
                break;
            default:
                break;
            }
        }
        sb.append(',');
        sb.append("srceLine:" + getSrceLine());
        sb.append('}');
        return sb.toString();
    }

    /**
     * Pretty printing for a complete data description entry.
     * @param sb the string builder
     */
    private void toStringDataDescription(final StringBuilder sb) {
        if (getRedefines() != null) {
            sb.append(',');
            sb.append("redefines:" + getRedefines());
        }
        if (isBlankWhenZero()) {
            sb.append(',');
            sb.append("isBlankWhenZero:" + isBlankWhenZero());
        }
        if (isExternal()) {
            sb.append(',');
            sb.append("isExternal:" + isExternal());
        }
        if (isGlobal()) {
            sb.append(',');
            sb.append("isGlobal:" + isGlobal());
        }
        if (isGroupUsageNational()) {
            sb.append(',');
            sb.append("groupUsageNational:" + isGroupUsageNational());
        }
        if (getMaxOccurs() > 0) {
            if (getMinOccurs() > -1) {
                sb.append(',');
                sb.append("minOccurs:" + getMinOccurs());
            }
            sb.append(',');
            sb.append("maxOccurs:" + getMaxOccurs());
            if (getDependingOn() != null) {
                sb.append(',');
                sb.append("dependingOn:" + getDependingOn());
            }
            if (getIndexes().size() > 0) {
                toStringList(sb, getIndexes(), "indexes");
            }
            if (getAscendingKeys().size() > 0) {
                toStringList(sb, getAscendingKeys(), "ascendingKeys");
            }
            if (getDescendingKeys().size() > 0) {
                toStringList(sb, getDescendingKeys(), "descendingKeys");
            }
        }
        if (isSign()) {
            sb.append(',');
            sb.append("isSign:" + isSign());
            sb.append(',');
            sb.append("isSignLeading:" + isSignLeading());
            sb.append(',');
            sb.append("isSignSeparate:" + isSignSeparate());
        }
        if (isSynchronized()) {
            sb.append(',');
            sb.append("isSynchronized:" + isSynchronized());
        }
        if (getUsage() != null) {
            sb.append(',');
            sb.append("usage:" + getUsage());
        }
        if (getChildren().size() == 0) {
            if (isJustifiedRight()) {
                sb.append(',');
                sb.append("isJustifiedRight:" + isJustifiedRight());
            }
            if (getPicture() != null) {
                sb.append(',');
                sb.append("picture:" + '\"' + getPicture() + '\"');
            }
        }
        if (getValues().size() > 0) {
            toStringList(sb, getValues(), "values");
        }
        if (getDateFormat() != null) {
            sb.append(',');
            sb.append("dateFormat:" + getDateFormat());
        }
        if (getChildren().size() > 0) {
            toStringList(sb, getChildren(), "children");
        }
    }

    /**
     * Pretty printing for a  renames entry.
     * @param sb the string builder
     */
    private void toStringRenames(final StringBuilder sb) {
        if (getRenamesSubject() != null) {
            sb.append(',');
            sb.append("renamesSubject:" + getRenamesSubject());
        }
        if (getRenamesSubjectRange() != null) {
            sb.append(',');
            sb.append("renamesSubjectRange:" + getRenamesSubjectRange());
        }

    }

    /**
     * Pretty printing for a  condition entry.
     * @param sb the string builder
     */
    private void toStringCondition(final StringBuilder sb) {
        if (getConditionLiterals().size() > 0) {
            toStringList(sb, getConditionLiterals(), "conditionLiterals");
        }
        if (getConditionRanges().size() > 0) {
            toStringList(sb, getConditionRanges(), "conditionRanges");
        }

    }

    /**
     * Adds list elements to a string builder.
     * @param sb the string builder
     * @param list list of elements
     * @param title name of elements list
     */
    private void toStringList(final StringBuilder sb, final List < ? > list, final String title) {
        sb.append(',');
        sb.append(title + ":[");
        boolean first = true;
        for (Object child : list) {
            if (!first) {
                sb.append(",");
            } else {
                first = false;
            }
            sb.append(child.toString());
        }
        sb.append("]");
    }

}
