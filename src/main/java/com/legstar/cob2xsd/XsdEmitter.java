package com.legstar.cob2xsd;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;

import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaAnnotation;
import org.apache.ws.commons.schema.XmlSchemaAppInfo;
import org.apache.ws.commons.schema.XmlSchemaComplexType;
import org.apache.ws.commons.schema.XmlSchemaElement;
import org.apache.ws.commons.schema.XmlSchemaFractionDigitsFacet;
import org.apache.ws.commons.schema.XmlSchemaLengthFacet;
import org.apache.ws.commons.schema.XmlSchemaMaxInclusiveFacet;
import org.apache.ws.commons.schema.XmlSchemaMinInclusiveFacet;
import org.apache.ws.commons.schema.XmlSchemaPatternFacet;
import org.apache.ws.commons.schema.XmlSchemaSequence;
import org.apache.ws.commons.schema.XmlSchemaSimpleType;
import org.apache.ws.commons.schema.XmlSchemaSimpleTypeRestriction;
import org.apache.ws.commons.schema.XmlSchemaTotalDigitsFacet;
import org.apache.ws.commons.schema.XmlSchemaType;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.legstar.coxb.CobolMarkup;

/**
 * Populates an XML Schema from COBOL data items.
 * TODO comment
 * TODO add option to control inclusion of LegStar specific annotations
 *
 */
public class XsdEmitter {

    /** This builder is used for annotation markup elements. */
    private DocumentBuilder _docBuilder;

    /** The XML Schema being built. */
    private XmlSchema _xsd;

    /** The translator options in effect. */
    private Cob2XsdContext _context;

    /**TODO externalize in options Cobol annotations namespace. */
    private static final String COBOL_NS = "http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd";

    /**TODO externalize in options Cobol annotation parent element name. */
    private static final String COBOL_PARENT_ELN = "cb:cobolElements";

    /**TODO externalize in options Cobol annotation element name. */
    private static final String COBOL_ELN = "cb:cobolElement";

    /**TODO make XSD name formatting optional*/

    /**
     * Constructor.
     * @param xsd the XML Schema to be populated.
     * @param docBuilder a DOM document builder for COBOL annotations markup
     * @param context the translator options
     */
    public XsdEmitter(
            final XmlSchema xsd,
            final DocumentBuilder docBuilder,
            final Cob2XsdContext context) {
        _docBuilder = docBuilder;
        _xsd = xsd;
        _context = context;
    }

    /**
     * Maps a COBOL data item to an XML schema type.
     * <ul>
     * <li>COBOL elementary data items are mapped to XML Schema simple types.</li>
     * <li>COBOL structures are mapped to XML schema complex Types.</li>
     * </ul>
     * @param xsdDataItem COBOL data item decorated with XSD attributes
     * @return a corresponding XML schema type
     */
    public XmlSchemaType createXmlSchemaType(final XsdDataItem xsdDataItem) {
        switch (xsdDataItem.getXsdType()) {
        case COMPLEX: 
            return createXmlSchemaComplexType(xsdDataItem);
        case STRING: 
            return createAlphaXmlSchemaSimpleType(xsdDataItem, "string");
        case HEXBINARY: 
            return createAlphaXmlSchemaSimpleType(xsdDataItem, "hexBinary");
        case SHORT: 
            return createNumericXmlSchemaSimpleType(xsdDataItem, "short");
        case USHORT: 
            return createNumericXmlSchemaSimpleType(xsdDataItem, "unsignedShort");
        case INT: 
            return createNumericXmlSchemaSimpleType(xsdDataItem, "int");
        case UINT: 
            return createNumericXmlSchemaSimpleType(xsdDataItem, "unsignedInt");
        case LONG: 
            return createNumericXmlSchemaSimpleType(xsdDataItem, "long");
        case ULONG: 
            return createNumericXmlSchemaSimpleType(xsdDataItem, "unsignedLong");
        case INTEGER: 
            return createNumericXmlSchemaSimpleType(xsdDataItem, "integer");
        case DECIMAL: 
            return createNumericXmlSchemaSimpleType(xsdDataItem, "decimal");
        case FLOAT: 
            return createNumericXmlSchemaSimpleType(xsdDataItem, "float");
        case DOUBLE: 
            return createNumericXmlSchemaSimpleType(xsdDataItem, "double");
        default:
            return null;
        }
    }

    /**
     * Create an XML schema complex type.
     * @param xsdDataItem COBOL data item decorated with XSD attributes
     * @return a new complex type
     */
    public XmlSchemaComplexType createXmlSchemaComplexType(final XsdDataItem xsdDataItem) {
        XmlSchemaSequence xmlSchemaSequence = new XmlSchemaSequence();
        for (XsdDataItem child : xsdDataItem.getChildren()) {
            xmlSchemaSequence.getItems().add(
                    createXmlSchemaElement(child));
        }
        XmlSchemaComplexType xmlSchemaComplexType  = new XmlSchemaComplexType(getXsd());
        xmlSchemaComplexType.setParticle(xmlSchemaSequence);
        xmlSchemaComplexType.setName(xsdDataItem.getXsdTypeName());

        return xmlSchemaComplexType;
    }

    /**
     * Create an XML Schema element from a COBOL data item.
     * @param xsdDataItem COBOL data item decorated with XSD attributes
     * @return the XML schema element
     */
    public XmlSchemaElement createXmlSchemaElement(final XsdDataItem xsdDataItem) {
        XmlSchemaElement element = new XmlSchemaElement();
        element.setName(xsdDataItem.getXsdElementName());
        /* TODO review the semantic discrepancy between COBOL and XSD here .
         * We should allow elements to be optional. */
        if (xsdDataItem.getMaxOccurs() > 0) {
            element.setMinOccurs(xsdDataItem.getMinOccurs());
            element.setMaxOccurs(xsdDataItem.getMaxOccurs());
        }
        element.setSchemaType(createXmlSchemaType(xsdDataItem));
        if (getContext().addLegStarAnnotations()) {
            element.setAnnotation(createLegStarAnnotation());
        }
        return element;
    }

    /**
     * Create a simple type for an alphanumeric type.
     * <p/>
     * COBOL alphanumeric fields are fixed length so we create a facet to enforce that constraint.
     * A pattern derived from the picture clause can also be used as a facet.
     * @param xsdDataItem COBOL data item decorated with XSD attributes
      * @param xsdTypeName the XML schema built-in type name to use as a restriction
    * @return an XML schema simple type
     */
    protected XmlSchemaSimpleType createAlphaXmlSchemaSimpleType(
            final XsdDataItem xsdDataItem, final String xsdTypeName) {

        XmlSchemaSimpleTypeRestriction restriction = createRestriction(xsdTypeName);
        if (xsdDataItem.getLength() > -1) {
            restriction.getFacets().add(createLengthFacet(xsdDataItem.getLength()));
        }
        if (xsdDataItem.getPattern() != null) {
            restriction.getFacets().add(createPatternFacet(xsdDataItem.getPattern()));
        }
        return createXmlSchemaSimpleType(restriction);
    }

    /**
     * Create a simple type for an numeric type.
     * <p/>
     * These fields have totaDigits and fractionDigits facets as well as minInclusive and MaxInclusive.
     * @param xsdDataItem COBOL data item decorated with XSD attributes
     * @param xsdTypeName the XML schema built-in type name to use as a restriction
     * @return an XML schema simple type
     */
    protected XmlSchemaSimpleType createNumericXmlSchemaSimpleType(
            final XsdDataItem xsdDataItem, final String xsdTypeName) {

        XmlSchemaSimpleTypeRestriction restriction = createRestriction(xsdTypeName);
        if (xsdDataItem.getTotalDigits() > -1) {
            restriction.getFacets().add(createTotalDigitsFacet(xsdDataItem.getTotalDigits()));
        }
        /* fractionDigits is a fixed facet for most numerics so be careful */
        if (xsdDataItem.getFractionDigits() > 0) {
            restriction.getFacets().add(createFractionDigitsFacet(xsdDataItem.getFractionDigits()));
        }
        if (xsdDataItem.getMinInclusive() != null) {
            restriction.getFacets().add(createMinInclusiveFacet(xsdDataItem.getMinInclusive()));
        }
        if (xsdDataItem.getMaxInclusive() != null) {
            restriction.getFacets().add(createMaxInclusiveFacet(xsdDataItem.getMaxInclusive()));
        }
        return createXmlSchemaSimpleType(restriction);
    }
    
    /**
     * Create an XML schema simple type from a restriction.
     * @param restriction the XML schema restriction
     * @return the XML schema simple type
     */
    protected XmlSchemaSimpleType createXmlSchemaSimpleType(final XmlSchemaSimpleTypeRestriction restriction) {
        XmlSchemaSimpleType xmlSchemaSimpleType = new XmlSchemaSimpleType(getXsd());
        xmlSchemaSimpleType.setContent(restriction);
        return xmlSchemaSimpleType;
    }
    
    /**
     * Create an XML schema restriction.
     * @param xsdTypeName the XML schema built-in type name to use as a restriction
     * @return an XML schema restriction
     */
    protected XmlSchemaSimpleTypeRestriction createRestriction(final String xsdTypeName) {
        XmlSchemaSimpleTypeRestriction restriction = new XmlSchemaSimpleTypeRestriction();
        restriction.setBaseTypeName(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, xsdTypeName));
        return restriction;
    }

    /**
     * Create an XML schema length facet.
     * @param length the value to set
     * @return an XML schema length facet
     */
    protected XmlSchemaLengthFacet createLengthFacet(final int length) {
        XmlSchemaLengthFacet xmlSchemaLengthFacet = new XmlSchemaLengthFacet();
        xmlSchemaLengthFacet.setValue(length);
        return xmlSchemaLengthFacet;
    }

    /**
     * Create an XML schema pattern facet.
     * @param pattern the value to set
     * @return an XML schema pattern facet
     */
    protected XmlSchemaPatternFacet createPatternFacet(final String pattern) {
        XmlSchemaPatternFacet xmlSchemaPatternFacet = new XmlSchemaPatternFacet();
        xmlSchemaPatternFacet.setValue(pattern);
        return xmlSchemaPatternFacet;
    }

    /**
     * Create an XML schema totalDigits facet.
     * @param totalDigits the value to set
     * @return an XML schema totalDigits facet
     */
    protected XmlSchemaTotalDigitsFacet createTotalDigitsFacet(final int totalDigits) {
        XmlSchemaTotalDigitsFacet xmlSchemaTotalDigitsFacet = new XmlSchemaTotalDigitsFacet();
        xmlSchemaTotalDigitsFacet.setValue(totalDigits);
        return xmlSchemaTotalDigitsFacet;
    }
    
    /**
     * Create an XML schema fractionDigits facet.
     * @param fractionDigits the value to set
     * @return an XML schema fractionDigits facet
     */
    protected XmlSchemaFractionDigitsFacet createFractionDigitsFacet(final int fractionDigits) {
        XmlSchemaFractionDigitsFacet xmlSchemaFractionDigitsFacet = new XmlSchemaFractionDigitsFacet();
        xmlSchemaFractionDigitsFacet.setValue(fractionDigits);
        return xmlSchemaFractionDigitsFacet;
    }
    
    /**
     * Create an XML schema minInclusive facet.
     * @param minInclusive the value to set
     * @return an XML schema minInclusive facet
     */
    protected XmlSchemaMinInclusiveFacet createMinInclusiveFacet(final String minInclusive) {
        XmlSchemaMinInclusiveFacet xmlSchemaMinInclusiveFacet = new XmlSchemaMinInclusiveFacet();
        xmlSchemaMinInclusiveFacet.setValue(minInclusive);
        return xmlSchemaMinInclusiveFacet;
    }
    
    /**
     * Create an XML schema maxInclusive facet.
     * @param maxInclusive the value to set
     * @return an XML schema maxInclusive facet
     */
    protected XmlSchemaMaxInclusiveFacet createMaxInclusiveFacet(final String maxInclusive) {
        XmlSchemaMaxInclusiveFacet xmlSchemaMaxInclusiveFacet = new XmlSchemaMaxInclusiveFacet();
        xmlSchemaMaxInclusiveFacet.setValue(maxInclusive);
        return xmlSchemaMaxInclusiveFacet;
    }
    
    /**
     * Create an XML Schema annotation with markup corresponding to the original COBOL
     * data item attributes. 
     * @return an XML schema annotation
     */
    protected XmlSchemaAnnotation createLegStarAnnotation() {
        
        Document doc = _docBuilder.newDocument();
        Element el = doc.createElementNS(COBOL_NS, COBOL_PARENT_ELN);
        Element elc = doc.createElementNS(COBOL_NS, COBOL_ELN);

        elc.setAttribute(CobolMarkup.LEVEL_NUMBER, Integer.toString(5));
        elc.setAttribute(CobolMarkup.COBOL_NAME, "C-ARRAY");

        el.appendChild(elc);

        XmlSchemaAppInfo appInfo = new XmlSchemaAppInfo();
        appInfo.setMarkup(el.getChildNodes());

        /* Create annotation */
        XmlSchemaAnnotation annotation = new XmlSchemaAnnotation();
        annotation.getItems().add(appInfo);
        return annotation;
    }

    /**
     * @return the builder used for annotation markup elements
     */
    public DocumentBuilder getDocBuilder() {
        return _docBuilder;
    }

    /**
     * @return the XML Schema being built
     */
    public XmlSchema getXsd() {
        return _xsd;
    }

    /**
     * @return the translator options in effect
     */
    public Cob2XsdContext getContext() {
        return _context;
    }

}
