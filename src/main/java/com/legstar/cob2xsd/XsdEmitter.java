package com.legstar.cob2xsd;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;

import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaComplexType;
import org.apache.ws.commons.schema.XmlSchemaElement;
import org.apache.ws.commons.schema.XmlSchemaLengthFacet;
import org.apache.ws.commons.schema.XmlSchemaSequence;
import org.apache.ws.commons.schema.XmlSchemaSimpleType;
import org.apache.ws.commons.schema.XmlSchemaSimpleTypeRestriction;
import org.apache.ws.commons.schema.XmlSchemaType;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.legstar.cobol.model.CobolDataItem;
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

    /**TODO externalize in options Cobol annotations namespace. */
    private static final String COBOL_NS = "http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd";

    /**TODO externalize in options Cobol annotation parent element name. */
    private static final String COBOL_PARENT_ELN = "cb:cobolElements";

    /**TODO externalize in options Cobol annotation element name. */
    private static final String COBOL_ELN = "cb:cobolElement";
    
    /**TODO externalize in options whether we should generate COBOL/JAXB annotations */
    private boolean addLegstarAnnotations = true;

    /**
     * Constructor.
     * @param xsd the XML Schema to be populated.
     * @param docBuilder a DOM document builder for COBOL annaotations markup
     */
    public XsdEmitter(final XmlSchema xsd, final DocumentBuilder docBuilder) {
        _docBuilder = docBuilder;
        _xsd = xsd;
    }
    
    /**
     * Create an XML Schema element from a COBOL data item.
     * @param dataItem COBOL data item
     * @param xsd the XML schema being built
     * @return the XML schema element
     */
    public XmlSchemaElement createXmlSchemaElement(final CobolDataItem dataItem, final XmlSchema xsd) {
        XmlSchemaElement element = new XmlSchemaElement();
        element.setName(formatName(dataItem.getCobolName(), false));
        element.setMinOccurs(dataItem.getMinOccurs());
        element.setMaxOccurs(dataItem.getMaxOccurs());
        element.setSchemaType(createXmlSchemaType(dataItem, xsd));
        if (addLegstarAnnotations) {
            element.setAnnotation(annotation);
        }
    }
    
    /**
     * Maps a COBOL data item to an XML schema type.
     * <ul>
     * <li>COBOL elementary data items are mapped to XML Schema simple types.</li>
     * <li>COBOL structures are mapped to XML schema complex Types.</li>
     * </ul>
     * @param dataItem COBOL data item
     * @param xsd the XML schema being built
     * @return a corresponding XML schema type
     */
    public XmlSchemaType createXmlSchemaType(final CobolDataItem dataItem, final XmlSchema xsd) {
        if (dataItem.isStructure()) {
            return createXmlSchemaComplexType(dataItem, xsd);
        } else {
            return createStringXmlSchemaSimpleType(5, xsd);
        }
    }
    
    /**
     * Create an XML schema complex type.
     * @param dataItem COBOL data item
     * @param xsd the XML schema being built
     * @return a new complex type
     */
    public XmlSchemaComplexType createXmlSchemaComplexType(final CobolDataItem dataItem, final XmlSchema xsd) {
        XmlSchemaSequence xmlSchemaSequence = new XmlSchemaSequence();
        for (CobolDataItem child : dataItem.getChildren()) {
            xmlSchemaSequence.getItems().add(createXmlSchemaElement(child, xsd));
        }

        XmlSchemaComplexType xmlSchemaComplexType  = new XmlSchemaComplexType(xsd);
        xmlSchemaComplexType.setParticle(xmlSchemaSequence);
        xmlSchemaComplexType.setName(formatName(dataItem.getCobolName(), true));
        
        return xmlSchemaComplexType;
    }
    
    /**
     * Create a simple type for an xsd:string type.
     * <p/>
     * COBOL alphanumeric fields are fixed length so we create a facet to enforce that constraint.
     * @param length the maximum length of the string
     * @param xsd the XML schema being built
     * @return an XML schema simple type
     */
    public XmlSchemaSimpleType createStringXmlSchemaSimpleType(final int length, final XmlSchema xsd) {
        /* Create length facet */
        XmlSchemaLengthFacet xmlSchemaLengthFacet = new XmlSchemaLengthFacet();
        xmlSchemaLengthFacet.setValue(length);

        /* Create restriction */
        XmlSchemaSimpleTypeRestriction restriction = new XmlSchemaSimpleTypeRestriction();
        restriction.setBaseTypeName(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "string"));
        restriction.getFacets().add(xmlSchemaLengthFacet);
        
        /* Create simpleType */
        XmlSchemaSimpleType xmlSchemaSimpleType = new XmlSchemaSimpleType(xsd);
        xmlSchemaSimpleType.setContent(restriction);
        return xmlSchemaSimpleType;
    }
    
    /**
     * Create a DOM document to hold annotation notes and create a markup with the COBOL data item
     * characteristics. 
     * @return a DOM node list holding the markup
     */
    public NodeList createCOBOLMarkup() {
        Document doc = _docBuilder.newDocument();
        Element el = doc.createElementNS(COBOL_NS, COBOL_PARENT_ELN);
        Element elc = doc.createElementNS(COBOL_NS, COBOL_ELN);

        elc.setAttribute(CobolMarkup.LEVEL_NUMBER, Integer.toString(5));
        elc.setAttribute(CobolMarkup.COBOL_NAME, "C-ARRAY");

        el.appendChild(elc);
        return el.getChildNodes();
    }

    /**
     * Turn a COBOL name to an XSD element name.
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
     * Complex type names customarily start with uppercase while element names start
     * with a lower case.
     * @param cobolName the original COBOL name
     * @param isComplexType true if we ware looking for a complex type name
     * @return a nice XML element name
     */
    public static String formatName(final String cobolName, final boolean isComplexType) {
        
        StringBuilder sb = new StringBuilder();
        boolean wordBreaker = (isComplexType) ? true : false;
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

}
