package com.legstar.cob2xsd.xsd;

import java.io.StringWriter;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaAnnotation;
import org.apache.ws.commons.schema.XmlSchemaAppInfo;
import org.apache.ws.commons.schema.XmlSchemaCollection;
import org.apache.ws.commons.schema.XmlSchemaComplexType;
import org.apache.ws.commons.schema.XmlSchemaElement;
import org.apache.ws.commons.schema.XmlSchemaLengthFacet;
import org.apache.ws.commons.schema.XmlSchemaSequence;
import org.apache.ws.commons.schema.XmlSchemaSimpleType;
import org.apache.ws.commons.schema.XmlSchemaSimpleTypeRestriction;
import org.custommonkey.xmlunit.XMLTestCase;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.legstar.coxb.CobolMarkup;


public class XsdWriteTest extends XMLTestCase {
    
    /** Cobol annotations namespace. */
    private static final String COBOL_NS = "http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd";

    /** Cobol annotation parent element name. */
    private static final String COBOL_PARENT_ELN = "cb:cobolElements";

    /** Cobol annotation element name. */
    private static final String COBOL_ELN = "cb:cobolElement";

    /** This builder is used for annotation markup elements. */
    private DocumentBuilder _docBuilder;
    
    public void setUp() throws Exception {
        DocumentBuilderFactory docFac =
            DocumentBuilderFactory.newInstance();
        docFac.setNamespaceAware(true);
        _docBuilder = docFac.newDocumentBuilder();
    }

    public void test1() throws Exception {
        XmlSchema xsd = new XmlSchema("http://legstar.com/test/coxb/fixarsim", new XmlSchemaCollection());
        
        /* Create length facet */
        XmlSchemaLengthFacet xmlSchemaLengthFacet = new XmlSchemaLengthFacet();
        xmlSchemaLengthFacet.setValue(5);

        /* Create restriction */
        XmlSchemaSimpleTypeRestriction restriction = new XmlSchemaSimpleTypeRestriction();
        restriction.setBaseTypeName(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "string"));
        restriction.getFacets().add(xmlSchemaLengthFacet);
        
        /* Create simpleType */
        XmlSchemaSimpleType xmlSchemaSimpleType = new XmlSchemaSimpleType(xsd);
        xmlSchemaSimpleType.setContent(restriction);
        
        /* Create appinfo */
        XmlSchemaAppInfo appInfo = new XmlSchemaAppInfo();
        appInfo.setMarkup(createMarkup());

        /* Create annotation */
        XmlSchemaAnnotation annotation = new XmlSchemaAnnotation();
        annotation.getItems().add(appInfo);
        
        XmlSchemaElement element = new XmlSchemaElement();
        element.setName("CArray");
        element.setMinOccurs(3);
        element.setMaxOccurs(3);
        element.setSchemaType(xmlSchemaSimpleType);
        element.setAnnotation(annotation);
        
        XmlSchemaSequence xmlSchemaSequence = new XmlSchemaSequence();
        xmlSchemaSequence.getItems().add(element);

        XmlSchemaComplexType xmlSchemaComplexType  = new XmlSchemaComplexType(xsd);
        xmlSchemaComplexType.setParticle(xmlSchemaSequence);
        xmlSchemaComplexType.setName("Dfhcommarea");
        // XmlSchema#items = new XmlSchemaObjectCollection();
        xsd.getItems().add(xmlSchemaComplexType);
        // XmlSchema#schemaTypes = new XmlSchemaObjectTable();
        xsd.addType(xmlSchemaComplexType);
        
        StringWriter writer = new StringWriter();
        xsd.write(writer);
        assertEquals(
                "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                + " xmlns:tns=\"http://legstar.com/test/coxb/fixarsim\""
                + " attributeFormDefault=\"unqualified\""
                + " elementFormDefault=\"unqualified\""
                + " targetNamespace=\"http://legstar.com/test/coxb/fixarsim\"/>"
                    ,
                    writer.toString());
    }
    
    private NodeList createMarkup() {
        /* Create a DOM document to hold annotation notes */
        Document doc = _docBuilder.newDocument();
        Element el = doc.createElementNS(COBOL_NS, COBOL_PARENT_ELN);
        Element elc = doc.createElementNS(COBOL_NS, COBOL_ELN);
        /* Add cobol attributes valid for all types */
        elc.setAttribute(CobolMarkup.LEVEL_NUMBER, Integer.toString(5));
        elc.setAttribute(CobolMarkup.COBOL_NAME, "C-ARRAY");
        el.appendChild(elc);
        return el.getChildNodes();
    }

}
