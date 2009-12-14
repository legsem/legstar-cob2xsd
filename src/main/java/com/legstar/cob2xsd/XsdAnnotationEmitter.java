/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cob2xsd;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaAnnotation;
import org.apache.ws.commons.schema.XmlSchemaAppInfo;
import org.apache.ws.commons.schema.constants.Constants;
import org.apache.ws.commons.schema.utils.NamespaceMap;
import org.apache.ws.commons.schema.utils.NamespacePrefixList;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.legstar.cobol.utils.ValueUtil;
import com.legstar.coxb.CobolMarkup;
import com.legstar.coxb.CobolType;

/**
 * This class is used to generate LegStar annotations to be included
 * in an XML schema.
 * <p/>
 * There are 2 levels of annotations:
 * <ul>
 * <li>At the schema level. It is mainly the JAXB package name for generated classes</li>
 * <li>At the element level. These are the COBOL annotations per se.</li>
 * </ul>
 * There are 2 XML namespaces involved, JAXB and LegStar. LegStar being a JAXB plugin,
 * it is important that both be present.
 * <p/>
 * We expect elementFormDefault="qualified" and attributeFormDefault="unqualified"
 */
public class XsdAnnotationEmitter {

    /** This properties file holds the annotation names and namespaces. */
    private static final String ANNOTATIONS_FILE_NAME = "annotations.properties";

    /** The XML Schema being built. */
    private XmlSchema _xsd;

    /** Holds the annotations values.*/
    private Properties _annotations = new Properties();;

    /** This builder is used for annotation markup elements. */
    private DocumentBuilder _docBuilder;

    /** True if properly initialized.*/
    private boolean _initialized = false;

    /** The translator options in effect.*/
    private Cob2XsdContext _context;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * All annotations are externalized in a properties file loaded from the classpath.
     * We also create a DOM document builder that is needed to create the custom
     * annotations markup.
     * Errors, which are unlikely, are signaled by logging appropriate messages but
     * no exceptions are raised. Rather, the class disables itself and no annotations
     * are produced.
     * @param xsd the XML Schema to be populated.
     * @param context the translator options in effect
     */
    public XsdAnnotationEmitter(
            final XmlSchema xsd,
            final Cob2XsdContext context) {
        try {
            _xsd = xsd;
            _context = context;

            InputStream is =
                XsdAnnotationEmitter.class.getResourceAsStream(ANNOTATIONS_FILE_NAME);
            if (is == null) {
                _log.error("Was unable to locate file " + ANNOTATIONS_FILE_NAME + " from the classpath");
            } else {
                _annotations.load(is);
                DocumentBuilderFactory docFac = DocumentBuilderFactory.newInstance();
                docFac.setNamespaceAware(true);
                _docBuilder = docFac.newDocumentBuilder();
                addSchemaData();
                _initialized = true;
            }
        } catch (IOException e) {
            _log.error("Unable to load file " + ANNOTATIONS_FILE_NAME, e);
        } catch (ParserConfigurationException e) {
            _log.error("Unable to get DOM document builder ", e);
        }
    }

    /**
     * Adds schema level LegStar and JAXB markup.
     * <ul>
     * <li>Adds the JAXB and COXB namespaces to the target schema</li>
     * <li>Adds JAXB extension attributes to the target schema</li>
     * <li>Adds JAXB annotations to the target schema</li>
     * </ul>
     */
    protected void addSchemaData() {
        addNamespaceContext();
        addMetaInfo();
        addSchemaAnnotations();
    }

    /**
     * Create an XML Schema annotation with markup corresponding to the original COBOL
     * data item attributes. 
     * @param xsdDataItem COBOL data item decorated with XSD attributes
     * @return an XML schema annotation
     */
    public XmlSchemaAnnotation createLegStarAnnotation(final XsdDataItem xsdDataItem) {

        Document doc = _docBuilder.newDocument();
        Element el = doc.createElementNS(getCOXBNamespace(), getCOXBElements());
        Element elc = doc.createElementNS(getCOXBNamespace(), getCOXBElement());

        elc.setAttribute(CobolMarkup.LEVEL_NUMBER,
                Integer.toString(xsdDataItem.getLevelNumber()));
        elc.setAttribute(CobolMarkup.COBOL_NAME,
                xsdDataItem.getCobolName());
        elc.setAttribute(CobolMarkup.TYPE,
                xsdDataItem.getCobolType().toString());


        if (xsdDataItem.getCobolType() != CobolType.GROUP_ITEM) {
            if (xsdDataItem.getPicture() != null) {
                elc.setAttribute(CobolMarkup.PICTURE,
                        xsdDataItem.getPicture());
            }
            if (xsdDataItem.getUsage() != null) {
                elc.setAttribute(CobolMarkup.USAGE,
                        xsdDataItem.getUsageForCobol());
            }
            if (xsdDataItem.isJustifiedRight()) {
                elc.setAttribute(CobolMarkup.IS_JUSTIFIED_RIGHT, "true");
            }
            if (xsdDataItem.getTotalDigits() > 0) {
                elc.setAttribute(CobolMarkup.IS_SIGNED,
                        (xsdDataItem.isSigned()) ? "true" : "false");
                elc.setAttribute(CobolMarkup.TOTAL_DIGITS,
                        Integer.toString(xsdDataItem.getTotalDigits()));
                if (xsdDataItem.getFractionDigits() > 0) {
                    elc.setAttribute(CobolMarkup.FRACTION_DIGITS,
                            Integer.toString(xsdDataItem.getFractionDigits()));
                }
                if (xsdDataItem.isSignLeading()) {
                    elc.setAttribute(CobolMarkup.IS_SIGN_LEADING, "true");
                }
                if (xsdDataItem.isSignSeparate()) {
                    elc.setAttribute(CobolMarkup.IS_SIGN_SEPARATE, "true");
                }
            }
        }

        /* Annotations transfer the COBOL occurs semantic (as opposed to
         * the XSD semantic). No depending on => fixed size array*/
        if (xsdDataItem.getCobolMaxOccurs() > 0) {
            elc.setAttribute(CobolMarkup.MAX_OCCURS,
                    Integer.toString(xsdDataItem.getCobolMaxOccurs()));
            if (xsdDataItem.getDependingOn() == null) {
                elc.setAttribute(CobolMarkup.MIN_OCCURS,
                        Integer.toString(xsdDataItem.getCobolMaxOccurs()));
            } else {
                elc.setAttribute(CobolMarkup.DEPENDING_ON,
                        xsdDataItem.getDependingOn());
                elc.setAttribute(CobolMarkup.MIN_OCCURS,
                        Integer.toString(xsdDataItem.getCobolMinOccurs()));
            }
        }

        if (xsdDataItem.isODOObject()) {
            elc.setAttribute(CobolMarkup.IS_ODO_OBJECT, "true");
        }
        if (xsdDataItem.getRedefines() != null) {
            elc.setAttribute(CobolMarkup.REDEFINES, xsdDataItem.getRedefines());
        }
        if (xsdDataItem.isRedefined()) {
            elc.setAttribute(CobolMarkup.IS_REDEFINED, "true");
            elc.setAttribute(CobolMarkup.UNMARSHAL_CHOICE_STRATEGY, "");
        }

        if (xsdDataItem.getValue() != null && xsdDataItem.getValue().length() > 0) {
            elc.setAttribute(CobolMarkup.VALUE,
                    ValueUtil.resolveFigurative(
                            xsdDataItem.getValue(),
                            xsdDataItem.getMaxStorageLength(),
                            getContext().quoteIsQuote()));
        }

        if (xsdDataItem.getSrceLine() > 0) {
            elc.setAttribute(CobolMarkup.SRCE_LINE,
                    Integer.toString(xsdDataItem.getSrceLine()));
        }

        el.appendChild(elc);

        XmlSchemaAppInfo appInfo = new XmlSchemaAppInfo();
        appInfo.setMarkup(el.getChildNodes());

        /* Create annotation */
        XmlSchemaAnnotation annotation = new XmlSchemaAnnotation();
        annotation.getItems().add(appInfo);
        return annotation;
    }

    /**
     * Adds the JAXB and COXB namespaces and associated prefixes to the
     * XML schema.
     */
    protected void addNamespaceContext() {
        NamespaceMap prefixmap = new NamespaceMap();
        NamespacePrefixList npl = getXsd().getNamespaceContext();
        if (npl == null) {
            /* We get an NPE if we don't add this. */
            prefixmap.add("", XMLConstants.W3C_XML_SCHEMA_NS_URI);
        } else {
            for (int i = 0; i < npl.getDeclaredPrefixes().length; i++) {
                prefixmap.add(npl.getDeclaredPrefixes()[i], npl.getNamespaceURI(
                        npl.getDeclaredPrefixes()[i]));
            }
        }
        prefixmap.add(getCOXBNamespacePrefix(), getCOXBNamespace());
        prefixmap.add(getJAXBNamespacePrefix(), getJAXBNamespace());
        getXsd().setNamespaceContext(prefixmap);

    }

    /**
     * JAXB requires that certain attribute are added directly to the schema. The
     * Apache XmlSchema metaInfo allow to do so.
     */
    protected void addMetaInfo() {
        Document doc = _docBuilder.newDocument();
        Map < QName, Attr > extensionMap = new HashMap < QName, Attr >();
        Attr attrib = doc.createAttributeNS(getJAXBNamespace(), getJAXBVersionAttribute());
        attrib.setValue(getJAXBVersionValue());
        extensionMap.put(new QName(getJAXBNamespace(), getJAXBVersionAttribute()), attrib);

        attrib = doc.createAttributeNS(getJAXBNamespace(), getJAXBExtensionBindingPrefixesAttribute());
        attrib.setValue(getCOXBNamespacePrefix());
        extensionMap.put(
                new QName(getJAXBNamespace(), getJAXBExtensionBindingPrefixesAttribute()), attrib);

        getXsd().addMetaInfo(
                Constants.MetaDataConstants.EXTERNAL_ATTRIBUTES, extensionMap);
    }

    /**
     * The generated schema holds JAXB annotations needed when, later on, the
     * schema is used to generate JAXB classes. The markup looks like this:
     * <pre>
     * &lt;xsd:appinfo>
     *    &lt;jaxb:schemaBindings>
     *       &lt;jaxb:package name="com.legstar.test.coxb.schema"/>
     *       &lt;jaxb:nameXmlTransform>
     *          &lt;jaxb:typeName suffix="Type" />
     *        &lt;/jaxb:nameXmlTransform>
     *    &lt;/jaxb:schemaBindings>
     * &lt;/xsd:appinfo>
     * </pre>
     */
    protected void addSchemaAnnotations() {

        Document doc = _docBuilder.newDocument();
        Element el = doc.createElementNS(getJAXBNamespace(), getJAXBElementsElement());
        Element elsb = doc.createElementNS(getJAXBNamespace(), getJAXBSchemaBindingsElement());
        
        if (getContext().getJaxbPackageName() != null
                && getContext().getJaxbPackageName().length() > 0) {
            Element elpk = doc.createElementNS(getJAXBNamespace(), getJAXBPackageElement());
            elpk.setAttribute(getJAXBPackageNameAttribute(), getContext().getJaxbPackageName());
            elsb.appendChild(elpk);
        }

        if (getContext().getJaxbTypeClassesSuffix() != null
                && getContext().getJaxbTypeClassesSuffix().length() > 0) {
            Element eltr = doc.createElementNS(getJAXBNamespace(), getJAXBNameXmlTransformElement());
            Element eltn = doc.createElementNS(getJAXBNamespace(), getJAXBTypeNameElement());
            eltn.setAttribute(getJAXBTypeNameSuffixAttribute(), getContext().getJaxbTypeClassesSuffix());
            eltr.appendChild(eltn);
            elsb.appendChild(eltr);
        }

        el.appendChild(elsb);
        XmlSchemaAnnotation annotation = new XmlSchemaAnnotation();
        XmlSchemaAppInfo appInfo = new XmlSchemaAppInfo();
        NodeList markup = el.getChildNodes();
        appInfo.setMarkup(markup);
        annotation.getItems().add(appInfo);
        getXsd().setAnnotation(annotation);
    }

    /**
     * @return the JAXB namespace
     */
    public String getJAXBNamespace() {
        return _annotations.getProperty("jaxb-namespace");
    }

    /**
     * @return the JAXB namespace prefix
     */
    public String getJAXBNamespacePrefix() {
        return _annotations.getProperty("jaxb-ns-prefix");
    }

    /**
     * @return the JAXB version attribute
     */
    public String getJAXBVersionAttribute() {
        return _annotations.getProperty("jaxb-version-attr");
    }

    /**
     * @return the JAXB version value
     */
    public String getJAXBVersionValue() {
        return _annotations.getProperty("jaxb-version-value");
    }

    /**
     * @return the JAXB extension binding prefixes attribute
     */
    public String getJAXBExtensionBindingPrefixesAttribute() {
        return _annotations.getProperty("jaxb-extbpfx-attr");
    }

    /**
     * @return the qualified JAXB elements element
     */
    public String getJAXBElementsElement() {
        return getJAXBNamespacePrefix() + ':'
        + _annotations.getProperty("jaxb-elements");
    }
    /**
     * @return the qualified JAXB schema bindings element
     */
    public String getJAXBSchemaBindingsElement() {
        return getJAXBNamespacePrefix() + ':'
        + _annotations.getProperty("jaxb-schema-bindings");
    }
    /**
     * @return the qualified JAXB package element
     */
    public String getJAXBPackageElement() {
        return getJAXBNamespacePrefix() + ':'
        + _annotations.getProperty("jaxb-package");
    }
    /**
     * @return the JAXB package name attribute
     */
    public String getJAXBPackageNameAttribute() {
        return _annotations.getProperty("jaxb-package-name-attr");
    }
    /**
     * @return the qualified JAXB name XML transform element
     */
    public String getJAXBNameXmlTransformElement() {
        return getJAXBNamespacePrefix() + ':'
        + _annotations.getProperty("jaxb-name-xml-transform");
    }
    /**
     * @return the qualified JAXB type name element
     */
    public String getJAXBTypeNameElement() {
        return getJAXBNamespacePrefix() + ':'
        + _annotations.getProperty("jaxb-type-name");
    }
    /**
     * @return the JAXB type name suffix attribute
     */
    public String getJAXBTypeNameSuffixAttribute() {
        return _annotations.getProperty("jaxb-type-name-suffix-attr");
    }
    /**
     * @return the COXB namespace
     */
    public String getCOXBNamespace() {
        return _annotations.getProperty("coxb-namespace");
    }

    /**
     * @return the COXB namespace prefix
     */
    public String getCOXBNamespacePrefix() {
        return _annotations.getProperty("coxb-ns-prefix");
    }

    /**
     * @return the COXB qualified elements element
     */
    public String getCOXBElements() {
        return getCOXBNamespacePrefix() + ':'
        + _annotations.getProperty("coxb-elements");
    }

    /**
     * @return the COXB qualified element element
     */
    public String getCOXBElement() {
        return getCOXBNamespacePrefix() + ':'
        + _annotations.getProperty("coxb-element");
    }

    /**
     * @return true if properly initialized
     */
    public boolean initialized() {
        return _initialized;
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
