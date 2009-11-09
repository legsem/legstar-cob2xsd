<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:cb="http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd" xmlns:jaxb="http://java.sun.com/xml/ns/jaxb">
<!--
    This sample stylesheet shows how to customize the XML Schema produced
    by legstar-cob2xsd. Shows how to:
    - mark a data item as a customVariable
    - set a value for unmarshalChoiceStrategyClassName
 -->
<xsl:template match="@*|node()">
    <xsl:copy>
        <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
</xsl:template>
<xsl:template match="*/cb:cobolElement[@cobolName='LS-REQUEST-TYPE']">
    <xsl:copy>
        <xsl:copy-of select="@*"/>
        <xsl:attribute name="customVariable">true</xsl:attribute>
    </xsl:copy>
</xsl:template>
<xsl:template match="*/cb:cobolElement[@cobolName='LS-FILES-DATA']">
    <xsl:copy>
        <xsl:copy-of select="@*"/>
        <xsl:attribute name="unmarshalChoiceStrategyClassName">com.legstar.coxb.cust.dplarcht.ChoiceSelector</xsl:attribute>
    </xsl:copy>
</xsl:template>
</xsl:stylesheet>