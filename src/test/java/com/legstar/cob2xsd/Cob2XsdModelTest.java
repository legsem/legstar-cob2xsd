package com.legstar.cob2xsd;

import java.util.Properties;

import junit.framework.TestCase;

import com.legstar.cob2xsd.Cob2XsdModel.CodeFormat;

/**
 * Test the model serialization/deserialization.
 * 
 */
public class Cob2XsdModelTest extends TestCase {

    /**
     * Set all values to something different from the default,
     * the serialize/deserialize and make sure everything is there.
     */
    public void testSerialization() {
        Cob2XsdModel model = new Cob2XsdModel();
        model.setCodeFormat(CodeFormat.FREE_FORMAT);
        model.setStartColumn(9);
        model.setEndColumn(73);
        model.setXsdEncoding("ISO-8859-1");
        model.setTargetNamespace("urn:somespace");
        model.setMapConditionsToFacets(true);
        model.setNameConflictPrependParentName(true);
        model.setElementNamesStartWithUppercase(true);
        model.setCustomXsltFileName("xslt1.xsl");
        model.setAddLegStarAnnotations(true);
        model.setCurrencySign("£");
        model.setCurrencySymbol("£");
        model.setDecimalPointIsComma(true);
        model.setNSymbolDbcs(true);
        model.setQuoteIsQuote(false);

        Properties props = model.toProperties();

        Cob2XsdModel model2 = new Cob2XsdModel(props);

        assertEquals(CodeFormat.FREE_FORMAT.toString(), model2.getCodeFormat()
                .toString());
        assertEquals(9, model2.getStartColumn());
        assertEquals(73, model2.getEndColumn());
        assertEquals("ISO-8859-1", model2.getXsdEncoding());
        assertEquals("urn:somespace", model2.getTargetNamespace());
        assertEquals(true, model2.mapConditionsToFacets());
        assertEquals(true, model2.nameConflictPrependParentName());
        assertEquals(true, model2.elementNamesStartWithUppercase());
        assertEquals("xslt1.xsl", model2.getCustomXsltFileName());
        assertEquals(true, model2.addLegStarAnnotations());
        assertEquals("£", model2.getCurrencySign());
        assertEquals("£", model2.getCurrencySymbol());
        assertEquals(true, model2.decimalPointIsComma());
        assertEquals(true, model2.nSymbolDbcs());
        assertEquals(false, model2.quoteIsQuote());

    }
}
