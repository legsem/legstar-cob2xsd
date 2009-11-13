package com.legstar.cob2xsd;

import junit.framework.TestCase;

/**
 * Check the syntax of the documentation code snippet.
 *
 */
public class DocSampleTest extends TestCase {
    
    /**
     * Invoke COBOL structure to XML Schema snippet.
     */
    public void test() {
        try {
            Cob2XsdContext context = new Cob2XsdContext();
            context.setTargetNamespace("http://www.mycompany.com/test");
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(context);
            String xmlSchema = cob2xsd.translate("       01 A.\n           02 B PIC X.");
            System.out.println(xmlSchema);
        } catch (CobolStructureToXsdException e) {
            e.printStackTrace();
        }
    }

}
