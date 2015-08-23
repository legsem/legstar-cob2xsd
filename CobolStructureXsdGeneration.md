# XML Schema generation implementation #

This is done in a single step:

![http://legstar-cob2xsd.googlecode.com/svn/wiki/images/xml-schema-generation.png](http://legstar-cob2xsd.googlecode.com/svn/wiki/images/xml-schema-generation.png).

There are 3 helper classes:

| **Class name** | **description** |
|:---------------|:----------------|
| [Cob2XsdModel](http://www.legsem.com/legstar/legstar-cob2xsd/apidocs/com/legstar/cob2xsd/Cob2XsdModel.html) | Set of XML Schema generation options |
| [XsdDataItem](http://www.legsem.com/legstar/legstar-cob2xsd/apidocs/com/legstar/cob2xsd/XsdDataItem.html) | Maps a COBOL data item from the model to the XML Schema counterpart |
| [XsdAnnotationEmitter](http://www.legsem.com/legstar/legstar-cob2xsd/apidocs/com/legstar/cob2xsd/XsdAnnotationEmitter.html) | Used when LegStar annotations are to be added to the XML Schema |