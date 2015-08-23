# Overview #

legstar-cob2xsd is a standalone COBOL Structure to XML Schema translator written in Java.

It is open source and available for [download](http://code.google.com/p/legstar-cob2xsd/downloads/list) as a zip archive.

There are 3 ways you can use it:

  * [Running an executable jar](#Running_an_executable_jar.md)

  * [Running an Apache ANT Task](#Running_an_Apache_ANT_Task.md)

  * [Calling the API from your own Java code](#Calling_the_API_from_your_own_Java_code.md)

Here we describe these 3 implementations with an emphasis on the API.

# Running an executable jar #

This is the easiest way to invoke the utility.

The distribution comes with **run.sh** and **run.bat** command files.

By default, run picks up COBOL source code from a folder called "cobol" and places the result in a folder called "schema".

The -h option lists the parameters that you might want to set.

There are additional configuration properties that you can set in conf/cob2xsd.properties.

The executable jar is called legstar-cob2xsd-x.y.z-exe.jar where x.y.z is the version number. It depends on the lib and conf subfolders content.

You invoke such an executable the classical way (run.sh is an example of such a call):
```
java -jar legstar-cob2xsd-x.y.z-exe.jar -i cobol -o schema
```

The input can be a folder, in which case all files from that folder are processed, or a file name.

The output is usually a folder but can be a file if the input is a single file.

The XML Schema name is built by appending ".xsd" to the COBOL file name.

# Running an Apache ANT Task #

The distribution comes with a **build.xml** which shows how to invoke the ant task.

If you need to set additional parameters, all options from [Cob2XsdModel](http://www.legsem.com/legstar/legstar-cob2xsd/apidocs/com/legstar/cob2xsd/Cob2XsdModel.html) are available.

The rule is that the ant property name does not need the initial "set" prefix and should start
with a lowercase character. So for instance:
```
setTargetNamespace becomes targetNamespace
```

Here is an example where target namespace is set and LegStar annotations are requested:
```
<target name="cob2xsd" depends="init">
    <cob2xsd targetDir="${xsd.dir}"
         targetNamespace="http://www.mycompany.com/test"
         addLegStarAnnotations="true">
        <fileset dir="${cobol.dir}">
            <include name="*"/>
        </fileset>
    </cob2xsd>
</target>
```

# Calling the API from your own Java code #

The distribution lib folder contains the jars that you might need to add to your classpath.

legstar-cob2xsd-x.y.z.jar is the major one. It contains 2 main packages:

| **Package name** | **description** |
|:-----------------|:----------------|
| com.legstar.cobol | Contains the recognizer logic |
| com.legstar.cob2xsd | Contains the XML Schema generation |

This represents the separation of concern between recognizing and generation. We should be able to reuse the recognizing part to produce other formats (JSON Schema for instance).

There is more detail on the recognizer implementation in CobolStructureRecognizer and on the XML Schema generation in CobolStructureXsdGeneration.

There are only 2 classes you should care about as an API user and both are in the com.legstar.cob2xsd package:

| **Class name** | **description** |
|:---------------|:----------------|
| [Cob2XsdModel](http://www.legsem.com/legstar/legstar-cob2xsd/apidocs/com/legstar/cob2xsd/Cob2XsdModel.html) | Set of XML Schema generation options |
| [Cob2Xsd](http://www.legsem.com/legstar/legstar-cob2xsd/apidocs/com/legstar/cob2xsd/Cob2Xsd.html) | Implements the main **translate** methods |

This is a snippet of code that shows a typical use of these classes:
```
        try {
            Cob2XsdModel model = new Cob2XsdModel();
            model.setTargetNamespace("http://www.mycompany.com/test");
            Cob2Xsd cob2xsd = new Cob2Xsd(model);
            String xmlSchema = cob2xsd.translate("       01 A.\n           02 B PIC X.");
            System.out.println(xmlSchema);
        } catch (CobolStructureToXsdException e) {
            e.printStackTrace();
        }

```

# More info #

Apart from the [javadoc](http://www.legsem.com/legstar/legstar-cob2xsd/apidocs/index.html), you can find many use cases in the [JUnit tests](http://code.google.com/p/legstar-cob2xsd/source/browse/#svn/trunk/src/test/java/com/legstar/cob2xsd) or join us on the [LegStar discussion group](http://groups.google.com/group/legstar-user).