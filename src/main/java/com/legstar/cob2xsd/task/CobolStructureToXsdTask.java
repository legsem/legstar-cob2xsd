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
package com.legstar.cob2xsd.task;

import java.io.File;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.types.FileSet;

import com.legstar.cob2xsd.Cob2XsdContext;
import com.legstar.cob2xsd.CobolStructureToXsd;
import com.legstar.cob2xsd.CobolStructureToXsdException;


/**
 * COBOL Structure to XSD ANT Task.
 * <code>
 * Usage:<br>
 *
 * &lt;project ...&gt;<br>
 *   &lt;taskdef name="cob2xsd" classname="com.legstar.cob2xsd.task.CobolStructureToXsdTask" /&gt;<br>
 *   &lt;property name="cobol.dir" value="../cobol"/&gt;<br>
 *   &lt;property name="xsd.dir" value="../xsd"/&gt;<br>
 *   &lt;target name="generate"&gt;<br>
 *     &lt;cob2xsd targetDir="${xsd.dir}"&gt;<br>
 *       &lt;fileset dir="${cobol.dir}"&gt;<br>
 *         &lt;include name="*.cob" /&gt;<br>
 *       &lt;/fileset&gt;<br>
 *     &lt;/cob2xsd&gt;<br>
 *    &lt;/target&gt;<br>
 * &lt;/project&gt;<br>
 * </code>
 *
 */
public class CobolStructureToXsdTask extends Task {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** The list of filesets that were setup.*/
    private List < FileSet > _fileSets = new LinkedList < FileSet >();

    /** The target directory where xsd files will be created. */
    private File _targetDir;

    /** Set of translation options to use.    */
    private Cob2XsdContext _context = new Cob2XsdContext();;

    /**
     *  The ant execution method.
     *  Check parameters and produce XSD files.
     */
    public final void execute() {
        _log.info("Translating COBOL files");

        checkParameters();

        try {
            CobolStructureToXsd cob2xsd = new CobolStructureToXsd(getContext());
            Iterator < FileSet > itor = _fileSets.iterator();
            while (itor.hasNext()) {
                FileSet fileset = itor.next();

                DirectoryScanner scanner = fileset.getDirectoryScanner(getProject());
                scanner.scan();
                String[] files = scanner.getIncludedFiles();
                for (int i = 0; i < files.length; i++) {
                    File cobolFile = new File(fileset.getDir(getProject()), files[i]);
                    cob2xsd.translate(cobolFile, getTargetDir());
                }
            }
        } catch (IllegalStateException e) {
            throw (new BuildException(e));
        } catch (CobolStructureToXsdException e) {
            throw (new BuildException(e));
        }
    }

    /**
     * Check that we have enough parameters to get started.
     */
    private void checkParameters() {
        if (_fileSets.isEmpty()) {
            throw new BuildException("No fileset specified");
        }

        /* Check that we have a valid target directory.  */
        if (getTargetDir() == null) {
            throw (new BuildException(
            "You must provide a target directory"));
        }
        if (!getTargetDir().exists()) {
            throw (new BuildException(
                    "Directory " + getTargetDir() + " does not exist"));
        }
        if (!getTargetDir().isDirectory() || !getTargetDir().canWrite()) {
            throw (new BuildException(
                    getTargetDir() + " is not a directory or is not writable"));
        }

        if (_log.isDebugEnabled()) {
            _log.debug("Target folder: " + getTargetDir());
        }
    }

    /**
     * Gather all parameters into a context object.
     * @return a parameter context to be used throughout all code
     */
    private Cob2XsdContext getContext() {
        return _context;
    }

    /**
     * @return the currency symbol used (CURRENCY SIGN clause in the SPECIAL-NAMES)
     */
    public char getCurrencySymbol() {
        return getContext().getCurrencySymbol();
    }

    /**
     * @param currencySymbol the currency symbol used (CURRENCY SIGN clause in the SPECIAL-NAMES)
     */
    public void setCurrencySymbol(final char currencySymbol) {
        getContext().setCurrencySymbol(currencySymbol);
    }

    /**
     * @return the NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if false
     */
    public boolean isNSymbolDbcs() {
        return getContext().isNSymbolDbcs();
    }

    /**
     * @param nSymbolDbcs the NSYMBOL(DBCS) compiler option. Assume NSYMBOL(NATIONAL) if false
     */
    public void setNSymbolDbcs(final boolean nSymbolDbcs) {
        getContext().setNSymbolDbcs(nSymbolDbcs);
    }

    /**
     * @return whether comma is the decimal point (DECIMAL-POINT IS COMMA clause in the SPECIAL-NAMES)
     */
    public boolean decimalPointIsComma() {
        return getContext().decimalPointIsComma();
    }

    /**
     * @param decimalPointIsComma whether comma is the decimal point
     *  (DECIMAL-POINT IS COMMA clause in the SPECIAL-NAMES)
     */
    public void setDecimalPointIsComma(final boolean decimalPointIsComma) {
        getContext().setDecimalPointIsComma(decimalPointIsComma);
    }

    /**
     * @return whether we should generate COBOL/JAXB annotations
     */
    public boolean addLegStarAnnotations() {
        return getContext().addLegStarAnnotations();
    }

    /**
     * @param addLegStarAnnotations whether we should generate COBOL/JAXB annotations
     */
    public void setAddLegStarAnnotations(final boolean addLegStarAnnotations) {
        getContext().setAddLegStarAnnotations(addLegStarAnnotations);
    }

    /**
     * @return the JAXB package name for generated Java classes
     */
    public String getJaxbPackageName() {
        return getContext().getJaxbPackageName();
    }

    /**
     * @return the JAXB type name prefix (generated JAXB class names will have this suffix)
     */
    public String getJaxbTypeClassesSuffix() {
        return getContext().getJaxbTypeClassesSuffix();
    }

    /**
     * @param jaxbPackageName the JAXB package name for generated Java classes
     */
    public void setJaxbPackageName(final String jaxbPackageName) {
        getContext().setJaxbPackageName(jaxbPackageName);
    }

    /**
     * @param jaxbTypeClassesSuffix the JAXB type name prefix (generated JAXB class names will have this suffix)
     */
    public void setJaxbTypeClassesSuffix(final String jaxbTypeClassesSuffix) {
        getContext().setJaxbTypeClassesSuffix(jaxbTypeClassesSuffix);
    }

    /**
     * @return the target namespace for generated XML schema
     */
    public String getTargetNamespace() {
        return getContext().getTargetNamespace();
    }

    /**
     * @param targetNamespace the target namespace for generated XML schema
     */
    public void setTargetNamespace(final String targetNamespace) {
        getContext().setTargetNamespace(targetNamespace);
    }
    /**
     * @return whether COBOL conditions (level 88) should be mapped to facets. Facets 
     * restrict the content which might not be desirable
     */
    public boolean mapConditionsToFacets() {
        return getContext().mapConditionsToFacets();
    }

    /**
     * @param mapConditionsToFacets Whether COBOL conditions (level 88) should be mapped to facets. Facets 
     * restrict the content which might not be desirable
     */
    public void setMapConditionsToFacets(final boolean mapConditionsToFacets) {
        getContext().setMapConditionsToFacets(mapConditionsToFacets);
    }

    /**
     * @return an optional XSLT transform for XML schema customization
     */
    public File getCustomXslt() {
        return getContext().getCustomXslt();
    }

    /**
     * @param xsltDir an optional XSLT transform for XML schema customization
     */
    public void setCustomXslt(final File xsltDir) {
        getContext().setCustomXslt(xsltDir);
    }

    /**
     * @return true if parent complex type name should be prepended in case of name conflict
     * (otherwise, the COBOL source line will be appended)
     */
    public boolean nameConflictPrependParentName() {
        return getContext().nameConflictPrependParentName();
    }

    /**
     * @param nameConflictPrependParentName true if parent complex type name should be prepended
     * in case of name conflict (otherwise, the COBOL source line will be appended)
     */
    public void setNameConflictPrependParentName(
            final boolean nameConflictPrependParentName) {
        getContext().setNameConflictPrependParentName(nameConflictPrependParentName);
    }

    /**
     * @return a new FileSet
     */
    public FileSet createFileset() {
        FileSet fileset = new FileSet();
        _fileSets.add(fileset);
        return fileset;
    }    

    /**
     * @return the current target directory
     */
    public File getTargetDir() {
        return _targetDir;
    }

    /**
     * @param targetDir the target directory to set
     */
    public void setTargetDir(final File targetDir) {
        _targetDir = targetDir;
    }

}
