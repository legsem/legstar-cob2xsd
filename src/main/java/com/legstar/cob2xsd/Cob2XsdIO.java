package com.legstar.cob2xsd;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.antlr.RecognizerException;

/**
 * Common code for file translation (as opposed to string translation).
 * 
 */
public class Cob2XsdIO extends Cob2Xsd {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    public Cob2XsdIO(final Cob2XsdModel model) {
        super(model);
    }

    /**
     * Translates a single COBOL source file.
     * <p/>
     * When requested the file base name is appended to the target namespace.
     * 
     * @param cobolFile COBOL source file
     * @param target target file or folder
     * @param appendBaseFileNameToNamespace true to append base file name to
     *            namespace
     * @return the XML Schema
     * @throws RecognizerException if parser fails
     * @throws XsdGenerationException if COBOL model interpretation fails
     */
    public File translate(final File cobolFile, final File target,
            boolean appendBaseFileNameToNamespace) throws RecognizerException,
            XsdGenerationException {

        try {
            if (_log.isDebugEnabled()) {
                _log.debug("Translating COBOL file: " + cobolFile);
            }
            checkCobolSourceFile(cobolFile);
            checkTarget(target);

            if (appendBaseFileNameToNamespace) {
                String baseName = FilenameUtils.getBaseName(
                        cobolFile.getAbsolutePath()).toLowerCase();
                if (getModel().getTargetNamespace() != null) {
                    getModel().setTargetNamespace(
                            getModel().getTargetNamespace() + "/" + baseName);
                }

            }

            String xsdString = translate(FileUtils.readFileToString(cobolFile,
                    getModel().getCobolSourceFileEncoding()));
            File xsdFile = null;
            if (target.isDirectory()) {
                String xsdFileName = cobolFile.getName() + ".xsd";
                xsdFile = new File(target, xsdFileName);
            } else {
                xsdFile = target;
            }
            FileUtils.writeStringToFile(xsdFile, xsdString, getModel()
                    .getXsdEncoding());
            if (_log.isDebugEnabled()) {
                _log.debug("Created XML schema file: " + xsdFile);
            }
            return xsdFile;
        } catch (IOException e) {
            throw (new XsdGenerationException(e));
        }
    }

}
