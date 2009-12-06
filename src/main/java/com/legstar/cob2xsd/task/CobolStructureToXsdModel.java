package com.legstar.cob2xsd.task;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;

import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;

import com.legstar.cob2xsd.Cob2XsdContext;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.codegen.models.SourceToXsdCobolModel;

/**
 * At development time, this class collects a set of parameter that
 * are used to generate an ANT script.
 * <p/>
 * The main use case is for Eclipse plugins for instance which visually 
 * collect parameters and then use this class the generate a customized
 * ANT script which, in turn, translates a COBOL structure to an XML schema.
 *
 */
public class CobolStructureToXsdModel extends SourceToXsdCobolModel {

    /** The full path to the cobol source file. */
    private String _cobolSourceFilePath;

    /** Set of translation options to use.    */
    private Cob2XsdContext _context = new Cob2XsdContext();

    /** This generator name. */
    public static final String C2S_GENERATOR_NAME =
        "LegStar COBOL to XML Schema generator";

    /** This velocity template. */
    public static final String C2S_VELOCITY_MACRO_NAME =
        "vlc/build-cob2xsd-xml.vm";
    
    /** Whether velocity is already initialized.*/
    private boolean _velocityInitialized;

    /**
     * Creates an ant build script file ready for XSD generation.
     * @param targetFile the script file that must be created
     * @throws CodeGenMakeException if generation fails
     */
    public final void generateBuild(
            final File targetFile) throws CodeGenMakeException {
        try {
            if (!_velocityInitialized) {
                CodeGenUtil.initVelocity();
                _velocityInitialized = true;
            }
            VelocityContext context = CodeGenUtil.getContext(C2S_GENERATOR_NAME);
            context.put("antModel", this);
            Writer w = new OutputStreamWriter(
                    new FileOutputStream(targetFile), "UTF-8");
            Velocity.mergeTemplate(C2S_VELOCITY_MACRO_NAME, "UTF-8", context, w);
            w.close();
        } catch (IOException e) {
            throw new CodeGenMakeException(e);
        } catch (Exception e) {
            throw new CodeGenMakeException(e);
        }
    }

    /**
     * @return the full path to the COBOL source file
     */
    public final String getCobolSourceFilePath() {
        return _cobolSourceFilePath;
    }

    /**
     * @param cobolSourceFilePath the full path to the COBOL source file to set
     */
    public final void setCobolSourceFilePath(final String cobolSourceFilePath) {
        _cobolSourceFilePath = cobolSourceFilePath;
    }

    /**
     * @return the target XML schema file
     */
    public final File getTargetXsdFile() {
        return new File(getTargetDir(), getTargetXsdFileName());
    }

    /**
     * Gather all parameters into a context object.
     * @return a parameter context to be used throughout all code
     */
    public Cob2XsdContext getContext() {
        return _context;
    }

}
