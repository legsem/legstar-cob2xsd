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

import org.antlr.runtime.CharStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.RecognizerSharedState;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Overrides some of the ANTLR generated lexer methods so that the resulting
 * java class behaves like other LegStar classes, particularly for logging purposes.
 * <p/>
 * This code could be imbedded in the lexer grammar as well but its harder to
 * debug using ANTLRWorks because this code might have dependencies on jars which
 * are not naturally in ANTLRWorks classpath.
 *
 */
public class CobolStructureLexerImpl extends CobolStructureLexer {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Construct from a character stream.
     * @param input the character stream
     */
    public CobolStructureLexerImpl(final CharStream input) {
        this(input, new RecognizerSharedState());
    }
    
    /**
     * Construct from a character stream and a shared state.
     * @param input the character stream
     * @param state the shared state
     */
    public CobolStructureLexerImpl(final CharStream input, final RecognizerSharedState state) {
        super(input, state);

    }
    /** {@inheritDoc} */
    public String getErrorMessage(final RecognitionException e, final String[] tokenNames) {
        return ErrorHelper.getErrorMessage(_log, this, e, super.getErrorMessage(e, tokenNames));
    }
    

    /** {@inheritDoc} */
    public void emitErrorMessage(final String msg) {
        _log.warn(msg);
    }
}
