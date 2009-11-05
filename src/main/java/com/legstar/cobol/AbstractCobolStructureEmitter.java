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
package com.legstar.cobol;

import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.RecognizerSharedState;
import org.antlr.runtime.Token;
import org.antlr.runtime.tree.TreeNodeStream;
import org.antlr.runtime.tree.TreeParser;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * XSD emitting requires a lot of actions when walking the tree produced by the COBOL structure parser.
 * <p/>
 * Rather than embedding these actions into the tree grammar, we use this abstract class to
 * hold the methods referenced from the tree grammer.
 * <p/>
 * The consequence is that ANTLRWorks cannot debug the tree grammar without setting its
 * classpath appropriately.
 *
 */
public abstract class AbstractCobolStructureEmitter extends TreeParser {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Construct from a tree nodes stream.
     * @param input the tree nodes stream
     */
    public AbstractCobolStructureEmitter(final TreeNodeStream input) {
        this(input, new RecognizerSharedState());
    }

    /**
     * Construct from a tree nodes stream and a shared state.
     * @param input the tree nodes stream
     * @param state the shared state
     */
    public AbstractCobolStructureEmitter(final TreeNodeStream input, final RecognizerSharedState state) {
        super(input, state);
         
    }
    
    /**
     * Something issue with COBOL needs to be raised.  
     * @param message what is wrong
     */
    public void issueWarning(final String message) {
        _log.warn(message);
    }
    
    /** {@inheritDoc} */
    public String getErrorMessage(final RecognitionException e, final String[] tokenNames) {
        return ErrorHelper.getErrorMessage(_log, this, e, super.getErrorMessage(e, tokenNames));
    }

    /** {@inheritDoc} */
    public String getTokenErrorDisplay(final Token t) {
        if (_log.isDebugEnabled()) {
            return ErrorHelper.toString(t, getTokenNames());
        } else {
            return super.getTokenErrorDisplay(t);
        }
    }

    /** {@inheritDoc} */
    public void emitErrorMessage(final String msg) {
        _log.error(msg);
    }

}
