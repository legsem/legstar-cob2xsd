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

/**
 * The COBOL source contains statements that we are unable to parse.
 *
 */
public class CobolStructureParsingException extends Exception {

    /**
     * A serial ID.
     */
    private static final long serialVersionUID = -2725570452731517979L;

    /**
     * @param msg the exception message
     */
    public CobolStructureParsingException(final String msg) {
        super(msg);
    }

    /**
     * @param e the exception
     */
    public CobolStructureParsingException(final Throwable e) {
        super(e);
    }
}
