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
package com.legstar.antlr;

import java.io.IOException;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.CharStream;


/**
 * This is a case insensitive file stream.
 * This reduces the grammar complexity by delivering uppercase keywords
 * for comparisons.
 * @author Jim Idle
 */
public class ANTLRNoCaseFileStream  extends ANTLRFileStream {
    
    /**
     * Constructor from a file name.
     * @param fileName file name
     * @throws IOException if file cannot be read
     */
    public ANTLRNoCaseFileStream(final String fileName) throws IOException {
        super(fileName, null);
    }

    /**
     * Constructor from file name and encoding.
     * @param fileName file name
     * @param encoding character encoding
     * @throws IOException if file cannot be read
     */
    public ANTLRNoCaseFileStream(final String fileName, final String encoding)
    throws IOException {
        super(fileName, encoding);
    }

    /** {@inheritDoc} */
    @Override
    public int LA(final int i) {
        int ii = i;
        if (ii == 0) {
            return 0; // undefined
        }
        if (ii < 0) {
            ii++; // e.g., translate LA(-1) to use offset 0
        }

        if ((p + ii - 1) >= n) {

            return CharStream.EOF;
        }
        return Character.toUpperCase(data[p + ii - 1]);
    }
}
