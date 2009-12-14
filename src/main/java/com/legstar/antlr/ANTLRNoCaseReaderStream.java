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
import java.io.Reader;

import org.antlr.runtime.ANTLRReaderStream;
import org.antlr.runtime.CharStream;


/**
 * This is a case insensitive reader stream.
 * This reduces the grammar complexity by delivering uppercase keywords
 * for comparisons.
 * @author Jim Idle
 */
public class ANTLRNoCaseReaderStream extends ANTLRReaderStream {

    /**
     * Constructor.
     */
    public ANTLRNoCaseReaderStream() {
        super();
    }

    /**
     * Constructor from a reader.
     * @param r reader
     * @throws IOException if reader cannot be read
     */
    public ANTLRNoCaseReaderStream(final Reader r) throws IOException {
        super(r);
    }

    /**
     * Constructor from a reader and size.
     * @param r reader
     * @param size size
     * @throws IOException if reader cannot be read
     */
    public ANTLRNoCaseReaderStream(final Reader r, final int size) throws IOException {
        super(r, size);
    }

    /**
     * Constructor from a reader, size and chunk size.
     * @param r reader 
     * @param size size 
     * @param readChunkSize chunk size
     * @throws IOException if reader cannot be read
     */
    public ANTLRNoCaseReaderStream(final Reader r, final int size, final int readChunkSize) throws IOException {
        super(r, size, readChunkSize);
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
