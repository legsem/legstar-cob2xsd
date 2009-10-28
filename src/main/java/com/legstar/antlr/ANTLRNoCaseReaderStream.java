package com.legstar.antlr;

import java.io.IOException;
import java.io.Reader;

import org.antlr.runtime.ANTLRReaderStream;
import org.antlr.runtime.CharStream;


public class ANTLRNoCaseReaderStream extends ANTLRReaderStream {

    public ANTLRNoCaseReaderStream() {
        super();
    }

    public ANTLRNoCaseReaderStream(Reader r) throws IOException {
        super(r);
    }

    public ANTLRNoCaseReaderStream(Reader r, int size) throws IOException {
        super(r, size);
    }

    public ANTLRNoCaseReaderStream(Reader r, int size, int readChunkSize) throws IOException {
        super(r, size, readChunkSize);
    }

    public int LA(int i) {
        if ( i==0 ) {
            return 0; // undefined
        }
        if ( i<0 ) {
            i++; // e.g., translate LA(-1) to use offset 0
        }

        if ( (p+i-1) >= n ) {

            return CharStream.EOF;
        }
        return Character.toUpperCase(data[p+i-1]);
    }
}
