/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cobol;

import com.legstar.cob2xsd.Cob2XsdModel;

/**
 * Clean a fixed format COBOL source.
 * <p/>
 * Fixed format is the traditional IBM z/OS format where columns 1-6 contain
 * sequence numbers, column 7 is the indicator area, area A spans from column 8
 * to 11 and area B from 12 to 72.
 * 
 */
public class CobolFixedFormatSourceCleaner extends AbstractCobolSourceCleaner {

    /**
     * Column where code starts (inclusive, based 1). This is the indicator area
     * column.
     */
    private int _startColumn = Cob2XsdModel.DEFAULT_START_COLUMN;

    /** Column where code ends (inclusive, based 1). */
    private int _endColumn = Cob2XsdModel.DEFAULT_END_COLUMN;

    /**
     * Construct with a shared error handler.
     * 
     * @param errorHandler handles error messages
     */
    public CobolFixedFormatSourceCleaner(
            final RecognizerErrorHandler errorHandler) {
        super(errorHandler);

    }

    /**
     * Construct with a shared error handler.
     * 
     * @param errorHandler handles error messages
     * @param startColumn column where code starts (indicator area)
     * @param endColumn column where code ends (right margin)
     */
    public CobolFixedFormatSourceCleaner(
            final RecognizerErrorHandler errorHandler,
            final int startColumn,
            final int endColumn) {
        super(errorHandler);
        _startColumn = startColumn;
        _endColumn = endColumn;
    }

    /** {@inheritDoc} */
    @Override
    public String extendedCleanLine(final String line) {
        return cleanFixedLine(line);
    }

    /** {@inheritDoc} */
    @Override
    public int getIndicatorAreaPos() {
        return _startColumn - 1;
    }

    /**
     * Clear sequence numbers in column 1-6 and anything
     * beyond column 72.
     * 
     * @param line the line of code
     * @return a line of code without sequence numbers
     */
    public String cleanFixedLine(final String line) {

        StringBuilder cleanedLine = new StringBuilder();
        int length = line.length();

        /* Clear sequence numbering */
        for (int i = 0; i < _startColumn - 1; i++) {
            cleanedLine.append(" ");
        }

        /* Trim anything beyond end column */
        String areaA = line.substring(
                _startColumn - 1, (length > _endColumn) ? _endColumn : length);

        cleanedLine.append(areaA);
        return cleanedLine.toString();
    }

}
