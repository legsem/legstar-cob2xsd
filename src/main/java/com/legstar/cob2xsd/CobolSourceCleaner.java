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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * In order to reduce the lexer/parser grammar complexity, this class will remove
 * all unnecessary characters from the original source.
 * This way, the ANTLR lexer will be presented with a purified source that only
 * contains data division entries.
 * <p/>
 *
 */
public class CobolSourceCleaner {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** Line separator (OS specific).*/
    public static final String LS = System.getProperty("line.separator");

    /** Special characters that might be left over by IND$FILE for instance. */
    public static final Pattern UNWANTED_CHARACTERS = Pattern.compile("[\\x1A]");

    /** Pattern that recognizes the start of a data description entry. */
    public static final Pattern DATA_DESCRIPTION_START = Pattern.compile("^(\\s)+(\\d)+");

    /** Pattern that recognizes the end of a data description entry. */
    public static final Pattern DATA_DESCRIPTION_END = Pattern.compile("\\.(\\s)*$");

    /** Pattern that recognizes the start of a procedure division. */
    public static final Pattern PROCEDURE_DIVISION =
        Pattern.compile("^(\\s)+PROCEDURE DIVISION", Pattern.CASE_INSENSITIVE);

    /**
     * Takes in a raw COBOL source, potentially containing sequence numbers or
     * non data description statements and produces a clean source code.
     * <p/>
     * Statements which are not data descriptions become empty lines in order
     * to preserve original line numbering.
     * @param cobolSource the raw COBOL source
     * @return the source cleaned up
     */
    public String execute(final String cobolSource) {
        if (cobolSource != null) {
            BufferedReader reader = new BufferedReader(
                    new StringReader(cobolSource));
            String line;
            StringBuilder cleanedSource =  new StringBuilder();
            CleaningContext context = new CleaningContext();
            try {
                while ((line = reader.readLine()) != null) {
                    String cleanedLine = cleanLine(line);
                    setCleaningContext(cleanedLine, context);
                    if (context.isDataDescriptionStarted()) {
                        cleanedSource.append(cleanedLine + LS);
                    } else {
                        cleanedSource.append(LS);
                    }
                    /* prepare for the next data description*/
                    if (context.isDataDescriptionEnded()) {
                        context.setDataDescriptionStarted(false);
                    }
                }
                return cleanedSource.toString();
            } catch (IOException e) {
                _log.warn("Unable to read COBOL source", e);
            }
        } else {
            _log.warn("COBOL source was null");
        }
        return null;
    }

    /**
     * Each line of code goes through the following cleaning steps.
     * <ul>
     * <li>Remove sequence numbers</li>
     * <li>Remove special characters left over by file transfer</li>
     * </ul>
     * @param line the original line of code
     * @return a cleaned line
     */
    public String cleanLine(final String line) {
        String cleanedLine = removeLineSequenceNumbering(line);
        cleanedLine = removeUnwantedCharacters(cleanedLine);
        return cleanedLine;
    }

    /**
     * Replaces any characters in column 1 to 6 by spaces and removes
     * any characters past column 72.
     * @param line with potential sequence number
     * @return a line without a sequence number
     */
    public static String removeLineSequenceNumbering(final String line) {
        StringBuilder cleanedLine = new StringBuilder();
        int length = line.length();
        if (length < 7) {
            return "";
        } else {
            cleanedLine.append("      ");
            String areaA = line.substring(6, (length > 72) ? 72 : length);
            /* Right trim, no need to over burden the lexer with spaces*/
            cleanedLine.append(("a" + areaA).trim().substring(1));
        }
        return cleanedLine.toString();
    }

    /**
     * Sometimes special meaningless characters get inserted in source files.
     * This will remove such characters.
     * @param line the original source line
     * @return the source without unwanted characters
     */
    public static String removeUnwantedCharacters(final String line) {
        Matcher matcher = UNWANTED_CHARACTERS.matcher(line);
        return matcher.replaceAll("");
    }

    /**
     * Detects start and end of a data description. Since a single data description can span
     * multiple lines, the status is kept in the context instance.
     * <p/>
     * Once a PROCEDURE DIVISION is detected, this will stop looking at data description entries.
     * <p/>
     * On return from this method:
     * <ul>
     * <li>if dataDescription is neither started nor ended, then the line is not a data description.</li>
     * <li>If dataDescription is started but not ended then this is a multiline data description.</li>
     * <li>If dataDescription is started and ended then this the final part of a data description 
     * (possibly multiline).</li>
     * </ul>
     * 
     * @param line the line to set data description status from
     * @param context the data description detection context
     */
    public static void setCleaningContext(final String line, final CleaningContext context) {
        Matcher matcher;
        if (context.isDataDivision()) {
            matcher = PROCEDURE_DIVISION.matcher(line);
            if (matcher.find()) {
                context.setDataDivision(false);
            } else {
                if (!context.isDataDescriptionStarted()) {
                    matcher = DATA_DESCRIPTION_START.matcher(line);
                    context.setDataDescriptionStarted(matcher.find());
                }
                if (context.isDataDescriptionStarted()) {
                    matcher = DATA_DESCRIPTION_END.matcher(line);
                    context.setDataDescriptionEnded(matcher.find());
                }
            }
        }
    }

    /**
     * Describes the cleaning context.
     * Because data description sentences can be multiline or because it
     * does not make sense to look for data description entries once we
     * past a PROCEDURE DIVISION section, we need to keep track of the context.
     *
     */
    public static class CleaningContext {

        /** True if a data description has started. */
        private boolean _dataDescriptionStarted = false;

        /** True if a data description has ended. */
        private boolean _dataDescriptionEnded = false;

        /** True if we are likely to be in a COBOL DATA DIVISION section. */
        private boolean _inDataDivision = true;

        /**
         * @return true if a data description has started
         */
        public boolean isDataDescriptionStarted() {
            return _dataDescriptionStarted;
        }

        /**
         * @param isStarted set to true if a data description has started
         */
        public void setDataDescriptionStarted(final boolean isStarted) {
            _dataDescriptionStarted = isStarted;
        }

        /**
         * @return true if a data description has ended
         */
        public boolean isDataDescriptionEnded() {
            return _dataDescriptionEnded;
        }

        /**
         * @param isEnded set to true if a data description has ended
         */
        public void setDataDescriptionEnded(final boolean isEnded) {
            _dataDescriptionEnded = isEnded;
        }

        /**
         * @return true if we are likely to be in a COBOL DATA DIVISION section
         */
        public boolean isDataDivision() {
            return _inDataDivision;
        }

        /**
         * @param dataDivision set to true if we are likely to be in a COBOL DATA DIVISION section
         */
        public void setDataDivision(final boolean dataDivision) {
            _inDataDivision = dataDivision;
        }


    }

}
