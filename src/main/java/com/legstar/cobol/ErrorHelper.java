package com.legstar.cobol;

import java.util.List;

import org.antlr.runtime.BaseRecognizer;
import org.antlr.runtime.EarlyExitException;
import org.antlr.runtime.NoViableAltException;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.Token;
import org.apache.commons.logging.Log;

/**
 * A utility class to help with error message formating and reporting.
 *
 */
public final class ErrorHelper {
    
    /**
     * Utility class.
     */
    private ErrorHelper() {
        
    }

    /**
     * Format an error message as expected by ANTLR. It is basically the
     * same error message that ANTL BaseRecognizer generates with some
     * additional data.
     * Also used to log debugging information.
     * @param log the logger to use at debug time
     * @param recognizer the lexer or parser who generated the error
     * @param e the exception that occured
     * @param superMessage the error message that the super class generated
     * @return a formatted error message
     */
    public static String getErrorMessage(
            final Log log,
            final BaseRecognizer recognizer,
            final RecognitionException e,
            final String superMessage) {
        List < ? > stack = BaseRecognizer.getRuleInvocationStack(e, recognizer.getClass().getName());
        if (log.isDebugEnabled()) {
            String debugMsg = recognizer.getErrorHeader(e) + " " + e.getClass().getSimpleName() + ":";
            if (e instanceof NoViableAltException) {
                NoViableAltException nvae = (NoViableAltException) e;
                debugMsg += " (decision=" + nvae.decisionNumber
                    + " state=" + nvae.stateNumber + ")"
                    + " decision=<<" + nvae.grammarDecisionDescription + ">>";
            } else if (e instanceof EarlyExitException) {
                EarlyExitException eea = (EarlyExitException) e;
                debugMsg += " (decision=" + eea.decisionNumber + ")";
               
            }
            debugMsg += " ruleStack=" + stack.toString();
            log.debug(debugMsg);
        }
        String rootRule = getRootRule(stack);
        return superMessage + ((rootRule == null) ? "" : "(looking for " + rootRule + ")");
    }

    /**
     * From the rule invocation stack, extracts the top level rule which triggered
     * the error. 
     * @param stack invocation rule stack
     * @return top level rule name stripped from starting m
     */
    private static String getRootRule(final List < ? > stack) {
        if (stack.size() > 1) {
            return ((String) stack.get(1)).substring(1);
        }
        return null;
    }
 
    /**
     * Token traces are slightly more readable if numeric type is translated to a readable string.
     * @param token a lexer token
     * @param tokenNames the list of token names
     * @return same as Token.toString but with token type label rather than int
     */
    public static String toString(final Token token, final String[] tokenNames) {
        return token.toString().replace("<" + token.getType() + ">",
                "<" + ((token.getType() > -1) ? tokenNames[token.getType()] : "no type") + ">");
    }

}
