package com.legstar.cobol;


public class CobolFreeFormatSourceCleanerTest extends AbstractCobolTester {

    /** A shared instance of a fixed format cleaner. */
    private CobolFreeFormatSourceCleaner _cleaner;

    /** {@inheritDoc} */
    @Override
    public void setUp() throws Exception {
        super.setUp();
        _cleaner = new CobolFreeFormatSourceCleaner(getErrorHandler());
    }

    /**
     * Comment in first position.
     */
    public void testShouldCleanCommentInFirstColumn() throws Exception {
        assertEquals("\n01  DATA.\n",
                _cleaner.clean("* This is a comment\n01  DATA."));
    }

    /**
     * Comment in some position.
     */
    public void testShouldCleanCommentInAnyColumn() throws Exception {
        assertEquals("\n01  DATA.\n",
                _cleaner.clean("     / This is a comment\n01  DATA."));
    }
}
