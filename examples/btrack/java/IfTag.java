package sigue.btrack;

import javax.servlet.jsp.*;
import javax.servlet.jsp.tagext.*;
import javax.servlet.http.*;

/**
 * Example:
 * <bt:if test="foo">
 *   <bt:then test="foo">foo then</bt:then>
 *   <bt:else test="foo">foo else</bt:else>
 * </bt:if>
 *
 * To define a test for the IF tag, you must implement IfTag.Test and
 * register it under a specific name using the IfTag.registerTest method.
 */
public class IfTag extends TagSupport
{
    private static final java.util.Hashtable tests = new java.util.Hashtable();

    static {
        IfTag.registerTest("foo", new Test() {
                public boolean doTest (PageContext pc) {
                    return true;
                }
            });
        IfTag.registerTest("bar", new Test() {
                public boolean doTest (PageContext pc) {
                    return false;
                }
            });
    }

    /**
     * Users of IfTag should implement this and pass it to registerTest.
     */
    public interface Test {
        boolean doTest (PageContext pc);
    }

    /**
     * Register a test under the given name.
     */
    public static void registerTest (String name, Test t) {
        tests.put(name, t);
    }

    private String testName = null;
    private boolean testResult = false;

    public void setTest (String name) {
        testName = name;
    }

    public boolean getResult () {
        return this.testResult;
    }

    public int doStartTag () throws JspException {
        Test t = (Test) tests.get(this.testName);
        if (t == null) {
            Debug.println("IfTag.doStartTag: The test named " + this.testName + " was not found.");
        } else {
            this.testResult = t.doTest(pageContext);
        }
        // Always evaluate the body of the IF tag because it may have content outside
        // the THEN or ELSE tags.  The THEN and ELSE tags take care not to evaluate
        // their bodies if the test isn't found.
        return Tag.EVAL_BODY_INCLUDE;
    }

    public int doEndTag () {
        pageContext.removeAttribute(this.testName);
        return Tag.EVAL_PAGE;
    }
}
