package sigue.btrack;

import javax.servlet.jsp.*;
import javax.servlet.jsp.tagext.*;
import java.io.IOException;
import java.util.AbstractList;

/**
 * Example:
 * <bt:iterate name="all_records" class="sigue.btrack.Bug">
 *   <bt:noRowsMessage><tr><td>There are no rows to display.</td></tr></bt:noRowsMessage>
 *   <bt:row><tr><td>This is row number <bt:iterate key="row-number"/>.</bt:row>
 * </bt:iterate>
 */
public class IterateTag extends BodyTagSupport
{
    private Class klass;
    private String name;

    private AbstractList list = null;
    private int current_index = 0;

    // setter called by JSP container to set tag attribute values
    public void setClass (String k) {
        try {
            klass = Class.forName(k);
        } catch (ClassNotFoundException e) {
            Debug.backtrace(e);
        }
    }

    public void setName (String n) {
        name = n;
    }

    public boolean isEmpty () {
        return list == null || list.isEmpty();
    }

    public boolean hasMoreElements () {
        return !isEmpty() && current_index < list.size();
    }

    public Object currentElement () {
        if (hasMoreElements()) {
            return list.get(current_index);
        }
        return null;
    }

    /**
     * Returns the current (zero-based) iteration (or row) number.
     */
    public int iterationNumber () {
        return current_index;
    }

    public int doStartTag () throws JspException {
        /*
        if ("row-number".equalsIgnoreCase(key)) {
            // Just display the current row/iteration number...
            // Note this is a nested iteration tag, so we have to find the parent.
            try {
                IterateTag itag = (IterateTag) TagSupport.findAncestorWithClass(this, IterateTag.class);
                if (itag == null) {
                    Debug.println("RowTag.doStartTag: no enclosing 'iterate' tag found.");
                } else {
                    pageContext.getOut().print(itag.iterationNumber() + 1);
                }
            } catch (IOException e) {
                Debug.backtrace(e);
            }
            return Tag.SKIP_BODY;
        } else {
        */
            // Initialize the iteration context
            // ---TODO: The use of DatabaseRecord here should be removed.
            CollectionGenerator gen
                = (CollectionGenerator) DatabaseRecord.classPrototype(this.klass);
            this.list = (AbstractList) gen.generateCollection();
            current_index = 0;
            Debug.println("IterateTag.doStartTag: list = " + list);
            return BodyTag.EVAL_BODY_TAG;
        //}
    }

    public int doAfterBody () throws JspException {
        if (hasMoreElements()) {
            ++current_index;
            return BodyTag.EVAL_BODY_TAG;
        }
        // Within doEndTag getBodyContent mysteriously returns null, so this
        // must be done here.  Doesn't seem right to me...
        try {
            BodyContent body = getBodyContent();
            body.writeOut(body.getEnclosingWriter());
        } catch (IOException ee) {
            Debug.backtrace(ee);
        }
        return Tag.SKIP_BODY;
    }

    public int doEndTag () {
        list = null;
        return Tag.EVAL_PAGE;
    }
}
