package sigue.btrack;

import javax.servlet.jsp.*;
import javax.servlet.jsp.tagext.*;
import java.io.IOException;
import java.util.AbstractList;

/**
 * Example:
 * <bt:iterate name="all_records" type="sigue.btrack.Bug">
 *   <bt:noRowsMessage><tr><td>There are no rows to display.</td></tr></bt:noRowsMessage>
 *   <bt:row><tr><td>This is row number <bt:rowNumber/>.</bt:row>
 * </bt:iterate>
 */
public class IterateTag extends BodyTagSupport
{
    private Class type;
    private String name;
    private String beanId;

    private AbstractList list = null;
    private int current_index = 0;

    public void setName (String n) { name = n; }
    public void setBeanId (String id) { beanId = id; }

    // Setter called by JSP container to set tag attribute values.
    // Using the name "class" didn't work.  I assume there's some kind
    // of conflict with built-in Java or JSP setter/getter with that name.
    public void setType (String t) {
        try {
            type = Class.forName(t);
        } catch (ClassNotFoundException e) {
            Debug.backtrace(e);
        }
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

    /**
     * This is for the RowTag nested tag.  It stores the current row in
     * the PageContext under the beanId key so that jsp:useBean can find it.
     */
    public String getBeanId () { return beanId; }

    public int doStartTag () throws JspException {
        // Initialize the iteration context
        // ---TODO: The use of DatabaseRecord here should be removed.
        CollectionGenerator gen
            = (CollectionGenerator) DatabaseRecord.classPrototype(this.type);
        this.list = (AbstractList) gen.generateCollection(pageContext.getSession());
        current_index = 0;
        Debug.println("IterateTag.doStartTag: list = " + list);
        return BodyTag.EVAL_BODY_TAG;
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
