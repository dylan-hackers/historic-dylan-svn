package sigue.btrack;

import javax.servlet.jsp.*;
import javax.servlet.jsp.tagext.*;

public class RowTag extends TagSupport
{
    private Object current_row; // the current iteration element

    public Object getRow () {
        return current_row;
    }

    public int doStartTag () throws JspException {
        IterateTag itag = (IterateTag) TagSupport.findAncestorWithClass(this, IterateTag.class);
        if (itag == null) {
            Debug.println("RowTag.doStartTag: no enclosing 'iterate' tag found.");
        } else if (itag.isEmpty()) {
            return Tag.SKIP_BODY;
        } else {
            current_row = itag.currentElement();
            if (current_row != null && itag.getBeanId() != null) {
                // This is so that if the current row is a bean, jsp:useBean will work
                // inside of iterations.
                pageContext.setAttribute(itag.getBeanId(), current_row);
                Debug.println("storing record in page scope: " + current_row);
            }
            return (current_row == null
                    ? Tag.SKIP_BODY            // no more elements. iteration complete.
                    : Tag.EVAL_BODY_INCLUDE);
        }
        return Tag.SKIP_BODY;
    }
}
