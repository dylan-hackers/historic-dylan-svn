package sigue.btrack;

import javax.servlet.jsp.*;
import javax.servlet.jsp.tagext.*;

public class NoRowsTag extends TagSupport
{
    public int doStartTag () throws JspException {
        IterateTag itag = (IterateTag) TagSupport.findAncestorWithClass(this, IterateTag.class);
        if (itag == null) {
            Debug.println("RowTag.doStartTag: no enclosing 'iterate' tag found.");
        } else if (itag.isEmpty()) {
            return Tag.EVAL_BODY_INCLUDE;
        }
        return Tag.SKIP_BODY;
    }
}
