package sigue.btrack;

import java.io.IOException;

import javax.servlet.jsp.*;
import javax.servlet.jsp.tagext.*;

public class RowNumberTag extends TagSupport
{
    public int doStartTag () throws JspException {
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
    }
}
