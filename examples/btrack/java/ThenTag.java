package sigue.btrack;

import javax.servlet.jsp.*;
import javax.servlet.jsp.tagext.*;

public class ThenTag extends TagSupport
{
    public int doStartTag () throws JspException {
        IfTag iftag = (IfTag) TagSupport.findAncestorWithClass(this, IfTag.class);
        if (iftag == null) {
            Debug.println("ThenTag.doStartTag: no enclosing 'if' found.");
        } else if (iftag.getResult()) {
            return Tag.EVAL_BODY_INCLUDE;
        }
        return Tag.SKIP_BODY;
    }
}
