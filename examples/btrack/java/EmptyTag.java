package sigue.btrack;

import javax.servlet.jsp.*;
import javax.servlet.jsp.tagext.*;
import javax.servlet.http.HttpSession;
import java.io.IOException;

/**
 * All non-iteration tags are built on this.  It contains some simple
 * utilities.  The subclass need only define a doTag method.
 */
public abstract class EmptyTag extends TagSupport
{
    protected abstract void doTag (JspWriter out) throws IOException, JspException;

    public int doStartTag () throws JspException {
        try {
            doTag(pageContext.getOut());
        } catch (Exception e) {
            Debug.backtrace(e);
        }
        return Tag.SKIP_BODY;
    }

    protected IterateTag getIterateTag () {
        IterateTag itag = (IterateTag) TagSupport.findAncestorWithClass(this, IterateTag.class);
        if (itag == null) {
            throw new BugTrackException("Tag not inside an 'iterate' tag: " + this);
        }
        return itag;
    }

    public void showNumberedOptions (JspWriter out, Object[] options, int selected) {
        try {
            Debug.println("showNumberedOptions: options = " + options);
            for (int i = 0; i < options.length; i++) {
                Object opt = options[i];
                int number = (opt instanceof Option
                              ? ((Option) opt).getNumber()
                              : i);
                out.print("<option value='");
                out.print(number);
                out.print(number == selected
                          ? "' selected>"
                          : "'>");
                out.print(Util.escapeHTML(opt.toString()));
                out.print("</option>");
            }
        } catch (IOException e) {
            Debug.backtrace(e);
            throw new BugTrackException(e);
        }
    }

}
