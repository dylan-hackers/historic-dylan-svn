package sigue.btrack;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.BodyTagSupport;
import javax.servlet.jsp.JspWriter;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.util.Vector;

// <bt:errorsOrMessages/>
/**
 * Displays errors (if any) resulting from a form POST in a bulleted list.
 * Displays messages (if any) resulting from a form POST, one per paragraph.
 */
public class ErrorsTag extends BodyTagSupport
{
    public int doStartTag () throws JspException {
        try {
            JspWriter out = pageContext.getOut();
            HttpSession session = pageContext.getSession();
            Vector messages = Util.getMessages(session);
            if (messages != null) {
                out.println("<p><font color=\"green\">");
                for (int i = 0; i < messages.size(); i++) {
                    out.println("<p>");
                    out.println(messages.elementAt(i));
                }
                out.println("</font><p>");
                Util.clearMessages(session);
            }
            Vector errors = Util.getErrors(session);
            if (errors != null) {
                out.println("<p><font color=\"red\">");
                out.println("Please fix the following errors:<br>");
                for (int i = 0; i < errors.size(); i++) {
                    out.println("<li>");
                    out.println(errors.elementAt(i));
                    out.println("</li>");
                }
                out.println("</font><p>");
                Util.clearErrors(session);
            }
        }
	catch(IOException err) {
            Debug.backtrace(err);
        }
        return BodyTagSupport.SKIP_BODY;
    }
}
