package sigue.btrack;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.BodyTagSupport;
import javax.servlet.jsp.JspWriter;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.util.Vector;

// <bt:currentLogin/>
/**
 * Displays the current account name, or "(not logged in)".
 */
public class CurrentLoginTag extends BodyTagSupport
{
    public int doStartTag () throws JspException {
        try {
            JspWriter out = pageContext.getOut();
            HttpSession session = pageContext.getSession();
            if (Util.isLoggedIn(session)) {
                out.print("Logged in as ");
                out.print(Util.getCurrentUsername(session));
            } else {
                out.print("(not logged in)");
            }
        }
	catch(IOException err) {
            System.out.println(err);
        }
        return BodyTagSupport.SKIP_BODY;
    }
}
