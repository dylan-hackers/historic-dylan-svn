package sigue.btrack;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.BodyTagSupport;
import javax.servlet.jsp.JspWriter;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.util.Vector;

// <bt:stylesheet/>
public class StylesheetTag extends BodyTagSupport
{
    public int doStartTag () throws JspException {
        try {
            JspWriter out = pageContext.getOut();
            HttpSession session = pageContext.getSession();
            //---*** TODO: use different stylesheets for different browsers
            out.print("<link rel=\"stylesheet\" href=\"win-ie5-styles.css\" type=\"text/css\">");
        }
	catch(IOException err) {
            System.out.println(err);
        }
        return BodyTagSupport.SKIP_BODY;
    }
}
