package sigue.btrack;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.BodyTagSupport;
import javax.servlet.jsp.JspWriter;
import java.io.IOException;

// <img src="<bt:image name="foo.jpg"/>" width="100" height="100">
public class ImageTag extends BodyTagSupport
{
    private static final String PREFIX = "images/";
    private String name;

    public void setName (String n) {
        this.name = n;
    }
    
    public int doStartTag () throws JspException {
        JspWriter out = pageContext.getOut();
        try {
            out.print(PREFIX);
            if (name != null) {
                out.print(name);
            }
        }
	catch(IOException err) {
            System.out.println(err);
        }
        return BodyTagSupport.SKIP_BODY;
    }
}
