package sigue.btrack;

import javax.servlet.jsp.*;
import javax.servlet.jsp.tagext.*;
import javax.servlet.http.HttpSession;
import java.io.IOException;

public class BrowserTag extends RecordTag
{
    public void doRecordTag (DatabaseRecord record, JspWriter out, HttpSession session)
        throws JspException, IOException
    {
        Browser browser = (Browser) record;
        if ("name".equalsIgnoreCase(key)) {
            out.print(browser.getName());
        }
    }
}
