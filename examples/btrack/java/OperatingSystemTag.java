package sigue.btrack;

import javax.servlet.jsp.*;
import javax.servlet.jsp.tagext.*;
import javax.servlet.http.HttpSession;
import java.io.IOException;

public class OperatingSystemTag extends RecordTag
{
    protected void doRecordTag (DatabaseRecord rec,
                                JspWriter out,
                                HttpSession session)
        throws JspException, IOException
    {
        OperatingSystem record = (OperatingSystem) rec;
        if ("name".equalsIgnoreCase(key)) {
            out.print(record.getName());
        }
    }
}
