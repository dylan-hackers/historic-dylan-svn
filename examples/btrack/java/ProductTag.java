package sigue.btrack;

import javax.servlet.jsp.*;
import javax.servlet.jsp.tagext.*;
import javax.servlet.http.HttpSession;
import java.io.IOException;

public class ProductTag extends RecordTag
{
    protected void doRecordTag (DatabaseRecord rec,
                                JspWriter out,
                                HttpSession session)
        throws JspException, IOException
    {
        Product record = (Product) rec;
        if ("name".equalsIgnoreCase(key)) {
            out.print(record.getName());
        } else if ("owner".equalsIgnoreCase(key)) {
            out.print(record.getOwner().getName());
        }
    }
}
