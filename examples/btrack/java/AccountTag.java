package sigue.btrack;

import javax.servlet.jsp.*;
import javax.servlet.jsp.tagext.*;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.util.AbstractList;

public class AccountTag extends RecordTag
{
    public void doRecordTag (DatabaseRecord record, JspWriter out, HttpSession session)
        throws JspException, IOException
    {
        Account account = (Account) record;
        if ("name".equalsIgnoreCase(key)) {
            out.print(account.getName());
        } else if ("email".equalsIgnoreCase(key)) {
            out.print(account.getEmailAddress());
        }
    }
}
