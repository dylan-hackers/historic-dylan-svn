package sigue.btrack;

import javax.servlet.jsp.*;
import javax.servlet.jsp.tagext.*;
import javax.servlet.http.HttpSession;
import java.io.IOException;

public abstract class RecordTag extends EmptyTag
{
    protected String key;
    protected String name;
    protected String recordClass;

    public void setName (String n) { name = n; }
    public void setKey (String k) { key = k; }
    public void setRecordClass (String k) { recordClass = k; }

    /**
     * Subclasses must implement this to do the dirty work for the tag.
     * @param record  The database record.  Guaranteed not to be null.
     */
    protected abstract void doRecordTag (DatabaseRecord record,
                                         JspWriter out,
                                         HttpSession session)
        throws JspException, IOException;

    public void doTag (JspWriter out) throws JspException, IOException {
        HttpSession session = pageContext.getSession();
        if (this.name == null) {
            setName("record");
        }
        // If we're editing a record then the RecordServlet has already stored the
        // record in the session under the "record" attribute.  Otherwise, try to
        // retrieve the record from the enclosing iteration tag, below.
        DatabaseRecord record = (DatabaseRecord) session.getAttribute(this.name);
        IterateTag tag = null;
        try { tag = getIterateTag(); }
        catch (BugTrackException e) {}
        if (tag != null) {
            record = (DatabaseRecord) tag.currentElement(); // listing records
        }
        if ("init_all".equalsIgnoreCase(key)) {
            try {
                Class klass = Class.forName(this.recordClass);
                pageContext.setAttribute("all_records", DatabaseRecord.listRecords(klass));
            } catch (ClassNotFoundException e) {
                Debug.backtrace(e);
            }
        } else {
            if (record == null) {
                Debug.println("RecordTag.doTag: no record was found!");
            } else {
                if ("id".equalsIgnoreCase(key)) {
                    out.print(record.getId());
                } else {
                    doRecordTag(record, out, session);
                }
            }
        }
    }
}
