package sigue.btrack;

import java.sql.*;
import javax.servlet.*;         // should figure out how to not need this.
import javax.servlet.http.*;    // ditto.

public abstract class NamedRecord extends DatedRecord
{
    private String name;
    private String status;  // "O" = Obsolete, "A" = Active

    protected NamedRecord () {
        super();
        this.status = "A";
    }

    protected NamedRecord (Integer id) {
        super(id);
        this.status = "A";
    }


    public String getName () { return name; }
    public String getStatus () { return status; }

    public void setName   (String n) { name = n; }
    public void setStatus (String s) { status = s; }

    protected void initializeFromRow (ResultSet rset) throws SQLException {
        super.initializeFromRow(rset);
        this.name = Util.getString(rset, "name");
        this.status = rset.getString("status");
    }

    /**
     * This makes <jsp:getProperty ... property="owner"/> work.
     */
    public String toString () {
        return name;
    }

    public NamedRecord loadByName (String name) {
        return (NamedRecord) DatabaseRecord.loadRecord
            ("select * from "
             + tableName()
             + " where name = '" + name + "'", getClass());
    }

    //---TODO: Add a way to get the status from the form.
    public void respondToEditForm (HttpServletRequest request,
                                   HttpSession session)
    {
        String name = Util.getRequiredField(request, session, "name");
        if (name != null) {
            boolean is_new = isNew();
            NamedRecord old = loadByName(name);
            if ((is_new && old != null)
                || (old != null && old != this)) {
                Util.noteError(session,
                               "A " + prettyName()
                               + " by that name already exists.  Please choose a different name.");
                return;
            }
            setName(name);
            save();
            Util.noteMessage(session,
                             "The " + prettyName() + " '" + getName()
                             + (is_new ? "' was created." : "' updated."));
        }
    }

    /**
     * This will be overridden by any subclasses that add extra database fields.
     */
    public void save (Connection conn) throws SQLException {
        boolean is_new = isNew();
        String query
            = (is_new
               ? ("insert into " + tableName() + " (" + recordIDColumnName()
                  + ",mod_count,date_entered,date_modified,name,status) values (?,?,?,?,?,?)")
               : ("update " + tableName() + " set mod_count = ?, date_entered = ?, "
                  + "date_modified = ?, name = ?, status = ? where " + recordIDColumnName() + " = ?"));
        int idx = 1;
        PreparedStatement ps = conn.prepareStatement(query);
        if (is_new) {
            ps.setInt(idx++, getId().intValue());
        }
        int new_mod_count = mod_count.intValue() + 1;
        ps.setInt(idx++, new_mod_count);
        ps.setTimestamp(idx++, date_entered);
        ps.setTimestamp(idx++, date_modified); // ---TODO: set to current time
        ps.setString(idx++, getName());
        ps.setString(idx++, getStatus());
        if (!is_new) {
            ps.setInt(idx++, getId().intValue());
        }
        ps.executeUpdate();
        ps.close();
        // Only increment the mod_count if there was no exception during the save.
        setModCount(new Integer(new_mod_count));
    }

}
