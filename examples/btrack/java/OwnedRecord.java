package sigue.btrack;

import java.sql.*;
import java.util.Hashtable;
import javax.servlet.*;         // should figure out how to not need this.
import javax.servlet.http.*;    // ditto.

public abstract class OwnedRecord extends NamedRecord {

    private Account owner;

    protected OwnedRecord () {
        super();
    }

    protected OwnedRecord (Integer id) {
        super(id);
    }


    //// Getters...

    public Account getOwner () { return owner; }


    //// Setters...

    public void setOwner (Account a) { owner = a; }


    //// The rest...

    protected void initializeFromRow (ResultSet rset) throws SQLException {
        super.initializeFromRow(rset);
        int owner_id = rset.getInt("owner");
        this.setOwner(Account.loadAccount(new Integer(owner_id)));
    }

    public void respondToEditForm (HttpServletRequest request, HttpSession session) {
        String name = Util.getRequiredField(request, session, "name");
        String ownerName = Util.getRequiredField(request, session, "owner");
        if (name != null && ownerName != null) {
            boolean is_new = isNew();
            if (is_new) {
                NamedRecord old = loadByName(name); // loads record of same class
                if (old != null && old != this) {
                    Util.noteError(session,
                                   "A " + prettyName()
                                   + "by that name already exists.  Please choose a different name.");
                    return;
                }
            }
            Account owner = Account.loadAccountByName(ownerName);
            if (owner == null) {
                Util.noteError(session, "The account named '" + ownerName + "' was not found.");
                return;
            }
            setName(name);
            setOwner(owner);
            try {
                save();
                Util.noteMessage(session, "The " + prettyName() + " '" + getName()
                                 + (is_new ? "' was created." : "' updated."));
            } catch (SQLException se) {
                Util.noteError(session, se.toString());
                return;
            }
        }
    }

    //---TODO: remove duplication with NamedRecord.save
    public void save (Connection conn) throws BugTrackException, SQLException {
        boolean is_new = isNew();
        String query
            = (is_new
               ? ("insert into " + tableName() + " (" + recordIDColumnName()
                  + ",mod_count,date_entered,date_modified,name,status,owner) values (?,?,?,?,?,?,?)")
               : ("update " + tableName() + " set mod_count = ?, date_entered = ?, "
                  + "date_modified = ?, name = ?, status = ?, owner = ? where "
                  + recordIDColumnName() + " = ?"));
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
        ps.setInt(idx++, owner.getId().intValue());
        if (!is_new) {
            ps.setInt(idx++, getId().intValue());
        }
        ps.executeUpdate();
        ps.close();
        // Only increment the mod_count if there was no exception during the save.
        setModCount(new Integer(new_mod_count));
    }

}
