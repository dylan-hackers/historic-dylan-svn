package sigue.btrack;

import java.sql.*;
import java.util.Hashtable;
import javax.servlet.*;         // should figure out how to not need this.
import javax.servlet.http.*;    // ditto.

/**
 * Represents a user account.  This is a bean class.
 */
public class Account extends NamedRecord
    /* ---TODO: implements java.io.Serializable */
{

    // The following fields are stored in the database.
    // Their names correspond exactly to the database column names.
    private Integer bug_number;
    private String password;
    private String email_address;
    private Integer email_prefs;
    private Integer permissions;
    private Character role;     // e.g., developer or QA

    // Non-database fields...
    private static boolean all_loaded = false;
    private static char ROLE_QA = 'Q';
    private static char ROLE_DEV = 'D';


    //// Constructors...

    public Account () {
        super();
        initDefaults();
    }

    private Account (Integer id) {
        super(id);
        initDefaults();
    }

    private void initDefaults () {
        //---TODO: Need reasonable defaults for these.
        this.email_prefs = new Integer(0);
        this.permissions = new Integer(0);
        this.role = new Character(ROLE_QA);
    }

    /**
     * Note that this serves two different purposes.  It is used as the Account
     * prototype, for method dispatch purposes, and it is used as the default
     * value for various fields that take an Account.  Therefore it's important
     * for its ID to be DatabaseRecord.NO_RECORD, which indicates there's no value
     * in a column.
     */
    private static Account prototype = new Account(new Integer(DatabaseRecord.NO_RECORD));

    public static Account getPrototype () {
        return Account.prototype;
    }


    //// Getters...

    public String getPassword () { return password; }
    public String getEmailAddress () { return email_address; }
    public Integer getEmailPrefs () { return email_prefs; }
    public Integer getPermissions () { return permissions; }


    //// Setters...

    public void setPassword (String p) { password = p; }
    public void setEmailAddress (String e) { email_address = e; }
    public void setEmailPrefs (int p) { email_prefs = new Integer(p); }
    public void setPermissions (int p) { permissions = new Integer(p); }


    //// The rest...

    protected String loadRecordQuery () {
        return "select * from tbl_account where account_id = ?";
    }

    protected String tableName () {
        return "tbl_account";
    }

    protected String recordIDColumnName () {
        return "account_id";
    }

    protected void initializeFromRow (ResultSet rset)
        throws SQLException
    {
        super.initializeFromRow(rset);
        this.password = Util.getString(rset, "password");
        this.email_address = Util.getString(rset, "email_address");
        this.email_prefs = new Integer(rset.getInt("email_prefs"));
        this.permissions = new Integer(rset.getInt("permissions"));
        this.role = new Character(Util.getString(rset, "role").charAt(0));
    }

    public static Account loadAccountByName (String name) {
        return (Account) DatabaseRecord.loadRecord
            ("select * from tbl_account where name = '" + name + "'",
             Account.class);
    }

    public static Account loadAccountByName (String name, String password) {
        if (name == null || password == null) {
            return null;
        } else {
            return (Account) DatabaseRecord.loadRecord
                ("select * from tbl_account where name = '" + name
                 + "' and password = '" + password + "'",
                 Account.class);
        }
    }

    public static Account loadAccountByEmail (String email) {
        return (Account) DatabaseRecord.loadRecord
            ("select * from tbl_account where email_address = '" + email + "'",
             Account.class);
    }

    public static Account loadAccount (Integer account_id) {
        return (Account) DatabaseRecord.loadRecord(account_id, Account.class, true);
    }

    public static Account loadAccount (ResultSet rset)
    {
        return (Account) DatabaseRecord.loadRecord(rset, Account.class);
    }

    public void respondToEditForm (HttpServletRequest request, HttpSession session) {
        String name = Util.getRequiredField(request, session, "name");
        String password = Util.getRequiredField(request, session, "password");
        String email = Util.getRequiredField(request, session, "email_address");
        if (name != null && password != null && email != null) {
            boolean is_new = isNew();
            if (is_new) {
                NamedRecord old = loadByName(name);
                if (old == null) {
                    old = Account.loadAccountByEmail(email);
                }
                if (old != null && old != this) {
                    Util.noteError(session,
                                   "An account by that name already exists."
                                   + "  Please choose a different name.");
                    return;
                }
            }
            setName(name);
            setPassword(password);
            setEmailAddress(email);
            save();
            Util.noteMessage(session, "Account '" + getName()
                             + (is_new ? "' created." : "' updated."));
        }
    }

    /**
     * Load all accounts into the cache.
     */
    public void save (Connection conn) throws SQLException {
        String insert = "insert into tbl_account (account_id,mod_count,date_entered,date_modified,name,status,password,email_address,email_prefs,permissions,role) values (?,?,?,?,?,?,?,?,?,?,?)";
        String update = "update tbl_account set mod_count = ?, date_entered = ?, date_modified = ?, name = ?, status = ?, password = ?, email_address = ?, email_prefs = ?, permissions = ?, role = ? where account_id = ?";
        int idx = 1;
        boolean is_new = isNew();
        PreparedStatement ps = conn.prepareStatement(is_new ? insert : update);
        if (is_new) {
            ps.setInt(idx++, getId().intValue());
        }
        int new_mod_count = mod_count.intValue() + 1;
        ps.setInt(idx++, new_mod_count);
        ps.setTimestamp(idx++, date_entered);
        ps.setTimestamp(idx++, date_modified);
        ps.setString(idx++, getName());
        ps.setString(idx++, getStatus());
        ps.setString(idx++, password);
        ps.setString(idx++, email_address);
        ps.setInt(idx++, email_prefs.intValue());
        ps.setInt(idx++, permissions.intValue());
        ps.setString(idx++, role.toString());
        if (!is_new) {
            ps.setInt(idx++, getId().intValue());
        }
        ps.executeUpdate();
        ps.close();
        // Only increment the mod_count if there was no exception during the save.
        setModCount(new Integer(new_mod_count));
    }

}
