package sigue.btrack;

import java.sql.*;
import java.util.Vector;
import java.util.ArrayList;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpServletRequest;


/**
 * The unavoidable Util class...
 */
public class Util
{
    private static final String MESSAGES_ATTR = "bt.messages";
    private static final String ERRORS_ATTR   = "bt.errors";
    private static final String ABORT_ATTR    = "bt.abort";
    private static final String LOGIN_ATTR    = "bt.logged-in";
    private static final String CURRENT_ACCOUNT_ATTR    = "bt.current-account";

    private static final String DB_DRIVER_NAME = "com.microsoft.jdbc.sqlserver.SQLServerDriver";
    private static       Class  DB_DRIVER      = null;
    private static final String DB_URL         = "jdbc:microsoft:sqlserver://localhost:1433;DatabaseName=btrack";
    private static final String DB_USERNAME    = "sa";
    private static final String DB_PASSWORD    = "ddtww4";


    //
    // Database utilities
    //

    public static Connection openDatabaseConnection ()
        throws SQLException, BugTrackException
    {
        if (DB_DRIVER == null) {
            try {
                DB_DRIVER = Class.forName(DB_DRIVER_NAME);
            } catch (ClassNotFoundException e) {
                Debug.println("Couldn't find database driver class: " + e);
            }
        }
        if (DB_DRIVER != null) {
            return DriverManager.getConnection(DB_URL, DB_USERNAME, DB_PASSWORD);
        }
        throw new BugTrackException("Unable to open a database connection.");
    }

    private static final Object UID_LOCK = new Object();
    private static final int UID_BATCH_SIZE = 100;
    private static int max_uid = 0;
    private static int next_uid = 0;

    public static int getUniqueID () throws BugTrackException {
        synchronized (UID_LOCK) {
            while (next_uid >= max_uid) {
                try {
                    loadUniqueID();
                } catch (SQLException e) {
                    Debug.backtrace(e);
                    throw new BugTrackException("Original error: " + e);
                }
            }
            int next = next_uid++;
            Debug.println("getUniqueID returning " + next);
            return next;
        }
    }

    /**
     * Load the next unique ID number from the database, update next_uid and
     * max_uid, and store max_uid back into the database for next time.
     */
    private static void loadUniqueID ()
        throws BugTrackException, SQLException 
    {
        Connection conn = Util.openDatabaseConnection();
        conn.setAutoCommit(false);                       // start transaction
        Statement stmt = conn.createStatement(ResultSet.TYPE_FORWARD_ONLY,
                                              ResultSet.CONCUR_UPDATABLE);
        ResultSet rs = stmt.executeQuery("select next_unique_id from tbl_config");
        String config_message = "Configuration error: tbl_config has now rows.";
        if (rs.next()) {
            next_uid = rs.getInt("next_unique_id");
            max_uid = next_uid + UID_BATCH_SIZE;
            int rows_updated = stmt.executeUpdate("update tbl_config set next_unique_id = "
                                                  + max_uid);
            if (rows_updated < 1) {
                throw new BugTrackException(config_message);
            } else if (rows_updated > 1) {
                Debug.println("Configuration error: tbl_config has more than one row.");
            }
        } else {
            throw new BugTrackException(config_message);
        }
        conn.commit();
        stmt.close();
        conn.setAutoCommit(true);
    }

    private static final Object bug_number_lock = new Object();

    public static Integer nextBugNumber () {
        try {
            String query = "select next_bug_number from tbl_config";
            String update = "update tbl_config set next_bug_number = next_bug_number + 1";
            Connection conn = Util.openDatabaseConnection();
            ResultSet rset = conn.createStatement().executeQuery(query);
            if (rset.next()) {
                int bugnum = rset.getInt("next_bug_number");
                conn.createStatement().executeUpdate(update);
                return new Integer(bugnum);
            } else {
                throw new BugTrackException("Couldn't get next bug number from database.  Configuration error?");
            }
        } catch (SQLException e) {
            throw new BugTrackException(e);
        }
    }

 
    //
    // Login
    //

    public static Account getCurrentAccount (HttpSession session) {
        return (Account) session.getAttribute(CURRENT_ACCOUNT_ATTR);
    }

    public static boolean isLoggedIn (HttpSession session) {
        return Util.getCurrentAccount(session) != null;
    }

    public static int countRows (String query) {
        try {
            Connection conn = Util.openDatabaseConnection();
            ResultSet rset = conn.createStatement().executeQuery(query);
            if (rset.next()) {
                return rset.getInt(1);
            }
        } catch (SQLException e) {
            Debug.backtrace(e);
        }
        return -1;
    }

    private static void maybeCreateAdminAccount () {
        if (Util.countRows("select count(account_id) from tbl_account") == 0) {
            Account a = new Account();
            a.setName("admin");
            a.setPassword("admin");
            a.setEmailAddress("admin");
            try {
                a.save();
            } catch (SQLException e) {
                Debug.backtrace(e);
            }
        }
    }

    public static boolean validateLogin (HttpSession session,
                                         String username,
                                         String password)
    {
        //---TODO: there is probably a better place to do this.
        maybeCreateAdminAccount();
        Account acct = Account.loadAccountByName(username, password);
        if (acct != null) {
            session.setAttribute(LOGIN_ATTR, "true");
            session.setAttribute(CURRENT_ACCOUNT_ATTR, acct);
            return true;
        }
        return false;
    }

    public static String getCurrentUsername (HttpSession session) {
        Account acct = (Account) session.getAttribute(CURRENT_ACCOUNT_ATTR);
        if (acct == null) {
            return null;
        } else {
            return acct.getName();
        }
    }


    //
    // Form errors
    //

    /**
     * Registers an error that will be displayed the next time the
     * <bt:errors-or-messages/> tag is used.
     * @param message  A string to be displayed.  May contain HTML tags.
     * @param abort    If true, do not commit any database records related to the current POST.
     * ---*** TODO: Define an ErrorReport class or something that will allow
     * ---*** more flexible error display than a plain String.
     */
    public static void noteError (HttpSession session, String message, boolean abort) {
        Vector v = (Vector) session.getAttribute(ERRORS_ATTR);
        if (v == null) {
            v = new Vector();
        }
        v.add(message);
        session.setAttribute(ERRORS_ATTR, v);
        if (abort) {
            session.setAttribute(ABORT_ATTR, "x"); // anything non-null will do
        }
    }

    public static void noteError (HttpSession session, String message) {
        noteError(session, message, true);
    }

    public static Vector getErrors (HttpSession session) {
        Vector v = (Vector) session.getAttribute(ERRORS_ATTR);
        return ((v != null && v.size() > 0)
                ? v
                : null);
    }

    public static void clearErrors (HttpSession session) {
        session.removeAttribute(ERRORS_ATTR);
    }

    public static boolean formAborted (HttpSession session) {
        return session.getAttribute(ABORT_ATTR) != null;
    }


    //
    // Form messages
    //

    /**
     * Registers a message that will be displayed the next time the
     * <bt:errors-or-messages/> tag is used.  The message usually indicates
     * success (e.g., "Bug #177 created").  Do not use this for reporting
     * errors.
     * @param message  A string to be displayed.  May contain HTML tags.
     * @see #noteError
     */
    public static void noteMessage (HttpSession session, String message) {
        Vector v = (Vector) session.getAttribute(MESSAGES_ATTR);
        if (v == null) {
            v = new Vector();
        }
        v.add(message);
        session.setAttribute(MESSAGES_ATTR, v);
    }

    public static Vector getMessages (HttpSession session) {
        Vector v = (Vector) session.getAttribute(MESSAGES_ATTR);
        return ((v != null && v.size() > 0)
                ? v
                : null);
    }

    public static void clearMessages (HttpSession session) {
        session.removeAttribute(MESSAGES_ATTR);
    }

    public static String getRequiredField (HttpServletRequest request,
                                           HttpSession session,
                                           String field_name) {
        String val = request.getParameter(field_name);
        if (val == null || "".equals(val.trim())) {
            noteError(session, "Please enter a value for the required field '" + field_name + "'.");
            return null;
        }
        return val;
    }

    
    public static boolean isAdmin (HttpSession session) {
        //Account current_account = (Account) session.getAttribute(CURRENT_ACCOUNT_ATTR);
        return true;
    }
        
    public static String getString (ResultSet rset, String columnName)
        throws SQLException
    {
        String padded = rset.getString(columnName);
        if (padded == null) {
            return null;
        } else {
            int len = padded.length();
            int epos = len - 1;
            while (epos > 0 && padded.charAt(epos) == ' ') {
                --epos;
            }
            return ((++epos == len) ? padded : padded.substring(0, epos));
        }
    }

    /**
     * This escapes charcters that are special in HTML.  e.g., '>' is
     * converted to &lt;.
     */
    public static String escapeHTML (String text) {
        //---TODO
        return text;
    }

    /**
     * Maps a key (e.g., Version.class) to a Option array.
     */
    private static java.util.Hashtable options_cache = new java.util.Hashtable();

    private static Option[] getOptions (Class key, String select_query, String count_query) {
        try {
            DatabaseRecord prototype = DatabaseRecord.classPrototype(key);
            Connection conn = Util.openDatabaseConnection();
            Statement stmt = conn.createStatement();
            Option[] options = (Option[]) options_cache.get(key);
            int count = 0;
            if (options != null) {
                ResultSet rset = stmt.executeQuery(count_query);
                rset.next();        // this had better return true or the tables haven't been setup.
                count = rset.getInt(1);
            }
            if (options == null || count > options.length) {
                ResultSet rset = stmt.executeQuery(select_query);
                ArrayList list = new ArrayList();
                while (rset.next()) {
                    list.add(new Option(rset.getInt(prototype.recordIDColumnName()),
                                        Util.getString(rset, "name")));
                }
                options = (Option[]) list.toArray(new Option[list.size()]);
                options_cache.put(key, options);
            }
            return options;
        } catch (SQLException e) {
            Debug.backtrace(e);
        }
        Option[] os = new Option[1];
        os[0] = new Option(-1, "--error--");
        return os;
    }

    //---TODO: All these options should be loaded from a config file.

    private static Option[] priority_options
        = new Option[] { new Option(Bug.PRIORITY_NONE,      "None"),
                         new Option(Bug.PRIORITY_IMMEDIATE, "Immediate"),
                         new Option(Bug.PRIORITY_HIGH,      "High"),
                         new Option(Bug.PRIORITY_MEDIUM,    "Medium"),
                         new Option(Bug.PRIORITY_LOW,       "Low") };

    public static Option[] getPriorityOptions () {
        return priority_options;
    }

    private static Option[] severity_options
        = new Option[] { new Option(Bug.SEVERITY_NONE,     "None"),
                         new Option(Bug.SEVERITY_BLOCKER,  "Blocker"),
                         new Option(Bug.SEVERITY_CRITICAL, "Critical"),
                         new Option(Bug.SEVERITY_MAJOR,    "Major"),
                         new Option(Bug.SEVERITY_NORMAL,   "Normal"),
                         new Option(Bug.SEVERITY_MINOR,    "Minor"),
                         new Option(Bug.SEVERITY_TRIVIAL,  "Trivial") };

    public static Option[] getSeverityOptions () {
        return severity_options;
    }

    private static Option[] status_options
        = new Option[] { new Option(Bug.STATUS_NONE, "None"),
                         new Option(Bug.STATUS_NEW, "New"),
                         new Option(Bug.STATUS_INVESTIGATING, "Investigating"),
                         new Option(Bug.STATUS_FIXED, "Fixed"),
                         new Option(Bug.STATUS_OPEN, "Open"),
                         new Option(Bug.STATUS_INVALID, "Invalid"),
                         new Option(Bug.STATUS_TESTING, "Testing"),
                         new Option(Bug.STATUS_REOPENED, "Reopened"),
                         new Option(Bug.STATUS_VERIFIED, "Verified"),
                         new Option(Bug.STATUS_CLOSED, "Closed") };
        
    //---TODO: This should only return the options that are appropriate for
    //         the current status, so as to implement the correct state machine.
    public static Option[] getStatusOptions () {
        return status_options;
    }

    public static Option[] getVersionOptions () {
        return Util.getOptions(Version.class,
                               "select version_id, name from tbl_version",
                               "select count(version_id) from tbl_version");
    }

    public static Option[] getOperatingSystemOptions () {
        return Util.getOptions(OperatingSystem.class,
                               "select opsys_id, name from tbl_operating_system",
                               "select count(opsys_id) from tbl_operating_system");
    }

    public static Option[] getBrowserOptions () {
        return Util.getOptions(Browser.class,
                               "select browser_id, name from tbl_browser",
                               "select count(browser_id) from tbl_browser");
    }

    public static Option[] getPlatformOptions () {
        return Util.getOptions(Platform.class,
                               "select platform_id, name from tbl_platform",
                               "select count(platform_id) from tbl_platform");
    }

    public static Option[] getModuleOptions () {
        return Util.getOptions(Module.class,
                               "select module_id, name from tbl_module",
                               "select count(module_id) from tbl_module");
    }

    public static Option[] getProductOptions () {
        return Util.getOptions(Product.class,
                               "select product_id, name from tbl_product",
                               "select count(product_id) from tbl_product");
    }

    public static Option[] getAccountOptions () {
        return Util.getOptions(Account.class,
                               "select account_id, name from tbl_account",
                               "select count(account_id) from tbl_account");
    }

    
    //// Random small utils

    public static int parseInt (String s, int dflt) {
        try {
            return Integer.parseInt(s);
        } catch (NumberFormatException e) {
            Debug.backtrace(e);
        }
        return dflt;
    }
                    
    /**
     * @Return the full name of the package containing the given class.
     */
    public static String packageName (Class k) {
        String name = k.getName();
        int dot = name.lastIndexOf('.');
        return (dot == -1
                ? name
                : name.substring(0, dot));
    }

}
