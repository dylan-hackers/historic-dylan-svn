package sigue.btrack;

import java.sql.*;
import javax.servlet.*;         // should figure out how to not need this.
import javax.servlet.http.*;    // ditto.

public class Bug extends DatedRecord
    implements Comparable
{

    private static boolean all_loaded = false;

    // Should these be user-configurable?  Maybe just the user-visible names.
    
    public static final int STATUS_NONE = 0;
    public static final int STATUS_NEW = 1;
    public static final int STATUS_INVESTIGATING = 2;
    public static final int STATUS_FIXED = 3;
    public static final int STATUS_OPEN = 4;
    public static final int STATUS_INVALID = 5;
    public static final int STATUS_TESTING = 6;
    public static final int STATUS_REOPENED = 7;
    public static final int STATUS_VERIFIED = 8;
    public static final int STATUS_CLOSED = 9;

    public static final int PRIORITY_NONE = 0;
    public static final int PRIORITY_IMMEDIATE = 1;
    public static final int PRIORITY_HIGH = 2;
    public static final int PRIORITY_MEDIUM = 3;
    public static final int PRIORITY_LOW = 4;

    public static final int SEVERITY_NONE = 0;
    public static final int SEVERITY_BLOCKER = 1;
    public static final int SEVERITY_CRITICAL = 2;
    public static final int SEVERITY_MAJOR = 3;
    public static final int SEVERITY_NORMAL = 4;
    public static final int SEVERITY_MINOR = 5;
    public static final int SEVERITY_TRIVIAL = 6;



    //// Constructors
   
    public Bug () {
        super();
        initDefaults();
    }

    public Bug (Integer id) {
        super(id);
        initDefaults();
    }

    private void initDefaults () {
        setStatus(new Integer(STATUS_NEW));
        setProduct(Product.getPrototype());
        setModule(Module.getPrototype());
        setVersion(Version.getPrototype());
        setReportedBy(Account.getPrototype());
        setFixedBy(Account.getPrototype());
        setFixedInVersion(Version.getPrototype());
        setTargetVersion(Version.getPrototype());
        setOperatingSystem(OperatingSystem.getPrototype());
        setPlatform(Platform.getPrototype());
        setBrowser(Browser.getPrototype());
        // location here
        setPriority(new Integer(PRIORITY_NONE));
        setSeverity(new Integer(SEVERITY_NONE));
        setDevAssigned(Account.getPrototype());
        setQaAssigned(Account.getPrototype());
        setDuplicateOf(Bug.getPrototype());
    }

    private static Bug prototype = new Bug(new Integer(DatabaseRecord.NO_RECORD));

    public static Bug getPrototype () {
        return Bug.prototype;
    }


    //// Database fields...

    private Integer bug_number;
    private Integer status;
    private String synopsis;
    private String description;
    private Product product;
    private Module module;
    private Version version;
    private Account reported_by;
    private Account fixed_by;
    private Version fixed_in;
    private Version target_version;
    private OperatingSystem operating_system;
    private Platform platform;
    private Browser browser;
    private String location;
    private Integer priority;
    private Integer severity;
    private Account dev_assigned;
    private Account qa_assigned;
    private Bug duplicate_of;

    //// Non-database fields...


    //// Getters...

    public Integer getBugNumber () { return bug_number; }
    public Integer getStatus    () { return status; }
    public String getSynopsis   () { return synopsis; }
    public String getDescription () { return description; }
    public Product getProduct   () { return product; }
    public Module getModule     () { return module; }
    public Version getVersion   () { return version; }
    public Account getReportedBy () { return reported_by; }
    public Account getFixedBy   () { return fixed_by; }
    public Version getFixedInVersion () { return fixed_in; }
    public Version getTargetVersion () { return target_version; }
    public OperatingSystem getOperatingSystem () { return operating_system; }
    public Platform getPlatform () { return platform; }
    public Browser getBrowser   () { return browser; }
    public String getLocation   () { return location; }
    public Integer getPriority  () { return priority; }
    public Integer getSeverity  () { return severity; }
    public Account getDevAssigned () { return dev_assigned; }
    public Account getQaAssigned () { return qa_assigned; }
    public Bug getDuplicateOf () { return duplicate_of; }


    //// Setters...

    public void setBugNumber (Integer n) { this.bug_number = n; }
    public void setStatus (Integer s) { this.status = s; }
    public void setSynopsis (String s) { this.synopsis = s; }
    public void setDescription (String d) { this.description = d; }
    public void setProduct (Product p) { this.product = p; }
    public void setModule (Module m) { this.module = m; }
    public void setVersion (Version r) { this.version = r; }
    public void setReportedBy (Account a) { this.reported_by = a; }
    public void setFixedBy (Account a) { this.fixed_by = a; }
    public void setFixedInVersion (Version v) { this.fixed_in = v; }
    public void setTargetVersion (Version v) { this.target_version = v; }
    public void setOperatingSystem (OperatingSystem os) { this.operating_system = os; }
    public void setPlatform (Platform p) { this.platform = p; }
    public void setBrowser (Browser b) { this.browser = b; }
    public void setLocation (String loc) { this.location = loc; }
    public void setPriority (Integer p) { this.priority = p; }
    public void setSeverity (Integer s) { this.severity = s; }
    public void setDevAssigned (Account a) { this.dev_assigned = a; }
    public void setQaAssigned (Account a) { this.qa_assigned = a; }
    public void setDuplicateOf (Bug b) { this.duplicate_of = b; }
    

    //// The rest...

    public boolean editRequiresAdminPrivs () {
        return false;
    }

    protected String tableName () {
        return "tbl_bug_report";
    }

    protected String recordIDColumnName () {
        return "bug_report_id";
    }

    protected void initializeFromRow (ResultSet rset) throws SQLException {
        super.initializeFromRow(rset);  
        this.bug_number = new Integer(rset.getInt("bug_number"));
        this.status = new Integer(rset.getInt("status"));
        this.synopsis = Util.getString(rset, "synopsis");
        this.description = Util.getString(rset, "description");
        this.module = Module.loadModule(new Integer(rset.getInt("module")));
        this.version = Version.loadVersion(new Integer(rset.getInt("version")));
        this.reported_by = Account.loadAccount(new Integer(rset.getInt("reported_by")));
        this.fixed_by = Account.loadAccount(new Integer(rset.getInt("fixed_by")));
        this.fixed_in = Version.loadVersion(new Integer(rset.getInt("fixed_in")));
        this.target_version = Version.loadVersion(new Integer(rset.getInt("target_version")));
        this.operating_system = OperatingSystem.loadOperatingSystem(new Integer(rset.getInt("operating_system")));
        this.platform = Platform.loadPlatform(new Integer(rset.getInt("platform")));
        this.browser = Browser.loadBrowser(new Integer(rset.getInt("browser")));
        this.location = Util.getString(rset, "location");
        this.priority = new Integer(rset.getInt("priority"));
        this.severity = new Integer(rset.getInt("severity"));
        this.dev_assigned = Account.loadAccount(new Integer(rset.getInt("dev_assigned")));
        this.qa_assigned  = Account.loadAccount(new Integer(rset.getInt("qa_assigned")));
        this.duplicate_of = Bug.loadBug(new Integer(rset.getInt("duplicate_of")));
    }
            
    public static Bug loadBug (Integer id) {
        return (Bug) DatabaseRecord.loadRecord(id, Bug.class);
    }

    public static Bug loadBug (ResultSet rset, boolean must_be_unique)
        throws BugTrackException
    {
        return (Bug) DatabaseRecord.loadRecord(rset, Bug.class, must_be_unique);
    }

    //---TODO: this is truly disgusting code.  fix it to be more data driven.
    public void respondToEditForm (HttpServletRequest request, HttpSession session) {
        String synopsis    = Util.getRequiredField(request, session, "synopsis");
        String description = Util.getRequiredField(request, session, "description");
        int product_id  = Util.parseInt(Util.getRequiredField(request, session, "product"), 0);
        int module_id   = Util.parseInt(Util.getRequiredField(request, session, "module"), 0);
        int version_id  = Util.parseInt(Util.getRequiredField(request, session, "version"), 0);
        // reported_by automatically determined
        int fixed_by_id = Util.parseInt(Util.getRequiredField(request, session, "fixed_by"), 0);
        int fixed_in_id = Util.parseInt(Util.getRequiredField(request, session, "fixed_in"), 0);
        int target_version_id = Util.parseInt(Util.getRequiredField(request, session, "target_version"), 0);
        int opsys_id    = Util.parseInt(Util.getRequiredField(request, session, "operating_system"), 0);
        int platform_id = Util.parseInt(Util.getRequiredField(request, session, "platform"), 0);
        int browser_id  = Util.parseInt(Util.getRequiredField(request, session, "browser"), 0);
        String location = request.getParameter("location");
        int priority    = Util.parseInt(Util.getRequiredField(request, session, "priority"), 0);
        int severity    = Util.parseInt(Util.getRequiredField(request, session, "severity"), 0);
        int dev_id      = Util.parseInt(Util.getRequiredField(request, session, "dev_assigned"), 0);
        int qa_id       = Util.parseInt(Util.getRequiredField(request, session, "qa_assigned"), 0);
        int duplicate_of_id = Util.parseInt(Util.getRequiredField(request, session, "duplicate_of"), 0);
        Product product = Product.loadProduct(new Integer(product_id));
        Module module = Module.loadModule(new Integer(module_id));
        Version version = Version.loadVersion(new Integer(version_id));
        Account fixed_by = Account.loadAccount(new Integer(fixed_by_id));
        Version fixed_in = Version.loadVersion(new Integer(fixed_in_id));
        Version target_version = Version.loadVersion(new Integer(target_version_id));
        OperatingSystem opsys = OperatingSystem.loadOperatingSystem(new Integer(opsys_id));
        Platform platform = Platform.loadPlatform(new Integer(platform_id));
        Browser browser = Browser.loadBrowser(new Integer(browser_id));
        Account dev_acct = Account.loadAccount(new Integer(dev_id));
        Account qa_acct = Account.loadAccount(new Integer(qa_id));
        Bug duplicate_of = Bug.loadBug(new Integer(duplicate_of_id));

        boolean is_new = isNew();
        setSynopsis(synopsis);
        setDescription(description);
        setLocation(location);
        setProduct(product);
        setModule(module);
        setVersion(version);
        setFixedBy(fixed_by);
        setFixedInVersion(fixed_in);
        setTargetVersion(target_version);
        setOperatingSystem(opsys);
        setPriority(new Integer(priority));
        setSeverity(new Integer(severity));
        setDevAssigned(dev_acct);
        setQaAssigned(qa_acct);
        setDuplicateOf(duplicate_of);
        setBugNumber(is_new ? Util.nextBugNumber() : getBugNumber());
        setReportedBy(Util.getCurrentAccount(session));
        try {
            save();
            Util.noteMessage(session, "Bug #" + getBugNumber()
                             + (is_new ? " created." : " updated."));
        } catch (SQLException se) {
            Util.noteError(session, se.toString());
        }
    }

    /**
     * This will be overridden by any subclasses that add extra database fields.
     */
    public void save (Connection conn) throws BugTrackException, SQLException {
        boolean is_new = isNew();
        String query
            = (is_new
               ? ("insert into tbl_bug_report (bug_report_id,mod_count,date_entered,date_modified,"
                  + "bug_number,status,synopsis,description,module,version,reported_by,"
                  + "fixed_by,fixed_in,target_version,"
                  + "operating_system,platform,browser,location,priority,severity,"
                  + "dev_assigned,qa_assigned) values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)")
               : ("update tbl_bug_report set mod_count=?,date_entered=?,date_modified=?,"
                  + "bug_number=?,status=?,synopsis=?,description=?,module=?,version=?,"
                  + "reported_by=?,fixed_by=?,fixed_in=?,target_version=?,"
                  + "operating_system=?,platform=?,browser=?,location=?,"
                  + "priority=?,severity=?,dev_assigned=?,qa_assigned=? where bug_report_id=?"));
        int idx = 1;
        PreparedStatement ps = conn.prepareStatement(query);
        if (is_new) {
            ps.setInt(idx++, getId().intValue());
        }
        int new_mod_count = mod_count.intValue() + 1;
        ps.setInt(idx++, new_mod_count);
        ps.setTimestamp(idx++, date_entered);
        ps.setTimestamp(idx++, date_modified); // ---TODO: set to current time
        ps.setInt(idx++, bug_number.intValue());
        ps.setInt(idx++, status.intValue());
        ps.setString(idx++, synopsis);
        ps.setString(idx++, description);
        ps.setInt(idx++, module.getId().intValue());
        ps.setInt(idx++, version.getId().intValue());
        ps.setInt(idx++, reported_by.getId().intValue());
        ps.setInt(idx++, fixed_by.getId().intValue());
        ps.setInt(idx++, fixed_in.getId().intValue());
        ps.setInt(idx++, target_version.getId().intValue());
        ps.setInt(idx++, operating_system.getId().intValue());
        ps.setInt(idx++, platform.getId().intValue());
        ps.setInt(idx++, browser.getId().intValue());
        ps.setString(idx++, location);
        ps.setInt(idx++, priority.intValue());
        ps.setInt(idx++, severity.intValue());
        ps.setInt(idx++, dev_assigned.getId().intValue());
        ps.setInt(idx++, qa_assigned.getId().intValue());
        ps.setInt(idx++, duplicate_of.getId().intValue());
        if (!is_new) {
            ps.setInt(idx++, getId().intValue());
        }
        ps.executeUpdate();
        ps.close();
        // Only increment the mod_count if there was no exception during the save.
        setModCount(new Integer(new_mod_count));
    }

    public int compareTo (Object o) {
        return ((Bug)o).getBugNumber().compareTo(this.getBugNumber());
    }

    public java.util.AbstractList listAll () {
        java.util.AbstractList bugs = DatabaseRecord.listRecords(Bug.class);
        java.util.Collections.sort(bugs);
        return bugs;
    }
    
}
