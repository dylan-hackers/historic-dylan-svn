package sigue.btrack;

import java.sql.*;

public class Browser extends NamedRecord {
    
    public Browser () {
        super();
    }

    private Browser (Integer id) {
        super(id);
    }

    private static Browser prototype = new Browser(new Integer(DatabaseRecord.NO_RECORD));

    public static Browser getPrototype () {
        return Browser.prototype;
    }

    protected String tableName () {
        return "tbl_browser";
    }

    protected String recordIDColumnName () {
        return "browser_id";
    }

    public static Browser loadBrowser (Integer id) {
        return (Browser) DatabaseRecord.loadRecord(id, Browser.class);
    }

    public static Browser loadBrowser (ResultSet rset, boolean must_be_unique)
        throws BugTrackException
    {
        return (Browser) DatabaseRecord.loadRecord(rset, Browser.class, must_be_unique);
    }

}
