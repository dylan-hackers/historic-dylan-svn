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
        return (Browser) DatabaseRecord.loadRecord(id, Browser.class, true);
    }

    public static Browser loadBrowser (ResultSet rset)
    {
        return (Browser) DatabaseRecord.loadRecord(rset, Browser.class);
    }

}
