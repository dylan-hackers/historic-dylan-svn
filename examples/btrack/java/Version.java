package sigue.btrack;

import java.sql.*;

public class Version extends NamedRecord {
    
    // ---TODO: add a product field

    public Version () {
        super();
    }

    private Version (Integer id) {
        super(id);
    }

    private static Version prototype = new Version(new Integer(DatabaseRecord.NO_RECORD));

    public static Version getPrototype () {
        return Version.prototype;
    }

    protected String tableName () {
        return "tbl_version";
    }

    protected String recordIDColumnName () {
        return "version_id";
    }

    public static Version loadVersion (Integer id) {
        return (Version) DatabaseRecord.loadRecord(id, Version.class);
    }

    public static Version loadVersion (ResultSet rset, boolean must_be_unique)
        throws BugTrackException
    {
        return (Version) DatabaseRecord.loadRecord(rset, Version.class, must_be_unique);
    }

}
