package sigue.btrack;

import java.sql.*;

public class Platform extends NamedRecord {
    
    public Platform () {
        super();
    }

    private Platform (Integer id) {
        super(id);
    }

    private static Platform prototype = new Platform(new Integer(DatabaseRecord.NO_RECORD));

    public static Platform getPrototype () {
        return Platform.prototype;
    }

    protected String tableName () {
        return "tbl_platform";
    }

    protected String recordIDColumnName () {
        return "platform_id";
    }

    public static Platform loadPlatform (Integer id) {
        return (Platform) DatabaseRecord.loadRecord(id, Platform.class, true);
    }

    public static Platform loadPlatform (ResultSet rset)
    {
        return (Platform) DatabaseRecord.loadRecord(rset, Platform.class);
    }

}
