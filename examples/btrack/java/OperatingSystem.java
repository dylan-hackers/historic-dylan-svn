package sigue.btrack;

import java.sql.*;

public class OperatingSystem extends NamedRecord {
    
    public OperatingSystem () {
        super();
    }

    private OperatingSystem (Integer id) {
        super(id);
    }

    private static OperatingSystem prototype = new OperatingSystem(new Integer(DatabaseRecord.NO_RECORD));

    public static OperatingSystem getPrototype () {
        return OperatingSystem.prototype;
    }

    protected String tableName () {
        return "tbl_operating_system";
    }

    protected String recordIDColumnName () {
        return "opsys_id";      // hmmm.  this doesn't follow the usual naming pattern.
    }

    public static OperatingSystem loadOperatingSystem (Integer id) {
        return (OperatingSystem) DatabaseRecord.loadRecord(id, OperatingSystem.class, true);
    }

    public static OperatingSystem loadOperatingSystem (ResultSet rset)
    {
        return (OperatingSystem) DatabaseRecord.loadRecord(rset, OperatingSystem.class);
    }

}
